# ─────────────────────────────────────────────────────────────
# utils_data.R – chargement pickle + helpers
# ─────────────────────────────────────────────────────────────
# Objectif : lire UN seul pickle "all_results_combined.pickle"
# qui est un dict Python avec des clés de 1er niveau :
# ['Metropole_EU_U','Metropole_EP','Commune_EU_U','Commune_EP',
#  'Systeme Assainissement_EU_U','Systeme Assainissement_EP',
#  'Bassin versant_EU_U','Bassin versant_EP','Captage_EU_U','Captage_EP']
# On expose :
#   - load_combined_pickle(path)
#   - list_sections(store)
#   - get_section(store, key) -> liste R
#   - data_alias_get(x, aliases) pour accéder de façon robuste
# ─────────────────────────────────────────────────────────────

library(reticulate)

.sa_val <- function(sec, key, field, i = 1) {
  d <- .sa_get(sec, key)
  if (is.null(d)) return(NA)
  v <- d[[field]]
  if (is.null(v) || length(v) < i) return(NA)
  v[[i]]
}


read_pickle_python <- function(path) {
  # 1) pickle natif (pas de dépendance numpy)
  pickle <- import("pickle")
  builtins <- import_builtins()
  f <- builtins$open(path, "rb")
  on.exit(f$close(), add = TRUE)
  obj <- NULL
  try({
    obj <- pickle$load(f)
  }, silent = TRUE)

  if (!is.null(obj)) return(obj)

  # 2) fallback pandas (si dispo)
  pd <- import("pandas", convert = FALSE)
  pd$read_pickle(path)
}


# Convertit un objet Python -> R (listes, dicts, df)
py_to_r_safe <- function(x) reticulate::py_to_r(x)

# Charge le pickle combiné UNE FOIS
load_combined_pickle <- function(path = "data/all_results_combined.pickle") {
  if (!file.exists(path)) {
    stop("Fichier introuvable : ", normalizePath(path, mustWork = FALSE))
  }
  py_obj <- read_pickle_python(path)
  store <- py_to_r_safe(py_obj)  # devrait devenir une liste nommée côté R
  if (!is.list(store)) stop("Le pickle n'est pas un dict/list Python.")
  store
}

# Liste les clés disponibles (niveaux)
list_sections <- function(store) names(store)

# Récupère une section (ex. "Metropole_EP") sous forme de liste R
get_section <- function(store, key) {
  if (is.function(store)) store <- store()  # au cas où on passe un réactif
  stopifnot(is.list(store))
  if (!key %in% names(store)) {
    warning("Clé absente dans le pickle : ", key)
    return(NULL)
  }
  store[[key]]
}



# ── Helpers robustes pour accéder aux champs (avec alias) ──

# Renvoie le premier champ trouvé parmi une liste d'alias
data_alias_get <- function(x, aliases, default = NULL) {
  for (nm in aliases) {
    if (!is.null(x[[nm]])) return(x[[nm]])
  }
  default
}

# Transforme un objet en nom->valeur numérique (pour les donuts)
as_named_numeric <- function(x) {
  # tolère : vecteur nommé, liste nommée, data.frame 2 colonnes (lib, val)
  if (is.null(x)) return(NULL)
  if (is.numeric(x) && !is.null(names(x))) return(x)
  if (is.list(x) && !is.null(names(x))) {
    vals <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
    names(vals) <- names(x)
    return(vals)
  }
  if (is.data.frame(x) && ncol(x) >= 2) {
    vals <- suppressWarnings(as.numeric(x[[2]]))
    nm   <- as.character(x[[1]])
    names(vals) <- nm
    return(vals)
  }
  NULL
}

# ─────────────────────────────────────────────
# Communes — colonne 'texte' uniquement
# ─────────────────────────────────────────────

# Nettoyage basique
.clean_string_vals <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- x[nchar(x) > 0]
  is_num <- suppressWarnings(!is.na(as.numeric(gsub("[^0-9.-]", "", x))))
  x[!is_num]
}

# Récupère les noms de communes à partir d'une section (clé Commune_EP / Commune_EU_U)
.extract_commune_names_from_section <- function(sec_obj) {
  res <- character()
  walk <- function(obj) {
    if (is.null(obj)) return()
    if (is.data.frame(obj) && "texte" %in% names(obj) && nrow(obj)) {
      vals <- .clean_string_vals(obj[["texte"]])
      if (length(vals)) res <<- c(res, vals)
    } else if (is.list(obj)) {
      lapply(obj, walk)
    }
  }
  walk(sec_obj)
  sort(unique(res))
}

# --- Helpers ASCII/encodage sûrs ---------------------------------------------

.normalize_ascii <- function(x) {
  # Trim + minuscule + suppression des accents (ASCII//TRANSLIT)
  x <- as.character(x)
  x <- trimws(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  tolower(x)
}

# --- Filtre les libellés qui ne sont PAS des communes ------------------------
# (on ne met que des motifs ASCII; pas d'accents; pas de perl=TRUE)
.is_commune_like <- function(x) {
  if (is.null(x)) return(FALSE)
  s <- .normalize_ascii(x)
  s <- s[nchar(s) > 0]
  if (!length(s)) return(FALSE)

  # motifs à exclure (libellés techniques/catégories)
  bad <- c(
    "pas de donn", "aucune", "total", "connus", "estime",
    "classe", "etat", "itv", "branchement", "branchements",
    # ages
    "ans", "^\\s*\\d+\\s*ans\\s*$", ">\\s*\\d+\\s*ans",
    # materiaux / categories frequentes
    "beton", "plastique", "fonte", "macon", "amiante",
    "autre", "parcelles?", "line", "lineaire", "longueur", " km", "%",
    "indice", "note", "precision"
  )

  # Grepl sans perl=TRUE pour eviter les soucis d'encodage
  is_bad <- Reduce(`|`, lapply(bad, function(p) grepl(p, s)))
  !is_bad
}


# --- Récupère les communes à partir d'une section (EU/EP)
.extract_communes_from_section <- function(sec) {
  if (is.null(sec) || !is.list(sec)) return(character())

  # Tables "fiables" où `texte` porte des communes
  preferred <- c("LRU", "LEP", "LREU")
  vals <- character()

  # 1) priorité aux tables fiables si présentes
  for (k in preferred) {
    if (!is.null(sec[[k]]) && is.data.frame(sec[[k]]) && "texte" %in% names(sec[[k]])) {
      vals <- c(vals, as.character(sec[[k]]$texte))
    }
  }

  # 2) fallback : toutes les tables qui ont `texte`
  if (!length(vals)) {
    for (k in names(sec)) {
      d <- sec[[k]]
      if (is.data.frame(d) && "texte" %in% names(d)) {
        vals <- c(vals, as.character(d$texte))
      }
    }
  }

  # nettoyage & filtre
  vals <- unique(trimws(vals))
  vals <- vals[nchar(vals) > 0]
  vals <- vals[ .is_commune_like(vals) ]

  sort(unique(vals))
}

# --- Liste finale des communes (union EU/EP), débarrassée des libellés parasites
list_communes <- function(store) {
  if (is.function(store)) store <- store()
  eu <- store[["Commune_EU_U"]]
  ep <- store[["Commune_EP"]]

  out <- sort(unique(c(
    .extract_communes_from_section(eu),
    .extract_communes_from_section(ep)
  )))

  out
}


# Filtre une section (EU/EP) sur une commune donnée
get_commune_section <- function(store, commune, type = c("EU","EP")) {
  type <- match.arg(type)
  if (is.function(store)) store <- store()
  key <- if (type == "EU") "Commune_EU_U" else "Commune_EP"
  sec <- store[[key]]
  if (is.null(sec) || !is.list(sec)) return(NULL)

  commune_chr <- as.character(commune)

  lapply(sec, function(d) {
    if (is.data.frame(d) && "texte" %in% names(d) && nrow(d)) {
      d2 <- d[ trimws(as.character(d[["texte"]])) == trimws(commune_chr), , drop = FALSE ]
      if (nrow(d2) > 0) return(d2)
    }
    d
  })
}
