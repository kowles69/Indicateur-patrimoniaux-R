# R/utils_data_sa.R — MINIMAL DIAG

`%||%` <- function(x,y) if (is.null(x) || length(x)==0) y else x
# ==== Helpers SA (communs) ===============================================

.sa_num <- function(x) suppressWarnings(as.numeric(x))

# trouve la colonne par nom exact ou motif
.sa_pick_col <- function(df, candidates) {
  if (!is.data.frame(df) || !ncol(df)) return(NA_character_)
  for (nm in candidates) if (nm %in% names(df)) return(nm)
  for (rx in candidates) {
    ix <- grep(rx, names(df), ignore.case = TRUE, perl = TRUE)
    if (length(ix)) return(names(df)[ix[1]])
  }
  NA_character_
}

# extrait une valeur (km) pour un SA donné (code), dans une table clé
sa_get_km <- function(section, key, code, col_candidates) {
  d <- section[[key]]
  if (is.null(d) || !is.data.frame(d) || !nrow(d)) return(NA_real_)
  # colonne du code
  col_code <- .sa_pick_col(d, c("nomsteu","code","sa","texte"))
  if (is.na(col_code)) return(NA_real_)
  # ligne correspondant au code
  r <- which(toupper(trimws(as.character(d[[col_code]]))) == toupper(code))
  if (!length(r)) return(NA_real_)
  # colonne valeur
  col_val <- .sa_pick_col(d, col_candidates)
  if (is.na(col_val)) return(NA_real_)
  .sa_num(d[r[1], col_val, drop = TRUE])
}

# formats d’affichage homogènes
sa_fmt_km <- function(x){
  v <- .sa_num(x)
  if (!is.finite(v)) "—" else paste0(format(round(v, 2), big.mark = " ", decimal.mark = ","), " km")
}
sa_fmt_eur <- function(x){
  v <- .sa_num(x)
  if (!is.finite(v)) "—" else paste0(format(round(v, 0), big.mark = " ", decimal.mark = ","), " €")
}

# --------- DEBUG WRAPPERS ---------
dbg_log <- function(...) { cat("[SA-DEBUG]", format(Sys.time(), "%H:%M:%S"), ":", ..., "\n") }

trap <- function(label, expr) {
  dbg_log("CALL  ", label)
  out <- try(eval.parent(substitute(expr)), silent = TRUE)
  if (inherits(out, "try-error")) {
    dbg_log("ERROR ", label, " -> ", conditionMessage(attr(out, "condition")))
    return(structure(list(`.error` = TRUE, `.label` = label, `.msg` = conditionMessage(attr(out, "condition"))),
                     class = "sa_debug_error"))
  }
  dbg_log("OK    ", label, " [class:", paste(class(out), collapse=","), "]")
  out
}

is_sa_err <- function(x) inherits(x, "sa_debug_error")

to_r_maybe <- function(x) {
  if (inherits(x, "python.builtin.object")) {
    y <- try(reticulate::py_to_r(x), silent = TRUE)
    if (!inherits(y, "try-error")) return(y)
  }
  x
}
# ----------------------------------


# 1) Brute section
.get_sa_store_section <- function(store_list, type_key) {
  store_list <- to_r_maybe(store_list)
  keys <- c(
    paste0("Systeme Assainissement_", type_key),
    paste0("Système d’assainissement_", type_key),
    paste0("Systeme d’assainissement_", type_key),
    paste0("Système d'assainissement_", type_key),
    paste0("Systeme d'assainissement_", type_key),
    paste0("SA_", type_key)
  )

  # LOG noms racine
  trap("names(store_list)", names(store_list))

  for (k in keys) {
    dbg_log("TRY key:", k)
    # a) accès direct par [[ ]]
    sec1 <- trap(paste0("[[", k, "]]"), store_list[[k]])
    if (!is_sa_err(sec1) && !is.null(sec1)) return(to_r_maybe(sec1))

    # b) via get_section() si tu as cette fonction
    sec2 <- trap(paste0("get_section(", k, ")"), get_section(store_list, k))
    if (!is_sa_err(sec2) && !is.null(sec2)) return(to_r_maybe(sec2))
  }
  NULL
}

# 2) Choix SA
sa_choices <- function(store_react){
  dbg_log("sa_choices()")
  trap("sa_code_label_map names", names(sa_code_label_map))
  trap("sa_code_label_map unname", unname(sa_code_label_map))
  stats::setNames(names(sa_code_label_map), unname(sa_code_label_map))
}

# 3) Section filtrée (provisoirement on renvoie la section brute)
get_sa_section <- function(store_react, type_key, code){
  dbg_log("get_sa_section(type_key=", type_key, ", code=", code, ")")
  store_list <- if (is.function(store_react)) trap("store_react()", store_react()) else store_react
  if (is_sa_err(store_list)) return(NULL)
  store_list <- to_r_maybe(store_list)

  sec <- trap(paste0(".get_sa_store_section(", type_key, ")"),
              .get_sa_store_section(store_list, type_key))
  if (is_sa_err(sec) || is.null(sec)) return(NULL)

  # LOG 1er niveau
  trap("names(sec)", names(sec))
  sec
}
