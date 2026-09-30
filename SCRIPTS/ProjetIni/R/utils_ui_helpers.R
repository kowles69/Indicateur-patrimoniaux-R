library(magrittr)  # pour %>%

# ================== Mapping SA ==================

sa_code_label_map <- c(
  "FE" = "La Feyssine",
  "FO" = "Fontaines",
  "GE" = "Genay",
  "GI" = "Givors",
  "JO" = "Jonage",
  "LB" = "Lissieu Le Bourg",
  "LS" = "Lissieu Sémanet",
  "ME" = "Meyzieu",
  "NE" = "Neuville",
  "PB" = "Pierre-Bénite",
  "QU" = "Quincieux",
  "SF" = "Saint-Fons",
  "SG" = "Saint-Germain"
)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Label lisible pour un code SA
sa_label <- function(code) {
  if (is.null(code) || !length(code) || is.na(code[1])) return("—")
  code <- as.character(code[1])

  # 1) cas "FE" -> "La Feyssine"
  if (code %in% names(sa_code_label_map)) {
    return(unname(sa_code_label_map[[code]]))
  }

  # 2) cas où on reçoit déjà "La Feyssine"
  if (code %in% unname(sa_code_label_map)) {
    return(code)
  }

  # 3) fallback
  code
}

# ================== Composant carte info ==================

.eu_info_card <- function(title, ...) {
  shiny::div(
    class = "info-card",
    shiny::div(class = "card-title", title),
    shiny::pre(class = "card-body", paste0(c(...), collapse = ""))
  )
}

# ================== Helpers format ==================

.eu_fmt_km_inline <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (!is.finite(v)) return("—")
  paste0(format(round(v, 0), big.mark = " ", decimal.mark = ","), " km")
}

.eu_fmt_nb_inline <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (!is.finite(v)) return("—")
  format(round(v, 0), big.mark = " ", decimal.mark = ",")
}

# ================== DONUT ==================
# - plus de catégorie "Autres"
# - on masque juste les étiquettes texte trop petites, mais
#   toutes les valeurs sont récupérables via le tableau PDF.

# ================== DONUT ==================

.eu_donut <- function(values, labels, title = "",
                      unit_km = TRUE,
                      show_text_min_pct = 0.08) {

  vals <- as.numeric(values)
  labs <- as.character(labels)

  ok <- is.finite(vals) & vals >= 0
  vals <- vals[ok]
  labs <- labs[ok]

  if (!length(vals)) {
    vals <- 1
    labs <- "Aucune donnée"
  }

  tot <- sum(vals)
  pct <- if (tot > 0) vals / tot else rep(0, length(vals))

  txt <- paste0(round(pct * 100, 1), "%")
  txt[pct < show_text_min_pct] <- ""   # les toutes petites parts : pas de texte

  hover <- if (unit_km) {
    paste0(
      "<b>", labs, "</b><br>",
      "Valeur : ", .eu_fmt_km_inline(vals), "<br>",
      "Part : ", round(pct * 100, 1), "%<extra></extra>"
    )
  } else {
    paste0(
      "<b>", labs, "</b><br>",
      "Valeur : ", .eu_fmt_nb_inline(vals), "<br>",
      "Part : ", round(pct * 100, 1), "%<extra></extra>"
    )
  }

  center_label <- if (unit_km) .eu_fmt_km_inline(tot) else .eu_fmt_nb_inline(tot)

  p <- plotly::plot_ly(
    type = "pie",
    labels = labs,
    values = vals,
    text = txt,
    textposition = "auto",   # <-- laisse Plotly décider inside/outside
    textinfo = "text",
    hovertemplate = hover,
    hole = 0.6,
    sort = FALSE
  )

  p <- plotly::layout(
    p,
    title = list(
      text = title,
      x = 0.5, y = 1.05,
      xanchor = "center", yanchor = "top"
    ),
    showlegend = TRUE,
    margin = list(t = 60, r = 40, b = 40, l = 10),  # un peu plus de marge
    uniformtext = list(minsize = 9, mode = "hide"),
    annotations = list(
      list(
        text = paste0("<b>Total</b><br>", center_label),
        showarrow = FALSE,
        font = list(size = 11),
        x = 0.5, y = 0.5, xref = "paper", yref = "paper"
      )
    )
  )

  p <- plotly::config(p, displayModeBar = FALSE)
  p
}


# ================== BARRES MATÉRIAUX ==================

.eu_bar_materials <- function(values, labels, title = "",
                              unit_km = TRUE,
                              show_text_min_pct = 0.06,
                              max_bars = 20) {

  v <- as.numeric(values)
  l <- as.character(labels)
  ok <- is.finite(v) & v >= 0
  v <- v[ok]
  l <- l[ok]

  if (!length(v)) v <- 0

  ord <- order(v, decreasing = TRUE)
  v <- v[ord]
  l <- l[ord]

  if (length(v) > max_bars) {
    v <- v[seq_len(max_bars)]
    l <- l[seq_len(max_bars)]
  }

  tot <- sum(v)
  pct <- if (tot > 0) v / tot else rep(0, length(v))

  txt <- if (unit_km) .eu_fmt_km_inline(v) else .eu_fmt_nb_inline(v)
  txt[pct < show_text_min_pct] <- ""

  hover <- if (unit_km) {
    paste0(
      "<b>", l, "</b><br>Valeur : ", .eu_fmt_km_inline(v),
      "<br>Part : ", round(pct * 100, 1), "%<extra></extra>"
    )
  } else {
    paste0(
      "<b>", l, "</b><br>Valeur : ", .eu_fmt_nb_inline(v),
      "<br>Part : ", round(pct * 100, 1), "%<extra></extra>"
    )
  }

  plotly::plot_ly(
    type = "bar",
    x = v,
    y = l,
    orientation = "h",
    text = txt,
    textposition = "outside",
    hovertemplate = hover,
    cliponaxis = FALSE
  ) %>%
    plotly::layout(
      title = list(text = title, x = 0, y = 0.98, xanchor = "left"),
      xaxis = list(title = "", automargin = TRUE),
      yaxis = list(title = "", automargin = TRUE,
                   categoryorder = "total ascending"),
      margin = list(t = 50, r = 30, b = 20, l = 10),
      uniformtext = list(minsize = 11, mode = "hide"),
      showlegend = FALSE
    ) %>%
    plotly::config(displayModeBar = FALSE)
}

# ================== BARRE HORIZONTALE EMPILÉE ==================
# (Connus / Estimés / Pas de données)
# -> couleurs distinctes + texte toujours visible sur les segments
#    suffisamment grands.

.eu_single_hstack <- function(values, labels, title = "", unit_km = TRUE) {

  v <- as.numeric(values)
  l <- as.character(labels)
  ok <- is.finite(v) & v >= 0
  v <- v[ok]
  l <- l[ok]

  if (!length(v)) {
    v <- 1
    l <- "Aucune donnée"
  }

  txt <- if (unit_km) .eu_fmt_km_inline(v) else .eu_fmt_nb_inline(v)

  df <- data.frame(
    seg   = l,
    value = v,
    text  = txt,
    stringsAsFactors = FALSE
  )

  hover <- "<b>%{customdata}</b><br>Valeur : %{text}<extra></extra>"

  p <- plotly::plot_ly()

  for (i in seq_len(nrow(df))) {
    p <- plotly::add_bars(
      p,
      x = df$value[i],
      y = "",
      orientation = "h",
      name = df$seg[i],
      text = df$text[i],
      customdata = df$seg[i],
      textposition = "inside",
      hovertemplate = hover,
      cliponaxis = FALSE
    )
  }

  p <- plotly::layout(
    p,
    barmode = "stack",
    title = list(text = title, x = 0, y = 0.98, xanchor = "left"),
    xaxis = list(visible = FALSE),
    yaxis = list(visible = FALSE),
    margin = list(t = 40, r = 10, b = 10, l = 10),
    uniformtext = list(minsize = 11, mode = "hide"),
    showlegend = TRUE
  )

  p <- plotly::config(p, displayModeBar = FALSE)
  p
}

