library(ggplot2)
library(dplyr)
library(cachem)
data_cache <- cache_mem()

# UI du module donut
Metropole_EP_AnneePose_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("donut_plot")) # Correspond à l'identifiant dans le serveur
  )
}

# Server du module donut
Metropole_EP_AnneePose_server <- function(id, data, group_column, colors) {
  moduleServer(id, function(input, output, session) {


    prepare_data <- reactive({
      cached_data <- data_cache$get("prepare_data")
      if (!is.null(cached_data) && length(cached_data) > 0) {
        print("Cache récupéré avec succès :")
        print(head(cached_data))
        if (!inherits(cached_data, "data.frame")) {
          stop("Les données mises en cache ne sont pas un data.frame valide.")
        }
        return(cached_data)
      }

      # Si le cache est vide ou absent, recalculer les données
      print("Aucune donnée en cache, recalcul...")
      data_connues <- data %>% filter(!is.na(anneepose))
      data_estimees <- data %>%
        filter(is.na(anneepose) & (!is.na(anneeposesuperieur) | !is.na(anneeposeinferieur))) %>%
        mutate(anneepose_est = round(rowMeans(cbind(anneeposesuperieur, anneeposeinferieur), na.rm = TRUE)))

      result <- bind_rows(
        data_connues %>% group_by(anneepose) %>%
          summarise(Longueur = sum(longueur, na.rm = TRUE)/1000, .groups = "drop") %>%
          mutate(Type = "Connue"),
        data_estimees %>% group_by(anneepose_est) %>%
          summarise(Longueur = sum(longueur, na.rm = TRUE)/1000, .groups = "drop") %>%
          mutate(Type = "Estimée")
      )

      print("Données calculées, mise en cache :")
      print(head(result))
      data_cache$set("prepare_data", result) # Mise en cache
      return(result)
    })



    plot_data <- reactive({
      cached_plot_data <- data_cache$get("plot_data")
      if (!is.null(cached_plot_data)) {
        return(cached_plot_data)
      }

      # Préparation des données finales
      plot_result <- prepare_data() %>%
        group_by(Type) %>%
        summarise(Longueur = sum(Longueur, na.rm = TRUE))

      data_cache$set("plot_data", plot_result) # Mise en cache
      plot_result
    })



    # Génération du graphique en donut
    output$donut_plot <- renderPlot({
      plot_data <- prepare_data() %>%
        group_by(Type) %>%
        summarise(Longueur = sum(Longueur, na.rm = TRUE))

      ggplot(plot_data, aes(x = 2, y = Longueur, fill = Type)) +
        geom_col(width = 0.5) +
        coord_polar(theta = "y") +
        theme_void() +
        scale_fill_manual(values = colors) +
        xlim(0.5, 2.5)
    })


  })
}
