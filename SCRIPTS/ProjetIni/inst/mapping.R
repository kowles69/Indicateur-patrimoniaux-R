# Charger les packages
library(DBI)
library(RPostgres)
library(yaml)

# Fonction pour se connecter et récupérer les métadonnées des tables
generate_mapping <- function() {
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = "votre_nom_de_base",
    host = "votre_hote",
    port = 5432,
    user = "votre_utilisateur",
    password = "votre_mot_de_passe"
  )

  # Obtenir les noms de tables et colonnes de PostgreSQL
  tables <- dbListTables(con)
  columns <- lapply(tables, function(table) {
    dbListFields(con, table)
  })

  # Créer une structure de mappage
  mapping <- list(
    tables = list(
      data_table = "mesures",
      other_table = "details_mesures"
    ),
    columns = list(
      id = "identifiant",
      name = "nom",
      value = "valeur",
      date = "date_mesure"
    )
  )

  # Écrire le fichier YAML
  write_yaml(mapping, "inst/mapping.yaml")

  # Fermer la connexion
  dbDisconnect(con)
}

# Exécuter la fonction pour générer le fichier de mappage
generate_mapping()
