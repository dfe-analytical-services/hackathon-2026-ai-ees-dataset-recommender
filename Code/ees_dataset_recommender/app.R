library(shiny)

source(here::here("Code/ees_dataset_recommender/global.R"))
source(here::here("Code/ees_dataset_recommender/ui.R"))
source(here::here("Code/ees_dataset_recommender/server.R"))

shinyApp(
  ui = ui,
  server = server
)
