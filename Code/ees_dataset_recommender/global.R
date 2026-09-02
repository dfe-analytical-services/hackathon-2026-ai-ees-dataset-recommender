# Libraries
library(shinyjs)
library(shinyGovstyle)
library(bslib)
library(here)
library(dplyr)
library(eesyapi)
library(jsonlite)
library(devtools)

# Source the LLM function function
source(here::here("Code", "R", "fns_ees.r"))
source(here::here("Code", "R", "fns_llm.r"))
source(here::here("Code", "R", "fns_full_pipeline.r"))

# ------------------------------------------------------------------
# DUMMY FUNCTION
# Replace this with your real Databricks/EES logic later
# ------------------------------------------------------------------

recommend_dataset <- function(user_question) {
  
  Sys.sleep(3) # simulate LLM/API processing
  
  list(
    link = "https://explore-education-statistics.service.gov.uk/",
    message = paste(
      "Based on your question:",
      shQuote(user_question),
      "this is the most relevant dataset."
    )
  )
}

site_title <- "DfE Explore Education Statistics Dataset Recommender"