# install.packages("devtools")
library(devtools)
# Install eesyapi package from github
# devtools::install_github("dfe-analytical-services/eesyapi.R")
library(dplyr)
library(eesyapi)
library(jsonlite)
library(odbc)
library(DBI)

# Define the models to use, in order of preference. If the first doesn't work, the code shuld switch to the next one.
llm_models <- c("databricks-claude-sonnet-4-6"   # Strong reasoning and comparison of multiple inputs, best overall quality/cost balance
,"databricks-claude-opus-4-7"    # Strongest reasoning for filter inference, but significantly higher cost
,"databricks-gpt-oss-120b"       # Strong reasoning and structured extraction, potentially lower cost than Claude
,"databricks-qwen35-122b-a10b"   # Good extraction and classification capability, likely cheaper than Claude
,"databricks-claude-sonnet-4-5") # Slightly older Sonnet model, lower quality than 4.6 but still a strong option

# Find the dataset ids and metadata col ids
publications <- eesyapi::get_publications() %>%
  select(-c(lastPublished, slug)) %>%
  jsonlite::toJSON(
    dataframe = "rows",
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
    )

# Returned by the first LLM call
publication_id <- "8b7474f9-5870-4ecc-7557-08da5f64dcf1"

# Get the datasets based on the publication id
dataset_for_query <- eesyapi::get_data_catalogue(publication_id) %>%
  select(c(id, title, summary)) %>%
  jsonlite::toJSON(
    dataframe = "rows",
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )

# Returned by the second LLM call
dataset_id <- "0e2f9901-af3b-9f77-9e7d-3a93fd7c015e"

# Get the metadata for the dataset
metadata <- eesyapi::get_meta(dataset_id)

# Define the system prompt
system_request <- paste0("
You are an expert at using the Explore Education Statistics API. You need to pick the publication most relevant to the user's data request. Here are the publications:
", publications, "

You should return only the id of the most relevant publication. This should be a JSON. Do not add any description.
")

# Define the user prompt
user_request <- "What dataset has data on NEET people"

# Function to connect to DAtabricks LLM using the API, send the request, and store the response.
call_llm_with_fallback <- function(user_input = user_request, system_prompt = system_request, model_list = llm_models) {
  last_error <- NULL
  for (model in model_list) {
    
    message(sprintf("Trying model: %s", model ))
    
    url <- paste0(Sys.getenv("DATABRICKS_HOST"),  "/serving-endpoints/", model, "/invocations")
    
    # Build request body for chat completion API
    if (grepl("claude-opus", model)) {
      body <- list(messages = list(list(role = "system", content = system_prompt)
                                   ,list(role = "user", content = user_input)))  
    } else {
      body <- list(messages = list(list(role = "system", content = system_prompt)
                                   ,list(role = "user", content = user_input))
                   ,temperature = 0) # deterministic output (no randomness)
    }
    
    # Try model
    result <- tryCatch({
      response <- httr::POST(url
                             ,httr::add_headers(Authorization = paste("Bearer", Sys.getenv("DATABRICKS_TOKEN")))
                             ,encode = "json"
                             ,body = body)
      
      if (httr::status_code(response) >= 300) {
        stop(
          httr::content(response, as = "text",  encoding = "UTF-8"))
      }
      
      list(response = response, system_prompt = system_prompt, model_used = model)
      
    }, error = function(e) {
      
      message(sprintf("Model failed: %s", model))
      
      message(e$message)
      
      last_error <<- e
      
      NULL
      
    })
    
    if (!is.null(result)) {
      return(result)
    }
    
  }
  
  stop(sprintf("All models failed. Last error: %s", last_error$message))
}

result <- call_llm_with_fallback()
content <- httr::content(result$response, as = "parsed", simplifyVector = TRUE)
text <- content$choices$message$content
returned_publication_id <- fromJSON(text)$id

test <- eesyapi::get_publications() %>%
  filter(id == returned_publication_id)
