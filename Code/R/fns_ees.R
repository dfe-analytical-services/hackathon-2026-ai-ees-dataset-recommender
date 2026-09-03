# Get all EES publication data and convert to JSON for LLM
get_all_ees_publications_json <- function() {
  eesyapi::get_publications() |>
    dplyr::select(-c(lastPublished, slug)) |>
    jsonlite::toJSON(
      dataframe = "rows",
      pretty = TRUE,
      auto_unbox = TRUE,
      na = "null"
    )
}

# Get publication datasets and convert to JSON for LLM
get_publication_datasets_json <- function(publication_id) {
  eesyapi::get_data_catalogue(
    publication_id = publication_id
  ) |>
    dplyr::select(-latestVersion) |>
    jsonlite::toJSON(
      dataframe = "rows",
      pretty = TRUE,
      auto_unbox = TRUE,
      na = "null"
    )
}

# Get the dataset URL
get_dataset_url <- function(dataset_id) {
  response <- httr::GET(paste0(
    "https://api.education.gov.uk/statistics/v1/data-sets/",
    dataset_id
  ))
  response_data <- httr::content(response, "parsed")
  file_id <- response_data$latestVersion$file$id
  paste0(
    "https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/",
    file_id
  )
}
