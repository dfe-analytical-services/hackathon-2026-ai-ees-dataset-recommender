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
