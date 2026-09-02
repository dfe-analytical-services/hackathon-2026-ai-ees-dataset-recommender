################################################################################
# Initial playing around
################################################################################
chat <- ellmer::chat_databricks(
  # Automatically uses DATABRICKS_HOST env var if `workspace` not set
  system_prompt = NULL,
  model = "databricks-meta-llama-3-1-70b-instruct",
  token = Sys.getenv("DATABRICKS_TOKEN")
)

chat$chat("What is Explore Education Statistics")


eesyapi::get_publications()$title
eesyapi::get_publications()$summary


################################################################################
# Turning Daniel's pipeline into ellmer friendly
################################################################################

################################################################################
# Step 1: Identify the most relevant publication(s?)
################################################################################

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------
# Get EES publication data and convert to JSON for LLM
ees_publications <- eesyapi::get_publications() %>%
  select(-c(lastPublished, slug)) %>%
  jsonlite::toJSON(
    dataframe = "rows",
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )

# Setup databricks chat with system prompt
chat <- ellmer::chat_databricks(
  model = "databricks-meta-llama-3-1-70b-instruct",
  token = Sys.getenv("DATABRICKS_TOKEN"),
  system_prompt = "
You are an expert at using the Explore Education Statistics API.

Your job is to identify the publication most relevant to the
user's data request.

Use the supplied publication metadata to make your decision.
Do not invent publication IDs.
"
)

# Define LLM output layout
publication_type <- ellmer::type_object(
  publication_id = ellmer::type_string(
    description = "The ID of the most relevant publication."
  ),
  reasoning = ellmer::type_string(
    description = "Brief explanation of why this publication is the best match."
  ),
  confidence = ellmer::type_number(
    description = "Confidence in the selection, from 0 to 1."
  )
)


# ------------------------------------------------------------------------------
# Test LLM output
# ------------------------------------------------------------------------------
# User question
user_question <- "What dataset has data on NEET people"

# LLM call and output
result <- chat$chat_structured(
  paste0(
    "User request:\n",
    user_question,
    "\n\nAvailable publications:\n",
    ees_publications
  ),
  type = publication_type
)

# Output
# Publication ID
result$publication_id

# Publication title
eesyapi::get_publications() |>
  dplyr::filter(id == result$publication_id) |>
  dplyr::select(title)

# LLM reasoning
result$reasoning

# LLM confidence
result$confidence


################################################################################
# Step 2: From the publication, identify the most relevant dataset(s?)
################################################################################

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------
# Get the datasets within the identified publication
publication_datasets <- eesyapi::get_data_catalogue(
  publication_id = result$publication_id
)

# Function to build the dataset search LLM query
# Retrieve datasets within a publication
get_publication_datasets <- function(publication_id) {
  eesyapi::get_data_catalogue(
    publication_id = publication_id
  ) |>
    dplyr::select(-latestVersion)
}

# Make the function available to the LLM
get_publication_datasets_tool <- ellmer::tool(
  get_publication_datasets,
  name = "get_publication_datasets",
  description = "
Retrieve the datasets available within a selected Explore Education
Statistics publication.

Use this after identifying the relevant publication. The returned
dataset catalogue should be inspected to determine which dataset
best matches the user's request.
",
  arguments = list(
    publication_id = ellmer::type_string(
      description = "The ID of the selected publication."
    )
  )
)
chat$register_tool(get_publication_datasets_tool)

# Define structured dataset output
dataset_type <- ellmer::type_object(
  dataset_id = ellmer::type_string(
    description = "The ID of the most relevant dataset."
  ),
  dataset_title = ellmer::type_string(
    description = "The title of the most relevant dataset."
  ),
  reasoning = ellmer::type_string(
    description = "Brief explanation of why this dataset is the best match."
  ),
  confidence = ellmer::type_number(
    description = "Confidence in the selection, from 0 to 1."
  )
)

# Ask LLM to find the relevant dataset
response <- chat$chat(
  paste0(
    "The relevant publication has been identified as: ",
    result$publication_id,
    "\n\n",
    "Now find the most relevant dataset for this user request:\n",
    user_question
  )
)

# Extract the final decision as structured data
dataset_result <- chat$chat_structured(
  "Based on the dataset catalogue you just retrieved, identify the single ",
  "most relevant dataset for the user's request.",
  type = dataset_type
)

# Outputs
dataset_result$dataset_id
dataset_result$dataset_title
dataset_result$reasoning
dataset_result$confidence
