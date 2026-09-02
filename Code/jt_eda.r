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
publication_step_result <- chat$chat_structured(
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
publication_step_result$publication_id

# Publication title
eesyapi::get_publications() |>
  dplyr::filter(id == publication_step_result$publication_id) |>
  dplyr::select(title)

# LLM reasoning
publication_step_result$reasoning

# LLM confidence
publication_step_result$confidence


################################################################################
# Step 2: From the publication, identify the most relevant dataset(s?)
################################################################################

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------
# Get the datasets within the identified publication
publication_datasets <- eesyapi::get_data_catalogue(
  publication_id = publication_step_result$publication_id
) |>
  dplyr::select(-latestVersion) |>
  jsonlite::toJSON(
    dataframe = "rows",
    auto_unbox = TRUE,
    na = "null"
  )

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

# Extract decision as structured data
dataset_step_result <- chat$chat_structured(
  paste0(
    "User request:\n",
    user_question,
    "\n\n",
    "The relevant publication has already been identified.\n",
    "Publication ID: ",
    result$publication_id,
    "\n\n",
    "Available datasets within this publication:\n",
    datasets,
    "\n\n",
    "Select the single dataset that is most relevant to the user's request."
  ),
  type = dataset_type
)

# Outputs
dataset_step_result$dataset_id
dataset_step_result$dataset_title
dataset_step_result$reasoning
dataset_step_result$confidence
