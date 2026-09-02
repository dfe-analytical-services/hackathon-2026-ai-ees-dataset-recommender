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
  You are an expert in the Explore Education Statistics (EES) API.

  Your role is to help identify and interpret education statistics
  data that best answers the user's request.

  You will be provided with metadata from the EES API and should
  use that information to make evidence-based decisions.

  Only select IDs that are present in the supplied metadata.
  Do not invent publication IDs, dataset IDs, or other identifiers.

  When selecting between multiple options, consider the user's
  actual data requirement rather than relying solely on matching
  keywords.
  "
)

# Define LLM output layout
publication_type <- ellmer::type_object(
  publication_id = ellmer::type_string(
    description = "The ID of the most relevant publication."
  ),
  publication_title = ellmer::type_string(
    description = paste(
      "The title of the most relevant publication."
    )
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
    "Task: Identify the single publication that is most relevant ",
    "to the user's data request.\n\n",

    "User request:\n",
    user_question,
    "\n\n",

    "Available publications:\n",
    ees_publications,
    "\n\n",

    "Selection guidance:\n",
    "- Choose the publication whose subject matter and coverage ",
    "best match the user's request.\n",
    "- Consider the meaning of the request, not just keyword matches.\n",
    "- Prefer a publication that is likely to contain the data needed ",
    "to answer the request.\n",
    "- Select only an ID that appears in the supplied publication metadata."
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
    "Publication title: ",
    publication_step_result$publication_title,
    "\n\n",
    "Available datasets within this publication:\n",
    publication_datasets,
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
