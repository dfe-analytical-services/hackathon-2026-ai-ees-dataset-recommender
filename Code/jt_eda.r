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
ees_publications <- eesyapi::get_publications() |>
  dplyr::select(-c(lastPublished, slug)) |>
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

  When no supplied metadata provides a sufficiently relevant match,
  do not force a selection. Prefer returning no match over making
  a weak or speculative recommendation.

  An incorrect recommendation is worse than returning no
  recommendation.

  When no suitable match exists, clearly explain why and identify
  reasonable alternatives where available.
  "
)

# Define LLM output layout
publication_type <- ellmer::type_object(
  publication_id = ellmer::type_string(
    description = paste(
      "The ID of the single most relevant publication.",
      "Return null if no publication is sufficiently relevant.",
      "The ID must exactly match an ID in the supplied metadata."
    ),
    required = FALSE
  ),
  publication_title = ellmer::type_string(
    description = paste(
      "The title of the most relevant publication.",
      "Return null if no publication is sufficiently relevant."
    ),
    required = FALSE
  ),
  reasoning = ellmer::type_string(
    description = paste(
      "Explain the publication selection.",
      "If a suitable publication exists, explain why it matches",
      "the user's request.",
      "If no suitable publication exists, explain why no",
      "recommendation was made and mention potentially relevant",
      "alternatives."
    )
  ),
  confidence = ellmer::type_number(
    description = paste(
      "Confidence in the publication selection, from 0 to 1.",
      "Use a low value when there is no suitable match."
    )
  ),
  alternatives = ellmer::type_array(
    ellmer::type_string(),
    description = paste(
      "IDs of potentially relevant or similar alternative real publications.",
      "A bit like a 'Did you mean...?'"
    ),
    required = FALSE
  )
)


# ------------------------------------------------------------------------------
# Test LLM output
# ------------------------------------------------------------------------------
# User question
user_question <- "Motorways in Europe"

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
    "- Choose a publication only if it is genuinely relevant to ",
    "the user's request.\n",
    "- Do not select a publication simply because it is the closest ",
    "available option.\n",
    "- Consider the meaning of the request, not just keyword matches.\n",
    "- Prefer a publication that is likely to contain the data needed ",
    "to answer the request.\n",
    "- If no publication is sufficiently relevant, return null for ",
    "publication_id and publication_title.\n",
    "- If there is no suitable publication, explain why and provide ",
    "potentially relevant alternatives.\n",
    "- Select only IDs that appear in the supplied metadata.\n",
    "- An incorrect recommendation is worse than returning no match."
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

# Suggested alternatives
publication_step_result$alternatives

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
