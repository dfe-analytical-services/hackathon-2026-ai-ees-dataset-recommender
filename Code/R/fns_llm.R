# Setup databricks chat with system prompt for EES dataset
setup_llm_chat <- function() {
  ellmer::chat_databricks(
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
}


# Step 1 - Select most relevant publication ID from user query
select_publication <- function(user_question, publications, chat) {
  publication_type <- ellmer::type_object(
    publication_id = ellmer::type_string(
      description = paste(
        "The ID of the single most relevant publication.",
        "This must exactly match an ID in the supplied metadata."
      )
    ),
    publication_title = ellmer::type_string(
      description = paste(
        "The title of the most relevant publication."
      )
    ),
    reasoning = ellmer::type_string(
      description = paste(
        "Brief explanation of why this publication is the best",
        "match for the user's data request."
      )
    ),
    confidence = ellmer::type_number(
      description = paste(
        "Confidence in the publication selection, from 0 to 1."
      )
    )
  )

  chat$chat_structured(
    paste0(
      "Task: Identify the single publication that is most relevant ",
      "to the user's data request.\n\n",
      "User request:\n",
      user_question,
      "\n\n",
      "Available publications:\n",
      publications,
      "\n\n",
      "Selection guidance:\n",
      "- Consider the meaning of the request, not just keyword matches.\n",
      "- Choose the publication most likely to contain the required data.\n",
      "- Select only an ID present in the supplied metadata."
    ),
    type = publication_type
  )
}


# Step 2 -Select most relevant dataset from user query (and publication)
select_dataset <- function(
  user_question,
  publication,
  chat
) {
  dataset_catalogue <- get_publication_datasets_json(publication$publication_id)

  dataset_type <- ellmer::type_object(
    dataset_id = ellmer::type_string(
      description = paste(
        "The ID of the single most relevant dataset.",
        "This must exactly match an ID in the supplied dataset catalogue."
      )
    ),
    dataset_title = ellmer::type_string(
      description = paste(
        "The title of the most relevant dataset."
      )
    ),
    reasoning = ellmer::type_string(
      description = paste(
        "Brief explanation of why this dataset is the best",
        "match for the user's data request."
      )
    ),
    confidence = ellmer::type_number(
      description = paste(
        "Confidence in the dataset selection, from 0 to 1."
      )
    )
  )

  chat$chat_structured(
    paste0(
      "Task: Identify the single dataset that is most relevant ",
      "to the user's data request.\n\n",
      "User request:\n",
      user_question,
      "\n\n",
      "The relevant publication has already been identified.\n",
      "Publication title: ",
      publication$publication_title,
      "\n\n",
      "Available datasets within this publication:\n",
      dataset_catalogue,
      "\n\n",
      "Selection guidance:\n",
      "- Consider the meaning of the request, not just keyword matches.\n",
      "- Choose the dataset most likely to contain the required data.\n",
      "- Select only an ID present in the supplied dataset catalogue."
    ),
    type = dataset_type
  )
}
