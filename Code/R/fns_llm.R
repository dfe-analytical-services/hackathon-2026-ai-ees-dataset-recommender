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
        "Confidence in the publication selection, from 0 to 1. 
        0 means the dataset does not exist.
        1 means you are certain this is the correct data set.
        0.5 means you were 50/50 between multiple datasets."
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
  
  dataset_catalogue <- get_publication_datasets_json(
    publication$publication_id
  )
  
  dataset_item_type <- ellmer::type_object(
    dataset_id = ellmer::type_string(
      description = paste(
        "The ID of the dataset.",
        "This must exactly match an ID in the supplied dataset catalogue."
      )
    ),
    dataset_title = ellmer::type_string(
      description = "The title of the dataset."
    ),
    reasoning = ellmer::type_string(
      description = paste(
        "Brief explanation of why this dataset is relevant",
        "to the user's request."
      )
    ),
    confidence = ellmer::type_number(
      description = paste(
        "Confidence in this dataset selection, from 0 to 1.",
        "0 means the dataset does not exist.",
        "1 means you are certain this is the correct dataset.",
        "0.5 means you were 50/50 between multiple datasets."
      )
    )
  )
  
  dataset_type <- ellmer::type_object(
    datasets = ellmer::type_array(
      items = dataset_item_type,
      description = paste(
        "Up to 3 datasets ranked from most relevant",
        "to least relevant."
      )
    )
  )
  
  result <- chat$chat_structured(
    paste0(
      "Task: Identify up to 3 datasets that are most relevant ",
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
      "- Return between 1 and 3 datasets.\n",
      "- Rank datasets from most relevant to least relevant.\n",
      "- Consider the meaning of the request, not just keyword matches.\n",
      "- Choose datasets most likely to contain the required data.\n",
      "- Select only IDs present in the supplied dataset catalogue.\n",
      "- If only one dataset is clearly relevant, return only one dataset.\n",
      "- Provide a confidence score for each selection."
    ),
    type = dataset_type
  )
  
  # Keep only datasets with confidence > 0.8
  result$datasets <- subset(
    result$datasets,
    confidence > 0
  )
  
  # Defensive cap in case the model returns more than requested
  result$datasets <- head(result$datasets, 3)
  
  result
}
