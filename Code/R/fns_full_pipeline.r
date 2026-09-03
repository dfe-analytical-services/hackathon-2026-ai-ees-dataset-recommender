#' Run the complete EES dataset discovery workflow
#'
#' Takes a user's natural-language data request and uses an LLM together
#' with the Explore Education Statistics (EES) API to identify the most
#' relevant publication and dataset.
#'
#' The workflow consists of two stages:
#' \enumerate{
#'   \item Select the most relevant EES publication for the user's request.
#'   \item Retrieve the datasets within that publication and select the
#'         most relevant dataset.
#' }
#'
#' The function returns the original question along with the LLM's
#' structured publication and dataset selections, including reasoning
#' and confidence scores.
#'
#' @param user_question A character string containing the user's natural-
#'   language data request.
#'
#' @return A list containing:
#' \describe{
#'   \item{user_question}{The original user question.}
#'   \item{publication}{A structured result containing the selected
#'     publication ID, title, reasoning, and confidence.}
#'   \item{dataset}{A structured result containing the selected dataset
#'     ID, title, reasoning, and confidence.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- run_ees_workflow(
#'   "What dataset has data on NEET people"
#' )
#'
#' result$publication$publication_id
#' result$publication$publication_title
#' result$publication$reasoning
#' result$publication$confidence
#'
#' result$dataset$dataset_id
#' result$dataset$dataset_title
#' result$dataset$reasoning
#' result$dataset$confidence
#' }
#'
#' @export
run_ees_workflow <- function(user_question) {
  # Setup LLM
  chat <- setup_llm_chat()

  # Get publication catalogue
  publications <- get_all_ees_publications_json()

  # Step 1: Select publication
  publication_result <- select_publication(
    user_question = user_question,
    publications = publications,
    chat = chat
  )

  # Step 2: Select dataset
  dataset_result <- select_dataset(
    user_question = user_question,
    publication = publication_result,
    chat = chat
  )
  
  # Step 3: Get dataset URL
  dataset_urls <- vapply(
    dataset_result$datasets$dataset_id,
    get_dataset_url,
    character(1)
  )
  
  # Return results
  list(
    user_question = user_question,
    publication = publication_result,
    dataset = dataset_result,
    dataset_links = dataset_urls
  )
}