library(shiny)
library(bslib)
library(shinychat)
library(ellmer)

## Connect to databricks model ================================================
chat <- ellmer::chat_databricks(
  # Automatically uses DATABRICKS_HOST env var if `workspace` not set
  system_prompt = NULL,
  model = "databricks-meta-llama-3-1-70b-instruct",
  token = Sys.getenv("DATABRICKS_TOKEN")
)

## Example basic app ==========================================================
ui <- bslib::page_fillable(
  chat_ui(
    id = "chat",
    messages = "Hey, hey, hey! How can I help you today?"
  ),
  fillable_mobile = TRUE
)

server <- function(input, output, session) {
  observeEvent(input$chat_user_input, {
    stream <- chat$stream_async(input$chat_user_input)
    chat_append("chat", stream)
  })
}

shinyApp(ui, server)
