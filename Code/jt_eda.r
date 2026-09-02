chat <- ellmer::chat_databricks(
  # Automatically uses DATABRICKS_HOST env var if `workspace` not set
  system_prompt = NULL,
  model = "databricks-meta-llama-3-1-70b-instruct",
  token = Sys.getenv("DATABRICKS_TOKEN")
)

chat$chat("What is Explore Education Statistics")


eesyapi::get_publications()$title
eesyapi::get_publications()$summary
