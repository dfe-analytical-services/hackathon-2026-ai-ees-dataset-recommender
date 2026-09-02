ui <- function(input, output, session) {
  
  bslib::page_fluid(
    
    shinyjs::useShinyjs(),
    
    shinyGovstyle::header(
      org_name = "Department for Education",
      service_name = "EES Dataset Recommender"
    ),
    
    shinyGovstyle::banner(
      "beta",
      "Beta",
      "This service is experimental."
    ),
    
    shinyGovstyle::gov_main_layout(
      
      h1("Explore Education Statistics Dataset Recommender"),
      
      p(
        "Ask a question in plain English and we'll recommend the most relevant dataset."
      ),
      
      textAreaInput(
        inputId = "user_question",
        label = "What data are you looking for?",
        rows = 5,
        width = "100%",
        placeholder = "Example: What data do you have on NEET young people?"
      ),
      
      actionButton(
        "submit_question",
        "Find dataset",
        class = "govuk-button"
      ),
      
      tags$hr(),
      
      uiOutput("results_ui")
      
    ),
    
    shinyGovstyle::footer(full = TRUE)
  )
}