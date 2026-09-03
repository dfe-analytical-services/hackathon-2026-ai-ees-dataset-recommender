############################################################
# SERVER
############################################################

server <- function(input, output, session) {
  
  ##########################################################
  # APP STATE
  ##########################################################
  
  stage <- reactiveVal("idle")
  
  ##########################################################
  # RUN QUERY
  ##########################################################
  
  query_result <- eventReactive(input$submit_question, {
    
    req(input$user_question)
    req(nchar(trimws(input$user_question)) > 0)
    
    run_ees_workflow(input$user_question)
    
  })
  
  ##########################################################
  # SUBMIT BUTTON
  ##########################################################
  
  observeEvent(input$submit_question, {
    stage("loading")
  })
  
  ##########################################################
  # MOVE TO DONE ONCE RESULT EXISTS
  ##########################################################
  
  observeEvent(query_result(), {
    stage("done")
  })
  
  ##########################################################
  # RESET BUTTON
  ##########################################################
  
  observeEvent(input$start_again, {
    
    updateTextAreaInput(
      session = session,
      inputId = "user_question",
      value = ""
    )
    
    stage("idle")
  })
  
  ##########################################################
  # RESULTS UI
  ##########################################################
  
  output$results_ui <- renderUI({
    
    ########################################################
    # IDLE
    ########################################################
    
    if (stage() == "idle") {
      
      return(
        tagList(
          
          h3("How it works"),
          
          tags$ol(
            tags$li("Enter a question about education statistics."),
            tags$li("The Databricks LLM will identify the most relevant publication."),
            tags$li("The EES datasets for that publication will be evaluated."),
            tags$li("The most relevant dataset will be returned.")
          )
          
        )
      )
    }
    
    ########################################################
    # LOADING
    ########################################################
    
    if (stage() == "loading") {
      
      return(
        tagList(
          
          h3("Searching..."),
          
          tags$p(
            "Please wait while we find the most relevant dataset."
          )
          
        )
      )
    }
    
    ########################################################
    # DONE
    ########################################################
    
    if (stage() == "done") {
      
      result <- query_result()
      
      return(
        tagList(
          
          h2("Recommended dataset"),
          
          tags$p(
            tags$strong("Publication name: "),
            result$publication$publication_title
          ),
          
          tags$p(
            tags$strong("Dataset name: "),
            result$dataset$dataset_title
          ),
          
          # tags$p(
          #   tags$strong("Dataset ID: "),
          #   result$dataset$dataset_id
          # ),
          
          tags$p(
            tags$strong("Link to dataset: "),
            shiny::a(
              href = result$dataset_link,
              result$dataset_link,
              target = "_blank"
            )
          ),
          
          tags$p(
            tags$strong("Confidence: "),
            result$dataset$confidence
          ),
          
          tags$p(
            tags$strong("Reasoning:")
          ),
          
          tags$p(result$dataset$reasoning),
          
          tags$hr(),
          
          h2("Selected publication"),
          
          tags$p(
            tags$strong("Title: "),
            result$publication$publication_title
          ),
          
          # tags$p(
          #   tags$strong("Publication ID: "),
          #   result$publication$publication_id
          # ),
          
          tags$p(
            tags$strong("Confidence: "),
            result$publication$confidence
          ),
          
          tags$p(
            tags$strong("Reasoning:")
          ),
          
          tags$p(result$publication$reasoning),
          
          br(),
          
          actionButton(
            inputId = "start_again",
            label = "Ask another question",
            class = "govuk-button govuk-button--secondary"
          )
          
        )
      )
    }
    
    NULL
    
  })
  
  ##########################################################
  # OPTIONAL ERROR HANDLING
  ##########################################################
  
  observeEvent(input$submit_question, {
    
    tryCatch({
      
      query_result()
      
    }, error = function(e) {
      
      stage("idle")
      
      showNotification(
        paste("Error:", e$message),
        type = "error",
        duration = 10
      )
      
    })
    
  })
  
}