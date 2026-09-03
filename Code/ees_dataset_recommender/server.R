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
            tags$li(
              "The Databricks LLM will identify the most relevant publication."
            ),
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
      
      datasets <- result$dataset$datasets
      dataset_links <- result$dataset_links
      
      if (
        is.null(datasets) ||
        nrow(datasets) == 0
      ) {
        
        return(
          tagList(
            
            h2("No suitable dataset found"),
            
            tags$p(
              "A relevant publication was identified, but no dataset met ",
              "the minimum confidence threshold of 0.8."
            ),
            
            tags$hr(),
            
            h2("Selected publication"),
            
            tags$p(
              tags$strong("Title: "),
              result$publication$publication_title
            ),
            
            tags$p(
              tags$strong("Confidence: "),
              round(result$publication$confidence, 2)
            ),
            
            tags$p(
              tags$strong("Reasoning:")
            ),
            
            tags$p(
              result$publication$reasoning
            ),
            
            br(),
            
            actionButton(
              inputId = "start_again",
              label = "Ask another question",
              class = "govuk-button govuk-button--secondary"
            )
          )
        )
      }
      
      dataset_cards <- lapply(seq_len(nrow(datasets)), function(i) {
        
        ds <- datasets[i, ]
        
        dataset_link <- dataset_links[[i]]
        
        ranking_label <- switch(
          as.character(i),
          "1" = "🥇 Top recommendation",
          "2" = "🥈 Alternative recommendation",
          "3" = "🥉 Additional recommendation",
          paste("Recommendation", i)
        )
        
        tagList(
          
          tags$div(
            style = if (i == 1) {
              paste(
                "background:#f3f2f1;",
                "border-left:6px solid #1d70b8;",
                "padding:15px;",
                "margin-bottom:20px;"
              )
            } else {
              paste(
                "background:#ffffff;",
                "border-left:4px solid #b1b4b6;",
                "padding:15px;",
                "margin-bottom:20px;"
              )
            },
            
            h3(ranking_label),
            
            h4(as.character(ds$dataset_title)),
            
            tags$p(
              tags$strong("Link to dataset: "),
              shiny::a(
                href = dataset_link,
                dataset_link,
                target = "_blank"
              )
            ),
            
            tags$p(
              tags$strong("Confidence: "),
              round(as.numeric(ds$confidence), 2)
            ),
            
            tags$p(
              tags$strong("Reasoning:")
            ),
            
            tags$p(
              as.character(ds$reasoning)
            )
          )
        )
        
      })
      
      return(
        tagList(
          
          h2("Recommended datasets"),
          
          tags$p(
            "Datasets are ranked in order of relevance. ",
            tags$strong("The first dataset is the recommended dataset"),
            ", with subsequent datasets provided as alternatives."
          ),

          tags$p(
            tags$strong("Publication name: "),
            result$publication$publication_title
          ),
          
          dataset_cards,
          
          h2("Selected publication"),

          tags$p(
            tags$strong("Title: "),
            result$publication$publication_title
          ),

          tags$p(
            tags$strong("Confidence: "),
            round(result$publication$confidence, 2)
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
    tryCatch(
      {
        query_result()
      },
      error = function(e) {
        stage("idle")

        showNotification(
          paste("Error:", e$message),
          type = "error",
          duration = 10
        )
      }
    )
  })
}
