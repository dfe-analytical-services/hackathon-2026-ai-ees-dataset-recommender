############################################################
# SERVER
############################################################

server <- function(input, output, session) {
  ##########################################################
  # APP STATE
  ##########################################################

  stage <- reactiveVal("idle")
  query_result <- reactiveVal(NULL)

  ##########################################################
  # RUN QUERY
  ##########################################################

  observeEvent(input$submit_question, {
    req(input$user_question)
    req(nchar(trimws(input$user_question)) > 0)

    stage("loading")
    query_result(NULL)

    tryCatch(
      {
        result <- run_ees_workflow(
          trimws(input$user_question)
        )

        query_result(result)
        stage("done")
      },
      error = function(e) {
        stage("error")

        showNotification(
          paste("Error:", e$message),
          type = "error",
          duration = 10
        )
      }
    )
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

    query_result(NULL)
    stage("idle")
  })

  ##########################################################
  # RESULTS UI
  ##########################################################

  output$results_ui <- renderUI({
    if (stage() == "idle") {
      return(
        tagList(
          h3("How it works"),

          tags$ol(
            tags$li(
              "Enter a question about education statistics."
            ),
            tags$li(
              "The Databricks LLM will identify the most relevant publication."
            ),
            tags$li(
              "The EES datasets for that publication will be evaluated."
            ),
            tags$li(
              "The most relevant dataset will be returned."
            )
          )
        )
      )
    }

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

    if (stage() == "error") {
      return(
        tagList(
          h2("Something went wrong"),

          tags$p(
            "We were unable to complete the search."
          ),

          actionButton(
            inputId = "start_again",
            label = "Ask another question",
            class = "govuk-button govuk-button--secondary"
          )
        )
      )
    }

    ########################################################
    # DONE
    ########################################################

    if (stage() == "done") {
      result <- query_result()

      # No publication identified
      if (
        is.null(result$publication$publication_title) ||
          identical(result$publication$publication_title, "") ||
          is.na(result$publication$publication_title)
      ) {
        return(
          tagList(
            h2("No relevant publication found"),

            tags$p(
              "A suitable publication could not be identified from the request."
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

      datasets <- result$dataset$datasets

      # Publication found but no datasets passed confidence threshold
      if (
        is.null(datasets) ||
          length(datasets) == 0
      ) {
        return(
          tagList(
            h2("No suitable dataset found"),

            tags$p(
              "A relevant publication was identified, but no dataset met ",
              "the minimum confidence threshold of 0.7."
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

            h4(ds$dataset_title),

            tags$p(
              tags$strong("Link to dataset: "),
              shiny::a(
                href = ds$dataset_url,
                ds$dataset_url,
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
              ds$reasoning
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
