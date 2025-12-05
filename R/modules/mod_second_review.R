# ==============================================================================
# SECOND REVIEW MODULE
# R/modules/mod_second_review.R
# ==============================================================================
#
# Displays second review interface for secondary reviewers
# Shows dual spider plots (self-assessment and coach milestones)
# Provides approval fields and comments
#
# ==============================================================================

#' Second Review Module UI
#'
#' @param id Module namespace ID
#' @return UI elements for second review
#' @export
mod_second_review_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Header with resident info and navigation buttons
    fluidRow(
      column(
        width = 12,
        div(
          style = "margin-bottom: 20px; display: flex; justify-content: space-between; align-items: center;",

          # Left side: Navigation buttons
          div(
            style = "display: flex; gap: 10px;",
            actionButton(
              ns("back_to_table"),
              "← Back to Residents",
              class = "btn-secondary",
              icon = icon("table")
            )
          ),

          # Right side: Resident info header
          div(
            style = "font-size: 16px; font-weight: bold;",
            uiOutput(ns("resident_header"))
          )
        )
      )
    ),

    hr(),

    # Main content
    div(
      style = "max-width: 1400px; margin: 0 auto;",

      # Section 1: Milestone Comparison
      div(
        class = "card mb-4",
        div(
          class = "card-header bg-info text-white",
          h4(class = "mb-0", icon("chart-line"), " Milestone Assessment Review")
        ),
        div(
          class = "card-body",
          p(class = "mb-4",
            "Review the resident's self-assessment and the primary coach's milestone ratings below."
          ),

          # Dual spider plots side by side
          fluidRow(
            column(
              width = 6,
              h5(icon("user"), " Resident Self-Assessment"),
              plotly::plotlyOutput(ns("self_milestone_plot"), height = "450px")
            ),
            column(
              width = 6,
              h5(icon("user-tie"), " Coach Milestone Ratings"),
              plotly::plotlyOutput(ns("coach_milestone_plot"), height = "450px")
            )
          )
        )
      ),

      # Section 2: Coach ILP Final Comments
      div(
        class = "card mb-4",
        div(
          class = "card-header bg-success text-white",
          h4(class = "mb-0", icon("clipboard"), " Coach ILP Final Comments")
        ),
        div(
          class = "card-body",
          uiOutput(ns("coach_ilp_display"))
        )
      ),

      # Section 3: Second Reviewer Entry
      div(
        class = "card mb-4",
        div(
          class = "card-header bg-primary text-white",
          h4(class = "mb-0", icon("check-square"), " Second Reviewer Assessment")
        ),
        div(
          class = "card-body",

          # Approval question
          div(
            class = "form-group",
            tags$label(
              class = "font-weight-bold",
              "Do you agree with the milestones as reported by the primary coach?"
            ),
            div(
              style = "margin-top: 10px;",
              radioButtons(
                ns("second_approve"),
                label = NULL,
                choices = c("Yes" = "1", "No" = "0"),
                selected = character(0),
                inline = TRUE
              )
            )
          ),

          hr(),

          # Conditional milestone comments (only show if disagree)
          uiOutput(ns("milestone_comments_section")),

          # General comments
          div(
            class = "form-group",
            tags$label(
              class = "font-weight-bold",
              "Comments on ILP or anything else:"
            ),
            textAreaInput(
              ns("second_comments"),
              label = NULL,
              rows = 6,
              placeholder = "Enter your comments on the ILP, goals, or any other aspects of the review..."
            )
          )
        )
      )
    ),

    hr(),

    # Submit button at bottom
    fluidRow(
      column(
        width = 12,
        style = "margin-top: 20px; margin-bottom: 20px;",
        div(
          style = "text-align: center;",
          actionButton(
            ns("submit_second_review"),
            "Submit Second Review",
            class = "btn-primary btn-lg",
            style = "padding: 10px 30px;",
            icon = icon("check-circle")
          ),
          br(),
          br(),
          p(
            style = "color: #666; font-size: 14px;",
            "Complete all required fields before submitting"
          )
        )
      )
    )
  )
}

#' Second Review Module Server
#'
#' @param id Module namespace ID
#' @param selected_resident Reactive containing selected resident info
#' @param rdm_data Reactive containing all RDM data
#' @param current_period Reactive containing current period number
#' @export
mod_second_review_server <- function(id, selected_resident, rdm_data, current_period) {
  moduleServer(id, function(input, output, session) {

    # Reactive to get resident data
    resident_data <- reactive({
      req(selected_resident())
      req(rdm_data())
      req(current_period())

      # Extract values from reactives
      resident_id <- selected_resident()$record_id
      period_number <- current_period()
      data <- rdm_data()

      # Call helper function with actual values
      get_resident_period_data(
        rdm_data = data,
        record_id = resident_id,
        current_period = period_number,
        include_previous = FALSE
      )
    })

    # Display resident header
    output$resident_header <- renderUI({
      req(resident_data())
      req(current_period())

      res_data <- resident_data()$resident_info
      period_num <- current_period()

      HTML(sprintf(
        "<span style='color: #2c3e50;'>Second Review: %s</span> | <span style='color: #7f8c8d;'>%s | Period: %s</span>",
        res_data$full_name,
        res_data$Level,
        PERIOD_NAMES[period_num + 1]
      ))
    })

    # Get milestone workflow data
    milestone_results <- reactive({
      req(rdm_data())

      if (!is.null(rdm_data()$milestone_workflow)) {
        return(rdm_data()$milestone_workflow)
      }

      NULL
    })

    # Render self-assessment milestone plot
    output$self_milestone_plot <- plotly::renderPlotly({
      req(milestone_results())
      req(resident_data())
      req(current_period())

      tryCatch({
        period_name <- get_period_name(current_period())
        record_id <- resident_data()$resident_info$record_id

        # DEBUG: Log what period we're requesting
        message(sprintf("Second review self-assessment: requesting period '%s' for resident %s",
                       period_name, record_id))

        dashboard <- gmed::create_milestone_overview_dashboard(
          milestone_results = milestone_results(),
          resident_id = record_id,
          period_text = period_name,
          milestone_type = "self",
          milestone_system = "rep",
          resident_data = rdm_data()$residents
        )

        return(dashboard$spider_plot)

      }, error = function(e) {
        message("Error creating self milestone plot: ", e$message)
        return(plotly::plotly_empty() %>%
                 plotly::add_annotations(
                   text = "No self-assessment data available",
                   x = 0.5, y = 0.5, showarrow = FALSE,
                   font = list(size = 14, color = "gray")
                 ))
      })
    })

    # Render coach milestone plot
    output$coach_milestone_plot <- plotly::renderPlotly({
      req(milestone_results())
      req(resident_data())
      req(current_period())

      tryCatch({
        period_name <- get_period_name(current_period())
        record_id <- resident_data()$resident_info$record_id

        # DEBUG: Log what period we're requesting
        message(sprintf("Second review coach milestones: requesting period '%s' for resident %s",
                       period_name, record_id))

        dashboard <- gmed::create_milestone_overview_dashboard(
          milestone_results = milestone_results(),
          resident_id = record_id,
          period_text = period_name,
          milestone_type = "coach",
          milestone_system = "rep",
          resident_data = rdm_data()$residents
        )

        return(dashboard$spider_plot)

      }, error = function(e) {
        message("Error creating coach milestone plot: ", e$message)
        return(plotly::plotly_empty() %>%
                 plotly::add_annotations(
                   text = "No coach milestone data available",
                   x = 0.5, y = 0.5, showarrow = FALSE,
                   font = list(size = 14, color = "gray")
                 ))
      })
    })

    # Display coach ILP final comments
    output$coach_ilp_display <- renderUI({
      req(resident_data())

      curr_data <- resident_data()$current_period$coach_rev

      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(
          p(style = "font-style: italic; color: #95a5a6;",
            "No coach review available for this period")
        )
      }

      coach_ilp <- curr_data$coach_ilp_final[1]

      if (is.na(coach_ilp) || coach_ilp == "") {
        return(
          p(style = "font-style: italic; color: #95a5a6;",
            "Coach has not entered ILP final comments yet")
        )
      }

      div(
        class = "well",
        style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #28a745;",
        p(style = "white-space: pre-wrap;", coach_ilp)
      )
    })

    # Conditional milestone comments section
    output$milestone_comments_section <- renderUI({
      req(input$second_approve)

      if (input$second_approve == "0") {  # If "No" selected
        div(
          class = "form-group",
          div(
            class = "alert alert-warning",
            icon("exclamation-triangle"),
            " You indicated disagreement with the milestone ratings. Please explain your concerns below."
          ),
          tags$label(
            class = "font-weight-bold",
            "Comments on changes to Milestones:"
          ),
          textAreaInput(
            session$ns("second_miles_comment"),
            label = NULL,
            rows = 6,
            placeholder = "Explain which milestones you disagree with and why..."
          ),
          tags$hr()
        )
      } else {
        NULL
      }
    })

    # Back to table button
    back_to_table_clicked <- reactive({
      input$back_to_table
    })

    observeEvent(input$back_to_table, {
      showNotification(
        "Returning to resident table",
        type = "message",
        duration = 2
      )
    })

    # Handle submission
    observeEvent(input$submit_second_review, {
      req(resident_data())
      req(current_period())

      # Validate required fields
      if (is.null(input$second_approve) || input$second_approve == "") {
        showNotification(
          "Please indicate whether you agree with the milestone ratings",
          type = "warning",
          duration = 5
        )
        return()
      }

      # If disagree, require milestone comments
      if (input$second_approve == "0") {
        if (is.null(input$second_miles_comment) || trimws(input$second_miles_comment) == "") {
          showNotification(
            "Please provide comments explaining your disagreement with the milestones",
            type = "warning",
            duration = 5
          )
          return()
        }
      }

      # Show processing notification
      notification_id <- showNotification(
        tagList(icon("spinner", class = "fa-spin"), " Submitting second review..."),
        duration = NULL
      )

      # Build REDCap payload
      tryCatch({
        res_info <- resident_data()$resident_info
        period_num <- current_period()
        period_name <- get_period_name(period_num)

        # Extract numeric level
        level_num <- case_when(
          res_info$Level == "Intern" ~ 1,
          res_info$Level == "PGY2" ~ 2,
          res_info$Level == "PGY3" ~ 3,
          grepl("PGY[0-9]", res_info$Level) ~ as.numeric(gsub("[^0-9]", "", res_info$Level)),
          TRUE ~ NA_real_
        )

        # Calculate instance
        instance <- gmed::get_redcap_instance(
          level = level_num,
          period = period_name,
          review_type = "scheduled"
        )

        # Build second_review submission record
        second_review_record <- data.frame(
          record_id = res_info$record_id,
          redcap_repeat_instrument = "second_review",
          redcap_repeat_instance = instance,
          second_date = format(Sys.Date(), "%Y-%m-%d"),
          second_period = as.character(period_num),
          second_approve = as.character(input$second_approve),
          second_comments = as.character(input$second_comments %||% ""),
          second_miles_comment = as.character(input$second_miles_comment %||% ""),
          second_review_complete = "2",
          stringsAsFactors = FALSE
        )

        # Submit to REDCap
        result <- tryCatch({
          res <- REDCapR::redcap_write_oneshot(
            ds = second_review_record,
            redcap_uri = REDCAP_CONFIG$url,
            token = REDCAP_CONFIG$rdm_token
          )

          list(
            success = res$success,
            message = if (res$success) "Second review submitted" else res$outcome_message
          )
        }, error = function(e) {
          list(success = FALSE, message = e$message)
        })

        # Remove processing notification
        removeNotification(notification_id)

        # Check result
        if (result$success) {
          showModal(modalDialog(
            title = tagList(
              icon("check-circle", class = "text-success"),
              " Second Review Submitted Successfully"
            ),
            size = "m",
            easyClose = TRUE,
            footer = tagList(
              modalButton("Close"),
              actionButton(
                session$ns("return_to_table"),
                "Return to Resident Table",
                class = "btn-primary"
              )
            ),

            div(
              h4("Submitted Second Review"),
              p(
                tags$strong("Resident: "), res_info$full_name, br(),
                tags$strong("Period: "), PERIOD_NAMES[period_num + 1], br(),
                tags$strong("Date: "), format(Sys.Date(), "%B %d, %Y"), br(),
                tags$strong("Approved Milestones: "), if (input$second_approve == "1") "Yes" else "No"
              ),
              hr(),
              p("Your second review has been successfully submitted to REDCap.")
            )
          ))

          # Handle return to table button
          observeEvent(input$return_to_table, {
            removeModal()
          })

        } else {
          showNotification(
            tagList(
              icon("times-circle"),
              " Submission failed: ",
              result$message
            ),
            type = "error",
            duration = 15
          )
        }

      }, error = function(e) {
        removeNotification(notification_id)
        showNotification(
          tagList(
            icon("times-circle"),
            " Submission error: ",
            e$message
          ),
          type = "error",
          duration = 15
        )
      })
    })

    # Return reactive values
    return(
      list(
        back_to_table_clicked = back_to_table_clicked
      )
    )
  })
}
