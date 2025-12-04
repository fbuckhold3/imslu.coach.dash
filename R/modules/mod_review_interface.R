# Review Interface Module
# Displays accordion with 7 review sections when resident is selected
#
# FIXES APPLIED:
# 1. Added "Back to Resident Table" button that works
# 2. Added "Change Coach" button to return to coach selection
# 3. Improved header layout with both navigation options
# 4. Added preview page before submission with milestone spider plot

# Helper function to create review preview with milestone spider plot
create_review_preview <- function(review_data, resident_data, current_period, session_ns) {

  res_info <- resident_data()$resident_info
  period_num <- current_period()
  period_name <- get_period_name(period_num)

  tagList(
    # Header info
    div(
      class = "card mb-3",
      div(
        class = "card-header bg-primary text-white",
        h4(class = "mb-0", "Review Summary")
      ),
      div(
        class = "card-body",
        p(
          tags$strong("Resident: "), res_info$full_name, br(),
          tags$strong("Level: "), res_info$Level, br(),
          tags$strong("Period: "), PERIOD_NAMES[period_num + 1], br(),
          tags$strong("Date: "), format(Sys.Date(), "%B %d, %Y")
        )
      )
    ),

    # Milestone Spider Plot Visualization
    div(
      class = "card mb-3",
      div(
        class = "card-header bg-info text-white",
        h5(class = "mb-0", icon("chart-line"), " Milestone Ratings Visualization")
      ),
      div(
        class = "card-body",
        p(class = "text-info mb-3",
          icon("info-circle"),
          " Your milestone ratings compared to program medians for this period"
        ),
        plotly::plotlyOutput(session_ns("preview_spider_plot"), height = "500px")
      )
    ),

    # Coach Review Text Fields
    div(
      class = "card mb-3",
      div(
        class = "card-header bg-success text-white",
        h5(class = "mb-0", icon("clipboard"), " Coach Review Entries")
      ),
      div(
        class = "card-body",

        # Wellness
        div(
          class = "mb-3 p-2 border-start border-primary border-3",
          h6(icon("heart"), " Wellness & Progress"),
          p(class = "text-muted small", review_data$wellness$coach_wellness)
        ),

        hr(),

        # Evaluations
        div(
          class = "mb-3 p-2 border-start border-purple border-3",
          h6(icon("clipboard"), " Evaluations & Feedback"),
          div(
            class = "mb-2",
            strong("Evaluations: "),
            p(class = "text-muted small", review_data$evaluations$coach_evaluations)
          ),
          div(
            strong("Plus/Delta Comments: "),
            p(class = "text-muted small", review_data$evaluations$coach_p_d_comments)
          )
        ),

        hr(),

        # Learning
        div(
          class = "mb-3 p-2 border-start border-warning border-3",
          h6(icon("book"), " Learning & Board Preparation"),
          div(
            class = "mb-2",
            strong("Learning Topics & Styles: "),
            p(class = "text-muted small", review_data$learning$coach_ls_and_topic)
          ),
          div(
            strong("Board Preparation: "),
            p(class = "text-muted small", review_data$learning$coach_step_board)
          )
        ),

        hr(),

        # Career
        div(
          class = "mb-3 p-2 border-start border-info border-3",
          h6(icon("briefcase"), " Career Planning"),
          p(class = "text-muted small", review_data$career$coach_career)
        ),

        hr(),

        # Goals
        div(
          class = "mb-3 p-2 border-start border-success border-3",
          h6(icon("bullseye"), " Goals & ILP"),
          div(
            class = "mb-2",
            strong("Milestone Goals: "),
            p(class = "text-muted small", review_data$goals$coach_mile_goal)
          ),
          div(
            strong("ILP Final Comments: "),
            p(class = "text-muted small", review_data$goals$coach_ilp_final)
          )
        )
      )
    ),

    # Confirmation message
    div(
      class = "alert alert-warning",
      icon("exclamation-triangle"),
      strong(" Please review all information carefully. "),
      "Once submitted, this review will be saved to REDCap."
    )
  )
}

mod_review_interface_ui <- function(id) {
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
            ),
            actionButton(
              ns("change_coach"),
              "Change Coach",
              class = "btn-outline-secondary",
              icon = icon("user-tie")
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

    # Vertical scroll layout - all sections visible
    div(
      style = "max-width: 1400px; margin: 0 auto;",

      # Section 1: Wellness & Progress (collapsible, starts expanded)
      div(
        class = "card mb-4",
        style = "border-left: 4px solid #3498db;",
        tags$a(
          `data-toggle` = "collapse",
          href = paste0("#", ns("collapse1")),
          class = "text-decoration-none",
          div(
            class = "card-header",
            style = "background-color: #ecf0f1; border-bottom: 2px solid #3498db; cursor: pointer;",
            h4(style = "margin: 0; color: #2c3e50;",
               icon("heart"), " 1. Wellness & Progress ",
               tags$small(class = "float-right", icon("chevron-down")))
          )
        ),
        div(
          id = ns("collapse1"),
          class = "collapse show",
          div(
            class = "card-body",
            mod_wellness_ui(ns("wellness"))
          )
        )
      ),

      # Section 2: Evaluations & Feedback (collapsible)
      div(
        class = "card mb-4",
        style = "border-left: 4px solid #9b59b6;",
        tags$a(
          `data-toggle` = "collapse",
          href = paste0("#", ns("collapse2")),
          class = "text-decoration-none",
          div(
            class = "card-header",
            style = "background-color: #ecf0f1; border-bottom: 2px solid #9b59b6; cursor: pointer;",
            h4(style = "margin: 0; color: #2c3e50;",
               icon("clipboard"), " 2. Evaluations & Feedback ",
               tags$small(class = "float-right", icon("chevron-down")))
          )
        ),
        div(
          id = ns("collapse2"),
          class = "collapse",
          div(
            class = "card-body",
            mod_evaluations_ui(ns("evaluations"))
          )
        )
      ),

      # Sections 3-8 (all collapsible, start collapsed)

      # Section 3
      div(
        class = "card mb-4",
        style = "border-left: 4px solid #e67e22;",
        tags$a(
          `data-toggle` = "collapse",
          href = paste0("#", ns("collapse3")),
          class = "text-decoration-none",
          div(
            class = "card-header",
            style = "background-color: #ecf0f1; border-bottom: 2px solid #e67e22; cursor: pointer;",
            h4(style = "margin: 0; color: #2c3e50;",
               icon("book"), " 3. Learning & Board Preparation ",
               tags$small(class = "float-right", icon("chevron-down")))
          )
        ),
        div(
          id = ns("collapse3"),
          class = "collapse",
          div(
            class = "card-body",
            mod_learning_ui(ns("learning"))
          )
        )
      ),

      # Section 4
      div(
        class = "card mb-4",
        style = "border-left: 4px solid #16a085;",
        tags$a(
          `data-toggle` = "collapse",
          href = paste0("#", ns("collapse4")),
          class = "text-decoration-none",
          div(
            class = "card-header",
            style = "background-color: #ecf0f1; border-bottom: 2px solid #16a085; cursor: pointer;",
            h4(style = "margin: 0; color: #2c3e50;",
               icon("graduation-cap"), " 4. Scholarship ",
               tags$small(class = "float-right", icon("chevron-down")))
          )
        ),
        div(
          id = ns("collapse4"),
          class = "collapse",
          div(
            class = "card-body",
            mod_scholarship_ui(ns("scholarship"))
          )
        )
      ),

      # Section 5
      div(
        class = "card mb-4",
        style = "border-left: 4px solid #f39c12;",
        tags$a(
          `data-toggle` = "collapse",
          href = paste0("#", ns("collapse5")),
          class = "text-decoration-none",
          div(
            class = "card-header",
            style = "background-color: #ecf0f1; border-bottom: 2px solid #f39c12; cursor: pointer;",
            h4(style = "margin: 0; color: #2c3e50;",
               icon("briefcase"), " 5. Career Planning ",
               tags$small(class = "float-right", icon("chevron-down")))
          )
        ),
        div(
          id = ns("collapse5"),
          class = "collapse",
          div(
            class = "card-body",
            mod_career_ui(ns("career"))
          )
        )
      ),

      # Section 6: Goals & ILP (moved before Milestones)
      div(
        class = "card mb-4",
        style = "border-left: 4px solid #27ae60;",
        tags$a(
          `data-toggle` = "collapse",
          href = paste0("#", ns("collapse6")),
          class = "text-decoration-none",
          div(
            class = "card-header",
            style = "background-color: #ecf0f1; border-bottom: 2px solid #27ae60; cursor: pointer;",
            h4(style = "margin: 0; color: #2c3e50;",
               icon("bullseye"), " 6. Goals & ILP ",
               tags$small(class = "float-right", icon("chevron-down")))
          )
        ),
        div(
          id = ns("collapse6"),
          class = "collapse",
          div(
            class = "card-body",
            mod_goals_ui(ns("goals"))
          )
        )
      ),

      # Section 7: Milestones (moved after ILP)
      div(
        class = "card mb-4",
        style = "border-left: 4px solid #e74c3c;",
        tags$a(
          `data-toggle` = "collapse",
          href = paste0("#", ns("collapse7")),
          class = "text-decoration-none",
          div(
            class = "card-header",
            style = "background-color: #ecf0f1; border-bottom: 2px solid #e74c3c; cursor: pointer;",
            h4(style = "margin: 0; color: #2c3e50;",
               icon("chart-line"), " 7. Milestones ",
               tags$small(class = "float-right", icon("chevron-down")))
          )
        ),
        div(
          id = ns("collapse7"),
          class = "collapse",
          div(
            class = "card-body",
            mod_milestones_ui(ns("milestones"))
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
            ns("submit_review"),
            "Go to Final Review",
            class = "btn-primary btn-lg",
            style = "padding: 10px 30px;",
            icon = icon("eye")
          ),
          br(),
          br(),
          p(
            style = "color: #666; font-size: 14px;",
            "Complete all required sections, then review and submit"
          )
        )
      )
    )
  )
}

mod_review_interface_server <- function(id, selected_resident, rdm_data, current_period) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive to get resident data
    resident_data <- reactive({
      req(selected_resident())
      req(rdm_data())
      req(current_period())
      
      # Extract values from reactives BEFORE passing to helper
      resident_id <- selected_resident()$record_id
      period_number <- current_period()
      data <- rdm_data()
      
      # Call helper function with actual values, not reactives
      get_resident_period_data(
        rdm_data = data,           # Actual data, not reactive
        record_id = resident_id,
        current_period = period_number,
        include_previous = TRUE
      )
    })
    
    # Display resident header
    output$resident_header <- renderUI({
      req(resident_data())
      req(current_period())
      
      res_data <- resident_data()$resident_info
      period_num <- current_period()
      
      HTML(sprintf(
        "<span style='color: #2c3e50;'>%s</span> | <span style='color: #7f8c8d;'>%s | Period: %s</span>",
        res_data$full_name,
        res_data$Level,
        PERIOD_NAMES[period_num + 1]
      ))
    })
    
    # Extract data_dict from rdm_data to pass to child modules (matches working app pattern)
    data_dict <- reactive({
      req(rdm_data())
      rdm_data()$data_dict
    })

    # Call Section 1 module
    wellness_data <- mod_wellness_server("wellness", resident_data, current_period, rdm_data)

    # Call Section 2 module - pass data_dict as separate parameter (matches working app)
    evaluations_data <- mod_evaluations_server("evaluations", resident_data, current_period, rdm_data, data_dict)

    # Call Section 3 module
    learning_data <- mod_learning_server("learning", resident_data, current_period, rdm_data)

    # Call Section 4 module
    scholarship_data <- mod_scholarship_server("scholarship", resident_data, current_period, rdm_data)

    # Call Section 5 module
    career_data <- mod_career_server("career", resident_data, current_period, rdm_data)

    # Call Section 6 module (Goals & ILP - moved before Milestones)
    goals_data <- mod_goals_server("goals", resident_data, current_period, rdm_data, data_dict)

    # Call Section 7 module (Milestones - moved after ILP)
    milestones_data <- mod_milestones_server("milestones", resident_data, current_period, rdm_data, data_dict)

    # Back to table button - returns reactive that triggers navigation
    back_to_table_clicked <- reactive({
      input$back_to_table
    })
    
    # Change coach button - returns reactive that triggers navigation to coach select
    change_coach_clicked <- reactive({
      input$change_coach
    })
    
    # Submit button
    submit_clicked <- reactive({
      input$submit_review
    })
    
    # Handle back to table
    observeEvent(input$back_to_table, {
      message(sprintf(
        "[%s] Back to table button clicked",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ))
      
      showNotification(
        "Returning to resident table",
        type = "message",
        duration = 2
      )
    })
    
    # Handle change coach
    observeEvent(input$change_coach, {
      message(sprintf(
        "[%s] Change coach button clicked",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ))
      
      showNotification(
        "Returning to coach selection",
        type = "message",
        duration = 2
      )
    })
    
    # Handle submission
    observeEvent(input$submit_review, {
      req(wellness_data())
      req(evaluations_data())
      req(learning_data())
      req(scholarship_data())
      req(career_data())
      req(milestones_data())
      req(goals_data())
      req(resident_data())
      req(current_period())

      # Collect data from all sections
      review_data <- list(
        wellness = wellness_data(),
        evaluations = evaluations_data(),
        learning = learning_data(),
        scholarship = scholarship_data(),
        career = career_data(),
        goals = goals_data(),
        milestones = milestones_data()
      )

      # Validate all required sections are complete
      incomplete_sections <- c()
      if (!review_data$wellness$is_complete) incomplete_sections <- c(incomplete_sections, "Wellness")
      if (!review_data$evaluations$is_complete) incomplete_sections <- c(incomplete_sections, "Evaluations")
      if (!review_data$learning$is_complete) incomplete_sections <- c(incomplete_sections, "Learning")
      if (!review_data$milestones$is_complete) incomplete_sections <- c(incomplete_sections, "Milestones")

      if (length(incomplete_sections) > 0) {
        showNotification(
          paste("Please complete the following sections before submitting:", paste(incomplete_sections, collapse = ", ")),
          type = "warning",
          duration = 5
        )
        return()
      }

      # Show preview modal with all data before submission
      showModal(modalDialog(
        title = tagList(
          icon("eye"),
          " Review Before Submitting"
        ),
        size = "xl",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            session$ns("confirm_submit"),
            "Submit All",
            class = "btn-primary",
            icon = icon("check-circle")
          )
        ),

        # Preview content
        create_review_preview(
          review_data = review_data,
          resident_data = resident_data,
          current_period = current_period,
          session_ns = session$ns
        )
      ))

      # Render the spider plot for the preview
      output$preview_spider_plot <- plotly::renderPlotly({
        req(review_data$milestones$milestone_ratings)

        milestone_ratings <- review_data$milestones$milestone_ratings

        # Check if we have milestone_results from the milestone data
        if (is.null(milestone_ratings$milestone_results)) {
          return(plotly::plotly_empty() %>%
                   plotly::add_annotations(
                     text = "Milestone data not available",
                     x = 0.5, y = 0.5, showarrow = FALSE,
                     font = list(size = 16, color = "gray")
                   ))
        }

        tryCatch({
          # Get the current period name
          period_name <- get_period_name(current_period())

          # Get resident ID
          record_id <- resident_data()$resident_info$record_id

          # Create spider plot with median comparison using gmed function
          dashboard <- gmed::create_milestone_overview_dashboard(
            milestone_results = milestone_ratings$milestone_results,
            resident_id = record_id,
            period_text = period_name,
            milestone_type = "coach",  # Coach assessment
            milestone_system = "rep",
            resident_data = rdm_data()$residents
          )

          return(dashboard$spider_plot)

        }, error = function(e) {
          message("Error creating preview spider plot: ", e$message)
          return(plotly::plotly_empty() %>%
                   plotly::add_annotations(
                     text = "Unable to create visualization",
                     x = 0.5, y = 0.5, showarrow = FALSE,
                     font = list(size = 14, color = "orange")
                   ))
        })
      })
    })

    # Handle confirmed submission
    observeEvent(input$confirm_submit, {
      req(wellness_data())
      req(evaluations_data())
      req(learning_data())
      req(scholarship_data())
      req(career_data())
      req(milestones_data())
      req(goals_data())
      req(resident_data())
      req(current_period())

      # Close preview modal
      removeModal()

      # Collect data from all sections
      review_data <- list(
        wellness = wellness_data(),
        evaluations = evaluations_data(),
        learning = learning_data(),
        scholarship = scholarship_data(),
        career = career_data(),
        goals = goals_data(),
        milestones = milestones_data()
      )

      # Show processing notification
      notification_id <- showNotification(
        tagList(icon("spinner", class = "fa-spin"), " Submitting coaching review..."),
        duration = NULL
      )

      # Build REDCap payload
      tryCatch({
        res_info <- resident_data()$resident_info
        period_num <- current_period()
        period_name <- get_period_name(period_num)

        # Extract numeric level from Level field (e.g., "PGY2" -> 2, "Intern" -> 1)
        level_num <- case_when(
          res_info$Level == "Intern" ~ 1,
          res_info$Level == "PGY2" ~ 2,
          res_info$Level == "PGY3" ~ 3,
          grepl("PGY[0-9]", res_info$Level) ~ as.numeric(gsub("[^0-9]", "", res_info$Level)),
          TRUE ~ NA_real_
        )

        # Calculate instance for coach_rev form
        instance <- gmed::get_redcap_instance(
          level = level_num,
          period = period_name,
          review_type = "scheduled"
        )

        # Build coach_rev submission record (raw format, date and period included)
        coach_rev_record <- data.frame(
          record_id = res_info$record_id,
          redcap_repeat_instrument = "coach_rev",
          redcap_repeat_instance = instance,
          coach_date = format(Sys.Date(), "%Y-%m-%d"),  # REDCap date format
          coach_period = as.character(period_num),       # Raw format (numeric as string)
          coach_wellness = as.character(review_data$wellness$coach_wellness %||% ""),
          coach_evaluations = as.character(review_data$evaluations$coach_evaluations %||% ""),
          coach_p_d_comments = as.character(review_data$evaluations$coach_p_d_comments %||% ""),
          coach_ls_and_topic = as.character(review_data$learning$coach_ls_and_topic %||% ""),
          coach_step_board = as.character(review_data$learning$coach_step_board %||% ""),
          coach_career = as.character(review_data$career$coach_career %||% ""),
          coach_mile_goal = as.character(review_data$goals$coach_mile_goal %||% ""),
          coach_ilp_final = as.character(review_data$goals$coach_ilp_final %||% ""),
          coach_rev_complete = "2",  # Raw format (string)
          stringsAsFactors = FALSE
        )

        # Submit coach_rev to REDCap using REDCapR
        coach_result <- tryCatch({
          result <- REDCapR::redcap_write_oneshot(
            ds = coach_rev_record,
            redcap_uri = REDCAP_CONFIG$url,
            token = REDCAP_CONFIG$rdm_token
          )

          list(
            success = result$success,
            message = if (result$success) "Coach review submitted" else result$outcome_message
          )
        }, error = function(e) {
          list(success = FALSE, message = e$message)
        })

        # Build milestone_entry submission record (raw format)
        milestone_ratings <- review_data$milestones$milestone_ratings

        # Helper function to convert gmed field names to REDCap field names
        # Converts: PC_1 -> rep_pc1, MK_2 -> rep_mk2, PROF_1 -> rep_prof1, etc.
        convert_to_redcap_field <- function(gmed_field) {
          # Convert from format like "PC_1" or "PROF_1" to "rep_pc1" or "rep_prof1"
          redcap_field <- tolower(gmed_field)  # Convert to lowercase
          redcap_field <- gsub("_", "", redcap_field)  # Remove underscore
          redcap_field <- paste0("rep_", redcap_field)  # Add rep_ prefix
          return(redcap_field)
        }

        milestone_record <- data.frame(
          record_id = res_info$record_id,
          redcap_repeat_instrument = "milestone_entry",
          redcap_repeat_instance = instance,
          prog_mile_date = format(Sys.Date(), "%Y-%m-%d"),  # REDCap date format
          prog_mile_period = as.character(period_num),       # Raw format
          stringsAsFactors = FALSE
        )

        # Add milestone scores and descriptions if available
        if (!is.null(milestone_ratings$scores) && length(milestone_ratings$scores) > 0) {
          for (field_name in names(milestone_ratings$scores)) {
            # Convert field name to REDCap format
            redcap_field <- convert_to_redcap_field(field_name)
            milestone_record[[redcap_field]] <- as.character(milestone_ratings$scores[[field_name]])
          }
        }

        if (!is.null(milestone_ratings$descriptions) && length(milestone_ratings$descriptions) > 0) {
          for (field_name in names(milestone_ratings$descriptions)) {
            # Convert field name to REDCap format and add _desc suffix
            redcap_field <- paste0(convert_to_redcap_field(field_name), "_desc")
            milestone_record[[redcap_field]] <- as.character(milestone_ratings$descriptions[[field_name]])
          }
        }

        # Add milestone completion status (raw format)
        milestone_record$milestone_entry_complete <- "2"

        # Submit milestone_entry to REDCap using REDCapR
        milestone_result <- tryCatch({
          result <- REDCapR::redcap_write_oneshot(
            ds = milestone_record,
            redcap_uri = REDCAP_CONFIG$url,
            token = REDCAP_CONFIG$rdm_token
          )

          list(
            success = result$success,
            message = if (result$success) "Milestones submitted" else result$outcome_message
          )
        }, error = function(e) {
          list(success = FALSE, message = e$message)
        })

        # Remove processing notification
        removeNotification(notification_id)

        # Check results
        if (coach_result$success && milestone_result$success) {
          # Show success modal with summary
          showModal(modalDialog(
            title = tagList(
              icon("check-circle", class = "text-success"),
              sprintf(" Review Submitted Successfully")
            ),
            size = "l",
            easyClose = TRUE,
            footer = tagList(
              modalButton("Close"),
              actionButton(
                session$ns("return_to_table"),
                "Return to Resident Table",
                class = "btn-primary"
              )
            ),

            # Summary of submitted data
            div(
              h4("Submitted Coaching Review"),
              p(
                tags$strong("Resident: "), res_info$full_name, br(),
                tags$strong("Period: "), PERIOD_NAMES[period_num + 1], br(),
                tags$strong("Date: "), format(Sys.Date(), "%B %d, %Y"), br(),
                tags$strong("Instance: "), instance
              ),

              hr(),

              h5(icon("heart"), " Wellness & Progress"),
              div(
                class = "well",
                style = "background-color: #f8f9fa; padding: 10px; max-height: 150px; overflow-y: auto;",
                HTML(gsub("\n", "<br>", review_data$wellness$coach_wellness))
              ),

              h5(icon("clipboard"), " Evaluations & Feedback"),
              div(
                class = "well",
                style = "background-color: #f8f9fa; padding: 10px; max-height: 150px; overflow-y: auto;",
                tags$strong("Evaluations: "),
                HTML(gsub("\n", "<br>", review_data$evaluations$coach_evaluations)),
                br(), br(),
                tags$strong("Plus/Delta Comments: "),
                HTML(gsub("\n", "<br>", review_data$evaluations$coach_p_d_comments))
              ),

              h5(icon("book"), " Learning & Board Preparation"),
              div(
                class = "well",
                style = "background-color: #f8f9fa; padding: 10px; max-height: 150px; overflow-y: auto;",
                tags$strong("Learning Topics & Styles: "),
                HTML(gsub("\n", "<br>", review_data$learning$coach_ls_and_topic)),
                br(), br(),
                tags$strong("Board Preparation: "),
                HTML(gsub("\n", "<br>", review_data$learning$coach_step_board))
              ),

              h5(icon("briefcase"), " Career Planning"),
              div(
                class = "well",
                style = "background-color: #f8f9fa; padding: 10px; max-height: 150px; overflow-y: auto;",
                HTML(gsub("\n", "<br>", review_data$career$coach_career))
              ),

              h5(icon("bullseye"), " Goals & ILP"),
              div(
                class = "well",
                style = "background-color: #f8f9fa; padding: 10px; max-height: 150px; overflow-y: auto;",
                tags$strong("Milestone Goals: "),
                HTML(gsub("\n", "<br>", review_data$goals$coach_mile_goal)),
                br(), br(),
                tags$strong("ILP Final Comments: "),
                HTML(gsub("\n", "<br>", review_data$goals$coach_ilp_final))
              ),

              hr(),

              div(
                class = "alert alert-info",
                icon("info-circle"),
                " Milestone ratings have also been submitted successfully."
              )
            )
          ))

          # Handle return to table button in modal
          observeEvent(input$return_to_table, {
            removeModal()
            # Trigger back to table navigation
            # The parent app should handle this
          })

        } else {
          error_msg <- c()
          if (!coach_result$success) error_msg <- c(error_msg, paste("Coach Review:", coach_result$message))
          if (!milestone_result$success) error_msg <- c(error_msg, paste("Milestones:", milestone_result$message))

          showNotification(
            tagList(
              icon("times-circle"),
              " Submission failed: ",
              paste(error_msg, collapse = "; ")
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
        back_to_table_clicked = back_to_table_clicked,
        change_coach_clicked = change_coach_clicked,
        submit_clicked = submit_clicked
      )
    )
  })
}