# Review Interface Module
# Displays accordion with 8 review sections when resident is selected
#
# FIXES APPLIED:
# 1. Added "Back to Resident Table" button that works
# 2. Added "Change Coach" button to return to coach selection
# 3. Improved header layout with both navigation options

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
    
    # Main accordion with 8 sections
    bslib::accordion(
      id = ns("review_accordion"),
      open = "wellness",  # First section open by default
      multiple = TRUE,   # Allow multiple sections open at once

      # Section 1: Wellness & Progress
      bslib::accordion_panel(
        title = "1. Wellness & Progress",
        value = "wellness",
        mod_wellness_ui(ns("wellness"))
      ),

      # Section 2: Evaluations & Feedback
      bslib::accordion_panel(
        title = "2. Evaluations & Feedback",
        value = "evaluations",
        mod_evaluations_ui(ns("evaluations"))
      ),

      # Section 3: Learning & Board Preparation
      bslib::accordion_panel(
        title = "3. Learning & Board Preparation",
        value = "learning",
        "Section 3 - Coming soon"
      ),

      # Section 4: Scholarship
      bslib::accordion_panel(
        title = "4. Scholarship",
        value = "scholarship",
        "Section 4 - Coming soon"
      ),

      # Section 5: Career Planning
      bslib::accordion_panel(
        title = "5. Career Planning",
        value = "career",
        "Section 5 - Coming soon"
      ),

      # Section 6: Goals & ILP Review
      bslib::accordion_panel(
        title = "6. Goals & ILP Review",
        value = "goals",
        "Section 6 - Coming soon"
      ),

      # Section 7: ILP Summary
      bslib::accordion_panel(
        title = "7. ILP Summary",
        value = "ilp_summary",
        "Section 7 - Coming soon"
      ),

      # Section 8: Milestone Entry
      bslib::accordion_panel(
        title = "8. Milestone Entry",
        value = "milestones",
        "Section 8 - Coming soon"
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
            "Submit Review",
            class = "btn-primary btn-lg",
            style = "padding: 10px 30px;",
            icon = icon("check-circle")
          ),
          br(),
          br(),
          p(
            style = "color: #666; font-size: 14px;",
            "Complete all required sections before submitting"
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
  
  # Collect data from all sections
  review_data <- list(
    wellness = wellness_data(),
    evaluations = evaluations_data()
    # ... more sections as you build them
  )
      
      # TODO: Collect data from all sections
      # TODO: Validate required fields
      # TODO: Build REDCap payload
      # TODO: Submit to coach_rev form
      
      showNotification(
        "Review submission not yet implemented. This will collect data from all 8 sections and submit to REDCap.",
        type = "warning",
        duration = 5
      )
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