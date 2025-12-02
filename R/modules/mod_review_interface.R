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

    # Vertical scroll layout - all sections visible
    div(
      style = "max-width: 1400px; margin: 0 auto;",

      # Section 1: Wellness & Progress
      div(
        class = "card mb-4",
        style = "border-left: 4px solid #3498db;",
        div(
          class = "card-header",
          style = "background-color: #ecf0f1; border-bottom: 2px solid #3498db;",
          h4(style = "margin: 0; color: #2c3e50;",
             icon("heart"), " 1. Wellness & Progress")
        ),
        div(
          class = "card-body",
          mod_wellness_ui(ns("wellness"))
        )
      ),

      # Section 2: Evaluations & Feedback
      div(
        class = "card mb-4",
        style = "border-left: 4px solid #9b59b6;",
        div(
          class = "card-header",
          style = "background-color: #ecf0f1; border-bottom: 2px solid #9b59b6;",
          h4(style = "margin: 0; color: #2c3e50;",
             icon("clipboard"), " 2. Evaluations & Feedback")
        ),
        div(
          class = "card-body",
          mod_evaluations_ui(ns("evaluations"))
        )
      ),

      # Section 3: Learning & Board Preparation
      div(
        class = "card mb-4",
        style = "border-left: 4px solid #e67e22;",
        div(
          class = "card-header",
          style = "background-color: #ecf0f1; border-bottom: 2px solid #e67e22;",
          h4(style = "margin: 0; color: #2c3e50;",
             icon("book"), " 3. Learning & Board Preparation")
        ),
        div(
          class = "card-body",
          p("Section content coming soon")
        )
      ),

      # Section 4: Scholarship
      div(
        class = "card mb-4",
        style = "border-left: 4px solid #16a085;",
        div(
          class = "card-header",
          style = "background-color: #ecf0f1; border-bottom: 2px solid #16a085;",
          h4(style = "margin: 0; color: #2c3e50;",
             icon("graduation-cap"), " 4. Scholarship")
        ),
        div(
          class = "card-body",
          p("Section content coming soon")
        )
      ),

      # Section 5: Career Planning
      div(
        class = "card mb-4",
        style = "border-left: 4px solid #f39c12;",
        div(
          class = "card-header",
          style = "background-color: #ecf0f1; border-bottom: 2px solid #f39c12;",
          h4(style = "margin: 0; color: #2c3e50;",
             icon("briefcase"), " 5. Career Planning")
        ),
        div(
          class = "card-body",
          p("Section content coming soon")
        )
      ),

      # Section 6: Milestones
      div(
        class = "card mb-4",
        style = "border-left: 4px solid #e74c3c;",
        div(
          class = "card-header",
          style = "background-color: #ecf0f1; border-bottom: 2px solid #e74c3c;",
          h4(style = "margin: 0; color: #2c3e50;",
             icon("chart-line"), " 6. Milestones")
        ),
        div(
          class = "card-body",
          p("Section content coming soon")
        )
      ),

      # Section 7: Goals & ILP
      div(
        class = "card mb-4",
        style = "border-left: 4px solid #27ae60;",
        div(
          class = "card-header",
          style = "background-color: #ecf0f1; border-bottom: 2px solid #27ae60;",
          h4(style = "margin: 0; color: #2c3e50;",
             icon("bullseye"), " 7. Goals & Individual Learning Plan")
        ),
        div(
          class = "card-body",
          p("Section content coming soon")
        )
      ),

      # Section 8: Summary
      div(
        class = "card mb-4",
        style = "border-left: 4px solid #34495e;",
        div(
          class = "card-header",
          style = "background-color: #ecf0f1; border-bottom: 2px solid #34495e;",
          h4(style = "margin: 0; color: #2c3e50;",
             icon("check-circle"), " 8. Summary & Submission")
        ),
        div(
          class = "card-body",
          p("Review summary and submission coming soon")
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