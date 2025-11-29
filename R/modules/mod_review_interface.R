# Review Interface Module
# Displays accordion with 8 review sections when resident is selected

mod_review_interface_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Header with resident info and back button
    fluidRow(
      column(
        width = 12,
        div(
          style = "margin-bottom: 20px;",
          actionButton(
            ns("back_to_table"),
            "← Back to Resident Table",
            class = "btn-secondary"
          ),
          div(
            style = "float: right; font-size: 16px; font-weight: bold;",
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
      multiple = FALSE,   # Only one section open at a time
      
      # Section 1: Wellness & Progress
      bslib::accordion_panel(
        title = "1. Wellness & Progress",
        value = "wellness",
        icon = bsicons::bs_icon("heart-pulse"),
        mod_wellness_ui(ns("wellness"))
      ),
      
      # Section 2: Evaluations & Feedback
      bslib::accordion_panel(
        title = "2. Evaluations & Feedback",
        value = "evaluations",
        icon = bsicons::bs_icon("clipboard-data"),
        "Section 2 - Coming soon"
      ),
      
      # Section 3: Learning & Board Preparation
      bslib::accordion_panel(
        title = "3. Learning & Board Preparation",
        value = "learning",
        icon = bsicons::bs_icon("book"),
        "Section 3 - Coming soon"
      ),
      
      # Section 4: Scholarship
      bslib::accordion_panel(
        title = "4. Scholarship",
        value = "scholarship",
        icon = bsicons::bs_icon("mortarboard"),
        "Section 4 - Coming soon"
      ),
      
      # Section 5: Career Planning
      bslib::accordion_panel(
        title = "5. Career Planning",
        value = "career",
        icon = bsicons::bs_icon("briefcase"),
        "Section 5 - Coming soon"
      ),
      
      # Section 6: Goals & ILP Review
      bslib::accordion_panel(
        title = "6. Goals & ILP Review",
        value = "goals",
        icon = bsicons::bs_icon("bullseye"),
        "Section 6 - Coming soon"
      ),
      
      # Section 7: ILP Summary
      bslib::accordion_panel(
        title = "7. ILP Summary",
        value = "ilp_summary",
        icon = bsicons::bs_icon("journal-text"),
        "Section 7 - Coming soon"
      ),
      
      # Section 8: Milestone Entry
      bslib::accordion_panel(
        title = "8. Milestone Entry",
        value = "milestones",
        icon = bsicons::bs_icon("star"),
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
            style = "padding: 10px 30px;"
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
        "<span style='color: #2c3e50;'>%s</span> | <span style='color: #7f8c8d;'>PGY-%s | Period: %s</span>",
        res_data$full_name,
        res_data$current_level,
        PERIOD_NAMES[period_num + 1]
      ))
    })
    
    # Call Section 1 module
    wellness_data <- mod_wellness_server(
      "wellness",
      resident_data = resident_data,
      current_period = current_period
    )
    
    # Back button
    back_clicked <- reactive({
      input$back_to_table
    })
    
    # Submit button
    submit_clicked <- reactive({
      input$submit_review
    })
    
    # Handle submission
    observeEvent(input$submit_review, {
      req(wellness_data())
      
      # TODO: Collect data from all sections
      # TODO: Validate required fields
      # TODO: Build REDCap payload
      # TODO: Submit to coach_rev form
      
      showNotification(
        "Review submission not yet implemented",
        type = "warning",
        duration = 3
      )
    })
    
    # Return reactive values
    return(
      list(
        back_clicked = back_clicked,
        submit_clicked = submit_clicked
      )
    )
  })
}