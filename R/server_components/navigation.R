# ============================================================================
# NAVIGATION COMPONENT
# R/server_components/navigation.R
# ============================================================================

#' Initialize Navigation Handlers
#' 
#' Sets up all navigation event handlers and UI outputs
#' @param input Shiny input object
#' @param output Shiny output object  
#' @param session Shiny session object
#' @param values Reactive values object
#' @param processed_resident_data Reactive expression for processed resident data
#' @return NULL
initialize_navigation <- function(input, output, session, values, processed_resident_data) {
  
  # ============================================================================
  # ENHANCED RESIDENT SELECTION WITH GMED FEATURES
  # ============================================================================
  observeEvent(input$selected_resident_in_table, {
    req(input$selected_resident_in_table)
    
    resident_info <- input$selected_resident_in_table
    
    message("=== ENHANCED RESIDENT SELECTION ===")
    message("Selected resident: ", resident_info$resident_name)
    
    # Find full resident record
    resident_data <- processed_resident_data()
    
    full_resident_record <- tryCatch({
      resident_data %>%
        filter(name == resident_info$resident_name) %>%
        slice(1)
    }, error = function(e) {
      message("Error getting resident record: ", e$message)
      NULL
    })
    
    # Store enhanced resident info
    if (!is.null(full_resident_record) && nrow(full_resident_record) > 0) {
      values$selected_resident <- list(
        name = resident_info$resident_name,
        level = resident_info$resident_level,
        Level = resident_info$resident_level,
        access_code = resident_info$access_code,
        review_role = resident_info$review_role,
        period_code = resident_info$review_period,
        record_id = full_resident_record$record_id %||% "unknown"
      )
    } else {
      values$selected_resident <- list(
        name = resident_info$resident_name,
        level = resident_info$resident_level,
        Level = resident_info$resident_level,
        access_code = resident_info$access_code %||% "unknown",
        review_role = resident_info$review_role,
        period_code = resident_info$review_period,
        record_id = "unknown"
      )
    }
    
    values$selected_coach <- input$coach_name
    values$current_period <- resident_info$review_period
    values$review_type <- tolower(resident_info$review_role)
    
    # Reset step tracking
    values$current_step <- 1
    values$step_data <- list()
    
    # Show review interface with enhanced transitions
    shinyjs::hide("coach-selection-page")
    shinyjs::show("review-pages")
    
    showNotification(
      tagList(
        icon("play-circle"), 
        " Starting ", resident_info$review_role, " review for ", resident_info$resident_name
      ),
      type = "message",
      duration = 5
    )
  })
  
  # ============================================================================
  # ENHANCED RESIDENT INFO DISPLAYS
  # ============================================================================
  output$resident_name_display <- renderText({
    req(values$selected_resident)
    values$selected_resident$name
  })
  
  output$resident_level_display <- renderText({
    req(values$selected_resident)
    values$selected_resident$level
  })
  
  output$resident_coach_display <- renderText({
    req(values$selected_coach)
    values$selected_coach
  })
  
  output$resident_period_display <- renderText({
    req(values$current_period)
    period_names <- c(
      "1" = "Mid Intern",
      "2" = "End Intern", 
      "3" = "Mid PGY2",
      "4" = "End PGY2",
      "5" = "Mid PGY3",
      "6" = "Graduation",
      "7" = "Intern Intro"
    )
    period_names[values$current_period] %||% values$current_period
  })
  
  output$resident_access_display <- renderText({
    req(values$selected_resident)
    values$selected_resident$access_code %||% "Not provided"
  })
  
  # ============================================================================
  # ENHANCED PROGRESS TRACKING
  # ============================================================================
  output$current_step_text <- renderText({
    req(values$selected_resident)
    step_name <- switch(as.character(values$current_step),
                        "1" = "Getting Started",
                        "2" = "Coping & Wellness", 
                        "3" = "Evaluations",
                        "4" = "Knowledge & Board Prep",
                        "5" = "Scholarship",
                        "6" = "Milestone Goals",
                        "7" = "Career Planning",
                        "8" = "Summary & Submission",
                        "Getting Started"
    )
    paste("Step", values$current_step, "of", values$total_steps, ":", step_name, "-", values$selected_resident$review_role, "Review")
  })
  
  output$progress_text <- renderText({
    paste(round((values$current_step / values$total_steps) * 100), "%")
  })
  
  # Update progress bar
  observe({
    req(values$current_step)
    progress_percent <- (values$current_step / values$total_steps) * 100
    
    session$sendCustomMessage("updateProgressBar", list(
      width = paste0(progress_percent, "%"),
      text = paste0(round(progress_percent), "%")
    ))
  })
  
  # ============================================================================
  # ENHANCED NAVIGATION WITH STEP MANAGEMENT
  # ============================================================================
  observeEvent(input$next_step, {
    if (values$current_step < values$total_steps) {
      values$current_step <- values$current_step + 1
      
      # Update progress bar
      progress_percent <- (values$current_step / values$total_steps) * 100
      session$sendCustomMessage("updateProgressBar", list(
        width = paste0(progress_percent, "%")
      ))
      
      showNotification(
        paste("Moved to step", values$current_step),
        type = "message",
        duration = 2
      )
    }
  })
  
  observeEvent(input$prev_step, {
    if (values$current_step > 1) {
      values$current_step <- values$current_step - 1
      
      # Update progress bar
      progress_percent <- (values$current_step / values$total_steps) * 100
      session$sendCustomMessage("updateProgressBar", list(
        width = paste0(progress_percent, "%")
      ))
      
      showNotification(
        paste("Returned to step", values$current_step),
        type = "message",
        duration = 2
      )
    }
  })
  
  # ============================================================================
  # ENHANCED NAVIGATION HANDLERS
  # ============================================================================
  observeEvent(input$back_to_dashboard, {
    shinyjs::hide("review-pages")
    shinyjs::show("coach-selection-page")
    
    values$selected_resident <- NULL
    values$current_period <- NULL
    values$review_type <- NULL
    values$current_step <- 1
    values$step_data <- list()
    
    showNotification("Returned to dashboard", type = "message")
  })
  
  observeEvent(input$start_new_review, {
    shinyjs::hide("done-page")
    shinyjs::show("coach-selection-page")
    
    values$selected_resident <- NULL
    values$current_period <- NULL
    values$review_type <- NULL
    values$current_step <- 1
    values$step_data <- list()
  })
  
  observeEvent(input$view_dashboard, {
    shinyjs::hide("done-page")
    shinyjs::show("coach-selection-page")
    
    values$selected_resident <- NULL
    values$current_period <- NULL
    values$review_type <- NULL
    values$current_step <- 1
    values$step_data <- list()
  })
}