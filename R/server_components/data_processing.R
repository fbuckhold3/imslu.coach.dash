# ============================================================================
# DATA PROCESSING COMPONENT
# R/server_components/data_processing.R
# ============================================================================

#' Initialize Data Processing Reactives
#' 
#' Sets up data processing reactive expressions and dropdown updates
#' @param input Shiny input object
#' @param output Shiny output object  
#' @param session Shiny session object
#' @param values Reactive values object
#' @return List of reactive expressions
initialize_data_processing <- function(input, output, session, values) {
  
  # ============================================================================
  # ENHANCED DATA PROCESSING WITH GMED FUNCTIONS
  # ============================================================================
  processed_resident_data <- reactive({
    req(app_data())
    data <- app_data()
    req(data$resident_data)
    
    message("=== PROCESSING RESIDENT DATA WITH GMED ===")
    
    # Start with resident data
    resident_data <- data$resident_data
    message("Starting with ", nrow(resident_data), " residents")
    
    # Use gmed function for coach name conversion
    data_dict <- data$rdm_dict
    if (!is.null(data_dict)) {
      message("Converting coach codes using gmed function...")
      resident_data <- tryCatch({
        gmed::convert_coach_codes_to_names(resident_data, data_dict)
      }, error = function(e) {
        message("gmed conversion failed, using original data: ", e$message)
        resident_data
      })
    }
    
    # Filter invalid data
    initial_count <- nrow(resident_data)
    resident_data <- resident_data %>%
      filter(!is.na(name), name != "")
    
    message("After filtering: ", nrow(resident_data), " residents")
    
    # Standardize level column naming
    if ("Level" %in% names(resident_data)) {
      resident_data$level <- resident_data$Level
    } else if ("level" %in% names(resident_data)) {
      resident_data$Level <- resident_data$level
    } else {
      warning("No level column found in resident data")
      resident_data$level <- "Unknown"
      resident_data$Level <- "Unknown"
    }
    
    message("Final resident count: ", nrow(resident_data))
    message("=== PROCESSING COMPLETE ===")
    
    return(resident_data)
  })
  
  # ============================================================================
  # ENHANCED COACH DROPDOWN WITH GMED STYLING
  # ============================================================================
  observe({
    req(values$is_authenticated)
    
    if (values$dropdown_updated) return()
    
    data_ready <- tryCatch({
      test_data <- processed_resident_data()
      !is.null(test_data) && nrow(test_data) > 0
    }, error = function(e) {
      message("Data not ready yet: ", e$message)
      FALSE
    })
    
    if (!data_ready) {
      invalidateLater(1000, session)
      return()
    }
    
    message("=== UPDATING COACH DROPDOWN ===")
    
    resident_data <- processed_resident_data()
    
    # Get primary coaches
    primary_coaches <- character(0)
    if ("coach" %in% names(resident_data)) {
      primary_coaches <- resident_data %>%
        filter(!is.na(coach), coach != "", coach != "Unknown", coach != "Not assigned") %>%
        pull(coach) %>%
        unique()
    }
    
    # Get secondary coaches
    secondary_coaches <- character(0)
    if ("second_rev" %in% names(resident_data)) {
      secondary_coaches <- resident_data %>%
        filter(!is.na(second_rev), second_rev != "", second_rev != "Unknown", second_rev != "Not assigned") %>%
        pull(second_rev) %>%
        unique()
    }
    
    # Combine and sort
    all_coaches <- unique(c(primary_coaches, secondary_coaches)) %>%
      sort()
    
    message("Total unique coaches: ", length(all_coaches))
    
    # Update the dropdown
    tryCatch({
      updateSelectizeInput(
        session = session,
        inputId = "coach_name",
        choices = all_coaches,
        server = FALSE
      )
      
      values$dropdown_updated <- TRUE
      message("✅ Coach dropdown updated successfully!")
      
      showNotification(
        paste("Loaded", length(all_coaches), "coaches successfully!"),
        duration = 3
      )
      
    }, error = function(e) {
      message("❌ Dropdown update failed: ", e$message)
    })
  })
  
  # ============================================================================
  # PERIOD STATUS OUTPUT
  # ============================================================================
  output$period_status_text <- renderText({
    current_period <- tryCatch({
      gmed::get_current_period()
    }, error = function(e) {
      "Unknown Period"
    })
    
    if (input$enable_period_override && input$override_period != "auto") {
      override_text <- paste("Period Override Active:", input$override_period)
      paste("Current Period:", current_period, "|", override_text)
    } else {
      paste("Current Period:", current_period)
    }
  })
  
  # Return reactive expressions for use by other components
  return(list(
    processed_resident_data = processed_resident_data
  ))
}