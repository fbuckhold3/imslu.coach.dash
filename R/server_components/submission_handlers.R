# ============================================================================
# SUBMISSION HANDLERS COMPONENT
# R/server_components/submission_handlers.R
# ============================================================================

#' Initialize Submission Handlers
#' 
#' Sets up form submission event handlers
#' @param input Shiny input object
#' @param output Shiny output object  
#' @param session Shiny session object
#' @param values Reactive values object
#' @return NULL
initialize_submission_handlers <- function(input, output, session, values) {
  
  # ============================================================================
  # ENHANCED SUBMISSION WITH GMED INTEGRATION
  # ============================================================================
  observeEvent(input$submit_review, {
    req(values$selected_resident)
    
    message("=== ENHANCED SUBMISSION EVENT ===")
    message("Resident: ", values$selected_resident$name)
    
    # Determine if this is intern intro
    is_intro <- tryCatch({
      is_intern_intro_review_enhanced(values$selected_resident$level, values$current_period)
    }, error = function(e) {
      FALSE
    })
    
    # Show enhanced processing notification
    notification_id <- showNotification(
      tagList(
        icon("spinner fa-spin"), 
        " Submitting enhanced coaching review..."
      ),
      duration = NULL
    )
    
    # Enhanced validation
    validation <- tryCatch({
      if (exists("validate_coach_inputs", envir = .GlobalEnv)) {
        validate_coach_inputs(
          inputs = reactiveValuesToList(input),
          is_intern_intro = is_intro
        )
      } else {
        list(valid = TRUE, message = "Validation skipped")
      }
    }, error = function(e) {
      list(valid = FALSE, message = paste("Validation error:", e$message))
    })
    
    if (!validation$valid) {
      removeNotification(notification_id)
      showNotification(
        tagList(icon("exclamation-triangle"), " ", validation$message),
        type = "error",
        duration = 10
      )
      return()
    }
    
    # Submit using enhanced gmed integration
    result <- tryCatch({
      if (exists("submit_coach_review_coach_app", envir = .GlobalEnv)) {
        submit_coach_review_coach_app(
          resident_name = values$selected_resident$name,
          inputs = reactiveValuesToList(input),
          app_data = app_data(),
          is_intern_intro = is_intro
        )
      } else {
        list(success = FALSE, message = "Submission function not available")
      }
    }, error = function(e) {
      list(success = FALSE, message = paste("Submission error:", e$message))
    })
    
    removeNotification(notification_id)
    
    if (result$success) {
      showNotification(
        tagList(
          icon("check-circle"), 
          " Enhanced review submitted successfully for ", values$selected_resident$name
        ),
        type = "message",
        duration = 5
      )
      
      # Navigate to enhanced done page
      shinyjs::hide("review-pages")
      shinyjs::show("done-page")
      
    } else {
      showNotification(
        tagList(icon("times-circle"), " Submission failed: ", result$message),
        type = "error",
        duration = 15
      )
    }
  })
}