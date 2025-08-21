# ============================================================================
# AUTHENTICATION COMPONENT
# R/server_components/authentication.R
# ============================================================================

#' Initialize Authentication Handlers
#' 
#' Sets up authentication reactive values and event handlers
#' @param input Shiny input object
#' @param output Shiny output object  
#' @param session Shiny session object
#' @param values Reactive values object
#' @return NULL
initialize_authentication <- function(input, output, session, values) {
  
  # ============================================================================
  # AUTHENTICATION WITH ENHANCED FEEDBACK
  # ============================================================================
  observeEvent(input$submit_access, {
    if (validate_access_code(input$access_code)) {
      values$is_authenticated <- TRUE
      shinyjs::hide("login-page")
      shinyjs::show("coach-selection-page")
      showNotification(
        tagList(icon("check"), " Access granted! Welcome to the enhanced coaching platform."),
        type = "message", 
        duration = 5
      )
      message("User authenticated successfully")
    } else {
      shinyjs::show("access_error")
      showNotification(
        tagList(icon("times"), " Invalid access code"),
        type = "error",
        duration = 5
      )
    }
  })
  
  # ============================================================================
  # STARTUP NOTIFICATIONS
  # ============================================================================
  observe({
    req(app_data())
    data <- app_data()
    
    removeNotification("loading")
    
    if (!is.null(data$error)) {
      showNotification(
        paste("⚠️ Data loading issue:", data$error),
        type = "warning",
        duration = 15
      )
    } else {
      showNotification(
        "✅ Enhanced application loaded successfully with gmed integration",
        type = "message",
        duration = 5
      )
    }
  })
}