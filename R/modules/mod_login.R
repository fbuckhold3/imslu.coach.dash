# ==============================================================================
# LOGIN MODULE
# R/modules/mod_login.R
# ==============================================================================
#
# Simple access code authentication for coaching dashboard
# Validates against ACCESS_CODE environment variable
#
# ==============================================================================

#' Login Module UI
#'
#' @param id Module namespace ID
#' @return UI elements for login screen
#' @export
mod_login_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        .login-container {
          max-width: 400px;
          margin: 100px auto;
          padding: 40px;
          background: white;
          border-radius: 8px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        .login-header {
          text-align: center;
          margin-bottom: 30px;
        }
        .login-header h2 {
          color: #0072B2;
          margin-bottom: 10px;
        }
        .login-header p {
          color: #666;
          font-size: 14px;
        }
        .login-error {
          color: #dc3545;
          font-size: 14px;
          margin-top: 10px;
          display: none;
        }
        .login-error.show {
          display: block;
        }
        .login-button {
          width: 100%;
          background-color: #0072B2;
          border: none;
          padding: 12px;
          font-size: 16px;
          margin-top: 20px;
        }
        .login-button:hover {
          background-color: #005a8f;
        }
      "))
    ),
    
    div(
      class = "login-container",
      div(
        class = "login-header",
        h2("IMSLU Coaching Dashboard"),
        p("Internal Medicine Residency • Saint Louis University"),
        hr()
      ),
      
      textInput(
        ns("access_code"),
        label = "Access Code",
        placeholder = "Enter access code",
        width = "100%"
      ),
      
      div(
        id = ns("error_message"),
        class = "login-error",
        icon("exclamation-triangle"),
        " Invalid access code. Please try again."
      ),
      
      actionButton(
        ns("login_btn"),
        "Login",
        class = "btn-primary login-button",
        icon = icon("sign-in-alt")
      ),
      
      # Allow Enter key to submit
      tags$script(HTML(sprintf("
        $(document).on('keypress', function(e) {
          if(e.which == 13) {
            $('#%s').click();
          }
        });
      ", ns("login_btn"))))
    )
  )
}

#' Login Module Server
#'
#' @param id Module namespace ID
#' @param access_code Valid access code (from environment or config)
#' @return Reactive logical indicating authentication status
#' @export
mod_login_server <- function(id, access_code = AUTH_CONFIG$access_code) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Authentication state
    authenticated <- reactiveVal(FALSE)
    
    # Track failed login attempts
    failed_attempts <- reactiveVal(0)
    
    # Handle login button click
    observeEvent(input$login_btn, {
      req(input$access_code)
      
      # Validate access code
      if (nzchar(access_code) && input$access_code == access_code) {
        # Successful login
        authenticated(TRUE)
        
        # Clear input
        updateTextInput(session, "access_code", value = "")
        
        # Hide error message
        shinyjs::runjs(sprintf("
          $('#%s').removeClass('show');
        ", ns("error_message")))
        
        # Log successful authentication
        message(sprintf(
          "[%s] Successful login",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        ))
        
      } else {
        # Failed login
        failed_attempts(failed_attempts() + 1)
        
        # Show error message
        shinyjs::runjs(sprintf("
          $('#%s').addClass('show');
        ", ns("error_message")))
        
        # Clear input
        updateTextInput(session, "access_code", value = "")
        
        # Log failed attempt
        message(sprintf(
          "[%s] Failed login attempt #%d",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          failed_attempts()
        ))
        
        # Rate limiting: Add delay after multiple failed attempts
        if (failed_attempts() >= 3) {
          shinyjs::disable("login_btn")
          shinyjs::delay(3000, shinyjs::enable("login_btn"))
          
          showNotification(
            "Too many failed attempts. Please wait a moment.",
            type = "warning",
            duration = 3
          )
        }
      }
    })
    
    # Session timeout (optional)
    observe({
      req(authenticated())
      
      # Reset timeout on any input change
      invalidateLater(AUTH_CONFIG$timeout_minutes * 60 * 1000, session)
      
      # Could add auto-logout logic here
      # For now, sessions persist until browser close
    })
    
    # Return authentication status
    return(authenticated)
  })
}

#' Check if Access Code is Configured
#'
#' Helper function to verify access code is set up
#' 
#' @return Logical indicating if access code is available
#' @export
is_access_code_configured <- function() {
  nzchar(AUTH_CONFIG$access_code)
}

#' Bypass Login (for testing)
#'
#' Create a bypass for development/testing environments
#' Set BYPASS_LOGIN=true in .Renviron to skip authentication
#' 
#' @return Logical indicating if login should be bypassed
#' @export
should_bypass_login <- function() {
  bypass <- Sys.getenv("BYPASS_LOGIN", unset = "false")
  tolower(bypass) %in% c("true", "yes", "1")
}