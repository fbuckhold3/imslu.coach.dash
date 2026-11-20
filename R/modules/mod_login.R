# ==============================================================================
# LOGIN MODULE
# R/modules/mod_login.R
# ==============================================================================
#
# Per-coach access code authentication. Each coach enters the personal code
# stored in the FAC database (fac_access). See R/helpers/faculty_access.R.
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
          background: var(--roundsui-surface);
          border: 1px solid var(--roundsui-border);
          border-radius: 8px;
          box-shadow: var(--roundsui-shadow);
        }
        .login-header {
          text-align: center;
          margin-bottom: 30px;
        }
        .login-header h2 {
          color: var(--roundsui-ink);
          margin-bottom: 10px;
        }
        .login-header p {
          color: var(--roundsui-ink-muted);
          font-size: 14px;
        }
        .login-error {
          color: var(--roundsui-danger);
          font-size: 14px;
          margin-top: 10px;
          display: none;
        }
        .login-error.show {
          display: block;
        }
        .login-button {
          width: 100%;
          background-color: var(--roundsui-accent);
          color: var(--roundsui-accent-ink);
          border: none;
          padding: 12px;
          font-size: 16px;
          margin-top: 20px;
        }
        .login-button:hover {
          background-color: var(--roundsui-accent);
          color: var(--roundsui-accent-ink);
          filter: brightness(0.92);
        }
      "))
    ),
    
    div(
      class = "login-container",
      div(
        class = "login-header",
        h2("IMSLU Coaching Dashboard"),
        p("Internal Medicine Residency \u2022 Saint Louis University"),
        hr()
      ),
      
      textInput(
        ns("access_code"),
        label = "Access Code",
        placeholder = "Enter your personal access code",
        width = "100%"
      ),
      
      div(
        id = ns("error_message"),
        class = "login-error",
        icon("exclamation-triangle"),
        " ",
        textOutput(ns("error_text"), inline = TRUE)
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
#' Checks the typed code against each faculty member's personal FAC access
#' code (see R/helpers/faculty_access.R), or with the shared ACCESS_CODE,
#' which always works and opens the coach picker (any coach's residents).
#'
#' @param id Module namespace ID
#' @param coach_labels Reactive returning the RDM coach labels in use
#'   (used to link a faculty member to their assigned residents)
#' @param fallback_code Shared program code (always accepted, coach picker)
#' @return reactiveVal: NULL until login, then
#'   list(authenticated, fac_name, coach_name, is_leader, via)
#' @export
mod_login_server <- function(id,
                             coach_labels  = reactive(character(0)),
                             fallback_code = AUTH_CONFIG$access_code) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    login_info      <- reactiveVal(NULL)
    failed_attempts <- reactiveVal(0)
    error_text      <- reactiveVal("Invalid access code. Please try again.")

    output$error_text <- renderText(error_text())

    show_error <- function(msg) {
      error_text(msg)
      shinyjs::runjs(sprintf("$('#%s').addClass('show');", ns("error_message")))
    }

    stamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

    observeEvent(input$login_btn, {
      req(input$access_code)
      typed <- trimws(input$access_code)
      updateTextInput(session, "access_code", value = "")

      # Shared program code: always accepted, opens the coach picker
      if (nzchar(fallback_code) && identical(typed, fallback_code)) {
        res <- list(status = "ok", fac_name = "Shared code",
                    coach_name = NA_character_, is_leader = TRUE,
                    via = "shared")
      } else {
        fac <- load_faculty_access()
        res <- if (is.null(fac)) list(status = "unavailable") else
          match_access_code(typed, fac, coach_labels())
      }

      if (identical(res$status, "ok")) {
        shinyjs::runjs(sprintf("$('#%s').removeClass('show');", ns("error_message")))
        failed_attempts(0)
        login_info(list(
          authenticated = TRUE,
          fac_name      = res$fac_name,
          coach_name    = res$coach_name,
          is_leader     = isTRUE(res$is_leader),
          via           = if (is.null(res$via)) "fac" else res$via
        ))
        message(sprintf("[%s] Login: %s (coach label: %s, leadership: %s, via %s)",
                        stamp(), res$fac_name,
                        ifelse(is.na(res$coach_name), "none", res$coach_name),
                        isTRUE(res$is_leader),
                        if (is.null(res$via)) "fac" else res$via))
        return()
      }

      # Failed login
      failed_attempts(failed_attempts() + 1)
      show_error(switch(
        res$status,
        duplicate   = "This code is linked to more than one faculty record. Please contact the program office.",
        unavailable = "The faculty directory is unavailable right now. Please try again in a few minutes.",
        "Invalid access code. Please try again."
      ))
      message(sprintf("[%s] Failed login attempt #%d (%s)",
                      stamp(), failed_attempts(), res$status))

      # Rate limiting: pause after repeated failures
      if (failed_attempts() >= 3) {
        shinyjs::disable("login_btn")
        shinyjs::delay(3000, shinyjs::enable("login_btn"))
        showNotification("Too many failed attempts. Please wait a moment.",
                         type = "warning", duration = 3)
      }
    })

    login_info
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

#' Login Info for Development Bypass
#'
#' With BYPASS_LOGIN=true, set DEV_COACH to an RDM coach label (a last name,
#' e.g. "Kunnath") to test as that coach. Leave DEV_COACH unset to get the
#' leadership view.
#'
#' @return list in the same shape mod_login_server() returns
#' @export
bypass_login_info <- function() {
  dev_coach <- trimws(Sys.getenv("DEV_COACH", unset = ""))
  list(
    authenticated = TRUE,
    fac_name      = if (nzchar(dev_coach)) dev_coach else "Developer",
    coach_name    = if (nzchar(dev_coach)) dev_coach else NA_character_,
    is_leader     = !nzchar(dev_coach),
    via           = "bypass"
  )
}