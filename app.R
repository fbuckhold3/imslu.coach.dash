# ==============================================================================
# IMSLU COACHING DASHBOARD - MAIN APP
# app.R
# ==============================================================================
#
# Main application file for RDM 2.0 coaching dashboard
# Phase 1: Login, coach selection, resident table
# Phase 2: Review interface with accordion sections
#
# UPDATES:
# - Fixed navigation handlers for back_to_table and change_coach buttons
# - Properly integrated review_interface navigation
#
# ==============================================================================

# Load globals and modules
source("R/globals.R")
library(future)
library(promises)

# Enable async background loading
future::plan(future::multisession)

# ── Phase 1: fast startup (residents + data dict + cached medians, ~2-4 sec) ──
coach_phase1_data <- load_coach_phase1()

# ── Phase 2: full data loaded ONCE at server startup in a background worker ──
# All sessions share this via server_state; sessions poll every 3 sec.
server_state <- new.env(parent = emptyenv())
server_state$full_data     <- coach_phase1_data
server_state$load_complete <- FALSE

local({
  rdm_url   <- REDCAP_CONFIG$url
  rdm_token <- REDCAP_CONFIG$rdm_token

  promises::future_promise({
    source("R/globals.R")
    library(gmed); library(dplyr); library(purrr); library(REDCapR); library(lubridate)
    httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
    load_coaching_data(redcap_url = rdm_url, rdm_token = rdm_token)
  }) %...>% (function(full_data) {
    full_data$full_load_complete <- TRUE   # stops polling once session receives it
    server_state$full_data     <- full_data
    server_state$load_complete <- TRUE
    message("[Phase 2] Coach dash full data loaded at ",
            format(Sys.time(), "%H:%M:%S"))
  }) %...!% (function(err) {
    message("[Phase 2] Background load failed: ", err$message)
  })
})

# ==============================================================================
# UI
# ==============================================================================

# roundsui page shell ("Ward Notes"): a slim top bar over a server-driven
# main view. base_font/heading_font are left unset on purpose: passing
# "Inter" sets a bare font-family with no fallback stack, so machines
# without Inter installed render a serif. roundsui's default stack is used.
#
# gmed::load_gmed_styles() removed: nothing here (or in the gmed display
# modules used inside the review sections) uses .gmed-* classes or --gmed-*
# tokens, and its html/body rules fought roundsui's. If a future change
# reintroduces a --gmed-* reference it will silently lose its styling -
# grep for "--gmed-" across R/ and www/ before relying on it.
ui <- roundsui::roundsui_page(
  title = "IMSLU Coaching Dashboard",

  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),

  div(
    class = "coach-topbar",
    div(class = "coach-topbar__title", icon("user-doctor"), " IMSLU Coaching Dashboard"),
    div(
      class = "coach-topbar__meta",
      uiOutput("header_loading_status", inline = TRUE),
      uiOutput("header_coach_info", inline = TRUE)
    )
  ),

  uiOutput("main_content")
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  # ==========================================================================
  # REACTIVE VALUES
  # ==========================================================================
  
  # App state
  app_state <- reactiveValues(
    authenticated = FALSE,
    data_loaded = FALSE,
    current_view = "login"  # login, coach_select, resident_table, review
  )

  # App data — pre-populated with Phase 1 data so coach selector shows immediately.
  # Phase 2 polling observer (below) swaps in the full dataset when ready.
  app_data <- reactiveValues(
    data             = coach_phase1_data,
    data_dict        = coach_phase1_data$data_dict,
    last_loaded_time = Sys.time()
  )
  
  # ==========================================================================
  # AUTHENTICATION
  # ==========================================================================
  
  # Check if login should be bypassed (dev mode)
  if (should_bypass_login()) {
    message("========================================")
    message("LOGIN BYPASS ENABLED FOR DEVELOPMENT")
    message("========================================")
    app_state$authenticated <- TRUE
    app_state$current_view <- "coach_select"
  }
  
  # Phase 1 data is already in app_data — mark loaded immediately so the
  # coach selector renders without waiting for Phase 2.
  observe({
    req(app_state$authenticated)
    if (!app_state$data_loaded) {
      app_state$data_loaded <- TRUE
      message("[Session] Using Phase 1 data (", nrow(app_data$data$residents),
              " residents) while Phase 2 loads in background")
    }
  })

  # ── Phase 2 polling ─────────────────────────────────────────────────────────
  # server_state is set in app.R when the background load completes.
  # Poll every 3 sec; swap in full data when ready.
  observe({
    invalidateLater(3000)
    if (isTRUE(server_state$load_complete) &&
        !isTRUE(app_data$data$full_load_complete)) {
      full <- server_state$full_data
      app_data$data             <- full
      app_data$data_dict        <- full$data_dict
      app_data$last_loaded_time <- Sys.time()
      message("[Phase 2] Session received full data (",
              nrow(full$residents), " residents)")
    }
  })
  
  # Login module
  authenticated <- mod_login_server("login")
  
  # Update app state when authenticated via login
  observe({
    if (authenticated()) {
      app_state$authenticated <- TRUE
      app_state$current_view <- "coach_select"
      # Data loading is now handled by the unified loader above
    }
  })
  
  # ==========================================================================
  # COACH SELECTION
  # ==========================================================================

  # Wrap app_data$data in reactive for modules that expect reactiveVal
  app_data_reactive <- reactive({ app_data$data })

  coach_data <- mod_coach_select_server("coach_select", app_data_reactive)

  # Update view when coach is selected
  observe({
    # Validate coach_data first
    data <- tryCatch({
      coach_data()
    }, error = function(e) {
      NULL
    })

    # Only proceed if we have valid data
    req(data)
    req(is.list(data))

    # Safely access coach_name
    coach_name <- data$coach_name

    if (!is.null(coach_name) && nzchar(coach_name)) {
      app_state$current_view <- "resident_table"
    }
  })

  # ==========================================================================
  # RESIDENT SELECTION
  # ==========================================================================

  resident_selection <- mod_resident_table_server(
    "resident_table",
    coach_data,
    app_data_reactive,
    last_loaded = reactive({ app_data$last_loaded_time })
  )

  # Handle refresh trigger from resident table
  observe({
    tryCatch({
      count <- resident_selection$refresh_triggered()
      if (!is.null(count) && count > 0) {
        memoise::forget(load_coaching_data_cached)  # bust the cache
        app_state$data_loaded <- FALSE              # trigger reload
      }
    }, error = function(e) {})
  })

  # ==========================================================================
  # REVIEW INTERFACE (PHASE 2)
  # ==========================================================================

  # Primary review interface - pass both reactive data and reactiveValues for data_dict
  review_interface <- mod_review_interface_server(
    "review",
    selected_resident = reactive({ resident_selection$selected_resident() }),
    rdm_data = app_data_reactive,
    current_period = resident_selection$current_period,  # CORRECT - pass the reactive itself
    app_data_rv = app_data  # Pass reactiveValues for direct data_dict access
  )

  # Second review interface
  second_review_interface <- mod_second_review_server(
    "second_review",
    selected_resident = reactive({ resident_selection$selected_resident() }),
    rdm_data = app_data_reactive,
    current_period = resident_selection$current_period
  )
  
  # Update view when resident is selected
  observe({
    tryCatch({
      selected_res <- resident_selection$selected_resident

      if (is.function(selected_res)) {
        res_value <- selected_res()
        req(res_value)
        app_state$current_view <- "review"
      }
    }, error = function(e) {
      # Silently handle errors during reactive access
    })
  })
  
  # ==========================================================================
  # NAVIGATION HANDLERS
  # ==========================================================================

  # Helper: re-launch the Phase 2 background load so fresh data (including
  # newly submitted coach reviews) is picked up by the polling observer.
  # Safe to call from inside any observer — the future callback only writes
  # to server_state (a plain env), not to reactive values.
  trigger_data_refresh <- function() {
    rdm_url   <- REDCAP_CONFIG$url
    rdm_token <- REDCAP_CONFIG$rdm_token

    # Mark session data as stale so the polling observer will swap when done.
    server_state$load_complete <- FALSE
    stale <- app_data$data
    stale$full_load_complete <- FALSE
    app_data$data <- stale

    promises::future_promise({
      source("R/globals.R")
      library(gmed); library(dplyr); library(purrr); library(REDCapR); library(lubridate)
      httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
      load_coaching_data(redcap_url = rdm_url, rdm_token = rdm_token)
    }) %...>% (function(full_data) {
      full_data$full_load_complete <- TRUE
      server_state$full_data     <- full_data
      server_state$load_complete <- TRUE
      message("[Refresh] Data refreshed after submission at ",
              format(Sys.time(), "%H:%M:%S"))
    }) %...!% (function(err) {
      message("[Refresh] Data refresh failed: ", err$message)
      # Restore flags so the app doesn't stay in a loading state.
      server_state$load_complete <- TRUE
    })
  }

  # "Back to Residents" — primary review interface
  observeEvent(review_interface$back_to_table_clicked(), {
    resident_selection$clear_selection()
    app_state$current_view <- "resident_table"
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # "Return to Resident Table" — fired from the post-submission success modal.
  # Also triggers a background data refresh so completion indicators update.
  observeEvent(review_interface$post_submit_return(), {
    resident_selection$clear_selection()
    app_state$current_view <- "resident_table"
    trigger_data_refresh()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # "Back to Residents" — second review interface
  observeEvent(second_review_interface$back_to_table_clicked(), {
    resident_selection$clear_selection()
    app_state$current_view <- "resident_table"
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # "Change Coach" — primary review interface
  observeEvent(review_interface$change_coach_clicked(), {
    resident_selection$clear_selection()
    app_state$current_view <- "coach_select"
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # "Back to Coach Selection" — resident table
  observeEvent(resident_selection$back_to_coach_clicked(), {
    app_state$current_view <- "coach_select"
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # ==========================================================================
  # HEADER INFO
  # ==========================================================================

  # Loading spinner — visible until Phase 2 data arrives
  output$header_loading_status <- renderUI({
    if (app_state$authenticated && !isTRUE(app_data$data$full_load_complete)) {
      tags$span(
        icon("circle-notch", class = "fa-spin"),
        " Loading completion data\u2026"
      )
    }
  })

  output$header_coach_info <- renderUI({
    # Safely access coach_data with error handling
    data <- tryCatch({
      coach_data()
    }, error = function(e) {
      NULL
    })

    req(data)
    req(is.list(data))

    # Validate required fields exist
    if (!is.null(data$coach_name) && nzchar(data$coach_name)) {
      tagList(
        icon("user-tie"), " ", strong(data$coach_name), " | ",
        icon("users"), " ", data$stats$total, " residents"
      )
    }
  })
  
  # ==========================================================================
  # NAVIGATION - MAIN CONTENT
  # ==========================================================================
  
  output$main_content <- renderUI({
    tryCatch({

    # Show login screen if not authenticated
    if (!app_state$authenticated) {
      return(
        fluidRow(
          column(12, mod_login_ui("login"))
        )
      )
    }

    # Show loading screen if data not loaded
    if (!app_state$data_loaded) {
      return(
        fluidRow(
          column(
            12,
            div(
              style = "text-align: center; margin-top: 100px;",
              h3("Loading data..."),
              p("Please wait while we retrieve resident information.")
            )
          )
        )
      )
    }

    # Router based on current view
    switch(
      app_state$current_view,

      # Coach selection view
      "coach_select" = {
        fluidRow(
          column(12,
            div(
              style = "max-width: 800px; margin: 20px auto;",
              h3(
                style = "margin-bottom: 8px;",
                "Welcome to the Coaching Dashboard"
              ),
              p(
                style = "color: var(--roundsui-ink-muted); margin-bottom: 24px;",
                "Select your name below to view your assigned residents."
              ),
              mod_coach_select_ui("coach_select")
            )
          )
        )
      },
      
      # Resident table view
      "resident_table" = {
        fluidRow(
          column(12,
            div(
              style = "max-width: 1200px; margin: 20px auto;",
              mod_resident_table_ui("resident_table")
            )
          )
        )
      },
      
      # Review interface view (PHASE 2)
      "review" = {
        req(resident_selection$selected_resident())
        selected_res <- resident_selection$selected_resident()

        # Check if this is a second review or primary review
        if (!is.null(selected_res$role) && selected_res$role == "secondary") {
          # Show second review interface
          fluidRow(
            column(12,
              mod_second_review_ui("second_review")
            )
          )
        } else {
          # Show primary review interface
          fluidRow(
            column(12,
              mod_review_interface_ui("review")
            )
          )
        }
      },
      
      # Default/fallback
      {
        fluidRow(
          column(12,
            div(
              style = "text-align: center; margin-top: 100px;",
              h3("Navigation Error"),
              p("Unknown view state"),
              actionButton(
                "reset_to_login",
                "Return to Login",
                icon = icon("home"),
                onclick = "location.reload();"
              )
            )
          )
        )
      }
    )

    }, error = function(e) {
      # Return error UI
      fluidRow(
        column(12,
          div(
            style = "text-align: center; margin-top: 100px; color: red;",
            h3("Error Loading Interface"),
            p(e$message),
            actionButton(
              "reload_app",
              "Reload",
              icon = icon("refresh"),
              onclick = "location.reload();"
            )
          )
        )
      )
    })
  })

  # ==========================================================================
  # LEGACY NAVIGATION BUTTONS (from old UI - can be removed)
  # ==========================================================================
  
  # Back to coach selection
  observeEvent(input$back_to_coach, {
    message("WARNING: Using legacy back_to_coach button - should migrate to change_coach")
    app_state$current_view <- "coach_select"
  })
  
  # Back to resident table
  observeEvent(input$back_to_table, {
    message("WARNING: Using legacy back_to_table button - should migrate to new navigation")
    app_state$current_view <- "resident_table"
    resident_selection$clear_selection()
  })
  
  # ==========================================================================
  # SESSION INFO (for debugging)
  # ==========================================================================
  
  session$onSessionEnded(function() {
    message(sprintf(
      "[%s] Session ended",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ))
  })
}

# ==============================================================================
# RUN APP
# ==============================================================================

shinyApp(ui, server)