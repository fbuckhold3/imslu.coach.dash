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

# ==============================================================================
# UI
# ==============================================================================

ui <- dashboardPage(
  
  # ============================================================================
  # HEADER
  # ============================================================================
  dashboardHeader(
    title = "IMSLU Coaching Dashboard",
    titleWidth = 300,
    
    # Add coach info to header (right side)
    tags$li(
      class = "dropdown",
      style = "padding: 15px; color: #fff;",
      uiOutput("header_coach_info")
    )
  ),
  
  # ============================================================================
  # SIDEBAR - DISABLED
  # ============================================================================
  dashboardSidebar(
    disable = TRUE
  ),
  
  # ============================================================================
  # BODY
  # ============================================================================
  dashboardBody(

    # Load gmed styles for modern look
    gmed::load_gmed_styles(theme = "slucare"),

    # Custom CSS - Modern design matching self-assessment app
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$style(HTML("
        /* Modern color scheme */
        :root {
          --primary-gradient: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          --ssm-blue-gradient: linear-gradient(135deg, #003d5c, #0066a1);
          --success-color: #10b981;
          --gray-light: #e5e7eb;
          --gray-medium: #9ca3af;
          --gray-dark: #6b7280;
        }

        /* Override shinydashboard colors with modern palette */
        .content-wrapper {
          background: linear-gradient(to bottom, #f8f9fa 0%, #ffffff 100%);
        }

        .main-header .navbar {
          background: linear-gradient(135deg, #003d5c, #0066a1) !important;
          border: none;
          box-shadow: 0 2px 8px rgba(0, 61, 92, 0.15);
        }

        .main-header .logo {
          background: #003d5c !important;
          font-weight: 600;
          letter-spacing: 0.5px;
        }

        /* Modern card styling */
        .box {
          border: none;
          border-radius: 12px;
          box-shadow: 0 2px 8px rgba(0, 61, 92, 0.08);
          transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
          overflow: hidden;
        }

        .box:hover {
          transform: translateY(-2px);
          box-shadow: 0 8px 24px rgba(0, 61, 92, 0.12);
        }

        .box-header {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          border: none;
          padding: 15px 20px;
          font-weight: 600;
        }

        /* Modern buttons */
        .btn-primary {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          border: none;
          border-radius: 8px;
          padding: 10px 24px;
          font-weight: 600;
          letter-spacing: 0.5px;
          transition: all 0.3s ease;
          box-shadow: 0 4px 12px rgba(102, 126, 234, 0.3);
        }

        .btn-primary:hover {
          transform: translateY(-2px);
          box-shadow: 0 6px 20px rgba(102, 126, 234, 0.4);
        }

        .btn-success {
          background: linear-gradient(135deg, #10b981, #059669);
          border: none;
          border-radius: 8px;
          box-shadow: 0 4px 12px rgba(16, 185, 129, 0.3);
        }

        .btn-info {
          background: linear-gradient(135deg, #003d5c, #0066a1);
          border: none;
          border-radius: 8px;
          box-shadow: 0 4px 12px rgba(0, 61, 92, 0.3);
        }

        /* Card animations */
        @keyframes fadeIn {
          from {
            opacity: 0;
            transform: translateY(10px);
          }
          to {
            opacity: 1;
            transform: translateY(0);
          }
        }

        .box {
          animation: fadeIn 0.3s ease-in;
        }
      "))
    ),

    # Enable shinyjs
    useShinyjs(),

    # Main content area
    uiOutput("main_content")
  )
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

  # App data (loaded after authentication) - CHANGED TO reactiveValues to match self-assessment app
  # This allows direct access to app_data$... outside reactive contexts
  app_data <- reactiveValues(
    data = NULL,
    data_dict = NULL
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
  
  # Load data when authenticated (works for both bypass and login)
  observe({
    req(app_state$authenticated)
    req(!app_state$data_loaded)
    
    message("Starting data load...")
    
    isolate({
      withProgress(message = "Loading data...", value = 0, {
        incProgress(0.2, detail = "Connecting to REDCap...")
        
        tryCatch({
          loaded_data <- load_coaching_data()
          app_data$data <- loaded_data
          app_data$data_dict <- loaded_data$data_dict  # Store separately for easy access
          app_state$data_loaded <- TRUE

          incProgress(0.8, detail = "Processing complete")

          showNotification(
            sprintf("Data loaded: %d residents", nrow(loaded_data$residents)),
            type = "message",
            duration = 3
          )
          
        }, error = function(e) {
          showNotification(
            sprintf("Error loading data: %s", e$message),
            type = "error",
            duration = 10
          )
          message("ERROR in data loading: ", e$message)
          print(e)
        })
      })
    })
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
    app_data_reactive
  )

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
  # NAVIGATION HANDLERS - UPDATED FOR NEW BUTTONS
  # ==========================================================================
  
  # Handle "Back to Residents" button from primary review interface
  observe({
    tryCatch({
      if (!is.null(review_interface$back_to_table_clicked) && is.function(review_interface$back_to_table_clicked)) {
        clicked_count <- review_interface$back_to_table_clicked()
        if (!is.null(clicked_count) && clicked_count > 0) {
          resident_selection$clear_selection()
          app_state$current_view <- "resident_table"
        }
      }
    }, error = function(e) {
      # Silently handle errors
    })
  })

  # Handle "Back to Residents" button from second review interface
  observe({
    tryCatch({
      if (!is.null(second_review_interface$back_to_table_clicked) && is.function(second_review_interface$back_to_table_clicked)) {
        clicked_count <- second_review_interface$back_to_table_clicked()
        if (!is.null(clicked_count) && clicked_count > 0) {
          resident_selection$clear_selection()
          app_state$current_view <- "resident_table"
        }
      }
    }, error = function(e) {
      # Silently handle errors
    })
  })

  # Handle "Change Coach" button from review interface
  observe({
    tryCatch({
      if (!is.null(review_interface$change_coach_clicked) && is.function(review_interface$change_coach_clicked)) {
        clicked_count <- review_interface$change_coach_clicked()
        if (!is.null(clicked_count) && clicked_count > 0) {
          resident_selection$clear_selection()
          app_state$current_view <- "coach_select"
        }
      }
    }, error = function(e) {
      # Silently handle errors
    })
  })
  
  # LEGACY: Keep old back_clicked handler for compatibility
  observe({
    tryCatch({
      if (!is.null(review_interface$back_clicked) && is.function(review_interface$back_clicked)) {
        clicked_count <- review_interface$back_clicked()
        if (!is.null(clicked_count) && clicked_count > 0) {
          app_state$current_view <- "resident_table"
          resident_selection$clear_selection()
        }
      }
    }, error = function(e) {
      # Silently ignore - this is legacy code
    })
  })

  # Handle "Back to Coach Selection" button from resident table
  observe({
    tryCatch({
      if (!is.null(resident_selection$back_to_coach_clicked) && is.function(resident_selection$back_to_coach_clicked)) {
        clicked_count <- resident_selection$back_to_coach_clicked()
        if (!is.null(clicked_count) && clicked_count > 0) {
          app_state$current_view <- "coach_select"
        }
      }
    }, error = function(e) {
      # Silently handle errors
    })
  })
  
  # ==========================================================================
  # HEADER INFO
  # ==========================================================================
  
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
                style = "color: #0072B2; margin-bottom: 20px;",
                icon("user-tie"),
                " Welcome to the Coaching Dashboard"
              ),
              p(
                style = "font-size: 16px; color: #666; margin-bottom: 30px;",
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