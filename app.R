# ==============================================================================
# IMSLU COACHING DASHBOARD - MAIN APP
# app.R
# ==============================================================================
#
# Main application file for RDM 2.0 coaching dashboard
# Phase 1: Login, coach selection, resident table
# Phase 2+: Review interface with accordion sections
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
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper {
          background-color: #f4f6f9;
        }
        .main-header .navbar {
          background-color: #0072B2;
        }
        .main-header .logo {
          background-color: #005a8f;
        }
        .sidebar-menu > li.active > a {
          border-left-color: #0072B2;
        }
        .box {
          border-top: 3px solid #0072B2;
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
  
  # App data (loaded after authentication)
  app_data <- reactiveVal(NULL)
  
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
          data <- load_coaching_data()
          app_data(data)
          app_state$data_loaded <- TRUE
          
          incProgress(0.8, detail = "Processing complete")
          
          showNotification(
            sprintf("Data loaded: %d residents", nrow(data$residents)),
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
  
  coach_data <- mod_coach_select_server("coach_select", app_data)
  
  # Update view when coach is selected
  observe({
    req(coach_data())
    message("Coach data changed, coach_name: ", coach_data()$coach_name)
    if (!is.null(coach_data()$coach_name)) {
      message("=== Changing view to resident_table ===")
      app_state$current_view <- "resident_table"
    }
  })
  
  # ==========================================================================
  # RESIDENT SELECTION
  # ==========================================================================
  
  resident_selection <- mod_resident_table_server(
    "resident_table",
    coach_data,
    app_data
  )
  
  # Update view when resident is selected
  observe({
    req(resident_selection$selected_resident())
    app_state$current_view <- "review"
    
    # TODO: Navigate to review interface (Phase 2)
  })
  
  # ==========================================================================
  # HEADER INFO
  # ==========================================================================
  
  output$header_coach_info <- renderUI({
    req(coach_data())
    if (!is.null(coach_data()$coach_name)) {
      tagList(
        icon("user-tie"), " ", strong(coach_data()$coach_name), " | ",
        icon("users"), " ", coach_data()$stats$total, " residents"
      )
    }
  })
  
  # ==========================================================================
  # NAVIGATION - MAIN CONTENT
  # ==========================================================================
  
  output$main_content <- renderUI({
    
    message(sprintf("=== Rendering main_content, current_view: %s ===", app_state$current_view))
    
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
        message("=== Rendering resident_table UI ===")
        fluidRow(
          column(12,
            div(
              style = "margin: 20px;",
              
              # Back button
              actionButton(
                "back_to_coach",
                "Change Coach",
                icon = icon("arrow-left"),
                class = "btn-sm btn-default",
                style = "margin-bottom: 15px;"
              ),
              
              mod_resident_table_ui("resident_table")
            )
          )
        )
      },
      
      # Review interface (placeholder for Phase 2)
      "review" = {
        req(resident_selection$selected_resident())
        
        resident <- resident_selection$selected_resident()
        
        fluidRow(
          column(12,
            div(
              style = "margin: 20px;",
              
              # Back button
              actionButton(
                "back_to_table",
                "Back to Resident List",
                icon = icon("arrow-left"),
                class = "btn-sm btn-default",
                style = "margin-bottom: 15px;"
              ),
              
              # Placeholder for review interface
              box(
                width = 12,
                title = format_resident_summary(resident),
                status = "primary",
                solidHeader = TRUE,
                
                h4("Review Interface - Coming in Phase 2"),
                p("This is where the accordion review interface will go."),
                
                tags$ul(
                  tags$li("Section 1: Wellness & Progress"),
                  tags$li("Section 2: Evaluations & Feedback"),
                  tags$li("Section 3: Learning & Board Preparation"),
                  tags$li("Section 4: Scholarship"),
                  tags$li("Section 5: Career Planning"),
                  tags$li("Section 6: Goals & ILP Review"),
                  tags$li("Section 7: ILP Summary"),
                  tags$li("Section 8: Milestone Entry")
                ),
                
                hr(),
                
                actionButton(
                  "test_submit",
                  "Test Submit (Placeholder)",
                  icon = icon("check"),
                  class = "btn-success",
                  disabled = "disabled"
                )
              )
            )
          )
        )
      }
    )
  })
  
  # ==========================================================================
  # NAVIGATION BUTTONS
  # ==========================================================================
  
  # Back to coach selection
  observeEvent(input$back_to_coach, {
    app_state$current_view <- "coach_select"
  })
  
  # Back to resident table
  observeEvent(input$back_to_table, {
    app_state$current_view <- "resident_table"
    resident_selection$deselect()
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
