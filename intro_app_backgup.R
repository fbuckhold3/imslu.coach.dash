### Backup UI/ Server for intern intro that was working
### 
### 
### 
# Fixed UI with simpler structure to prevent session termination

ui <- tryCatch({
  # Use page_fluid instead of gmed_page to avoid potential conflicts
  page_fluid(
    title = "IMSLU Coaching",
    theme = gmed::create_gmed_theme(),
    
    # Load styles manually
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "gmed/css/gmed-components.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$title("IMSLU Coaching Dashboard")
    ),
    
    # Enable shinyjs
    shinyjs::useShinyjs(),
    
    # Header
    div(
      class = "p-4 text-white mb-4",
      style = "background: linear-gradient(135deg, #003d5c 0%, #0066a1 100%); border-radius: 0 0 16px 16px;",
      h1("IMSLU Coaching", class = "text-center mb-2"),
      h4("Resident Coaching and Assessment Platform", class = "text-center mb-0 opacity-75")
    ),
    
    # ============================================================================
    # LOGIN PAGE
    # ============================================================================
    div(
      id = "login-page",
      fluidRow(
        column(
          width = 6,
          offset = 3,
          card(
            card_header("Welcome to IMSLU Coaching"),
            card_body(
              p("This application is designed to facilitate coaching sessions with IMSLU residents."),
              p("Please enter your access code to continue."),
              
              textInput("access_code", "Access Code"),
              actionButton("submit_access", "Submit", class = "btn-primary"),
              
              br(), br(),
              div(id = "access_error", class = "alert alert-danger", style = "display: none;",
                  "Invalid access code. Please try again.")
            )
          )
        )
      )
    ),
    
    # ============================================================================
    # COACH SELECTION PAGE
    # ============================================================================
    div(
      id = "coach-selection-page",
      style = "display: none; padding-bottom: 100px;",
      fluidRow(
        column(
          width = 10,
          offset = 1,
          card(
            card_header("Select Your Profile"),
            card_body(
              # Simplified coach selection without extra wrapper divs
              div(
                class = "text-center",
                style = "min-height: 200px; padding: 30px 0;",
                
                tags$label(
                  "Select Your Name",
                  class = "form-label text-center mb-4",
                  style = "font-size: 1.25rem; font-weight: 600; color: #003d5c; display: block;"
                ),
                
                # Simplified selectizeInput
                selectizeInput(
                  "coach_name", 
                  label = NULL,
                  choices = c("Loading coach names..." = ""),
                  options = list(
                    placeholder = "Select or type to search for your name",
                    create = FALSE,
                    searchField = c('text', 'value')
                  ),
                  width = "80%"
                )
              )
            )
          )
        )
      ),
      
      # Conditional panel that appears after coach selection
      conditionalPanel(
        condition = "input.coach_name !== null && input.coach_name !== '' && input.coach_name !== 'Loading coach names...'",
        fluidRow(
          column(
            width = 10,
            offset = 1,
            
            # Coach dashboard header
            div(
              class = "p-3 text-white mt-4 mb-3",
              style = "background: linear-gradient(135deg, #003d5c 0%, #0066a1 100%); border-radius: 12px;",
              h2("Coaching Dashboard for: ", textOutput("dashboard_coach_name", inline = TRUE), 
                 class = "text-center")
            ),
            
            # Residents table card
            card(
              card_header("Your Assigned Residents"),
              card_body(
                p(
                  class = "mb-3 text-center fw-bold",
                  style = "color: #0066a1;",
                  "Click on a resident row to start the review process"
                ),
                
                DT::dataTableOutput("coach_residents_table"),
                
                p(
                  class = "text-muted mt-3",
                  "Primary review: You are the main coach",
                  br(),
                  "Secondary review: You are reviewing another coach's feedback"
                )
              )
            )
          )
        )
      )
    ),
    
    # ============================================================================
    # REVIEW PAGES - FIXED VERSION
    # ============================================================================
    div(
      id = "review-pages",
      style = "display: none;",
      
      # Simple resident info panel
      fluidRow(
        column(
          width = 12,
          div(
            class = "p-3 mb-4",
            style = "background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%); border-left: 4px solid #0066a1; border-radius: 0 12px 12px 0; box-shadow: 0 4px 16px rgba(0, 61, 92, 0.08);",
            fluidRow(
              column(3, h5("Resident:", style = "color: #003d5c; margin-bottom: 0.25rem; font-weight: 600;"), textOutput("resident_name_display")),
              column(3, h5("Level:", style = "color: #003d5c; margin-bottom: 0.25rem; font-weight: 600;"), textOutput("resident_level_display")),
              column(3, h5("Period:", style = "color: #003d5c; margin-bottom: 0.25rem; font-weight: 600;"), textOutput("resident_period_display")),
              column(3, h5("Coach:", style = "color: #003d5c; margin-bottom: 0.25rem; font-weight: 600;"), textOutput("resident_coach_display"))
            )
          )
        )
      ),
      
      # Simple progress indicator
      fluidRow(
        column(
          width = 12,
          div(
            class = "text-center mb-3",
            style = "font-size: 1rem; font-weight: 600; color: #0066a1;",
            textOutput("current_step_text")
          ),
          div(
            class = "mb-4",
            style = "height: 16px; border-radius: 10px; background: #e9ecef; overflow: hidden; box-shadow: inset 0 2px 4px rgba(0, 0, 0, 0.1);",
            div(
              style = "width: 12.5%; background: linear-gradient(90deg, #0066a1, #4a90a4); transition: width 0.6s ease; border-radius: 10px; height: 100%;"
            )
          )
        )
      ),
      
      # DYNAMIC REVIEW CONTENT - This replaces the hardcoded placeholder
      fluidRow(
        column(
          width = 12,
          uiOutput("review_content")  # This calls your server-side renderUI function
        )
      ),
      
      # Back button (moved outside of the dynamic content)
      fluidRow(
        column(
          width = 12,
          div(
            class = "text-center mt-4 pt-3 border-top",
            actionButton("back_to_dashboard", "← Back to Dashboard", class = "btn-secondary")
          )
        )
      )
    ),
    
    # ============================================================================
    # DONE PAGE
    # ============================================================================
    div(
      id = "done-page",
      style = "display: none;",
      fluidRow(
        column(
          width = 8,
          offset = 2,
          card(
            card_header("Review Submitted"),
            card_body(
              div(
                class = "text-center",
                icon("check-circle", class = "fa-4x text-success"),
                h3("Thank you!"),
                p("Your review has been successfully submitted."),
                actionButton("start_new_review", "Start New Review", class = "btn-primary")
              )
            )
          )
        )
      )
    )
  )
}, error = function(e) {
  # Ultimate fallback
  message("UI creation failed completely: ", e$message)
  
  page_fluid(
    title = "IMSLU Coaching - Fallback",
    
    div(
      class = "alert alert-danger",
      h3("UI Loading Error"),
      p("There was an error loading the user interface."),
      p("Error message: ", e$message),
      p("Please contact support.")
    )
  )
})

server <- function(input, output, session) {
  
  # ============================================================================
  # STARTUP AND DATA LOADING
  # ============================================================================
  
  showNotification(
    "Loading coaching application data...",
    id = "loading",
    type = "message",
    duration = NULL
  )
  
  # Handle app data loading
  observe({
    req(app_data())
    data <- app_data()
    
    removeNotification("loading")
    
    if (!is.null(data$error)) {
      showNotification(
        paste("⚠️ Data loading issue:", data$error),
        type = "warning",
        duration = 10
      )
    } else {
      showNotification(
        "✅ Application loaded successfully",
        type = "message",
        duration = 3
      )
    }
  })
  
  # ============================================================================
  # REACTIVE VALUES
  # ============================================================================
  values <- reactiveValues(
    is_authenticated = FALSE,
    selected_coach = NULL,
    selected_resident = NULL,
    review_type = NULL,
    current_period = NULL,
    redcap_period = NULL,
    redcap_instance = NULL,
    gmed_tested = FALSE,
    dropdown_updated = FALSE
  )
  
  # ============================================================================
  # AUTHENTICATION
  # ============================================================================
  observeEvent(input$submit_access, {
    if (validate_access_code(input$access_code)) {
      values$is_authenticated <- TRUE
      shinyjs::hide("login-page")
      shinyjs::show("coach-selection-page")
      showNotification("Access granted!", type = "message")
      message("User authenticated successfully")
    } else {
      shinyjs::show("access_error")
      showNotification("Invalid access code", type = "error")
    }
  })
  
  # ============================================================================
  # DATA PROCESSING REACTIVE - FIXED LEVEL COLUMN HANDLING
  # ============================================================================
  processed_resident_data <- reactive({
    req(app_data())
    data <- app_data()
    req(data$resident_data)
    
    message("=== PROCESSING RESIDENT DATA ===")
    
    # Start with resident data
    resident_data <- data$resident_data
    message("Starting with ", nrow(resident_data), " residents")
    
    # Show sample raw data before conversion
    if (nrow(resident_data) > 0 && "coach" %in% names(resident_data)) {
      sample_coaches_raw <- resident_data$coach[1:min(5, nrow(resident_data))]
      message("Sample raw coach codes: ", paste(sample_coaches_raw, collapse = ", "))
    }
    
    # Get data dictionary for coach name conversion
    data_dict <- data$rdm_dict
    
    # Convert coach codes to names
    if (!is.null(data_dict)) {
      message("Converting coach codes using data dictionary...")
      resident_data <- gmed::convert_coach_codes_to_names(resident_data, data_dict)
    } else {
      message("Warning: No data dictionary available for coach name conversion")
    }
    
    # Show sample data after conversion
    if (nrow(resident_data) > 0 && "coach" %in% names(resident_data)) {
      sample_coaches_converted <- resident_data$coach[1:min(5, nrow(resident_data))]
      message("Sample converted coach names: ", paste(sample_coaches_converted, collapse = ", "))
    }
    
    # Filter invalid data but keep all levels
    initial_count <- nrow(resident_data)
    resident_data <- resident_data %>%
      filter(!is.na(name), name != "")
    
    message("After name filtering: ", nrow(resident_data), " residents (removed ", initial_count - nrow(resident_data), ")")
    
    # Show final coach distribution
    if ("coach" %in% names(resident_data)) {
      coach_summary <- table(resident_data$coach, useNA = "always")
      message("Final coach distribution:")
      for (i in 1:length(coach_summary)) {
        message("  ", names(coach_summary)[i], ": ", coach_summary[i])
      }
    }
    
    # *** FIX: Standardize level column naming ***
    # First, check what level columns exist
    level_cols <- grep("level", names(resident_data), ignore.case = TRUE, value = TRUE)
    message("Found level columns: ", paste(level_cols, collapse = ", "))
    
    # Create standardized level column
    if ("Level" %in% names(resident_data)) {
      resident_data$level <- resident_data$Level  # Copy Level to level
      message("Copied 'Level' to 'level' column")
    } else if ("level" %in% names(resident_data)) {
      resident_data$Level <- resident_data$level  # Copy level to Level
      message("Copied 'level' to 'Level' column")
    } else {
      # No level column found - this shouldn't happen, but let's handle it
      warning("No level column found in resident data")
      resident_data$level <- "Unknown"
      resident_data$Level <- "Unknown"
    }
    
    message("Final resident count: ", nrow(resident_data))
    message("================================")
    
    return(resident_data)
  })
  
  # ============================================================================
  # COACH DROPDOWN UPDATE
  # ============================================================================
  observe({
    req(values$is_authenticated)
    
    # Don't run if already updated
    if (values$dropdown_updated) return()
    
    message("=== CHECKING DATA READINESS ===")
    
    # Check if data is actually ready
    data_ready <- tryCatch({
      test_data <- processed_resident_data()
      !is.null(test_data) && nrow(test_data) > 0
    }, error = function(e) {
      message("Data not ready yet: ", e$message)
      FALSE
    })
    
    if (!data_ready) {
      message("Data not ready, will retry...")
      invalidateLater(1000, session)
      return()
    }
    
    message("Data is ready! Proceeding with dropdown update...")
    
    resident_data <- processed_resident_data()
    
    message("Extracting coaches from resident data...")
    
    # Get primary coaches
    primary_coaches <- character(0)
    if ("coach" %in% names(resident_data)) {
      primary_coaches <- resident_data %>%
        filter(!is.na(coach), coach != "", coach != "Unknown", coach != "Not assigned") %>%
        pull(coach) %>%
        unique()
      message("Found ", length(primary_coaches), " primary coaches")
    }
    
    # Get secondary coaches
    secondary_coaches <- character(0)
    if ("second_rev" %in% names(resident_data)) {
      secondary_coaches <- resident_data %>%
        filter(!is.na(second_rev), second_rev != "", second_rev != "Unknown", second_rev != "Not assigned") %>%
        pull(second_rev) %>%
        unique()
      message("Found ", length(secondary_coaches), " secondary coaches")
    }
    
    # Combine and sort
    all_coaches <- unique(c(primary_coaches, secondary_coaches)) %>%
      sort()
    
    message("Total unique coaches: ", length(all_coaches))
    message("Coach list: ", paste(all_coaches, collapse = ", "))
    
    # Update the dropdown
    message("Updating dropdown with all coaches...")
    
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
      
      # Fallback: try with named vector
      tryCatch({
        updateSelectizeInput(
          session = session,
          inputId = "coach_name",
          choices = setNames(all_coaches, all_coaches),
          server = FALSE
        )
        values$dropdown_updated <- TRUE
        message("✅ Fallback approach worked!")
      }, error = function(e2) {
        message("❌ Even fallback failed: ", e2$message)
      })
    })
    
    message("=== DROPDOWN UPDATE COMPLETE ===")
  })
  
  # ============================================================================
  # GMED FUNCTION TEST
  # ============================================================================
  observe({
    req(values$is_authenticated)
    
    if (values$gmed_tested) return()
    values$gmed_tested <- TRUE
    
    message("=== GMED FUNCTION TEST ===")
    
    current <- gmed::get_current_period()
    message("Current period: ", current)
    
    # Test period mappings
    test_cases <- list(
      list("Intern", "Mid Review", expected = "Mid Intern"),
      list("Intern", "Intern Intro", expected = "Intern Intro"),
      list("PGY2", "Mid Review", expected = "Mid PGY2"),
      list("PGY3", "End Review", expected = "Graduating")
    )
    
    for (test in test_cases) {
      result <- gmed::map_to_milestone_period(test[[1]], test[[2]])
      message(sprintf("Map test: %s + %s = %s (expected: %s) %s",
                      test[[1]], test[[2]], 
                      ifelse(is.na(result), "NA", result), 
                      ifelse(is.na(test$expected), "NA", test$expected),
                      ifelse(identical(result, test$expected), "✓", "✗")))
    }
    
    message("=========================")
  })
  
  # ============================================================================
  # HELPER FUNCTIONS - FIXED LEVEL PARAMETER HANDLING
  # ============================================================================
  
  check_completion_status <- function(resident_name, period_code, app_data, level = "Intern") {
    tryCatch({
      self_eval_complete <- gmed::check_self_eval_complete(
        app_data$resident_data, 
        resident_name, 
        period_code
      )
      
      coach_review_complete <- if (exists("check_coach_review_complete_status", where = "package:gmed")) {
        gmed::check_coach_review_complete_status(
          app_data$resident_data, 
          resident_name, 
          period_code,
          level = level  # Provide the level parameter
        )
      } else {
        FALSE
      }
      
      return(list(
        self_eval = self_eval_complete,
        coach_review = coach_review_complete
      ))
    }, error = function(e) {
      message("Error checking completion status: ", e$message)
      return(list(self_eval = FALSE, coach_review = FALSE))
    })
  }
  
  datatable_with_click <- function(data, caption = NULL) {
    DT::datatable(
      data,
      escape = FALSE,
      options = list(
        pageLength = 10,
        dom = 'ftp',
        scrollX = TRUE,
        columnDefs = list(
          list(width = "80px", targets = c(5, 6, 7)),
          list(
            targets = "_all",
            render = DT::JS(
              "function(data, type, row) {
                if (data === null || data === '') {
                    return '<span style=\"color: #999; font-style: italic;\">Not provided</span>';
                }
                return data;
              }"
            )
          )
        )
      ),
      caption = caption,
      rownames = FALSE,
      class = 'cell-border stripe hover',
      selection = 'single',
      callback = JS("
        table.on('click', 'tbody tr', function() {
            table.$('tr.selected').removeClass('selected');
            $(this).addClass('selected');
            
            var rowData = table.row(this).data();
            var residentName = rowData[0];
            var residentLevel = rowData[1];
            var accessCode = rowData[2];
            var reviewRole = rowData[3];
            var reviewPeriod = rowData[4];
            
            Shiny.setInputValue('selected_resident_in_table', {
                resident_name: residentName,
                resident_level: residentLevel,
                access_code: accessCode,
                review_role: reviewRole,
                review_period: reviewPeriod
            }, {priority: 'event'});
        });
      ")
    )
  }
  
  # ============================================================================
  # COACH DASHBOARD OUTPUTS
  # ============================================================================
  output$dashboard_coach_name <- renderText({ 
    req(input$coach_name)
    input$coach_name 
  })
  
  output$coach_residents_table <- DT::renderDataTable({
    req(input$coach_name)
    req(processed_resident_data())
    
    message("=== CREATING COACH RESIDENTS TABLE ===")
    message("Selected coach: ", input$coach_name)
    
    resident_data <- processed_resident_data()
    current_period <- gmed::get_current_period()
    
    tryCatch({
      # Filter residents for this coach
      coach_residents <- resident_data %>%
        filter(
          (coach == input$coach_name) | 
            (!is.na(second_rev) & second_rev == input$coach_name)
        ) %>%
        mutate(
          review_role = ifelse(coach == input$coach_name, "Primary", "Secondary")
        )
      
      message("Found ", nrow(coach_residents), " residents for ", input$coach_name)
      
      if (nrow(coach_residents) > 0) {
        # Initialize status vectors
        self_eval_statuses <- character(nrow(coach_residents))
        primary_review_statuses <- character(nrow(coach_residents))
        secondary_review_statuses <- character(nrow(coach_residents))
        redcap_periods <- character(nrow(coach_residents))
        
        # Process each resident
        for (i in 1:nrow(coach_residents)) {
          res_name <- coach_residents$name[i]
          
          # *** FIX: Use standardized level column ***
          res_level <- coach_residents$level[i]  # Use lowercase level
          
          # Check if reviewed in current period
          should_review <- TRUE
          if (exists("should_resident_be_reviewed", where = "package:gmed")) {
            should_review <- gmed::should_resident_be_reviewed(res_level, current_period)
          }
          
          if (should_review) {
            # Get milestone period
            redcap_period <- gmed::map_to_milestone_period(res_level, current_period)
            redcap_periods[i] <- ifelse(is.na(redcap_period), "No Review", as.character(redcap_period))
            
            # Get completion statuses with level parameter
            if (!is.na(redcap_period)) {
              completion <- check_completion_status(res_name, redcap_period, app_data(), level = res_level)
              self_eval_statuses[i] <- ifelse(completion$self_eval, "✅", "❌")
              primary_review_statuses[i] <- ifelse(completion$coach_review, "✅", "❌")
              secondary_review_statuses[i] <- "❌"
            } else {
              self_eval_statuses[i] <- "—"
              primary_review_statuses[i] <- "—" 
              secondary_review_statuses[i] <- "—"
            }
          } else {
            # No review this period
            redcap_periods[i] <- "No Review This Period"
            self_eval_statuses[i] <- "—"
            primary_review_statuses[i] <- "—"
            secondary_review_statuses[i] <- "—"
          }
        }
        
        # Add status columns
        coach_residents$self_eval_status <- self_eval_statuses
        coach_residents$primary_review_status <- primary_review_statuses
        coach_residents$secondary_review_status <- secondary_review_statuses
        coach_residents$redcap_period <- redcap_periods
        
        # Select and rename columns - USE LOWERCASE level
        coach_residents <- coach_residents %>%
          select(name, level, access_code, review_role, redcap_period,
                 self_eval_status, primary_review_status, secondary_review_status) %>%
          arrange(
            review_role,
            factor(level, levels = c("Intern", "PGY2", "PGY3", "Unknown")),  # Use lowercase level
            name
          ) %>%
          setNames(c("Resident Name", "Level", "Access Code", "Review Role", "Review Period",
                     "Self-Eval", "Primary Review", "Secondary Review"))
        
        message("Successfully created table with ", nrow(coach_residents), " rows")
        
        return(datatable_with_click(
          coach_residents,
          caption = paste0("Residents Assigned to ", input$coach_name, " - ", current_period)
        ))
      }
      
      # Return empty table if no residents found
      message("No residents found for ", input$coach_name)
      return(datatable_with_click(
        data.frame(
          `Resident Name` = character(0),
          `Level` = character(0),
          `Access Code` = character(0),
          `Review Role` = character(0),
          `Review Period` = character(0),
          `Self-Eval` = character(0),
          `Primary Review` = character(0),
          `Secondary Review` = character(0)
        ),
        caption = paste0("No residents found for ", input$coach_name)
      ))
      
    }, error = function(e) {
      message("Error creating residents table: ", e$message)
      return(DT::datatable(
        data.frame(Error = paste("Table error:", e$message)),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    })
  })
  
  # ============================================================================
  # RESIDENT SELECTION HANDLER - FIXED LEVEL HANDLING
  # ============================================================================
  observeEvent(input$selected_resident_in_table, {
    req(input$selected_resident_in_table)
    
    resident_info <- input$selected_resident_in_table
    
    message("=== RESIDENT SELECTION EVENT ===")
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
    
    # *** FIX: Store resident info with consistent level naming ***
    if (!is.null(full_resident_record) && nrow(full_resident_record) > 0) {
      values$selected_resident <- list(
        name = resident_info$resident_name,
        level = resident_info$resident_level,           # lowercase level
        Level = resident_info$resident_level,           # uppercase Level for compatibility
        access_code = resident_info$access_code,
        review_role = resident_info$review_role,
        period_code = resident_info$review_period,
        record_id = full_resident_record$record_id %||% "unknown"
      )
    } else {
      values$selected_resident <- list(
        name = resident_info$resident_name,
        level = resident_info$resident_level,           # lowercase level
        Level = resident_info$resident_level,           # uppercase Level for compatibility
        access_code = resident_info$access_code %||% "unknown",
        review_role = resident_info$review_role,
        period_code = resident_info$review_period,
        record_id = "unknown"
      )
    }
    
    values$selected_coach <- input$coach_name
    values$current_period <- resident_info$review_period
    values$review_type <- tolower(resident_info$review_role)
    
    # Show review interface
    shinyjs::hide("coach-selection-page")
    shinyjs::show("review-pages")
    
    showNotification(
      paste("Starting", resident_info$review_role, "review for", resident_info$resident_name),
      type = "message"
    )
  })
  
  # ============================================================================
  # RESIDENT INFO DISPLAYS
  # ============================================================================
  output$resident_name_display <- renderText({
    req(values$selected_resident)
    values$selected_resident$name
  })
  
  output$resident_level_display <- renderText({
    req(values$selected_resident)
    values$selected_resident$level  # Use lowercase level consistently
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
  
  # ============================================================================
  # NAVIGATION
  # ============================================================================
  output$current_step_text <- renderText({
    req(values$selected_resident)
    paste("Step 1 of 8: Getting Started -", values$selected_resident$review_role, "Review")
  })
  
  observeEvent(input$back_to_dashboard, {
    shinyjs::hide("review-pages")
    shinyjs::show("coach-selection-page")
    
    values$selected_resident <- NULL
    values$current_period <- NULL
    values$review_type <- NULL
  })
  
  observeEvent(input$start_new_review, {
    shinyjs::hide("done-page")
    shinyjs::show("coach-selection-page")
    
    values$selected_resident <- NULL
    values$current_period <- NULL
    values$review_type <- NULL
  })
  
  # ============================================================================
  # INTERN INTRO REVIEW DETECTION
  # ============================================================================
  is_intern_intro_review <- reactive({
    req(values$selected_resident)
    
    is_intern <- values$selected_resident$level == "Intern"  # Use lowercase level
    
    is_intro_period <- if (exists("is_intern_intro_period", where = "package:gmed")) {
      gmed::is_intern_intro_period(values$current_period)
    } else {
      values$current_period %in% c("Intern Intro", "7", "Intro")
    }
    
    result <- is_intern && is_intro_period
    message("Is intern intro review: ", result)
    
    return(result)
  })
  
  # ============================================================================
  # REVIEW CONTENT RENDERING
  # ============================================================================
  output$review_content <- renderUI({
    req(values$selected_resident)
    
    message("=== RENDERING REVIEW CONTENT ===")
    message("Resident: ", values$selected_resident$name)
    message("Level: ", values$selected_resident$level)  # Use lowercase level
    message("Period: ", values$current_period)
    
    # Check if this is an intern intro review
    is_intro <- tryCatch({
      is_intern_intro_review_check <- function() {
        is_intern <- values$selected_resident$level == "Intern"  # Use lowercase level
        is_intro_period <- values$current_period %in% c("Intern Intro", "7", "Intro")
        
        result <- is_intern && is_intro_period
        message("Intern intro check: Level=", values$selected_resident$level, 
                ", Period=", values$current_period, ", Result=", result)
        
        return(result)
      }
      
      is_intern_intro_review_check()
    }, error = function(e) {
      message("Error in intern intro check: ", e$message)
      FALSE
    })
    
    if (is_intro) {
      message("Creating intern intro interface...")
      
      # Use the intern intro module
      tryCatch({
        create_intern_intro_interface_with_submit(
          resident_name = values$selected_resident$name,
          app_data = app_data()
        )
      }, error = function(e) {
        message("Error creating intern intro interface: ", e$message)
        
        # Enhanced fallback interface
        div(
          class = "alert alert-danger mb-4",
          h4("Interface Loading Error"),
          p("There was an error loading the intern introduction interface."),
          p("Error: ", e$message),
          
          # Basic intern intro form as fallback
          h5("Basic Intern Introduction Review"),
          
          div(
            class = "mb-4",
            h6("Background Information"),
            textAreaInput(
              "coach_intro_back",
              "Where is this resident from, what are they excited about?",
              placeholder = "Document background information about the resident...",
              height = "100px"
            )
          ),
          
          div(
            class = "mb-4", 
            h6("Coping and Adjustment"),
            textAreaInput(
              "coach_coping", 
              "How is the resident adjusting to residency?",
              placeholder = "Document how the resident is coping...",
              height = "100px"
            )
          ),
          
          div(
            class = "mb-4",
            h6("Individual Learning Plan Summary"),
            textAreaInput(
              "coach_ilp_final",
              "Summarize the learning plan and next steps",
              placeholder = "Summarize the individual learning plan...",
              height = "100px"
            )
          ),
          
          br(),
          div(
            class = "text-center",
            actionButton(
              "submit_review",
              "Submit Intern Intro Review",
              class = "btn-success btn-lg",
              icon = icon("check")
            )
          )
        )
      })
      
    } else {
      message("Creating regular review interface...")
      
      # Regular review interface (your existing tabbed interface)
      navset_card_tab(
        id = "review_tabs",
        
        nav_panel(
          "Getting Started",
          icon = icon("play-circle"),
          div(
            class = "p-4",
            h4("Getting Started"),
            p("Welcome to the coaching review process."),
            
            textAreaInput(
              "coach_pre_rev",
              "Pre-review Discussion",
              placeholder = "Document your pre-review discussion with the resident...",
              height = "120px"
            )
          )
        ),
        
        nav_panel(
          "Coping & Wellness", 
          icon = icon("heart"),
          div(
            class = "p-4",
            h4("Coping and Wellness"),
            
            textAreaInput(
              "coach_coping",
              "How is the resident dealing with residency?",
              placeholder = "Document how the resident is coping with residency demands...",
              height = "120px"
            ),
            
            textAreaInput(
              "coach_wellness",
              "Wellness and wellbeing concerns",
              placeholder = "Any wellness or wellbeing concerns to document?",
              height = "120px"
            )
          )
        ),
        
        nav_panel(
          "Evaluations",
          icon = icon("clipboard-check"),
          div(
            class = "p-4",
            h4("Rotation & Clinical Evaluations"),
            
            textAreaInput(
              "coach_evaluations",
              "Assessment of evaluations",
              placeholder = "Your thoughts on how the resident has done with evaluations...",
              height = "120px"
            )
          )
        ),
        
        nav_panel(
          "Summary",
          icon = icon("flag-checkered"),
          div(
            class = "p-4",
            h4("Final Summary"),
            
            textAreaInput(
              "coach_summary",
              "Overall summary",
              placeholder = "Overall summary of the coaching session...",
              height = "120px"
            ),
            
            textAreaInput(
              "coach_ilp_final",
              "Individual Learning Plan Summary",
              placeholder = "Summarize the individual learning plan discussion...",
              height = "120px"
            ),
            
            br(),
            div(
              class = "text-center",
              actionButton(
                "submit_review",
                "Submit Review",
                class = "btn-success btn-lg",
                icon = icon("check")
              )
            )
          )
        )
      )
    }
  })
  
  # Replace your submission handlers in server.R with these:
  
  # ============================================================================
  # SUBMISSION HANDLERS - USING COACH APP FUNCTIONS (FIXED)
  # ============================================================================
  
  observeEvent(input$submit_intern_intro, {
    req(values$selected_resident)
    
    current_resident <- values$selected_resident$name
    req(current_resident)
    
    # Show processing notification (FIXED - remove type parameter)
    notification_id <- showNotification(
      "Submitting intern introduction data...", 
      duration = NULL
    )
    
    message("=== SUBMITTING INTERN INTRO DATA ===")
    message("Resident: ", current_resident)
    message("Level: ", values$selected_resident$level)
    message("Period: ", values$current_period)
    
    # Use coach app submission function
    result <- submit_coach_review_coach_app(
      resident_name = current_resident,
      inputs = reactiveValuesToList(input),
      app_data = app_data(),
      is_intern_intro = TRUE
    )
    
    # Remove processing notification
    removeNotification(notification_id)
    
    # Show result notification (FIXED - use correct type values)
    if (result$success) {
      showNotification(
        paste("✅ Successfully saved intern introduction data for", current_resident),
        type = "message",
        duration = 5
      )
      
      # Clear form data and return to coach selection
      # Reset all relevant inputs
      updateTextAreaInput(session, "coach_intro_back", value = "")
      updateTextAreaInput(session, "coach_coping", value = "")
      updateTextAreaInput(session, "coach_ilp_final", value = "")
      
      # Navigate back to coach selection page
      shinyjs::hide("review-pages")
      shinyjs::show("coach-selection-page")
      
      # Clear selected resident
      values$selected_resident <- NULL
      values$current_period <- NULL
      values$review_type <- NULL
      
    } else {
      showNotification(
        paste("❌ Failed to save data:", result$message),
        type = "error",
        duration = 10
      )
    }
  })
  
  # ============================================================================
  # REGULAR REVIEW SUBMISSION HANDLER (FIXED)
  # ============================================================================
  observeEvent(input$submit_review, {
    req(values$selected_resident)
    
    message("=== SUBMISSION EVENT TRIGGERED ===")
    message("Resident: ", values$selected_resident$name)
    message("Level: ", values$selected_resident$level)
    message("Period: ", values$current_period)
    
    # Determine if this is intern intro
    is_intro <- is_intern_intro_review()
    message("Is intern intro: ", is_intro)
    
    # Show processing notification (FIXED)
    notification_id <- showNotification(
      "Submitting coaching review data...", 
      duration = NULL
    )
    
    # Validate inputs using coach app function
    validation <- validate_coach_inputs(
      inputs = reactiveValuesToList(input),
      is_intern_intro = is_intro
    )
    
    if (!validation$valid) {
      removeNotification(notification_id)
      showNotification(
        paste("❌", validation$message),
        type = "error",
        duration = 8
      )
      return()
    }
    
    # Submit using coach app function
    result <- submit_coach_review_coach_app(
      resident_name = values$selected_resident$name,
      inputs = reactiveValuesToList(input),
      app_data = app_data(),
      is_intern_intro = is_intro
    )
    
    # Remove processing notification
    removeNotification(notification_id)
    
    # Handle the result (FIXED)
    if (result$success) {
      showNotification(
        paste("✅ Review submitted successfully for", values$selected_resident$name),
        type = "message",
        duration = 5
      )
      
      # Navigate back to coach selection instead of done page
      shinyjs::hide("review-pages")
      shinyjs::show("coach-selection-page")
      
      # Clear selected resident
      values$selected_resident <- NULL
      values$current_period <- NULL
      values$review_type <- NULL
      
    } else {
      showNotification(
        paste("❌ Submission failed:", result$message),
        type = "error",
        duration = 10
      )
      message("Submission failed with message: ", result$message)
    }
  })
  
  # ============================================================================
  # SESSION CLEANUP
  # ============================================================================
  session$onSessionEnded(function() {
    message("Coach app session ended")
  })
  
} 