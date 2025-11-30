# Simplified Server Code Using GMED Package Functions

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
    
    if (!is.null(data$mock_mode) && data$mock_mode) {
      showNotification(
        "⚠️ Running in DEMO mode with mock data",
        type = "warning",
        duration = 10
      )
    } else if (!is.null(data$error)) {
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
    redcap_instance = NULL
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
    } else {
      shinyjs::show("access_error")
      showNotification("Invalid access code", type = "error")
    }
  })
  
  # ============================================================================
  # DATA PROCESSING REACTIVE (USING GMED FUNCTIONS)
  # ============================================================================
  
  # Process resident data with archive filtering and coach label mapping
  processed_resident_data <- reactive({
    req(app_data())
    data <- app_data()
    req(data$resident_data)
    
    resident_data <- data$resident_data
    message("Starting data processing with ", nrow(resident_data), " total rows")
    
    # STEP 1: Filter out archived residents using gmed function
    if ("res_archive" %in% names(resident_data)) {
      before_count <- nrow(resident_data)
      # Use the existing gmed archive function, but filter directly
      resident_data <- resident_data %>%
        filter(is.na(res_archive) | res_archive != "Yes")
      after_count <- nrow(resident_data)
      message("Filtered out archived residents: ", before_count, " -> ", after_count, " rows")
    } else {
      message("No res_archive column found - keeping all residents")
    }
    
    # STEP 2: Convert coach codes to readable names using gmed function
    data_dict <- data$rdm_dict %||% NULL
    resident_data <- gmed::convert_coach_codes_to_names(resident_data, data_dict)
    message("Converted coach codes to names using gmed function")
    
    # STEP 3: Ensure required columns exist with defaults
    required_cols <- c("name", "access_code", "coach", "second_rev", "Level")
    missing_cols <- setdiff(required_cols, names(resident_data))
    
    for (col in missing_cols) {
      resident_data[[col]] <- if (col == "Level") "Intern" else "Unknown"
    }
    
    # STEP 4: Filter for valid coaching data
    resident_data <- resident_data %>%
      filter(
        !is.na(name) & name != "",
        !is.na(coach) & coach != "" & coach != "Unknown",
        !is.na(access_code) & access_code != ""
      )
    
    message("Final processed data: ", nrow(resident_data), " residents")
    
    return(resident_data)
  })
  
  # ============================================================================
  # COACH SELECTION & RESIDENTS TABLE
  # ============================================================================
  
  # Populate coach dropdown with converted names
  observe({
    req(values$is_authenticated)
    req(processed_resident_data())
    
    resident_data <- processed_resident_data()
    
    coaches <- tryCatch({
      # Get unique coach names (should now be readable names, not codes)
      unique_coaches <- resident_data %>%
        filter(!is.na(coach), coach != "", coach != "Unknown") %>%
        pull(coach) %>%
        unique()
      
      # Also include second reviewers
      second_coaches <- resident_data %>%
        filter(!is.na(second_rev), second_rev != "", second_rev != "Unknown") %>%
        pull(second_rev) %>%
        unique()
      
      # Combine and sort
      all_coaches <- unique(c(unique_coaches, second_coaches)) %>%
        sort()
      
      message("Found coaches: ", paste(all_coaches, collapse = ", "))
      return(all_coaches)
      
    }, error = function(e) {
      message("Error getting coaches: ", e$message)
      character(0)
    })
    
    if (length(coaches) > 0) {
      updateSelectizeInput(session, "coach_name", choices = coaches, server = FALSE)
    } else {
      updateSelectizeInput(session, "coach_name", 
                           choices = c("No coaches found" = ""), 
                           server = FALSE)
    }
  })
  
  # Coach dashboard header
  output$dashboard_coach_name <- renderText({ 
    req(input$coach_name)
    input$coach_name 
  })
  
  # Helper function for data tables with click handling
  datatable_with_click <- function(data, caption = NULL) {
    DT::datatable(
      data,
      escape = FALSE,
      options = list(
        pageLength = 10,
        dom = 'ftp',
        scrollX = TRUE,
        columnDefs = list(
          list(width = "80px", targets = c(5, 6, 7)),  # Status columns
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
  
  # Coach residents table with filtering and label mapping
  output$coach_residents_table <- DT::renderDataTable({
    req(input$coach_name)
    req(processed_resident_data())
    
    resident_data <- processed_resident_data()
    current_period <- get_current_period()
    
    tryCatch({
      message("Creating coach residents table for: ", input$coach_name)
      message("Current period: ", current_period)
      
      # Filter residents for this coach (names should now be readable)
      coach_residents <- resident_data %>%
        filter(coach == input$coach_name | second_rev == input$coach_name) %>%
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
        
        # Process each resident to determine completion statuses
        for (i in 1:nrow(coach_residents)) {
          res_name <- coach_residents$name[i]
          res_level <- coach_residents$Level[i]
          
          # Map the period to REDCap format using gmed function
          redcap_period <- gmed::map_to_milestone_period(res_level, current_period)
          redcap_periods[i] <- ifelse(is.na(redcap_period), current_period, as.character(redcap_period))
          
          # Convert period to REDCap instance using mapping function
          coach_period <- map_app_period_to_coach_period(current_period, res_level)
          
          # Check completion statuses (simplified for now)
          has_self_eval <- check_completion_status(res_name, coach_period, app_data())$self_eval
          has_coach_review <- check_completion_status(res_name, coach_period, app_data())$coach_review
          has_second_review <- FALSE  # Simplified for now
          
          # Create status indicators
          self_eval_statuses[i] <- ifelse(has_self_eval, "✅", "❌")
          primary_review_statuses[i] <- ifelse(has_coach_review, "✅", "❌")
          secondary_review_statuses[i] <- ifelse(has_second_review, "✅", "❌")
        }
        
        # Add status columns
        coach_residents$self_eval_status <- self_eval_statuses
        coach_residents$primary_review_status <- primary_review_statuses
        coach_residents$secondary_review_status <- secondary_review_statuses
        coach_residents$redcap_period <- redcap_periods
        
        # Select columns for display
        coach_residents <- coach_residents %>%
          select(name, Level, access_code, review_role, redcap_period, 
                 self_eval_status, primary_review_status, secondary_review_status) %>%
          arrange(review_role, Level, name) %>%
          setNames(c("Resident Name", "Level", "Access Code", "Review Role", "Review Period", 
                     "Self-Eval", "Primary Review", "Secondary Review"))
        
        return(datatable_with_click(
          coach_residents, 
          caption = paste0("Residents Assigned to ", input$coach_name)
        ))
      }
      
      # Return empty datatable if no data found
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
  # RESIDENT SELECTION HANDLER
  # ============================================================================
  
  # Handle resident selection in the coach residents table
  observeEvent(input$selected_resident_in_table, {
    req(input$selected_resident_in_table)
    
    # Get selected resident info
    resident_info <- input$selected_resident_in_table
    
    message("=== RESIDENT SELECTION EVENT ===")
    message("Selected resident: ", resident_info$resident_name)
    message("Level: ", resident_info$resident_level)
    message("Review role: ", resident_info$review_role)
    
    # Find the full resident data
    resident_data <- processed_resident_data()
    
    full_resident_record <- tryCatch({
      resident_data %>%
        filter(name == resident_info$resident_name) %>%
        slice(1)
    }, error = function(e) {
      message("Error getting resident record: ", e$message)
      NULL
    })
    
    # Store resident info in values
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
    
    # Show review interface
    shinyjs::hide("coach-selection-page")
    shinyjs::show("review-pages")
    
    # Determine which review type to show
    if (resident_info$review_role == "Primary") {
      shinyjs::show("primary-review-content")
      shinyjs::hide("second-review-content")
    } else {
      shinyjs::hide("primary-review-content")
      shinyjs::show("second-review-content")
    }
    
    showNotification(
      paste("Starting", resident_info$review_role, "review for", resident_info$resident_name),
      type = "message"
    )
  })

  # Handle back button to return to coach table
  observeEvent(input$back_to_coach_table, {
    # Hide review pages
    shinyjs::hide("review-pages")
    shinyjs::hide("primary-review-content")
    shinyjs::hide("second-review-content")

    # Show coach selection page with table
    shinyjs::show("coach-selection-page")

    # Show notification
    showNotification("Returned to coach table", type = "message", duration = 2)
  })

  # ============================================================================
  # RESIDENT INFO DISPLAY
  # ============================================================================
  
  output$display_resident_name <- renderText({
    req(values$selected_resident)
    values$selected_resident$name
  })
  
  output$display_resident_level <- renderText({
    req(values$selected_resident)
    values$selected_resident$level
  })
  
  output$display_coach_name <- renderText({
    req(values$selected_coach)
    values$selected_coach
  })
  
  output$display_access_code <- renderText({
    req(values$selected_resident)
    values$selected_resident$access_code
  })
  
  output$display_current_period <- renderText({
    req(values$current_period)
    # Convert period code to readable format
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
  # PROGRESS AND NAVIGATION
  # ============================================================================
  
  output$current_step_text <- renderText({
    req(values$selected_resident)
    paste("Step 1 of 8: Getting Started -", values$selected_resident$review_role, "Review")
  })
  
  # Navigation back to coach selection
  observeEvent(input$back_to_dashboard, {
    shinyjs::hide("review-pages")
    shinyjs::show("coach-selection-page")
    
    # Reset selections
    values$selected_resident <- NULL
    values$current_period <- NULL
    values$review_type <- NULL
  })
  
  # ============================================================================
  # COMPLETION STATUS HELPER
  # ============================================================================
  
  check_completion_status <- function(resident_name, period_code, data) {
    # Simplified status checking
    record_id <- tryCatch({
      data$resident_data %>%
        filter(name == resident_name) %>%
        slice(1) %>%
        pull(record_id)
    }, error = function(e) NULL)
    
    if (is.null(record_id) || is.na(record_id)) {
      return(list(self_eval = FALSE, coach_review = FALSE))
    }
    
    # Check self-evaluation completion
    has_self_eval <- tryCatch({
      !is.null(data$s_eval) && 
        any(data$s_eval$name == resident_name, na.rm = TRUE)
    }, error = function(e) FALSE)
    
    # Check coach review completion  
    has_coach_review <- tryCatch({
      !is.null(data$coach_review) && 
        any(data$coach_review$name == resident_name, na.rm = TRUE)
    }, error = function(e) FALSE)
    
    return(list(
      self_eval = has_self_eval,
      coach_review = has_coach_review
    ))
  }
  
  # ============================================================================
  # SESSION CLEANUP
  # ============================================================================
  session$onSessionEnded(function() {
    message("Coach app session ended")
  })
}