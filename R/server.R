server <- function(input, output, session) {
    redcap_url <- "https://redcapsurvey.slu.edu/api/"

    # Get list of valid coach_rev fields from the data dictionary
    coach_rev_fields <- reactive({
        req(app_data())
        
        # Extract fields for coach_rev form
        if (!is.null(app_data()$rdm_dict)) {
            fields <- app_data()$rdm_dict %>%
                filter(form_name == "coach_rev") %>%
                pull(field_name)
            
            if (length(fields) > 0) {
                return(fields)
            }
        }
        
        # Fallback list if dictionary not available
        c("coach_date", "coach_period", "coach_pre_rev", "coach_intro_back", 
          "coach_coping", "coach_wellness", "coach_evaluations", "coach_p_d_comments", 
          "coach_ls_and_topic", "coach_step_board", "coach_scholarship", "coach_mile_goal", 
          "coach_career", "coach_anyelse", "coach_summary", "coach_rev_complete")
    })
    
    # Development mode flag (set to FALSE for production)
    dev_mode <- FALSE
    
    setup_imres_resources()
    
    # Reactive values to store session state
    values <- reactiveValues(
        is_authenticated = FALSE,
        selected_coach = NULL,
        selected_resident = NULL,
        current_tab = "pre_review",
        review_type = NULL,
        primary_review_data = NULL,
        current_period = NULL,
        redcap_period = NULL,  # Add this line
        redcap_prev_period = NULL,
        tab_order = c("pre_review", "wellness", "evaluations", "knowledge", 
                      "scholarship", "ilp", "career", "summary", "milestones")
    )
    
    # Reactive values to store resident data
    resident_data_cache <- reactiveValues(
        s_eval = NULL,
        ilp_current = NULL,
        ilp_previous = NULL,
        ccc = NULL,
        scholarship = NULL
    )
    
    milestone_data <- reactiveValues(
        current_data = NULL,
        submission_status = NULL
    )
    
    # Show loading notification
    showNotification("Loading data... please wait", type = "message", duration = NULL, id = "loading")
    
    # Load app data reactively - this uses the ensure_data_loaded function from global.R
    app_data <- reactive({
        data <- ensure_data_loaded()
        
        # Remove loading notification once data is loaded
        removeNotification("loading")
        
        # For debugging - show what data was loaded
        if (dev_mode) {
            message("Data loaded with components: ", paste(names(data), collapse=", "))
            if (!is.null(data$ilp)) {
                message("ILP data loaded with ", nrow(data$ilp), " rows")
            } else {
                message("WARNING: ILP data is NULL!")
            }
        }
        
        return(data)
    })
    
    # Processed resident data with calculated levels
    processed_resident_data <- reactive({
        data <- app_data()
        
        if (!is.null(data$resident_data)) {
            # Calculate and add resident levels
            processed_data <- calculate_resident_level(data$resident_data)
            
            # For debugging
            if (dev_mode) {
                message("Processed resident data with levels. Sample:")
                print(head(processed_data %>% select(name, type, grad_yr, Level)))
            }
            
            return(processed_data)
        } else {
            return(NULL)
        }
    })
    
    
    # Centralized period mapping to incorporate into other functions
    observe({
        req(values$selected_resident)
        req(values$current_period)
        
        # Map the current period
        values$redcap_period <- map_to_milestone_period(
            values$selected_resident$Level, 
            values$current_period
        )
        
        # Get the previous period in app format
        prev_app_period <- get_previous_period(
            values$current_period, 
            values$selected_resident$Level
        )
        
        # Map the previous period if it exists
        if (!is.na(prev_app_period)) {
            values$redcap_prev_period <- map_to_milestone_period(
                values$selected_resident$Level, 
                prev_app_period
            )
            
            # Special case handling for End Review Interns, similar to your original code
            if (values$current_period == "End Review" && values$selected_resident$Level == "Intern") {
                values$redcap_prev_period <- "Mid Intern"
            }
        } else {
            values$redcap_prev_period <- NA
        }
        
        # For debugging
        if (dev_mode) {
            message(paste("Centralized period mapping:", values$current_period, "->", values$redcap_period, 
                          "for resident level:", values$selected_resident$Level))
            message(paste("Previous period mapping:", 
                          ifelse(is.na(prev_app_period), "None", prev_app_period), "->", 
                          ifelse(is.na(values$redcap_prev_period), "None", values$redcap_prev_period)))
        }
    })
    
    # Reactive to determine the REDCap instance
    redcap_instance <- reactive({
        req(values$selected_resident$Level)
        req(values$current_period)
        
        # Map level and period to a REDCap instance
        instance <- map_period_format(
            level = values$selected_resident$Level,
            period = values$current_period,
            return_type = "instance"
        )
        
        # For debugging
        if (dev_mode) {
            message(paste("REDCap instance calculated:", instance, 
                          "for Level:", values$selected_resident$Level, 
                          "and Period:", values$current_period))
        }
        
        return(instance)
    })
    
    # Authentication logic
    #--------------------------------------------------
    # Access code validation
    observeEvent(input$submit_access, {
        # Get the access code based on mode
        stored_access_code <- if(dev_mode) "default123" else Sys.getenv("ACCESS_CODE")
        
        # Compare the input with the stored access code
        if (input$access_code == stored_access_code) {
            values$is_authenticated <- TRUE
            shinyjs::hide("login-page")
            shinyjs::show("coach-selection-page")
        } else {
            shinyjs::show("access_error")
        }
    })
    
    # Coach selection
    #--------------------------------------------------
    # Populate coach dropdown immediately after app_data is loaded
    observe({
        # Get coach list from data - use app_data() reactive
        data <- app_data()
        
        # Extract unique coach names from resident_data
        if (!is.null(data$resident_data) && "coach" %in% names(data$resident_data)) {
            coaches <- unique(data$resident_data$coach)
            coaches <- coaches[!is.na(coaches) & coaches != ""]
            
            if (length(coaches) > 0) {
                updateSelectizeInput(session, "coach_name", 
                                     choices = coaches,
                                     server = FALSE)  # Set server=FALSE to load all options at once
            } else {
                # Fallback if no coaches found
                updateSelectizeInput(session, "coach_name", 
                                     choices = c("No coaches found" = ""))
            }
        } else {
            # Fallback for development
            fake_coaches <- c("Dr. Smith", "Dr. Johnson", "Dr. Williams")
            updateSelectizeInput(session, "coach_name", choices = fake_coaches)
        }
    })
    
    # Coach dashboard header
    output$dashboard_coach_name <- renderText({
        req(input$coach_name)
        input$coach_name
    })
    
    # Helper function for data tables with click handling
    datatable_with_click <- function(data, caption = NULL) {
        # Base DT with click handling
        dt <- DT::datatable(
            data,
            options = list(
                pageLength = 10,
                dom = 'ftp',
                scrollX = TRUE,
                columnDefs = list(list(
                    targets = "_all",
                    render = DT::JS(
                        "function(data, type, row) {
                if (data === null || data === '') {
                    return '<span style=\"color: #999; font-style: italic;\">Not provided</span>';
                }
                return data;
            }"
                    )
                ))
            ),
            caption = caption,
            rownames = FALSE,
            class = 'cell-border stripe hover',
            selection = 'single',
            callback = JS("
        table.on('click', 'tbody tr', function() {
            // Clear previously selected rows
            table.$('tr.selected').removeClass('selected');
            
            // Select this row
            $(this).addClass('selected');
            
            // Get the row data
            var rowData = table.row(this).data();
            var residentName = rowData[0];
            var residentLevel = rowData[1];
            var accessCode = rowData[2];
            var reviewRole = rowData[3];
            var reviewPeriod = rowData[4];
            
            console.log('Selected resident: ' + residentName + ', Level: ' + residentLevel + 
                        ', Access code: ' + accessCode + ', Review role: ' + reviewRole + 
                        ', Period: ' + reviewPeriod);
            
            // Set input value for Shiny with Level, review role and period
            Shiny.setInputValue('selected_resident_in_table', 
                {
                    name: residentName, 
                    level: residentLevel,
                    access_code: accessCode, 
                    review_role: reviewRole, 
                    review_period: reviewPeriod
                }, 
                {priority: 'event'});
        });
    ")
        ) %>%
            DT::formatStyle(
                columns = names(data),
                backgroundColor = '#f8f9fa',
                borderColor = '#dfe2e5'
            )
        
        return(dt)
    }
    
    # Function to check self-evaluation completeness
    check_self_eval_complete <- function(s_miles_data, s_eval_data, resident_name, period, level) {
        # Check if either miles data or eval data is available
        if (is.null(s_miles_data) && is.null(s_eval_data)) {
            return(FALSE)
        }
        
        # Check milestone data (if available)
        if (!is.null(s_miles_data)) {
            # Filter for this resident, period, level
            miles_filtered <- s_miles_data %>%
                filter(name == resident_name, period == period, level == level)
            
            if (nrow(miles_filtered) > 0) {
                # Check if most milestone fields are filled in
                # This is a simplified placeholder - replace with your actual logic
                return(TRUE)
            }
        }
        
        # Check self-eval data (if available)
        if (!is.null(s_eval_data)) {
            # Filter for this resident's self-eval data
            eval_filtered <- s_eval_data %>%
                filter(name == resident_name)
            
            # Additional period check if s_e_period field exists
            if ("s_e_period" %in% names(eval_filtered)) {
                eval_filtered <- eval_filtered %>% filter(s_e_period == period)
            }
            
            if (nrow(eval_filtered) > 0) {
                # Check if key fields have content
                required_fields <- c("s_e_plus", "s_e_delta")
                required_fields <- intersect(required_fields, names(eval_filtered))
                
                if (length(required_fields) > 0) {
                    for (field in required_fields) {
                        if (!is.na(eval_filtered[[field]][1]) && eval_filtered[[field]][1] != "") {
                            return(TRUE)
                        }
                    }
                }
            }
        }
        
        # Default: not complete
        return(FALSE)
    }
    
    output$coach_residents_table <- DT::renderDataTable({
        req(input$coach_name)
        
        # Get resident data
        resident_data <- processed_resident_data()
        
        # Calculate current period
        current_period <- get_current_period()
        message("Current period: ", current_period)
        
        # Debug: Check if we have coach_rev fields for alpha test
        if (any(resident_data$name == "alpha test")) {
            alpha_rows <- which(resident_data$name == "alpha test")
            message("Found alpha test rows: ", length(alpha_rows))
            
            # Check for coach_rev fields
            if ("coach_rev_complete" %in% names(resident_data)) {
                message("Sample coach_rev_complete values for alpha test: ", 
                        paste(head(resident_data$coach_rev_complete[alpha_rows], 3), collapse=", "))
            }
            
            if ("coach_ilp_final" %in% names(resident_data)) {
                for (row in alpha_rows) {
                    if (!is.na(resident_data$coach_ilp_final[row]) && 
                        resident_data$coach_ilp_final[row] != "") {
                        message("Found coach_ilp_final for alpha test in row ", row, ": ", 
                                substr(resident_data$coach_ilp_final[row], 1, 30), "...")
                    }
                }
            }
        }
        
        # Check if we have resident data with the required columns
        if (!is.null(resident_data) && is.data.frame(resident_data) && 
            all(c("name", "access_code", "coach", "second_rev", "Level") %in% names(resident_data))) {
            
            # Filter for residents where selected coach is either primary or secondary reviewer
            coach_residents <- resident_data %>%
                filter(coach == input$coach_name | second_rev == input$coach_name) %>%
                mutate(
                    review_role = case_when(
                        coach == input$coach_name ~ "Primary",
                        second_rev == input$coach_name ~ "Secondary",
                        TRUE ~ NA_character_
                    )
                )
            
            # Create empty vector to store REDCap periods
            redcap_periods <- character(nrow(coach_residents))
            
            message("Found ", nrow(coach_residents), " residents for coach ", input$coach_name)
            
            # Process each resident to determine self-eval status
            self_eval_statuses <- character(nrow(coach_residents))
            primary_review_statuses <- character(nrow(coach_residents))
            secondary_review_statuses <- character(nrow(coach_residents))
            
            for (i in 1:nrow(coach_residents)) {
                res_name <- coach_residents$name[i]
                res_role <- coach_residents$review_role[i]
                res_level <- coach_residents$Level[i]
                
                message("\nProcessing completion statuses for: ", res_name, " (Role: ", res_role, ")")
                
                # Map the period to REDCap format
                redcap_period <- map_to_milestone_period(res_level, current_period)
                redcap_periods[i] <- redcap_period
                
                # Convert period to REDCap instance
                coach_period <- map_period_format(
                    level = res_level,
                    period = current_period,
                    return_type = "instance"
                )
                
                message("Mapped period: ", current_period, " -> ", redcap_period, " (instance: ", coach_period, ")")
                
                # Check self-evaluation status (for beta test we still hardcode)
                has_self_eval <- FALSE
                
                # Hard-code beta test only to have complete self-evaluations
                if (res_name == "beta test") {
                    message("Setting beta test account self-eval to complete: ", res_name)
                    has_self_eval <- TRUE
                } else {
                    # For all other residents including alpha test, do a thorough check
                    resident_rows <- which(resident_data$name == res_name)
                    
                    if (length(resident_rows) > 0) {
                        message("Found ", length(resident_rows), " rows for ", res_name)
                        
                        # Check for any completion indicators
                        for (row_idx in resident_rows) {
                            # Check all potential completion indicators
                            if ("s_eval_complete" %in% names(resident_data)) {
                                val <- resident_data$s_eval_complete[row_idx]
                                if (!is.na(val) && val == "Complete") {
                                    has_self_eval <- TRUE
                                    message("Found s_eval_complete = Complete in row ", row_idx)
                                    break
                                }
                            }
                            
                            if ("s_e_period" %in% names(resident_data)) {
                                period_val <- resident_data$s_e_period[row_idx]
                                
                                # If period matches and key fields have content
                                if (!is.na(period_val) && period_val == redcap_period) {
                                    message("Period matches redcap_period in row ", row_idx)
                                    
                                    # Check key fields
                                    for (field in c("s_e_plus", "s_e_delta")) {
                                        if (field %in% names(resident_data)) {
                                            field_val <- resident_data[[field]][row_idx]
                                            
                                            if (!is.na(field_val) && nchar(field_val) > 0) {
                                                has_self_eval <- TRUE
                                                message("Found content in ", field, " in row ", row_idx)
                                                break
                                            }
                                        }
                                    }
                                }
                            }
                            
                            if (has_self_eval) break
                        }
                    }
                }
                
                # Set the self-eval status
                self_eval_statuses[i] <- if(has_self_eval) "✅" else "❌"
                message("Final self-eval status for ", res_name, ": ", self_eval_statuses[i])
                
                # Now check primary review completion status for ALL residents including alpha test
                has_primary_review <- FALSE
                
                # Look for coach_rev fields in the resident data
                resident_rows <- which(resident_data$name == res_name)
                
                if (length(resident_rows) > 0) {
                    message("Checking primary review for ", res_name, " with ", length(resident_rows), " rows")
                    
                    for (row_idx in resident_rows) {
                        # First check: Any coach_rev fields have content regardless of period
                        if (res_name == "alpha test") {
                            # Special debugging for alpha test
                            message("Checking alpha test row ", row_idx, " for any coach_rev content")
                            
                            # Check key fields directly without period filter for alpha test
                            for (field in c("coach_pre_rev", "coach_summary", "coach_ilp_final")) {
                                if (field %in% names(resident_data)) {
                                    field_val <- resident_data[[field]][row_idx]
                                    if (!is.na(field_val) && nchar(field_val) > 0) {
                                        has_primary_review <- TRUE
                                        message("FOUND! ", field, " has content in row ", row_idx, ": ", 
                                                substr(field_val, 1, 30), "...")
                                        break
                                    } else {
                                        message("No content in ", field, " in row ", row_idx)
                                    }
                                }
                            }
                            
                            # Check completion status field directly
                            if ("coach_rev_complete" %in% names(resident_data)) {
                                val <- resident_data$coach_rev_complete[row_idx]
                                if (!is.na(val) && val == "2") {  # 2 = Complete in REDCap
                                    has_primary_review <- TRUE
                                    message("FOUND! coach_rev_complete = 2 in row ", row_idx)
                                    break
                                } else {
                                    message("coach_rev_complete value: ", ifelse(is.na(val), "NA", val))
                                }
                            }
                        } else {
                            # Regular period check for normal residents
                            # Check for coach_period field and match with instance
                            if ("coach_period" %in% names(resident_data) && 
                                !is.na(resident_data$coach_period[row_idx]) && 
                                resident_data$coach_period[row_idx] == as.character(coach_period)) {
                                
                                message("Found matching coach_period: ", resident_data$coach_period[row_idx])
                                
                                # Check if any key coach_rev fields have content
                                for (field in c("coach_pre_rev", "coach_summary", "coach_ilp_final")) {
                                    if (field %in% names(resident_data)) {
                                        field_val <- resident_data[[field]][row_idx]
                                        if (!is.na(field_val) && nchar(field_val) > 0) {
                                            message("Found content in field: ", field)
                                            has_primary_review <- TRUE
                                            break
                                        }
                                    }
                                }
                                
                                # Check completion status field
                                if ("coach_rev_complete" %in% names(resident_data)) {
                                    val <- resident_data$coach_rev_complete[row_idx]
                                    if (!is.na(val) && val == "2") {  # 2 = Complete in REDCap
                                        message("Found coach_rev_complete = 2")
                                        has_primary_review <- TRUE
                                        break
                                    }
                                }
                            }
                        }
                        
                        if (has_primary_review) break
                    }
                }
                
                # Set primary review status for all residents
                primary_review_statuses[i] <- if(has_primary_review) "✅" else "❌"
                message("Primary review status for ", res_name, ": ", primary_review_statuses[i])
                
                # Check secondary review completion status for ALL residents
                has_secondary_review <- FALSE
                
                # Use the same resident rows
                if (length(resident_rows) > 0) {
                    message("Checking secondary review for ", res_name, " with ", length(resident_rows), " rows")
                    
                    for (row_idx in resident_rows) {
                        # For alpha test, check any secondary review content regardless of period
                        if (res_name == "alpha test") {
                            # Special debugging for alpha test
                            message("Checking alpha test row ", row_idx, " for any secondary review content")
                            
                            # Check key fields directly without period filter for alpha test
                            for (field in c("second_comments", "second_approve")) {
                                if (field %in% names(resident_data)) {
                                    field_val <- resident_data[[field]][row_idx]
                                    if (!is.na(field_val) && nchar(field_val) > 0) {
                                        has_secondary_review <- TRUE
                                        message("FOUND! ", field, " has content in row ", row_idx, ": ", 
                                                substr(field_val, 1, 30), "...")
                                        break
                                    } else {
                                        message("No content in ", field, " in row ", row_idx)
                                    }
                                }
                            }
                            
                            # Check completion status field directly
                            if ("second_rev_complete" %in% names(resident_data)) {
                                val <- resident_data$second_rev_complete[row_idx]
                                if (!is.na(val) && val == "2") {  # 2 = Complete in REDCap
                                    has_secondary_review <- TRUE
                                    message("FOUND! second_rev_complete = 2 in row ", row_idx)
                                    break
                                } else {
                                    message("second_rev_complete value: ", ifelse(is.na(val), "NA", val))
                                }
                            }
                        } else {
                            # Regular period check for other residents
                            # Check for second_period field and match with instance
                            if ("second_period" %in% names(resident_data) && 
                                !is.na(resident_data$second_period[row_idx]) && 
                                resident_data$second_period[row_idx] == as.character(coach_period)) {
                                
                                message("Found matching second_period: ", resident_data$second_period[row_idx])
                                
                                # Check if any key second_review fields have content
                                for (field in c("second_comments", "second_approve")) {
                                    if (field %in% names(resident_data)) {
                                        field_val <- resident_data[[field]][row_idx]
                                        if (!is.na(field_val) && nchar(field_val) > 0) {
                                            message("Found content in field: ", field)
                                            has_secondary_review <- TRUE
                                            break
                                        }
                                    }
                                }
                                
                                # Check completion status field
                                if ("second_rev_complete" %in% names(resident_data)) {
                                    val <- resident_data$second_rev_complete[row_idx]
                                    if (!is.na(val) && val == "2") {  # 2 = Complete in REDCap
                                        message("Found second_rev_complete = 2")
                                        has_secondary_review <- TRUE
                                        break
                                    }
                                }
                            }
                        }
                        
                        if (has_secondary_review) break
                    }
                }
                
                # Set secondary review status for all residents
                secondary_review_statuses[i] <- if(has_secondary_review) "✅" else "❌"
                message("Secondary review status for ", res_name, ": ", secondary_review_statuses[i])
            }
            
            # Add all status columns to the data frame
            coach_residents$self_eval_status <- self_eval_statuses
            coach_residents$primary_review_status <- primary_review_statuses
            coach_residents$secondary_review_status <- secondary_review_statuses
            coach_residents$redcap_period <- redcap_periods  # Add the REDCap period
            
            # Select columns for display
            coach_residents <- coach_residents %>%
                select(name, Level, access_code, review_role, redcap_period, 
                       self_eval_status, primary_review_status, secondary_review_status) %>%
                arrange(review_role, Level, name) %>%
                setNames(c("Resident Name", "Level", "Access Code", "Review Role", "Review Period", 
                           "Self-Eval", "Primary Review", "Secondary Review"))
            
            if (nrow(coach_residents) > 0) {
                # Create table with click handling and formatted columns
                return(datatable_with_click(
                    coach_residents, 
                    caption = paste0("Residents Assigned to ", input$coach_name)
                ) %>%
                    # Apply styling to status columns
                    DT::formatStyle(
                        'Self-Eval',
                        color = styleEqual(c("✅", "❌"), c('#155724', '#721c24')),
                        backgroundColor = styleEqual(c("✅", "❌"), c('#d4edda', '#f8d7da')),
                        fontWeight = 'bold',
                        textAlign = 'center'
                    ) %>%
                    DT::formatStyle(
                        'Primary Review',
                        color = styleEqual(c("✅", "❌"), c('#155724', '#721c24')),
                        backgroundColor = styleEqual(c("✅", "❌"), c('#d4edda', '#f8d7da')),
                        fontWeight = 'bold',
                        textAlign = 'center'
                    ) %>%
                    DT::formatStyle(
                        'Secondary Review',
                        color = styleEqual(c("✅", "❌"), c('#155724', '#721c24')),
                        backgroundColor = styleEqual(c("✅", "❌"), c('#d4edda', '#f8d7da')),
                        fontWeight = 'bold',
                        textAlign = 'center'
                    )
                )
            }
        } else {
            # Error messages for debugging
            if (is.null(resident_data)) {
                message("resident_data is NULL")
            } else if (!is.data.frame(resident_data)) {
                message("resident_data is not a dataframe")
            } else {
                message("Missing required columns in resident_data: ", 
                        paste(setdiff(c("name", "access_code", "coach", "second_rev", "Level"), 
                                      names(resident_data)), collapse=", "))
            }
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
    })
    
    # Updated datatable_with_click helper function to include the status columns
    datatable_with_click <- function(data, caption = NULL) {
        # Base DT with click handling
        dt <- DT::datatable(
            data,
            options = list(
                pageLength = 10,
                dom = 'ftp',
                scrollX = TRUE,
                columnDefs = list(
                    # Make status columns narrower
                    list(width = "80px", targets = c(5, 6, 7)),  # Targets the status columns
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
          // Clear previously selected rows
          table.$('tr.selected').removeClass('selected');
          
          // Select this row
          $(this).addClass('selected');
          
          // Get the row data
          var rowData = table.row(this).data();
          var residentName = rowData[0];
          var residentLevel = rowData[1];
          var accessCode = rowData[2];
          var reviewRole = rowData[3];
          var reviewPeriod = rowData[4];
          
          console.log('Selected resident: ' + residentName + ', Level: ' + residentLevel + 
                      ', Access code: ' + accessCode + ', Review role: ' + reviewRole + 
                      ', Period: ' + reviewPeriod);
          
          // Set input value for Shiny with Level, review role and period
          Shiny.setInputValue('selected_resident_in_table', 
              {
                  name: residentName, 
                  level: residentLevel,
                  access_code: accessCode, 
                  review_role: reviewRole, 
                  review_period: reviewPeriod
              }, 
              {priority: 'event'});
      });
    ")
        ) %>%
            DT::formatStyle(
                columns = names(data)[1:5],  # Base formatting for non-status columns
                backgroundColor = '#f8f9fa',
                borderColor = '#dfe2e5'
            )
        
        return(dt)
    }
    
    # Handle resident selection in the coach residents table
    observeEvent(input$selected_resident_in_table, {
        req(input$selected_resident_in_table)
        
        # Get selected resident info
        resident_info <- input$selected_resident_in_table
        
        # Find the full resident data
        resident_data <- processed_resident_data()  # Use processed data with Level
        selected_resident <- NULL
        
        if (!is.null(resident_data)) {
            selected_resident <- resident_data %>%
                filter(name == resident_info$name, access_code == resident_info$access_code) %>%
                select(name, access_code, year, coach, Level) %>%  # Include Level in selection
                distinct()
            
            if (nrow(selected_resident) > 0) {
                # Store selected resident in reactiveValues
                values$selected_resident <- selected_resident[1, ]
                values$selected_coach <- input$coach_name
                values$current_period <- resident_info$review_period
                
                # Explicitly check the review_role from the table
                review_role <- as.character(resident_info$review_role)
                
                # Set review type based on the role
                if (!is.null(review_role) && review_role == "Primary") {
                    values$review_type <- "primary"
                } else {
                    values$review_type <- "second"
                }
                
                # Force UI update with invalidateLater
                invalidateLater(100)
                
                # Navigate directly to review pages
                shinyjs::hide("coach-selection-page")
                shinyjs::show("review-pages")
                
                # Force another UI update after showing review pages
                invalidateLater(200)
                
                if (values$review_type == "primary") {
                    shinyjs::show("primary-review-content")
                    shinyjs::hide("second-review-content")
                } else {
                    shinyjs::hide("primary-review-content")
                    shinyjs::show("second-review-content")
                }
                
                # Show a notification with level information
                showNotification(paste0("Starting ", values$review_type, " review for ", 
                                        values$selected_resident$name, " (", 
                                        values$selected_resident$Level, " - ", 
                                        values$current_period, ")"), 
                                 type = "message")
                
            } else {
                # Show error message if resident not found
                showNotification("Resident data not found. Please try again.", type = "error")
            }
        } else {
            showNotification("Resident data not available. Please try again.", type = "error")
        }
    })
    
    # Display resident info in header
    #--------------------------------------------------
    output$display_resident_name <- renderText({
        req(values$selected_resident)
        values$selected_resident$name
    })
    
    output$display_coach_name <- renderText({
        req(values$selected_coach)
        values$selected_coach
    })
    
    output$display_access_code <- renderText({
        req(values$selected_resident)
        values$selected_resident$access_code
    })
    
    # Output for displaying resident level
    output$display_resident_level <- renderText({
        req(values$selected_resident$Level)  # Note the capital L in Level
        values$selected_resident$Level
    })
    
    # Output for displaying current period
    output$display_current_period <- renderText({
        req(values$current_period)
        values$current_period
    })
    
    # Check if resident is an intern
    output$is_intern <- reactive({
        req(values$selected_resident)
        return(values$selected_resident$Level == "Intern")
    })
    outputOptions(output, "is_intern", suspendWhenHidden = FALSE)
    
    # Primary Review - Meeting Pre-review
    #--------------------------------------------------
    # Reactive value to check if self-evaluation is complete
    self_eval_completed <- reactive({
        req(values$selected_resident)
        req(values$current_period)
        
        data <- app_data()
        
        # Check using both s_miles and s_eval data
        is_complete <- check_self_eval_complete(
            s_miles = data$s_miles,
            s_eval = data$s_eval,
            resident_name = values$selected_resident$name,
            period = values$current_period,
            level = values$selected_resident$Level
        )
        
        # For debugging
        if (dev_mode) {
            message(paste("Self-evaluation complete for", values$selected_resident$name, 
                          "in period", values$current_period, ":", is_complete))
        }
        
        return(is_complete)
    })
    
    # Update the checkbox when the resident is selected
    observe({
        if (!is.null(self_eval_completed())) {
            updateCheckboxInput(session, "self_eval_completed", value = self_eval_completed())
        }
    })
    
    # Self-evaluation status UI
    output$self_eval_status_ui <- renderUI({
        req(values$selected_resident)
        req(values$current_period)
        
        is_complete <- self_eval_completed()
        
        # Get completion date if available
        completion_date <- NULL
        data <- app_data()
        if (!is.null(data$s_eval)) {
            resident_eval <- data$s_eval %>% 
                filter(name == values$selected_resident$name)
            
            if (nrow(resident_eval) > 0 && "date" %in% names(resident_eval)) {
                completion_date <- resident_eval$date[1]
            }
        }
        
        if (is_complete) {
            div(
                class = "alert alert-success",
                icon("check-circle"), 
                "Self-evaluation is complete",
                if (!is.null(completion_date)) {
                    span(" (completed on ", format(as.Date(completion_date), "%b %d, %Y"), ")")
                }
            )
        } else {
            div(
                class = "alert alert-warning",
                icon("exclamation-triangle"),
                "Self-evaluation is not complete. Please have the resident complete it before proceeding with the review."
            )
        }
    })
    
    # Render the current self-assessment plot
    output$self_milestones_plot <- renderPlot({
        req(values$selected_resident)
        req(values$redcap_period)  # <-- CHANGE: Use redcap_period instead
        
        data <- app_data()
        
        # REMOVE: No longer need to map the period here
        # mile_period <- map_to_milestone_period(values$selected_resident$Level, values$current_period)
        
        # Use the centrally mapped period directly
        mile_period <- values$redcap_period  # <-- CHANGE: Use redcap_period
        
        if (!is.null(data$s_miles) && !is.na(mile_period)) {
            tryCatch({
                message(paste("Attempting to render self milestone plot for period:", mile_period))
                
                # Debug: check if there's any data for this resident and period
                has_data <- any(data$s_miles$name == values$selected_resident$name & 
                                    data$s_miles$period == mile_period, na.rm = TRUE)
                message("Data exists for this resident and period: ", has_data)
                
                # Call miles_plot with the correct parameters
                miles_plot(data$s_miles, values$selected_resident$name, mile_period)
            }, error = function(e) {
                message(paste("Error rendering self milestone plot:", e$message))
                ggplot() +
                    annotate("text", x = 0.5, y = 0.5,
                             label = paste("Error rendering milestone plot:", e$message),
                             color = "red", size = 4) +
                    theme_void()
            })
        } else {
            ggplot() +
                annotate("text", x = 0.5, y = 0.5,
                         label = paste("No self-assessment data available for period:", 
                                       ifelse(is.na(mile_period), "Unknown", mile_period)),
                         color = "darkgray", size = 5) +
                theme_void()
        }
    })
    
    # Render the previous program assessment plot
    output$program_milestones_plot <- renderPlot({
        req(values$selected_resident)
        # Require the previously mapped period instead of calculating it again
        req(values$redcap_prev_period)  
        
        data <- app_data()
        
        # Debugging
        message("In program_milestones_plot - Using mapped previous period")
        message("Current period: ", values$current_period)
        message("Current mapped period: ", values$redcap_period)
        message("Previous mapped period: ", values$redcap_prev_period)
        
        # Use the centrally calculated previous period directly
        prev_mile_period <- values$redcap_prev_period
        
        # Only attempt to render if previous period exists and should have program data
        if (!is.na(prev_mile_period) && !is.null(data$p_miles)) {
            tryCatch({
                message(paste("Attempting to render program milestone plot for period:", prev_mile_period))
                
                # Debug: check if there's any data for this resident and period
                if (!is.null(data$p_miles)) {
                    has_data <- any(data$p_miles$name == values$selected_resident$name & 
                                        data$p_miles$period == prev_mile_period, na.rm = TRUE)
                    message("Data exists for this resident and previous period: ", has_data)
                    
                    if (has_data) {
                        # Call miles_plot with the correct parameters
                        p <- miles_plot(data$p_miles, values$selected_resident$name, prev_mile_period)
                        return(p)
                    }
                }
                
                # If we get here, no data was found
                ggplot() +
                    annotate("text", x = 0.5, y = 0.5,
                             label = paste("No milestone data found for", values$selected_resident$name, 
                                           "in period", prev_mile_period),
                             color = "darkgray", size = 5) +
                    theme_void()
                
            }, error = function(e) {
                message(paste("Error rendering previous program plot:", e$message))
                ggplot() +
                    annotate("text", x = 0.5, y = 0.5,
                             label = paste("Error rendering milestone plot:", e$message),
                             color = "red", size = 4) +
                    theme_void()
            })
        } else {
            # Create an empty plot when no previous program data exists or is expected
            reason <- if(is.na(prev_mile_period)) {
                "No previous assessment period"
            } else if (is.null(data$p_miles)) {
                "No milestone data available"
            } else {
                "No program assessment for the previous period"
            }
            
            ggplot() +
                annotate("text", x = 0.5, y = 0.5,
                         label = reason,
                         color = "darkgray", size = 5) +
                theme_void()
        }
    })
    
    output$prior_ccc_notes <- renderUI({
        req(values$selected_resident)
        req(values$redcap_period)  # We need the current period in REDCap format
        
        # Get resident level
        resident_level <- values$selected_resident$Level
        
        # Get app data
        data <- app_data()
        
        # Create CCC notes table - pass the mapping directly
        ccc_table <- tryCatch({
            create_ccc_notes_table(
                resident_name = values$selected_resident$name,
                current_period = values$redcap_period,  # Use mapped period here
                resident_level = resident_level,
                resident_data = processed_resident_data(),
                rdm_dict = data$rdm_dict
            )
        }, error = function(e) {
            message("Error creating CCC notes table: ", e$message)
            return(NULL)
        })
        
        if (is.null(ccc_table) || nrow(ccc_table) == 0) {
            return(div(class = "alert alert-info", 
                       paste("No prior CCC review data available for", values$selected_resident$name)))
        }
        
        # Generate a unique ID for this instance
        toggle_id <- paste0("ccc_toggle_", sample.int(1000, 1))
        collapse_id <- paste0("ccc_collapse_", sample.int(1000, 1))
        
        # Return the UI
        div(
            h4("Prior CCC Reviews"),
            p("Click to view previous CCC review notes for this resident."),
            
            # Toggle button
            actionButton(
                inputId = toggle_id,
                label = paste("Show CCC Reviews (", nrow(ccc_table), " records)"),
                icon = icon("table"),
                class = "btn-primary mb-3"
            ),
            
            # Collapsible table
            div(
                id = collapse_id,
                class = "collapse",
                div(
                    style = "overflow-x: auto;",
                    DT::renderDataTable({
                        DT::datatable(
                            ccc_table,
                            options = list(
                                pageLength = 5,
                                scrollX = TRUE,
                                dom = 'ftp',
                                ordering = TRUE,
                                order = list(list(0, 'desc')),  # Sort by first column (Date) descending
                                columnDefs = list(
                                    list(className = 'dt-left', targets = "_all")
                                )
                            ),
                            rownames = FALSE,
                            filter = "top",
                            class = "display compact stripe hover"
                        ) %>%
                            DT::formatStyle(
                                'Date',
                                fontWeight = 'bold'
                            ) %>%
                            DT::formatStyle(
                                'Milestones Complete',
                                backgroundColor = styleEqual(
                                    c("Yes", "No"), 
                                    c('#d4edda', '#f8d7da')
                                ),
                                color = styleEqual(
                                    c("Yes", "No"), 
                                    c('#155724', '#721c24')
                                ),
                                fontWeight = 'bold',
                                textAlign = 'center'
                            ) %>%
                            DT::formatStyle(
                                'Concerns',
                                backgroundColor = styleEqual(
                                    c("Yes"), 
                                    c('#f8d7da')
                                ),
                                color = styleEqual(
                                    c("Yes"), 
                                    c('#721c24')
                                ),
                                fontWeight = 'bold'
                            )
                    })
                )
            ),
            
            # JavaScript to toggle the collapse
            tags$script(HTML(paste0("
      $(document).ready(function() {
        $('#", toggle_id, "').on('click', function() {
          $('#", collapse_id, "').collapse('toggle');
          
          // Change button text and icon
          var button = $(this);
          if ($('#", collapse_id, "').is(':visible')) {
            button.html('<i class=\"fa fa-table\"></i> Hide CCC Reviews');
          } else {
            button.html('<i class=\"fa fa-table\"></i> Show CCC Reviews (", nrow(ccc_table), " records)');
          }
        });
      });
    ")))
        )
    })
    
    output$prior_ilp <- renderUI({
        req(values$selected_resident)
        req(values$redcap_prev_period)  # Use the centrally calculated previous period
        
        # Get resident level
        resident_level <- values$selected_resident$Level
        
        # Debug
        message("Looking for previous ILP data for resident: ", values$selected_resident$name, 
                ", previous period: ", values$redcap_prev_period)
        
        # Skip the period calculation since we already have it in values$redcap_prev_period
        
        if (is.na(values$redcap_prev_period)) {
            return(div(class = "alert alert-info", 
                       "No previous ILP data available for this resident's current level and period."))
        }
        
        # Get app data
        data <- app_data()
        
        # Create ILP table - pass the centrally mapped previous period
        ilp_table <- tryCatch({
            create_ilp_data_table(
                resident_name = values$selected_resident$name,
                period = values$redcap_prev_period,  # Use centrally mapped previous period
                resident_data = processed_resident_data(),
                rdm_dict = data$rdm_dict
            )
        }, error = function(e) {
            message("Error creating ILP table: ", e$message)
            return(NULL)
        })
        
        if (is.null(ilp_table) || nrow(ilp_table) == 0) {
            return(div(class = "alert alert-info", 
                       paste("No prior ILP data available for", values$selected_resident$name, 
                             "in the", values$redcap_prev_period, "period.")))
        }
        
        # Clean up column names
        colnames(ilp_table) <- c(
            "Competency", 
            "Goal Met", 
            "Comments", 
            "Goal Description", 
            "Action Plan", 
            "Milestone Descriptions"
        )
        
        # Clean up milestone descriptions
        ilp_table$`Milestone Descriptions` <- gsub("\\n\\nNA", "", ilp_table$`Milestone Descriptions`)
        
        # Return the formatted table
        div(
            h4(paste("Previous ILP from", values$redcap_prev_period, "Period")), # FIXED: Using values$redcap_prev_period instead of prev_period
            div(
                style = "overflow-x: auto;",
                DT::renderDataTable({
                    DT::datatable(
                        ilp_table,
                        options = list(
                            pageLength = 3,
                            scrollX = TRUE,
                            dom = 't',
                            ordering = FALSE,
                            columnDefs = list(
                                list(className = 'dt-left', targets = "_all"),
                                list(width = '18%', targets = 0),
                                list(width = '8%', targets = 1),
                                list(width = '15%', targets = 2),
                                list(width = '20%', targets = 3),
                                list(width = '15%', targets = 4),
                                list(width = '24%', targets = 5)
                            )
                        ),
                        rownames = FALSE,
                        escape = FALSE,
                        class = "display compact stripe"
                    ) %>%
                        DT::formatStyle(
                            'Competency',
                            fontWeight = 'bold',
                            backgroundColor = '#f0f7fa'
                        ) %>%
                        DT::formatStyle(
                            'Goal Met',
                            backgroundColor = styleEqual(
                                c("Yes", "No"), 
                                c('#d4edda', '#f8d7da')
                            ),
                            color = styleEqual(
                                c("Yes", "No"), 
                                c('#155724', '#721c24')
                            ),
                            fontWeight = 'bold',
                            textAlign = 'center'
                        ) %>%
                        DT::formatStyle(
                            'Goal Description',
                            fontWeight = 'bold',
                            backgroundColor = '#f8f9fa'
                        ) %>%
                        DT::formatStyle(
                            'Milestone Descriptions',
                            whiteSpace = 'pre-line',
                            fontSize = '90%',
                            backgroundColor = '#fafafa'
                        )
                })
            )
        )
    })
    
    # Wellness Card
    # ----------------------------------------------
    # Check if resident is an intern in intro period
    output$is_intern_intro <- reactive({
        req(values$selected_resident)
        req(values$current_period)
        
        # Check if resident is an intern and in intro period
        is_intro <- values$selected_resident$Level == "Intern" && 
            values$current_period == "Intern Intro"
        
        if(dev_mode) {
            message("Is intern intro period: ", is_intro)
        }
        
        return(is_intro)
    })
    outputOptions(output, "is_intern_intro", suspendWhenHidden = FALSE)
    
    # Evaluation Card
    # ---------------------------------
    # 
    get_most_recent_self_eval <- function(resident_name, period, column_name) {
        # Get app data directly
        data <- app_data()
        resident_data <- data$resident_data
        
        # Filter for this resident
        resident_rows <- resident_data %>% 
            filter(name == resident_name)
        
        # If no rows found
        if(nrow(resident_rows) == 0) {
            return(NULL)
        }
        
        # If we have a period column, try to filter by period
        if("s_e_period" %in% names(resident_rows) && period %in% resident_rows$s_e_period) {
            # Get the rows for this period
            period_rows <- resident_rows %>% filter(s_e_period == period)
            
            if(nrow(period_rows) > 0) {
                # Check for date column to sort by (try common date column names)
                date_cols <- intersect(c("date", "s_e_date", "entry_date", "created_date", "timestamp"), 
                                       names(period_rows))
                
                if(length(date_cols) > 0) {
                    # Use the first date column found
                    date_col <- date_cols[1]
                    
                    # Convert to Date object if needed
                    if(!inherits(period_rows[[date_col]], "Date")) {
                        period_rows[[date_col]] <- as.Date(period_rows[[date_col]])
                    }
                    
                    # Sort by date descending (most recent first)
                    period_rows <- period_rows[order(period_rows[[date_col]], decreasing = TRUE), ]
                }
                
                # Check if the requested column exists and has a non-NA value
                if(column_name %in% names(period_rows) && !is.na(period_rows[[column_name]][1])) {
                    return(period_rows[[column_name]][1])
                }
            }
        }
        
        # If no period match, check for any rows with the column
        if(column_name %in% names(resident_rows)) {
            # Check for date column to sort by
            date_cols <- intersect(c("date", "s_e_date", "entry_date", "created_date", "timestamp"), 
                                   names(resident_rows))
            
            if(length(date_cols) > 0) {
                # Use the first date column found
                date_col <- date_cols[1]
                
                # Convert to Date object if needed
                if(!inherits(resident_rows[[date_col]], "Date")) {
                    resident_rows[[date_col]] <- as.Date(resident_rows[[date_col]])
                }
                
                # Sort by date descending (most recent first)
                resident_rows <- resident_rows[order(resident_rows[[date_col]], decreasing = TRUE), ]
            }
            
            # Get the first non-NA value
            non_na_rows <- resident_rows[!is.na(resident_rows[[column_name]]), ]
            
            if(nrow(non_na_rows) > 0) {
                return(non_na_rows[[column_name]][1])
            }
        }
        
        # No data found
        return(NULL)
    }
    output$resident_plus_header <- renderText({
        req(values$selected_resident)
        paste0("What ", values$selected_resident$name, " feels is done well:")
    })
    
    output$resident_plus_assessment <- renderText({
        req(values$selected_resident)
        req(values$current_period)
        
        # Use helper function to get the most recent value
        plus_value <- get_most_recent_self_eval(
            resident_name = values$selected_resident$name,
            period = values$current_period,
            column_name = "s_e_plus"
        )
        
        # Return the value or a default message
        if(!is.null(plus_value)) {
            return(plus_value)
        } else {
            return("No self-assessment of strengths provided for this period.")
        }
    })
    
    # 3. Use the helper function for delta assessment
    output$resident_delta_header <- renderText({
        req(values$selected_resident)
        paste0("What ", values$selected_resident$name, " can improve upon:")
    })
    
    output$resident_delta_assessment <- renderText({
        req(values$selected_resident)
        req(values$current_period)
        
        # Use helper function to get the most recent value
        delta_value <- get_most_recent_self_eval(
            resident_name = values$selected_resident$name,
            period = values$current_period,
            column_name = "s_e_delta"
        )
        
        # Return the value or a default message
        if(!is.null(delta_value)) {
            return(delta_value)
        } else {
            return("No self-assessment of areas for improvement provided for this period.")
        }
    })
    
    # 4. Simplified plus/delta table implementation
    output$plus_delta_table <- DT::renderDT({
        req(values$selected_resident)
        
        # Get raw resident data directly
        data <- app_data()
        
        # Call the generate_p_d function directly
        plus_delta_data <- generate_p_d(data$resident_data, values$selected_resident$name)
        
        # Use create_styled_dt to format the table
        create_styled_dt(plus_delta_data, caption = paste0("Plus/Delta Feedback for ", values$selected_resident$name))
    })
    
    
    #Modal handlers to open p_d table  
    # Function to show the evaluation modal with plus/delta table
    show_evaluation_modal <- function() {
        # Create modal with large size
        showModal(modalDialog(
            title = "Plus/Delta Feedback",
            size = "l", # Large modal
            easyClose = TRUE,
            
            # Add class for styling
            class = "big-modal",
            
            # The plus/delta table
            DT::DTOutput("plus_delta_table"),
            
            # Footer buttons
            footer = tagList(
                modalButton("Close")
            )
        ))
    } 
    observeEvent(input$open_eval_modal, {
        show_evaluation_modal()
    })
    
    observeEvent(input$open_eval_modal_top, {
        show_evaluation_modal()
    })
    
    observeEvent(input$reopen_eval_modal, {
        show_evaluation_modal()
    })
    
    # Card 4: Knowledge adn Board Prep

    output$knowledge_topics_ui <- renderUI({
        req(values$selected_resident)
        req(values$current_period)
        
        # Get app data
        data <- app_data()
        resident_data <- data$resident_data
        
        # Debug
        message("Looking for knowledge data for: ", values$selected_resident$name, ", period: ", values$current_period)
        
        # Use direct query based on your example
        s_e_rows <- resident_data %>%
            filter(name == values$selected_resident$name & s_e_period == "End Intern") %>%
            select(starts_with("s_e_")) %>%
            select(where(~ !all(is.na(.))))
        
        message("Direct query found ", nrow(s_e_rows), " rows")
        
        if(nrow(s_e_rows) == 0) {
            # If direct query failed, try mapped period
            mapped_period <- map_to_milestone_period(values$selected_resident$Level, values$current_period)
            message("Trying mapped period: ", mapped_period)
            
            s_e_rows <- resident_data %>%
                filter(name == values$selected_resident$name & s_e_period == mapped_period) %>%
                select(starts_with("s_e_")) %>%
                select(where(~ !all(is.na(.))))
            
            message("Mapped period query found ", nrow(s_e_rows), " rows")
        }
        
        if(nrow(s_e_rows) == 0) {
            message("No rows found with period filtering")
            return(div(class = "alert alert-info", "No self-assessment data available for this period."))
        }
        
        # If multiple rows, use the most recent by date
        if(nrow(s_e_rows) > 1 && "s_e_date" %in% names(s_e_rows)) {
            s_e_rows <- s_e_rows %>% 
                arrange(desc(s_e_date)) %>%
                slice(1)
            message("Using most recent row from date: ", s_e_rows$s_e_date[1])
        }
        
        # Get topic columns - these contain actual text values
        topic_cols <- grep("^s_e_topic_sel___", names(s_e_rows), value=TRUE)
        message("Topic columns found: ", paste(topic_cols, collapse=", "))
        
        # Extract non-NA values directly
        selected_topics <- c()
        for(col in topic_cols) {
            if(!is.na(s_e_rows[[col]][1]) && s_e_rows[[col]][1] != "") {
                selected_topics <- c(selected_topics, s_e_rows[[col]][1])
                message("Found topic: ", s_e_rows[[col]][1])
            }
        }
        
        # Get learning style columns
        style_cols <- grep("^s_e_learn_style___", names(s_e_rows), value=TRUE)
        message("Style columns found: ", paste(style_cols, collapse=", "))
        
        # Extract non-NA values directly
        selected_styles <- c()
        for(col in style_cols) {
            if(!is.na(s_e_rows[[col]][1]) && s_e_rows[[col]][1] != "") {
                selected_styles <- c(selected_styles, s_e_rows[[col]][1])
                message("Found style: ", s_e_rows[[col]][1])
            }
        }
        
        # Create a visually appealing UI
        fluidRow(
            column(
                width = 6,
                div(
                    class = "card mb-4",
                    div(
                        class = "card-header bg-primary text-white",
                        h4("Topics Least Comfortable With", class = "m-0")
                    ),
                    div(
                        class = "card-body p-0",
                        if(length(selected_topics) > 0) {
                            tags$ul(
                                class = "list-group list-group-flush",
                                lapply(selected_topics, function(topic) {
                                    tags$li(
                                        class = "list-group-item d-flex align-items-center",
                                        tags$i(class = "fas fa-exclamation-circle text-warning me-2"),
                                        topic  # Use the value directly
                                    )
                                })
                            )
                        } else {
                            div(
                                class = "p-3 text-center text-muted",
                                tags$i(class = "fas fa-info-circle me-2"),
                                "No topics identified"
                            )
                        }
                    )
                )
            ),
            
            column(
                width = 6,
                div(
                    class = "card mb-4",
                    div(
                        class = "card-header bg-success text-white",
                        h4("Preferred Learning Styles", class = "m-0")
                    ),
                    div(
                        class = "card-body p-0",
                        if(length(selected_styles) > 0) {
                            tags$ul(
                                class = "list-group list-group-flush",
                                lapply(selected_styles, function(style) {
                                    tags$li(
                                        class = "list-group-item d-flex align-items-center",
                                        tags$i(class = "fas fa-check-circle text-success me-2"),
                                        style  # Use the value directly
                                    )
                                })
                            )
                        } else {
                            div(
                                class = "p-3 text-center text-muted",
                                tags$i(class = "fas fa-info-circle me-2"),
                                "No learning styles identified"
                            )
                        }
                    )
                )
            )
        )
    })
    
    # Modified board prep data table to remove "Not Available" rows
    output$board_prep_data <- renderTable({
        req(values$selected_resident)
        
        # Get the resident data
        data <- app_data()
        resident_data <- data$resident_data
        
        # Filter for this resident
        resident_rows <- resident_data %>% 
            filter(name == values$selected_resident$name)
        
        # Try to filter by period if available
        if("s_e_period" %in% names(resident_rows) && !is.null(values$current_period)) {
            period_rows <- resident_rows %>% filter(s_e_period == values$current_period)
            
            if(nrow(period_rows) > 0) {
                resident_rows <- period_rows
            }
        }
        
        # If no rows found
        if(nrow(resident_rows) == 0) {
            message("No rows found for this resident, returning default table")
            return(data.frame(
                "Category" = c("Step 3 Status"),
                "Value" = c("Not Available")
            ))
        }
        
        # Sort by date if available, to get most recent entry
        if("s_e_date" %in% names(resident_rows) && 
           any(!is.na(resident_rows$s_e_date))) {
            resident_rows <- resident_rows %>%
                arrange(desc(s_e_date))
        }
        
        # Create a function to safely extract a value from the first non-NA row
        get_first_non_na <- function(df, col_name) {
            if(!(col_name %in% names(df))) return("Not Available")
            
            for(i in 1:nrow(df)) {
                if(!is.na(df[[col_name]][i])) {
                    return(df[[col_name]][i])
                }
            }
            return("Not Available")
        }
        
        # Create initial board prep data with all categories
        all_categories <- c("Step 3 Status", "Step 3 Contact", "Step 3 Date Set", "Step 3 Date", 
                            "Board Concerns", "Board Help Needed", "Board Discussed", "MKSAP Complete")
        
        all_values <- c(
            get_first_non_na(resident_rows, "s_e_step3"),
            get_first_non_na(resident_rows, "s_e_step3_contact"),
            get_first_non_na(resident_rows, "s_e_step3_date_set"),
            # Special handling for date
            ifelse(
                "s_e_step3_date" %in% names(resident_rows) && 
                    any(!is.na(resident_rows$s_e_step3_date)),
                format(as.Date(resident_rows$s_e_step3_date[min(which(!is.na(resident_rows$s_e_step3_date)))]), "%b %d, %Y"),
                "Not Available"
            ),
            get_first_non_na(resident_rows, "s_e_board_concern"),
            get_first_non_na(resident_rows, "s_e_board_help"),
            get_first_non_na(resident_rows, "s_e_board_discu"),
            get_first_non_na(resident_rows, "s_e_mksap_comp")
        )
        
        # Filter out "Not Available" rows
        available_indices <- which(all_values != "Not Available")
        
        # Only keep rows with actual values
        if(length(available_indices) > 0) {
            board_prep_data <- data.frame(
                "Category" = all_categories[available_indices],
                "Value" = all_values[available_indices]
            )
        } else {
            # If all rows would be removed, return a message
            board_prep_data <- data.frame(
                "Category" = "No Board Prep Data",
                "Value" = "No data available"
            )
        }
        
        message("Board prep table has ", nrow(board_prep_data), " rows after filtering out 'Not Available'")
        return(board_prep_data)
    }, striped = TRUE, bordered = TRUE, hover = TRUE, align = 'l', width = "100%")
    
    # Modify the exam scores data function to use the same knowledge data
    output$exam_scores_data <- renderTable({
        req(values$selected_resident)
        req(values$current_period)
        
        # Get app data
        data <- app_data()
        
        # Get knowledge data using our helper function
        knowledge_data <- get_knowledge_data(
            resident_name = values$selected_resident$name,
            current_period = values$current_period,
            resident_level = values$selected_resident$Level,
            resident_data = data$resident_data
        )
        
        # Extract exam scores data
        exam_scores <- knowledge_data$exam_scores
        
        # Create the display table
        exam_scores_data <- data.frame(
            "Exam" = c("USMLE Step 1", "USMLE Step 2", "COMLEX Level 1", "COMLEX Level 2", 
                       "USMLE Step 3", "ITE Intern Year", "ITE PGY-2", "ITE PGY-3"),
            "Score" = c(
                ifelse("usmle1" %in% names(exam_scores), exam_scores[["usmle1"]], "Not Available"),
                ifelse("usmle2" %in% names(exam_scores), exam_scores[["usmle2"]], "Not Available"),
                ifelse("comlex1" %in% names(exam_scores), exam_scores[["comlex1"]], "Not Available"),
                ifelse("comlex2" %in% names(exam_scores), exam_scores[["comlex2"]], "Not Available"),
                ifelse("usmle3" %in% names(exam_scores), exam_scores[["usmle3"]], "Not Available"),
                ifelse("ite_int" %in% names(exam_scores), exam_scores[["ite_int"]], "Not Available"),
                ifelse("ite2" %in% names(exam_scores), exam_scores[["ite2"]], "Not Available"),
                ifelse("ite3" %in% names(exam_scores), exam_scores[["ite3"]], "Not Available")
            )
        )
        
        return(exam_scores_data)
    }, striped = TRUE, bordered = TRUE, hover = TRUE, align = 'l', width = "100%", caption = "ITE scores are in percentiles for PGY")
    
    # Add this below your existing tables to display warnings
    output$board_prep_warnings <- renderUI({
        req(values$selected_resident)
        
        # Get the resident data
        data <- app_data()
        resident_data <- data$resident_data
        
        # Filter for this resident
        resident_rows <- resident_data %>% 
            filter(name == values$selected_resident$name)
        
        # Sort by date if available, to get most recent entry
        if("s_e_date" %in% names(resident_rows) && 
           any(!is.na(resident_rows$s_e_date))) {
            resident_rows <- resident_rows %>%
                arrange(desc(s_e_date))
        }
        
        # Create a function to safely extract a value from the first non-NA row
        get_first_non_na <- function(df, col_name) {
            if(!(col_name %in% names(df))) return(NA)
            
            for(i in 1:nrow(df)) {
                if(!is.na(df[[col_name]][i])) {
                    return(df[[col_name]][i])
                }
            }
            return(NA)
        }
        
        # Get Step 3 and board concerns status
        step3_status <- get_first_non_na(resident_rows, "s_e_step3")
        board_concerns <- get_first_non_na(resident_rows, "s_e_board_concern")
        
        # Create warnings if needed
        warnings_list <- tagList()
        
        if(!is.na(step3_status) && step3_status == "No") {
            warnings_list <- tagAppendChild(
                warnings_list,
                div(
                    class = "alert alert-danger mt-3",
                    tags$strong("Warning: "), 
                    paste(values$selected_resident$name, "has not completed Step 3.")
                )
            )
        }
        
        if(!is.na(board_concerns) && board_concerns == "Yes") {
            warnings_list <- tagAppendChild(
                warnings_list,
                div(
                    class = "alert alert-danger mt-3",
                    tags$strong("Warning: "), 
                    paste(values$selected_resident$name, "has concerns about passing boards.")
                )
            )
        }
        
        return(warnings_list)
    })

    
    #-----------------------------------
    # Scholarship card
    # ----------------------------------
    
    # Process scholarship data function
    process_scholarship_data <- function(data, resident_name, rdm_dict) {
        # Debug
        message(paste("Processing scholarship data for resident:", resident_name))
        
        # Skip processing if resident_name is missing or NA
        if (is.null(resident_name) || is.na(resident_name) || resident_name == "") {
            message("Resident name is empty or NA, returning empty data")
            return(list(
                table_data = data.frame(
                    Scholarship_Type = character(0),
                    Description = character(0),
                    stringsAsFactors = FALSE
                ),
                completed_ps = FALSE,
                completed_rca = FALSE
            ))
        }
        
        # Filter data for the specific resident
        filtered_data <- data[data$name == resident_name, ]
        
        # Filter to keep only rows with schol_type column filled
        if ("schol_type" %in% names(filtered_data)) {
            filtered_data <- filtered_data[!is.na(filtered_data$schol_type), ]
        }
        
        message(paste("After filtering, found", nrow(filtered_data), "scholarship rows"))
        
        # Skip processing if no data is found
        if (nrow(filtered_data) == 0) {
            return(list(
                table_data = data.frame(
                    Scholarship_Type = character(0),
                    Description = character(0),
                    stringsAsFactors = FALSE
                ),
                completed_ps = FALSE,
                completed_rca = FALSE
            ))
        }
        
        # Check for Patient Safety and RCA completion
        completed_ps <- FALSE
        completed_rca <- FALSE
        
        # Check if columns exist first
        if ("schol_ps" %in% names(filtered_data)) {
            completed_ps <- any(filtered_data$schol_ps == "1" |
                                    filtered_data$schol_ps == 1 |
                                    filtered_data$schol_ps == "Yes",
                                na.rm = TRUE)
        }
        
        if ("schol_rca" %in% names(filtered_data)) {
            completed_rca <- any(filtered_data$schol_rca == "1" |
                                     filtered_data$schol_rca == 1 |
                                     filtered_data$schol_rca == "Yes",
                                 na.rm = TRUE)
        }
        
        message(paste("PS flag =", completed_ps, ", RCA flag =", completed_rca))
        
        # Create a display table based on what's available
        if ("schol_type" %in% names(filtered_data)) {
            # Get type labels if available in the data dictionary
            type_labels <- NULL
            if (!is.null(rdm_dict) && "field_name" %in% names(rdm_dict)) {
                type_row <- rdm_dict[rdm_dict$field_name == "schol_type", ]
                if (nrow(type_row) > 0 && "select_choices_or_calculations" %in% names(type_row)) {
                    choices_str <- type_row$select_choices_or_calculations[1]
                    if (!is.na(choices_str) && choices_str != "") {
                        choices <- strsplit(choices_str, "\\|")[[1]]
                        type_labels <- list()
                        for (choice in choices) {
                            parts <- strsplit(trimws(choice), ",")[[1]]
                            if (length(parts) >= 2) {
                                key <- trimws(parts[1])
                                value <- trimws(paste(parts[-1], collapse = ", "))
                                type_labels[[key]] <- value
                            }
                        }
                    }
                }
            }
            
            # Create a safe version that avoids using field names that might not exist
            table_data <- data.frame(
                Scholarship_Type = as.character(filtered_data$schol_type),
                Description = "",
                stringsAsFactors = FALSE
            )
            
            # Replace type codes with labels if available
            if (!is.null(type_labels)) {
                for (i in 1:nrow(table_data)) {
                    type_code <- as.character(table_data$Scholarship_Type[i])
                    if (type_code %in% names(type_labels)) {
                        table_data$Scholarship_Type[i] <- type_labels[[type_code]]
                    }
                }
            }
            
            # Add description if available, checking each possible field
            for (i in 1:nrow(table_data)) {
                desc <- ""
                
                if ("schol_cit" %in% names(filtered_data) && i <= nrow(filtered_data) && 
                    !is.na(filtered_data$schol_cit[i]) && filtered_data$schol_cit[i] != "") {
                    desc <- filtered_data$schol_cit[i]
                } else if ("schol_res" %in% names(filtered_data) && i <= nrow(filtered_data) && 
                           !is.na(filtered_data$schol_res[i]) && filtered_data$schol_res[i] != "") {
                    desc <- filtered_data$schol_res[i]
                } else if ("schol_qi" %in% names(filtered_data) && i <= nrow(filtered_data) && 
                           !is.na(filtered_data$schol_qi[i]) && filtered_data$schol_qi[i] != "") {
                    desc <- filtered_data$schol_qi[i]
                } else if ("schol_pres_conf" %in% names(filtered_data) && i <= nrow(filtered_data) && 
                           !is.na(filtered_data$schol_pres_conf[i]) && filtered_data$schol_pres_conf[i] != "") {
                    desc <- filtered_data$schol_pres_conf[i]
                } else if ("schol_comm" %in% names(filtered_data) && i <= nrow(filtered_data) && 
                           !is.na(filtered_data$schol_comm[i]) && filtered_data$schol_comm[i] != "") {
                    desc <- filtered_data$schol_comm[i]
                }
                
                table_data$Description[i] <- desc
            }
        } else {
            # No schol_type column - create empty table
            table_data <- data.frame(
                Scholarship_Type = character(0),
                Description = character(0),
                stringsAsFactors = FALSE
            )
        }
        
        # Return the results
        list(
            table_data = table_data,
            completed_ps = completed_ps,
            completed_rca = completed_rca
        )
    }
       
    # Process scholarship data for the selected resident
    output$scholarship_data <- renderUI({
        req(values$selected_resident)
        req(values$current_period)
        
        # Get app data
        data <- app_data()
        resident_data <- data$resident_data
        
        # Debug
        message(paste("Processing scholarship data for:", values$selected_resident$name))
        
        # Process scholarship data
        scholarship_results <- process_scholarship_data(
            data = resident_data,
            resident_name = values$selected_resident$name,
            rdm_dict = data$rdm_dict
        )
        
        # Create UI elements to display
        tagList(
            # Achievement notifications (Patient Safety & RCA status)
            div(
                class = "mb-4",
                h4("Patient Safety Achievements", class = "text-primary mb-3"),
                
                # Build notifications for PS and RCA status
                if (isTRUE(scholarship_results$completed_ps)) {
                    div(
                        class = "alert alert-success",
                        tags$p(
                            tags$span(icon("check-circle"), class = "text-success me-2"),
                            tags$strong("Achievement: "),
                            paste(values$selected_resident$name, "has completed a Patient Safety Review")
                        )
                    )
                } else {
                    div(
                        class = "alert alert-warning",
                        tags$p(
                            tags$span(icon("exclamation-circle"), class = "text-warning me-2"),
                            tags$strong("Pending: "),
                            paste(values$selected_resident$name, "has not yet completed a Patient Safety Review")
                        )
                    )
                },
                
                # Root Cause Analysis status
                if (isTRUE(scholarship_results$completed_rca)) {
                    div(
                        class = "alert alert-success",
                        tags$p(
                            tags$span(icon("check-circle"), class = "text-success me-2"),
                            tags$strong("Achievement: "),
                            paste(values$selected_resident$name, "has completed a Root Cause Analysis")
                        )
                    )
                } else {
                    div(
                        class = "alert alert-warning",
                        tags$p(
                            tags$span(icon("exclamation-circle"), class = "text-warning me-2"),
                            tags$strong("Pending: "),
                            paste(values$selected_resident$name, "has not yet completed a Root Cause Analysis")
                        )
                    )
                }
            ),
            
            # Scholarship table
            div(
                class = "mt-4",
                h4("Scholarship Activities", class = "text-primary mb-3"),
                
                # Render the table using DTOutput
                if (nrow(scholarship_results$table_data) == 0) {
                    div(
                        class = "alert alert-info",
                        tags$p(
                            tags$span(icon("info-circle"), class = "me-2"),
                            paste(values$selected_resident$name, "has no scholarship activities recorded")
                        )
                    )
                } else {
                    # Using DT::renderDataTable directly within renderUI
                    tags$div(
                        style = "overflow-x: auto;",
                        DT::renderDataTable({
                            create_styled_dt(scholarship_results$table_data, 
                                             caption = paste("Scholarship Activities for", values$selected_resident$name))
                        })
                    )
                }
            )
        )
    })
    
    
    #--------------------------
    # Milestone Goals
    # --------------------
    # Render the previous milestone goals
    output$previous_milestone_goals <- renderUI({
        req(values$selected_resident)
        req(values$redcap_prev_period)  # Use the centrally mapped previous period
        
        # Get app data
        data <- app_data()
        
        # Check if we have a previous period
        if (is.na(values$redcap_prev_period)) {
            return(div(class = "alert alert-info", 
                       "No previous period data available for this resident's current level and period."))
        }
        
        # Get the ILP table for the previous period - reuse the existing function
        ilp_table <- tryCatch({
            create_ilp_data_table(
                resident_name = values$selected_resident$name,
                period = values$redcap_prev_period,
                resident_data = processed_resident_data(),
                rdm_dict = data$rdm_dict
            )
        }, error = function(e) {
            message("Error creating ILP table: ", e$message)
            return(NULL)
        })
        
        if (is.null(ilp_table) || nrow(ilp_table) == 0) {
            return(div(class = "alert alert-info", 
                       paste("No prior milestone goals available for", values$selected_resident$name, 
                             "in the", values$redcap_prev_period, "period.")))
        }
        
        # Clean up column names
        colnames(ilp_table) <- c(
            "Competency", 
            "Goal Met", 
            "Comments", 
            "Goal Description", 
            "Action Plan", 
            "Milestone Descriptions"
        )
        
        # Clean up milestone descriptions
        ilp_table$`Milestone Descriptions` <- gsub("\\n\\nNA", "", ilp_table$`Milestone Descriptions`)
        
        # Return the formatted table
        div(
            div(
                style = "overflow-x: auto;",
                DT::renderDataTable({
                    DT::datatable(
                        ilp_table,
                        options = list(
                            pageLength = 3,
                            scrollX = TRUE,
                            dom = 't',
                            ordering = FALSE,
                            columnDefs = list(
                                list(className = 'dt-left', targets = "_all"),
                                list(width = '18%', targets = 0),
                                list(width = '8%', targets = 1),
                                list(width = '15%', targets = 2),
                                list(width = '20%', targets = 3),
                                list(width = '15%', targets = 4),
                                list(width = '24%', targets = 5)
                            )
                        ),
                        rownames = FALSE,
                        escape = FALSE,
                        class = "display compact stripe"
                    ) %>%
                        DT::formatStyle(
                            'Competency',
                            fontWeight = 'bold',
                            backgroundColor = '#f0f7fa'
                        ) %>%
                        DT::formatStyle(
                            'Goal Met',
                            backgroundColor = styleEqual(
                                c("Yes", "No"), 
                                c('#d4edda', '#f8d7da')
                            ),
                            color = styleEqual(
                                c("Yes", "No"), 
                                c('#155724', '#721c24')
                            ),
                            fontWeight = 'bold',
                            textAlign = 'center'
                        ) %>%
                        DT::formatStyle(
                            'Goal Description',
                            fontWeight = 'bold',
                            backgroundColor = '#f8f9fa'
                        ) %>%
                        DT::formatStyle(
                            'Milestone Descriptions',
                            whiteSpace = 'pre-line',
                            fontSize = '90%',
                            backgroundColor = '#fafafa'
                        )
                })
            )
        )
    })
    
    # Current milestone goals UI outputs
    output$pc_mk_goal_ui <- renderUI({
        req(values$selected_resident)
        req(values$redcap_period)
        
        # Get app data
        data <- app_data()
        
        # Get milestone goals
        milestone_goals <- get_milestone_goals(
            resident_name = values$selected_resident$name,
            current_period = values$redcap_period,
            resident_data = data$resident_data,
            rdm_dict = data$rdm_dict
        )
        
        # Format PC/MK goal
        pc_mk_goal <- milestone_goals$pc_mk_goal
        pc_mk_action <- milestone_goals$pc_mk_action
        
        if (is.null(pc_mk_goal)) {
            goal_content <- div(
                class = "alert alert-warning",
                tags$i(class = "fas fa-exclamation-triangle me-2"),
                "No Patient Care / Medical Knowledge milestone goal specified"
            )
        } else {
            # Format the goal with milestone ID
            goal_content <- div(
                div(
                    class = "card mb-3",
                    div(
                        class = "card-body",
                        tags$p(
                            tags$strong(paste0("Goal (", pc_mk_goal$column, "): ")),
                            pc_mk_goal$value
                        )
                    )
                )
            )
        }
        
        # Format action plan
        if (is.null(pc_mk_action) || is.na(pc_mk_action) || pc_mk_action == "") {
            action_content <- div(
                class = "alert alert-warning",
                tags$i(class = "fas fa-exclamation-triangle me-2"),
                "No action plan specified for this goal"
            )
        } else {
            action_content <- div(
                div(
                    class = "card",
                    div(
                        class = "card-header bg-light",
                        "Action Plan"
                    ),
                    div(
                        class = "card-body",
                        tags$p(pc_mk_action)
                    )
                )
            )
        }
        
        # Combine goal and action plan
        tagList(
            goal_content,
            action_content
        )
    })
    
    output$sbp_pbl_goal_ui <- renderUI({
        req(values$selected_resident)
        req(values$redcap_period)
        
        # Get app data
        data <- app_data()
        
        # Get milestone goals
        milestone_goals <- get_milestone_goals(
            resident_name = values$selected_resident$name,
            current_period = values$redcap_period,
            resident_data = data$resident_data,
            rdm_dict = data$rdm_dict
        )
        
        # Format SBP/PBL goal
        sbp_pbl_goal <- milestone_goals$sbp_pbl_goal
        sbp_pbl_action <- milestone_goals$sbp_pbl_action
        
        if (is.null(sbp_pbl_goal)) {
            goal_content <- div(
                class = "alert alert-warning",
                tags$i(class = "fas fa-exclamation-triangle me-2"),
                "No Systems-Based Practice / Practice-Based Learning milestone goal specified"
            )
        } else {
            # Format the goal with milestone ID
            goal_content <- div(
                div(
                    class = "card mb-3",
                    div(
                        class = "card-body",
                        tags$p(
                            tags$strong(paste0("Goal (", sbp_pbl_goal$column, "): ")),
                            sbp_pbl_goal$value
                        )
                    )
                )
            )
        }
        
        # Format action plan
        if (is.null(sbp_pbl_action) || is.na(sbp_pbl_action) || sbp_pbl_action == "") {
            action_content <- div(
                class = "alert alert-warning",
                tags$i(class = "fas fa-exclamation-triangle me-2"),
                "No action plan specified for this goal"
            )
        } else {
            action_content <- div(
                div(
                    class = "card",
                    div(
                        class = "card-header bg-light",
                        "Action Plan"
                    ),
                    div(
                        class = "card-body",
                        tags$p(sbp_pbl_action)
                    )
                )
            )
        }
        
        # Combine goal and action plan
        tagList(
            goal_content,
            action_content
        )
    })
    
    output$prof_ics_goal_ui <- renderUI({
        req(values$selected_resident)
        req(values$redcap_period)
        
        # Get app data
        data <- app_data()
        
        # Get milestone goals
        milestone_goals <- get_milestone_goals(
            resident_name = values$selected_resident$name,
            current_period = values$redcap_period,
            resident_data = data$resident_data,
            rdm_dict = data$rdm_dict
        )
        
        # Format Prof/ICS goal
        prof_ics_goal <- milestone_goals$prof_ics_goal
        prof_ics_action <- milestone_goals$prof_ics_action
        
        if (is.null(prof_ics_goal)) {
            goal_content <- div(
                class = "alert alert-warning",
                tags$i(class = "fas fa-exclamation-triangle me-2"),
                "No Professionalism / Interpersonal Communication Skills milestone goal specified"
            )
        } else {
            # Format the goal with milestone ID
            goal_content <- div(
                div(
                    class = "card mb-3",
                    div(
                        class = "card-body",
                        tags$p(
                            tags$strong(paste0("Goal (", prof_ics_goal$column, "): ")),
                            prof_ics_goal$value
                        )
                    )
                )
            )
        }
        
        # Format action plan
        if (is.null(prof_ics_action) || is.na(prof_ics_action) || prof_ics_action == "") {
            action_content <- div(
                class = "alert alert-warning",
                tags$i(class = "fas fa-exclamation-triangle me-2"),
                "No action plan specified for this goal"
            )
        } else {
            action_content <- div(
                div(
                    class = "card",
                    div(
                        class = "card-header bg-light",
                        "Action Plan"
                    ),
                    div(
                        class = "card-body",
                        tags$p(prof_ics_action)
                    )
                )
            )
        }
        
        # Combine goal and action plan
        tagList(
            goal_content,
            action_content
        )
    })
    
    
    #---------------------
    # Career Planning
    # ------------------
    output$career_data_ui <- renderUI({
        req(values$selected_resident)
        req(values$redcap_period)  # Use redcap_period which is already properly mapped
        
        # Get app data
        data <- app_data()
        resident_data <- data$resident_data
        
        # Debug - print resident information
        message("Rendering career data for: ", values$selected_resident$name)
        message("Using period: ", values$redcap_period, " for data lookup")
        
        # Get career data using our helper function with the already mapped period
        career_data <- process_career_data(
            resident_name = values$selected_resident$name,
            current_period = values$redcap_period,  # Use redcap_period directly
            resident_data = resident_data,
            rdm_dict = data$rdm_dict
        )
        
        # If no career data could be found
        if (is.null(career_data)) {
            return(
                div(
                    class = "alert alert-info",
                    icon("info-circle"), 
                    paste("No career planning data is available for", values$selected_resident$name, 
                          "in the", values$redcap_period, "period.")
                )
            )
        }
        
        message("Found career data - is_graduating: ", career_data$is_graduating)
        
        # Create UI based on whether resident is graduating or not
        if (career_data$is_graduating) {
            # UI for graduating residents
            tagList(
                # Graduation plans section
                if (!is.null(career_data$grad_info)) {
                    div(
                        class = "career-section",
                        h5("Post-Graduation Plans", class = "text-primary"),
                        
                        # Next step after graduation
                        if (!is.null(career_data$grad_info$next_step)) {
                            div(
                                class = "mb-3",
                                tags$strong("Next Step:"),
                                p(career_data$grad_info$next_step, class = "ms-3")
                            )
                        },
                        
                        # Location if available
                        if (!is.null(career_data$grad_info$location)) {
                            div(
                                class = "mb-3",
                                tags$strong("Location:"),
                                p(career_data$grad_info$location, class = "ms-3")
                            )
                        },
                        
                        # Location type if available
                        if (!is.null(career_data$grad_info$location_type)) {
                            div(
                                class = "mb-3",
                                tags$strong("Location Type:"),
                                p(career_data$grad_info$location_type, class = "ms-3")
                            )
                        },
                        
                        # Fellowship location if applicable
                        if (!is.null(career_data$grad_info$fellowship_location)) {
                            div(
                                class = "mb-3",
                                tags$strong("Fellowship Location:"),
                                p(career_data$grad_info$fellowship_location, class = "ms-3")
                            )
                        }
                    )
                } else {
                    div(
                        class = "alert alert-info",
                        icon("info-circle"), 
                        paste("No post-graduation plans are recorded for", values$selected_resident$name)
                    )
                },
                
                # Fellowship section for graduating residents
                if (!is.null(career_data$fellowship) && length(career_data$fellowship) > 0) {
                    div(
                        class = "career-section",
                        h5("Fellowship Plans", class = "text-primary"),
                        tags$ul(
                            class = "list-group list-group-flush",
                            lapply(career_data$fellowship, function(fellow) {
                                tags$li(
                                    class = "list-group-item",
                                    icon("award", class = "text-primary me-2"),
                                    fellow
                                )
                            })
                        )
                    )
                }
            )
        } else {
            # UI for non-graduating residents
            tagList(
                # Career path interests section
                if (!is.null(career_data$career_path) && length(career_data$career_path) > 0) {
                    div(
                        class = "career-section",
                        h5("Career Path Interests", class = "text-primary"),
                        tags$ul(
                            class = "list-group list-group-flush",
                            lapply(names(career_data$career_path), function(key) {
                                value <- career_data$career_path[[key]]
                                if (is.character(value) && !grepl("fellow", key, ignore.case = TRUE)) {
                                    tags$li(
                                        class = "list-group-item",
                                        icon("briefcase", class = "text-primary me-2"),
                                        value
                                    )
                                }
                            })
                        )
                    )
                } else {
                    div(
                        class = "alert alert-info",
                        icon("info-circle"), 
                        paste("No career path interests are recorded for", values$selected_resident$name)
                    )
                },
                
                # Fellowship interests section
                if ((!is.null(career_data$fellowship_interest) && career_data$fellowship_interest) || 
                    any(grepl("fellow", names(career_data$career_path), ignore.case = TRUE))) {
                    # Collect fellowship interests
                    fellowship_items <- c()
                    
                    # Check career path for fellowship items
                    if (!is.null(career_data$career_path)) {
                        fellowship_keys <- grep("fellow", names(career_data$career_path), ignore.case = TRUE, value = TRUE)
                        for (key in fellowship_keys) {
                            fellowship_items <- c(fellowship_items, career_data$career_path[[key]])
                        }
                    }
                    
                    div(
                        class = "career-section mt-4",
                        h5("Fellowship Interests", class = "text-primary"),
                        tags$ul(
                            class = "list-group list-group-flush",
                            lapply(fellowship_items, function(fellow) {
                                tags$li(
                                    class = "list-group-item",
                                    icon("award", class = "text-primary me-2"),
                                    fellow
                                )
                            })
                        )
                    )
                },
                
                # Track information section
                if (!is.null(career_data$track_info) && !is.null(career_data$track_info$has_track) && 
                    career_data$track_info$has_track == "Yes") {
                    
                    # Collect track types
                    track_types <- c()
                    
                    # Check for track_name
                    if (!is.null(career_data$track_info$track_name)) {
                        track_types <- c(track_types, career_data$track_info$track_name)
                    }
                    
                    # Check for track_types as a list
                    if (!is.null(career_data$track_info$track_types) && is.list(career_data$track_info$track_types)) {
                        for (key in names(career_data$track_info$track_types)) {
                            track_types <- c(track_types, career_data$track_info$track_types[[key]])
                        }
                    }
                    
                    div(
                        class = "career-section mt-4",
                        h5("Interest in Special Tracks", class = "text-primary"),
                        p(strong("Interested in a Track: "), "Yes"),
                        
                        if (length(track_types) > 0) {
                            tagList(
                                p(strong("Track Types:")),
                                tags$ul(
                                    lapply(track_types, function(track) {
                                        tags$li(track)
                                    })
                                )
                            )
                        }
                    )
                },
                
                # Discussion topics section
                div(
                    class = "discussion-topics mt-4",
                    h5("Suggested Discussion Topics"),
                    p("Consider discussing these topics with the resident:"),
                    tags$ul(
                        tags$li("Career timeline and major decision points"),
                        tags$li("Resources for exploring specialties and fellowship options"),
                        tags$li("Networking opportunities and mentorship connections"),
                        tags$li("Work-life balance considerations"),
                        tags$li("CV/resume development and interview preparation")
                    )
                )
            )
        }
    })
    
    #------------------------
    # Summary Review
    # ------------------------
    # Server code for the discussion topics component
    # Add this to your server.R file
    # 
    # Render the discussion topics UI
    output$discussion_topics_display <- renderUI({
      req(values$selected_resident)
      req(values$current_period)
      
      # Get app data
      data <- app_data()
      resident_data <- data$resident_data
      
      # Debug
      message("Rendering discussion topics display for: ", values$selected_resident$name)
      
      # Get discussion topics using the fixed function
      discussion_topics <- get_discussion_topics(
        resident_name = values$selected_resident$name,
        current_period = values$current_period,
        resident_data = resident_data
      )
      
      # Display the topics if available
      if (!is.null(discussion_topics) && discussion_topics != "") {
        message("Displaying discussion topics: ", substr(discussion_topics, 1, 50), "...")
        return(
          div(
            class = "p-3 bg-light border rounded mb-4",
            style = "white-space: pre-wrap;",
            discussion_topics
          )
        )
      } else {
        # Show a message if no topics found
        message("No discussion topics found")
        return(
          div(
            class = "alert alert-info",
            icon("info-circle"),
            "No additional concerns or discussion topics found in the resident's self-evaluation."
          )
        )
      }
    })
    
    
    
    #==================================
    # Milestones card functions
    # ==================================
    # Re-render the current self-assessment plot for milestone review
    output$self_milestones_plot_m <- renderPlot({
      req(values$selected_resident)
      req(values$redcap_period)  # <-- CHANGE: Use redcap_period instead
      
      data <- app_data()
      
      # REMOVE: No longer need to map the period here
      # mile_period <- map_to_milestone_period(values$selected_resident$Level, values$current_period)
      
      # Use the centrally mapped period directly
      mile_period <- values$redcap_period  # <-- CHANGE: Use redcap_period
      
      if (!is.null(data$s_miles) && !is.na(mile_period)) {
        tryCatch({
          message(paste("Attempting to render self milestone plot for period:", mile_period))
          
          # Debug: check if there's any data for this resident and period
          has_data <- any(data$s_miles$name == values$selected_resident$name & 
                            data$s_miles$period == mile_period, na.rm = TRUE)
          message("Data exists for this resident and period: ", has_data)
          
          # Call miles_plot with the correct parameters
          miles_plot(data$s_miles, values$selected_resident$name, mile_period)
        }, error = function(e) {
          message(paste("Error rendering self milestone plot:", e$message))
          ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = paste("Error rendering milestone plot:", e$message),
                     color = "red", size = 4) +
            theme_void()
        })
      } else {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste("No self-assessment data available for period:", 
                                 ifelse(is.na(mile_period), "Unknown", mile_period)),
                   color = "darkgray", size = 5) +
          theme_void()
      }
    })
    
    # Re-render the previous program assessment plot for milestone review
    output$program_milestones_plot_m <- renderPlot({
      req(values$selected_resident)
      # Require the previously mapped period instead of calculating it again
      req(values$redcap_prev_period)  
      
      data <- app_data()
      
      # Debugging
      message("In program_milestones_plot - Using mapped previous period")
      message("Current period: ", values$current_period)
      message("Current mapped period: ", values$redcap_period)
      message("Previous mapped period: ", values$redcap_prev_period)
      
      # Use the centrally calculated previous period directly
      prev_mile_period <- values$redcap_prev_period
      
      # Only attempt to render if previous period exists and should have program data
      if (!is.na(prev_mile_period) && !is.null(data$p_miles)) {
        tryCatch({
          message(paste("Attempting to render program milestone plot for period:", prev_mile_period))
          
          # Debug: check if there's any data for this resident and period
          if (!is.null(data$p_miles)) {
            has_data <- any(data$p_miles$name == values$selected_resident$name & 
                              data$p_miles$period == prev_mile_period, na.rm = TRUE)
            message("Data exists for this resident and previous period: ", has_data)
            
            if (has_data) {
              # Call miles_plot with the correct parameters
              p <- miles_plot(data$p_miles, values$selected_resident$name, prev_mile_period)
              return(p)
            }
          }
          
          # If we get here, no data was found
          ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = paste("No milestone data found for", values$selected_resident$name, 
                                   "in period", prev_mile_period),
                     color = "darkgray", size = 5) +
            theme_void()
          
        }, error = function(e) {
          message(paste("Error rendering previous program plot:", e$message))
          ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = paste("Error rendering milestone plot:", e$message),
                     color = "red", size = 4) +
            theme_void()
        })
      } else {
        # Create an empty plot when no previous program data exists or is expected
        reason <- if(is.na(prev_mile_period)) {
          "No previous assessment period"
        } else if (is.null(data$p_miles)) {
          "No milestone data available"
        } else {
          "No program assessment for the previous period"
        }
        
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = reason,
                   color = "darkgray", size = 5) +
          theme_void()
      }
    })
    
    # Re-render milestone goals for milestone review
    output$pc_mk_goal_ui_m <- renderUI({
      req(values$selected_resident)
      req(values$redcap_period)
      
      # Get app data
      data <- app_data()
      
      # Get milestone goals
      milestone_goals <- get_milestone_goals(
        resident_name = values$selected_resident$name,
        current_period = values$redcap_period,
        resident_data = data$resident_data,
        rdm_dict = data$rdm_dict
      )
      
      # Format PC/MK goal
      pc_mk_goal <- milestone_goals$pc_mk_goal
      pc_mk_action <- milestone_goals$pc_mk_action
      
      if (is.null(pc_mk_goal)) {
        goal_content <- div(
          class = "alert alert-warning",
          tags$i(class = "fas fa-exclamation-triangle me-2"),
          "No Patient Care / Medical Knowledge milestone goal specified"
        )
      } else {
        # Format the goal with milestone ID
        goal_content <- div(
          div(
            class = "card mb-3",
            div(
              class = "card-body",
              tags$p(
                tags$strong(paste0("Goal (", pc_mk_goal$column, "): ")),
                pc_mk_goal$value
              )
            )
          )
        )
      }
      
      # Format action plan
      if (is.null(pc_mk_action) || is.na(pc_mk_action) || pc_mk_action == "") {
        action_content <- div(
          class = "alert alert-warning",
          tags$i(class = "fas fa-exclamation-triangle me-2"),
          "No action plan specified for this goal"
        )
      } else {
        action_content <- div(
          div(
            class = "card",
            div(
              class = "card-header bg-light",
              "Action Plan"
            ),
            div(
              class = "card-body",
              tags$p(pc_mk_action)
            )
          )
        )
      }
      
      # Combine goal and action plan
      tagList(
        goal_content,
        action_content
      )
    })
    output$sbp_pbl_goal_ui_m <- renderUI({
      req(values$selected_resident)
      req(values$redcap_period)
      
      # Get app data
      data <- app_data()
      
      # Get milestone goals
      milestone_goals <- get_milestone_goals(
        resident_name = values$selected_resident$name,
        current_period = values$redcap_period,
        resident_data = data$resident_data,
        rdm_dict = data$rdm_dict
      )
      
      # Format SBP/PBL goal
      sbp_pbl_goal <- milestone_goals$sbp_pbl_goal
      sbp_pbl_action <- milestone_goals$sbp_pbl_action
      
      if (is.null(sbp_pbl_goal)) {
        goal_content <- div(
          class = "alert alert-warning",
          tags$i(class = "fas fa-exclamation-triangle me-2"),
          "No Systems-Based Practice / Practice-Based Learning milestone goal specified"
        )
      } else {
        # Format the goal with milestone ID
        goal_content <- div(
          div(
            class = "card mb-3",
            div(
              class = "card-body",
              tags$p(
                tags$strong(paste0("Goal (", sbp_pbl_goal$column, "): ")),
                sbp_pbl_goal$value
              )
            )
          )
        )
      }
      
      # Format action plan
      if (is.null(sbp_pbl_action) || is.na(sbp_pbl_action) || sbp_pbl_action == "") {
        action_content <- div(
          class = "alert alert-warning",
          tags$i(class = "fas fa-exclamation-triangle me-2"),
          "No action plan specified for this goal"
        )
      } else {
        action_content <- div(
          div(
            class = "card",
            div(
              class = "card-header bg-light",
              "Action Plan"
            ),
            div(
              class = "card-body",
              tags$p(sbp_pbl_action)
            )
          )
        )
      }
      
      # Combine goal and action plan
      tagList(
        goal_content,
        action_content
      )
    })
    output$prof_ics_goal_ui_m <- renderUI({
      req(values$selected_resident)
      req(values$redcap_period)
      
      # Get app data
      data <- app_data()
      
      # Get milestone goals
      milestone_goals <- get_milestone_goals(
        resident_name = values$selected_resident$name,
        current_period = values$redcap_period,
        resident_data = data$resident_data,
        rdm_dict = data$rdm_dict
      )
      
      # Format Prof/ICS goal
      prof_ics_goal <- milestone_goals$prof_ics_goal
      prof_ics_action <- milestone_goals$prof_ics_action
      
      if (is.null(prof_ics_goal)) {
        goal_content <- div(
          class = "alert alert-warning",
          tags$i(class = "fas fa-exclamation-triangle me-2"),
          "No Professionalism / Interpersonal Communication Skills milestone goal specified"
        )
      } else {
        # Format the goal with milestone ID
        goal_content <- div(
          div(
            class = "card mb-3",
            div(
              class = "card-body",
              tags$p(
                tags$strong(paste0("Goal (", prof_ics_goal$column, "): ")),
                prof_ics_goal$value
              )
            )
          )
        )
      }
      
      # Format action plan
      if (is.null(prof_ics_action) || is.na(prof_ics_action) || prof_ics_action == "") {
        action_content <- div(
          class = "alert alert-warning",
          tags$i(class = "fas fa-exclamation-triangle me-2"),
          "No action plan specified for this goal"
        )
      } else {
        action_content <- div(
          div(
            class = "card",
            div(
              class = "card-header bg-light",
              "Action Plan"
            ),
            div(
              class = "card-body",
              tags$p(prof_ics_action)
            )
          )
        )
      }
      
      # Combine goal and action plan
      tagList(
        goal_content,
        action_content
      )
    })
    
    # Milestone module UI output
    output$milestone_module_ui <- renderUI({
      req(values$selected_resident)
      req(values$redcap_period)
      
      mod_miles_rating_ui("miles")
    })
    
    # Initialize milestone module
    miles_mod <- mod_miles_rating_server(
      id = "miles",
      period = reactive({
        req(values$redcap_period)
        values$redcap_period
      })
    )
    
    # -------------------------------
    # Secondary Review Functionality
    # -------------------------------
    
    # Display resident info for secondary review
    output$secondary_review_resident_name <- renderText({
      req(values$selected_resident)
      values$selected_resident$name
    })
    
    output$secondary_review_period <- renderText({
      req(values$current_period)
      values$current_period
    })
    
    output$secondary_review_primary_coach <- renderText({
      req(values$selected_resident)
      
      # Get primary coach name
      primary_coach <- NULL
      
      # Use resident data to find primary coach
      resident_data <- processed_resident_data()
      if (!is.null(resident_data)) {
        coach_row <- resident_data %>%
          filter(name == values$selected_resident$name) %>%
          select(coach) %>%
          slice(1)
        
        if (nrow(coach_row) > 0 && !is.na(coach_row$coach)) {
          primary_coach <- coach_row$coach
        }
      }
      
      # Return primary coach name or placeholder
      return(primary_coach %||% "Not specified")
    })
    
    # Current Milestone Plot for secondary review
    output$secondary_current_milestones_plot <- renderPlot({
      req(values$selected_resident)
      req(values$redcap_period)
      
      data <- app_data()
      
      # Use the centrally mapped period directly
      mile_period <- values$redcap_period
      
      if (!is.null(data$p_miles) && !is.na(mile_period)) {
        tryCatch({
          message(paste("Attempting to render current program milestone plot for period:", mile_period))
          
          # Debug: check if there's any data for this resident and period
          has_data <- any(data$p_miles$name == values$selected_resident$name & 
                            data$p_miles$period == mile_period, na.rm = TRUE)
          message("Data exists for this resident and period: ", has_data)
          
          if (has_data) {
            # Call miles_plot with the correct parameters
            p <- miles_plot(data$p_miles, values$selected_resident$name, mile_period)
            return(p)
          } else {
            # If no data in p_miles, try to find data in s_miles
            message("No program milestone data found, checking self-milestone data")
            
            if (!is.null(data$s_miles)) {
              has_self_data <- any(data$s_miles$name == values$selected_resident$name & 
                                     data$s_miles$period == mile_period, na.rm = TRUE)
              
              if (has_self_data) {
                message("Using self-milestone data instead")
                p <- miles_plot(data$s_miles, values$selected_resident$name, mile_period)
                return(p)
              }
            }
          }
          
          # If we get here, no data was found in either data set
          ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = paste("No milestone data found for", values$selected_resident$name, 
                                   "in period", mile_period),
                     color = "darkgray", size = 5) +
            theme_void()
          
        }, error = function(e) {
          message(paste("Error rendering milestone plot:", e$message))
          ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = paste("Error rendering milestone plot:", e$message),
                     color = "red", size = 4) +
            theme_void()
        })
      } else {
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste("No milestone data available for period:", 
                                 ifelse(is.na(mile_period), "Unknown", mile_period)),
                   color = "darkgray", size = 5) +
          theme_void()
      }
    })
    
    # Fetch and display primary coach's ILP final comments
    output$primary_coach_comments <- renderText({
      req(values$selected_resident)
      req(values$redcap_period)
      
      # Get resident data
      data <- app_data()
      resident_data <- data$resident_data
      
      # Get REDCap instance number
      instance <- map_period_format(
        level = values$selected_resident$Level,
        period = values$current_period,
        return_type = "instance"
      )
      
      # Try to find coach_ilp_final for this resident and period
      filtered_data <- resident_data %>%
        filter(name == values$selected_resident$name)
      
      # Check for coach_ilp_final
      ilp_final <- NULL
      
      if ("coach_ilp_final" %in% names(filtered_data)) {
        # Try to match the specific period if coach_period exists
        if ("coach_period" %in% names(filtered_data)) {
          period_rows <- filtered_data %>%
            filter(coach_period == as.character(instance))
          
          if (nrow(period_rows) > 0) {
            for (i in 1:nrow(period_rows)) {
              if (!is.na(period_rows$coach_ilp_final[i]) && period_rows$coach_ilp_final[i] != "") {
                ilp_final <- period_rows$coach_ilp_final[i]
                break
              }
            }
          }
        }
        
        # If still not found, try any non-NA coach_ilp_final
        if (is.null(ilp_final)) {
          for (i in 1:nrow(filtered_data)) {
            if (!is.na(filtered_data$coach_ilp_final[i]) && filtered_data$coach_ilp_final[i] != "") {
              ilp_final <- filtered_data$coach_ilp_final[i]
              break
            }
          }
        }
      }
      
      # Try alternative fields if coach_ilp_final not found
      if (is.null(ilp_final)) {
        # Try coach_summary
        if ("coach_summary" %in% names(filtered_data)) {
          for (i in 1:nrow(filtered_data)) {
            if (!is.na(filtered_data$coach_summary[i]) && filtered_data$coach_summary[i] != "") {
              ilp_final <- filtered_data$coach_summary[i]
              break
            }
          }
        }
        
        # Try other coach review fields
        alt_fields <- c("coach_pre_rev", "coach_miles", "coach_rev_notes", "coach_anyelse")
        for (field in alt_fields) {
          if (is.null(ilp_final) && field %in% names(filtered_data)) {
            for (i in 1:nrow(filtered_data)) {
              if (!is.na(filtered_data[[field]][i]) && filtered_data[[field]][i] != "") {
                ilp_final <- filtered_data[[field]][i]
                break
              }
            }
          }
        }
      }
      
      # Return the comments or a default message
      return(ilp_final %||% "No primary coach ILP summary available for this resident.")
    })
    
    # Validation UI for secondary review
    output$secondary_review_validation_ui <- renderUI({
      # Check if all required fields are filled
      ready_to_submit <- FALSE
      missing_items <- character(0)
      
      # Check comments field
      if (is.null(input$secondary_coach_comments) || trimws(input$secondary_coach_comments) == "") {
        missing_items <- c(missing_items, "Your comments on the ILP")
      }
      
      # Check milestone approval
      if (is.null(input$approve_milestones) || input$approve_milestones == "") {
        missing_items <- c(missing_items, "Milestone assessment approval")
      } else if (input$approve_milestones == "no") {
        # If "No" is selected, check for concerns input
        if (is.null(input$milestone_concerns) || trimws(input$milestone_concerns) == "") {
          missing_items <- c(missing_items, "Explanation of milestone concerns")
        }
      }
      
      # Determine if ready to submit
      ready_to_submit <- length(missing_items) == 0
      
      # Render appropriate UI
      if (ready_to_submit) {
        div(
          class = "validation-success",
          icon("check-circle"), 
          "All required fields are complete. You can submit the review."
        )
      } else {
        div(
          class = "validation-error",
          icon("exclamation-triangle"),
          p("Please complete the following required items:"),
          tags$ul(
            lapply(missing_items, function(item) {
              tags$li(item)
            })
          ),
          p("Please complete all required fields before submitting.")
        )
      }
    })
    
    # ----------------------
    # Tab navigation
    # --------------------
    observeEvent(input$next_tab, {
        req(values$current_tab)
        current_index <- match(values$current_tab, values$tab_order)
        if (!is.na(current_index) && current_index < length(values$tab_order)) {
            # Check if self-evaluation is completed before allowing to proceed
            if (values$current_tab == "pre_review" && !input$self_eval_completed) {
                showNotification("Please confirm the resident has completed their self-evaluation.", 
                                 type = "warning")
                return()
            }
            
            next_tab <- values$tab_order[current_index + 1]
            values$current_tab <- next_tab
            
            # Use updateTabsetPanel
            updateTabsetPanel(session, "primary_review_tabs", selected = next_tab)
            
            # Show submit button on last tab
            if (next_tab == values$tab_order[length(values$tab_order)]) {
                shinyjs::show("submit_primary_review")
                shinyjs::hide("next_tab")
            }
        }
    })
    
    observeEvent(input$prev_tab, {
        req(values$current_tab)
        current_index <- match(values$current_tab, values$tab_order)
        
        # If we're on the first tab (pre_review), go back to coach selection page
        if (values$current_tab == "pre_review") {
            # Hide review pages and show coach selection page
            shinyjs::hide("review-pages")
            shinyjs::show("coach-selection-page")
            
            # Reset resident selection
            values$selected_resident <- NULL
            values$review_type <- NULL
            
            # Add notification
            showNotification("Returned to coach dashboard", type = "message")
        } 
        # Otherwise, navigate to the previous tab
        else if (current_index > 1) {
            prev_tab <- values$tab_order[current_index - 1]
            values$current_tab <- prev_tab
            
            # Use updateTabsetPanel
            updateTabsetPanel(session, "primary_review_tabs", selected = prev_tab)
            
            # Hide submit button if not on last tab
            if (values$current_tab != values$tab_order[length(values$tab_order)]) {
                shinyjs::hide("submit_primary_review")
                shinyjs::show("next_tab")
            }
        }
    })
    
    #--------------------------------
    # Submission of coach_rev data to redcap
    # ------------------------------------
    # 

    show_done_page <- function() {
        # Hide review pages and show done page
        shinyjs::hide("review-pages")
        shinyjs::show("done-page")
    }
    # Get list of valid coach_rev fields from the data dictionary
    coach_rev_fields <- reactive({
        req(app_data())
        
        # Extract fields for coach_rev form
        if (!is.null(app_data()$rdm_dict)) {
            fields <- app_data()$rdm_dict %>%
                filter(form_name == "coach_rev") %>%
                pull(field_name)
            
            if (length(fields) > 0) {
                message("Found ", length(fields), " coach_rev fields in data dictionary")
                return(fields)
            }
        }
        
        # Fallback list if dictionary doesn't have coach_rev fields
        message("Using fallback list for coach_rev fields")
        c("coach_date", "coach_period", "coach_pre_rev", "coach_intro_back", 
          "coach_coping", "coach_wellness", "coach_evaluations", "coach_p_d_comments", 
          "coach_ls_and_topic", "coach_step_board", "coach_scholarship", "coach_mile_goal", 
          "coach_career", "coach_anyelse", "coach_summary", "coach_rev_complete")
    })
    
    # Reactive to check if all required fields are filled
    review_is_complete <- reactive({
        # Get all inputs
        all_inputs <- reactiveValuesToList(input)
        
        # Create mapped inputs for coach_rev
        mapped_inputs <- map_inputs_to_coach_rev(
            all_inputs, 
            is_intern_intro = (values$selected_resident$Level == "Intern" && values$current_period == "Intern Intro")
        )
        
        # Validate inputs
        validation <- validate_coach_review_inputs(mapped_inputs, values$tab_order)
        
        # Also require the confirmation checkbox for the summary
        if (validation$valid && (is.null(input$summary_complete) || !input$summary_complete)) {
            validation$valid <- FALSE
            validation$message <- paste(validation$message, "Please check the confirmation box on the Summary tab.")
            validation$missing_tabs <- c(validation$missing_tabs, match("summary", values$tab_order))
        }
        
        return(validation)
    })
    
    # Output for validation UI in summary tab - revised to properly check summary_comments
    output$summary_validation_ui <- renderUI({
        req(values$current_tab == "summary")
        
        # Check if summary is filled out directly
        summary_filled <- !is.null(input$summary_comments) && 
            nchar(trimws(input$summary_comments)) > 0
        
        # Check if summary_complete checkbox is checked
        summary_confirmed <- !is.null(input$summary_complete) && input$summary_complete
        
        if(summary_filled && summary_confirmed) {
            # All good - show success message
            div(
                class = "validation-success",
                icon("check-circle"), 
                "All required fields are complete. You can submit the review."
            )
        } else {
            # Missing fields - show warning with specific message
            missing_items <- c()
            if(!summary_filled) {
                missing_items <- c(missing_items, "Summary content")
            }
            if(!summary_confirmed) {
                missing_items <- c(missing_items, "Confirmation checkbox")
            }
            
            div(
                class = "validation-error",
                icon("exclamation-triangle"),
                p("Please complete the following required items:"),
                tags$ul(
                    lapply(missing_items, function(item) {
                        tags$li(item)
                    })
                ),
                p("Please complete all required fields before submitting.")
            )
        }
    })
    
    # Update the submit button based on completion status - revised to check summary_comments directly
    observe({
        req(values$current_tab)
        
        # Only show submit button on the last tab
        if (values$current_tab == "summary") {
            # Show the submit button
            shinyjs::show("submit_primary_review")
            
            # Directly check if summary is complete
            summary_filled <- !is.null(input$summary_comments) && 
                nchar(trimws(input$summary_comments)) > 0
            summary_confirmed <- !is.null(input$summary_complete) && input$summary_complete
            
            if (summary_filled && summary_confirmed) {
                # Enable submit button with success class
                shinyjs::removeClass("submit_primary_review", "btn-secondary")
                shinyjs::addClass("submit_primary_review", "btn-success")
            } else {
                # Keep enabled but use secondary style
                shinyjs::removeClass("submit_primary_review", "btn-success")
                shinyjs::addClass("submit_primary_review", "btn-secondary")
            }
        } else {
            # Hide submit button if not on summary tab
            shinyjs::hide("submit_primary_review")
        }
    })
    
    observeEvent(input$submit_summary, {
        # Check if summary is complete
        summary_filled <- !is.null(input$summary_comments) && 
            nchar(trimws(input$summary_comments)) > 0
        summary_confirmed <- !is.null(input$summary_complete) && input$summary_complete
        
        if(!summary_filled || !summary_confirmed) {
            # Show validation error
            showNotification("Please complete the summary and check the confirmation box before submitting.", 
                             type = "error", duration = 5)
            return()
        }
        
        # Show confirmation modal
        showModal(modalDialog(
            title = "Confirm Submission",
            "Are you sure you want to submit this coach review? After submission, you'll proceed to the milestone assessment.",
            footer = tagList(
                modalButton("Cancel"),
                actionButton("confirm_submit", "Submit Review", class = "btn-success")
            ),
            easyClose = TRUE
        ))
    })
    
    
    # Add observers for tab navigation via validation links
    observe({
        for (i in seq_along(values$tab_order)) {
            local({
                local_i <- i
                observeEvent(input[[paste0("goto_tab_", local_i)]], {
                    # Navigate to the specified tab
                    tab_name <- values$tab_order[local_i]
                    values$current_tab <- tab_name
                    updateTabsetPanel(session, "primary_review_tabs", selected = tab_name)
                })
            })
        }
    })

     # Testing for milestones
    observeEvent(input$test_milestone_tab, {
        values$current_tab <- "milestones"
        updateTabsetPanel(session, "primary_review_tabs", selected = "milestones")
    })
    
    # Listen for the module's done button click
    observeEvent(miles_mod$done(), {
        req(values$selected_resident)
        req(values$redcap_period)
        req(miles_mod$scores())
        
        # Show confirmation modal
        showModal(modalDialog(
            title = "Confirm Milestone Submission",
            "Are you sure you want to submit the milestone assessment? This will complete the entire review process.",
            footer = tagList(
                modalButton("Cancel"),
                actionButton("confirm_submit_milestones", "Submit Milestones", class = "btn-success")
            ),
            easyClose = TRUE
        ))
    })
    

    # Handle secondary review submission
    observeEvent(input$submit_secondary_review, {
        # Validate required fields
        if (is.null(input$secondary_coach_comments) || trimws(input$secondary_coach_comments) == "" ||
            is.null(input$approve_milestones) || input$approve_milestones == "" ||
            (input$approve_milestones == "no" && 
             (is.null(input$milestone_concerns) || trimws(input$milestone_concerns) == ""))) {
            
            showNotification("Please complete all required fields before submitting.", 
                             type = "error", duration = 5)
            return()
        }
        
        # Show confirmation modal
        showModal(modalDialog(
            title = "Confirm Submission",
            "Are you sure you want to submit this secondary coach review?",
            footer = tagList(
                modalButton("Cancel"),
                actionButton("confirm_submit_secondary", "Submit Review", class = "btn-success")
            ),
            easyClose = TRUE
        ))
    })
    
    # =============================================================================
    # Primary Review Submission
    # =============================================================================
    
    observeEvent(input$confirm_submit, {
      # Show processing notification
      withProgress(message = "Submitting review...", {
        # Get REDCap info
        redcap_url <- app_data()$redcap_url %||% "https://redcapsurvey.slu.edu/api/"
        token <- app_data()$rdm_token
        
        # Try to safely get the record_id
        rec_id <- tryCatch({
          find_record_id(app_data(), values$selected_resident$name)
        }, error = function(e) {
          message("Error finding record_id: ", e$message)
          return(NULL)
        })
        
        # Check if record_id was found
        if (is.null(rec_id)) {
          showNotification("Error: Could not find record ID for resident", type = "error", duration = 5)
          return()
        }
        
        # Get all inputs
        all_inputs <- reactiveValuesToList(input)
        
        # Determine REDCap instance from level and period
        instance <- as.character(get_redcap_instance(
          level = values$selected_resident$Level,
          period = values$current_period
        ))
        
        message("Attempting to submit coach review for resident: ", values$selected_resident$name)
        message("Using record_id: ", rec_id)
        message("Using instance: ", instance)
        
        # Map inputs to REDCap fields
        mapped_inputs <- map_inputs_to_coach_rev(
          all_inputs, 
          is_intern_intro = (values$selected_resident$Level == "Intern" && 
                               values$current_period == "Intern Intro")
        )
        
        # Set the instance number
        mapped_inputs$coach_period <- instance
        
        # Build direct JSON string with required repeating instrument fields
        json_data <- paste0(
          '[{"record_id":"', escape_json_string(rec_id),
          '","redcap_repeat_instrument":"coach_rev",',
          '"redcap_repeat_instance":"', escape_json_string(instance), '"',
          ',"coach_date":"', format(Sys.Date(), "%Y-%m-%d"), '"',
          ',"coach_period":"', escape_json_string(instance), '"'
        )
        
        # Add all mapped fields
        for (field in names(mapped_inputs)) {
          # Skip record_id, coach_date, and coach_period (already added)
          if (field %in% c("record_id", "coach_date", "coach_period")) next
          
          # Only add non-NULL, non-NA values
          if (!is.null(mapped_inputs[[field]]) && !is.na(mapped_inputs[[field]])) {
            # Escape double quotes and encode properly
            value <- gsub('"', '\\\\"', mapped_inputs[[field]])
            # Replace newlines with \n for JSON
            value <- gsub("\r?\n", "\\\\n", value)
            json_data <- paste0(json_data, ',"', field, '":"', value, '"')
          }
        }
        
        # Close the JSON
        json_data <- paste0(json_data, "}]")
        
        message("Submission JSON (first 150 chars): ", substr(json_data, 1, 150))
        
        # IMPORTANT: Always submit to REDCap even in dev_mode for testing
        # This is key to verify if submissions work correctly
        message("Submitting to REDCap")
        response <- httr::POST(
          url = redcap_url,
          body = list(
            token = token,
            content = "record",
            format = "json",
            type = "flat",
            overwriteBehavior = "normal",
            forceAutoNumber = "false",
            data = json_data,
            returnContent = "count",
            returnFormat = "json"
          ),
          encode = "form"
        )
        
        # Process response
        status_code <- httr::status_code(response)
        content_text <- httr::content(response, "text", encoding = "UTF-8")
        
        message("REDCap response status: ", status_code)
        message("REDCap response content: ", content_text)
        
        if (status_code == 200) {
          # Show success notification
          showNotification("Review successfully submitted! Moving to milestone assessment.", 
                           type = "message", duration = 5)
          
          # Navigate to milestone tab
          values$current_tab <- "milestones"
          updateTabsetPanel(session, "primary_review_tabs", selected = "milestones")
        } else {
          # Show error notification
          showNotification(paste("Error submitting review to REDCap:", content_text), 
                           type = "error", duration = 10)
        }
      })
    })

    # =============================================================================
    # Milestone Assessment Submission
    # =============================================================================
    
    # Handle confirmation of milestone submission
    observeEvent(input$confirm_submit_milestones, {
      req(values$selected_resident)
      req(values$redcap_period)
      req(miles_mod$scores())
      
      # Show processing notification
      withProgress(message = "Submitting milestone assessment...", {
        # Get REDCap info
        redcap_url <- app_data()$redcap_url %||% "https://redcapsurvey.slu.edu/api/"
        token <- app_data()$rdm_token
        
        # Get record ID for the selected resident
        record_id <- find_record_id(app_data(), values$selected_resident$name)
        
        # Determine instance number for the period
        instance_number <- get_redcap_instance(
          level = values$selected_resident$Level,
          period = values$current_period
        )
        
        message("Using record_id: ", record_id)
        message("Using instance number: ", instance_number)
        
        # Format today's date
        today_date <- format(Sys.Date(), "%Y-%m-%d")
        
        # Get milestone field mappings
        mile_key2field <- get_milestone_field_mapping()
        desc_enabled_fields <- get_milestone_desc_fields()
        
        # Build JSON directly with required repeating instrument fields
        json_data <- paste0(
          '[{"record_id":"', escape_json_string(record_id),
          '","redcap_repeat_instrument":"milestone_selfevaluation_c33c",',
          '"redcap_repeat_instance":"', escape_json_string(as.character(instance_number)), '"',
          ',"prog_mile_date":"', today_date, '"',
          ',"prog_mile_period":"', escape_json_string(as.character(instance_number)), '"'
        )
        
        # Add milestone scores
        scores <- miles_mod$scores()
        for(key in names(scores)) {
          field_name <- mile_key2field[key]
          if (!is.null(field_name)) {
            # Add score
            json_data <- paste0(json_data, ',"', field_name, '":"', 
                                escape_json_string(as.character(scores[[key]])), '"')
            
            # Add description if available
            if (key %in% desc_enabled_fields) {
              desc <- miles_mod$desc()[[key]]
              if (!is.null(desc) && !is.na(desc) && desc != "") {
                desc_field <- paste0(field_name, "_desc")
                json_data <- paste0(json_data, ',"', desc_field, '":"', 
                                    escape_json_string(as.character(desc)), '"')
              }
            }
          }
        }
        
        # Close the JSON
        json_data <- paste0(json_data, "}]")
        
        message("Submission JSON (first 150 chars): ", substr(json_data, 1, 150))
        
        # Submit to REDCap
        response <- httr::POST(
          url = redcap_url,
          body = list(
            token = token,
            content = "record",
            format = "json",
            type = "flat",
            overwriteBehavior = "normal",
            forceAutoNumber = "false",
            data = json_data,
            returnContent = "count",
            returnFormat = "json"
          ),
          encode = "form"
        )
        
        # Process response
        status_code <- httr::status_code(response)
        content_text <- httr::content(response, "text", encoding = "UTF-8")
        
        message("REDCap response status: ", status_code)
        message("REDCap response content: ", content_text)
        
        if (status_code == 200) {
          # Close the modal
          removeModal()
          
          # Save the data for possible display
          milestone_data$current_data <- process_current_milestone(
            milestone_scores = miles_mod,
            resident_name = values$selected_resident$name,
            current_period = values$redcap_period
          )
          
          milestone_data$submission_status <- "complete"
          
          # Show success notification
          showNotification("Milestone assessment successfully submitted!", 
                           type = "message", duration = 5)
          
          # Navigate to done page
          show_done_page()
        } else {
          # Show error notification
          showNotification(paste("Error submitting milestone assessment:", content_text), 
                           type = "error", duration = 10)
        }
      })
    })
    
    # =============================================================================
    # Secondary Review Submission
    # =============================================================================
    
    # Handle confirmation of secondary review submission
    observeEvent(input$confirm_submit_secondary, {
      req(values$selected_resident)
      req(values$current_period)
      
      # Show processing notification
      withProgress(message = "Submitting secondary review...", {
        # Get REDCap info
        redcap_url <- app_data()$redcap_url %||% "https://redcapsurvey.slu.edu/api/"
        token <- app_data()$rdm_token
        
        # Get record ID
        record_id <- find_record_id(app_data(), values$selected_resident$name)
        
        # Get instance number for the period
        instance_number <- get_redcap_instance(
          level = values$selected_resident$Level,
          period = values$current_period
        )
        
        # Collect the review data
        secondary_review_data <- list(
          second_date = format(Sys.Date(), "%Y-%m-%d"),
          second_period = as.character(instance_number),
          second_comments = input$secondary_coach_comments,
          second_approve = ifelse(input$approve_milestones == "yes", "Yes", "No"),
          second_miles_comment = ifelse(input$approve_milestones == "no", 
                                        input$milestone_concerns, ""),
          second_rev_complete = "2"  # Complete
        )
        
        # Build JSON directly with required repeating instrument fields
        json_data <- paste0(
          '[{"record_id":"', escape_json_string(record_id),
          '","redcap_repeat_instrument":"second_review",',
          '"redcap_repeat_instance":"', escape_json_string(as.character(instance_number)), '"'
        )
        
        # Add all fields
        for (field in names(secondary_review_data)) {
          if (!is.null(secondary_review_data[[field]]) && !is.na(secondary_review_data[[field]])) {
            value <- escape_json_string(secondary_review_data[[field]])
            json_data <- paste0(json_data, ',"', field, '":"', value, '"')
          }
        }
        
        # Close the JSON
        json_data <- paste0(json_data, "}]")
        
        message("Submission JSON (first 150 chars): ", substr(json_data, 1, 150))
        
        # Submit to REDCap
        response <- httr::POST(
          url = redcap_url,
          body = list(
            token = token,
            content = "record",
            format = "json",
            type = "flat",
            overwriteBehavior = "normal",
            forceAutoNumber = "false",
            data = json_data,
            returnContent = "count",
            returnFormat = "json"
          ),
          encode = "form"
        )
        
        # Process response
        status_code <- httr::status_code(response)
        content_text <- httr::content(response, "text", encoding = "UTF-8")
        
        message("REDCap response status: ", status_code)
        message("REDCap response content: ", content_text)
        
        if (status_code == 200) {
          # Close the modal
          removeModal()
          
          # Show success notification
          showNotification("Secondary review successfully submitted!", 
                           type = "message", duration = 5)
          
          # Navigate to done page
          show_done_page()
        } else {
          # Show error notification
          showNotification(paste("Error submitting secondary review:", content_text), 
                           type = "error", duration = 10)
        }
      })
    })
    
    
    
    
    # Handle the "Start New Review" button on the done page
    observeEvent(input$start_new_review, {
        # Reset selection values
        values$selected_resident <- NULL
        values$selected_coach <- NULL
        values$current_period <- NULL
        values$review_type <- NULL
        values$current_tab <- "pre_review"
        
        # Reset any form inputs if needed
        updateTextAreaInput(session, "discussion_points", value = "")
        updateTextAreaInput(session, "resident_background", value = "")
        updateTextAreaInput(session, "dealing_with_residency", value = "")
        updateTextAreaInput(session, "resident_wellbeing", value = "")
        updateTextAreaInput(session, "evaluations_assessment", value = "")
        updateTextAreaInput(session, "evaluations_comments", value = "")
        updateTextAreaInput(session, "knowledge_topics_comments", value = "")
        updateTextAreaInput(session, "board_prep_comments", value = "")
        updateTextAreaInput(session, "knowledge_overall_comments", value = "")
        updateTextAreaInput(session, "scholarship_comments", value = "")
        updateTextAreaInput(session, "milestone_goals_comments", value = "")
        updateTextAreaInput(session, "career_comments", value = "")
        updateTextAreaInput(session, "summary_comments", value = "")
        updateCheckboxInput(session, "summary_complete", value = FALSE)
        
        # Navigate back to coach selection
        shinyjs::hide("done-page")
        shinyjs::show("coach-selection-page")
        
        # Reset the tab panel
        updateTabsetPanel(session, "primary_review_tabs", selected = "pre_review")
    })
    
}
