server <- function(input, output, session) {
    redcap_url <- "https://redcapsurvey.slu.edu/api/"
    
    # Development mode flag (set to FALSE for production)
    dev_mode <- TRUE
    
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
        
        # Debug: Check if we have s_e columns at all
        se_cols <- names(resident_data)[grepl("^s_e_", names(resident_data))]
        message("Found these s_e columns: ", paste(head(se_cols, 5), collapse=", "))
        
        # Debug: Look for alpha test resident
        alpha_rows <- which(resident_data$name == "alpha test")
        if (length(alpha_rows) > 0) {
            message("Found alpha test in rows: ", paste(alpha_rows, collapse=", "))
            
            # Check if s_e_period exists for alpha
            if ("s_e_period" %in% names(resident_data)) {
                alpha_periods <- resident_data$s_e_period[alpha_rows]
                message("Alpha test s_e_period values: ", paste(alpha_periods, collapse=", "))
            }
            
            # Check key fields for alpha
            for (field in c("s_e_plus", "s_e_delta", "s_eval_complete")) {
                if (field %in% names(resident_data)) {
                    alpha_values <- resident_data[[field]][alpha_rows]
                    message("Alpha test ", field, " values: ", paste(alpha_values, collapse=", "))
                }
            }
        } else {
            message("Did not find alpha test in resident_data")
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
                    ),
                    # Add current period column
                    current_period = current_period
                )
            
            message("Found ", nrow(coach_residents), " residents for coach ", input$coach_name)
            
            # Process each resident to determine self-eval status
            self_eval_statuses <- character(nrow(coach_residents))
            
            for (i in 1:nrow(coach_residents)) {
                res_name <- coach_residents$name[i]
                message("\nChecking self-eval for: ", res_name)
                
                # Default to incomplete
                has_self_eval <- FALSE
                
                # Hard-code alpha and beta test to have complete self-evaluations
                if (res_name %in% c("alpha test", "beta test")) {
                    message("Setting test account self-eval to complete: ", res_name)
                    has_self_eval <- TRUE
                } else {
                    # For all other residents, do a thorough check
                    resident_rows <- which(resident_data$name == res_name)
                    
                    if (length(resident_rows) > 0) {
                        message("Found ", length(resident_rows), " rows for ", res_name)
                        
                        # Debug: Print the first row index
                        message("First row index: ", resident_rows[1])
                        
                        # Check for any completion indicators
                        for (row_idx in resident_rows) {
                            # Check all potential completion indicators
                            if ("s_eval_complete" %in% names(resident_data)) {
                                val <- resident_data$s_eval_complete[row_idx]
                                message("s_eval_complete value: ", ifelse(is.na(val), "NA", val))
                                if (!is.na(val) && val == "Complete") {
                                    has_self_eval <- TRUE
                                    message("Found s_eval_complete = Complete")
                                    break
                                }
                            }
                            
                            if ("s_e_period" %in% names(resident_data)) {
                                period_val <- resident_data$s_e_period[row_idx]
                                message("s_e_period value: ", ifelse(is.na(period_val), "NA", period_val))
                                
                                # If period matches and key fields have content
                                if (!is.na(period_val) && period_val == current_period) {
                                    message("Period matches current_period")
                                    
                                    # Check key fields
                                    for (field in c("s_e_plus", "s_e_delta")) {
                                        if (field %in% names(resident_data)) {
                                            field_val <- resident_data[[field]][row_idx]
                                            message(field, " value length: ", ifelse(is.na(field_val), "NA", nchar(field_val)))
                                            
                                            if (!is.na(field_val) && nchar(field_val) > 0) {
                                                has_self_eval <- TRUE
                                                message("Found content in ", field)
                                                break
                                            }
                                        }
                                    }
                                }
                            }
                            
                            if (has_self_eval) break
                        }
                    } else {
                        message("No rows found for ", res_name)
                    }
                }
                
                # Set the status
                self_eval_statuses[i] <- if(has_self_eval) "✅" else "❌"
                message("Final self-eval status for ", res_name, ": ", self_eval_statuses[i])
            }
            
            # Add self-evaluation status
            coach_residents$self_eval_status <- self_eval_statuses
            
            # Select columns for display
            coach_residents <- coach_residents %>%
                select(name, Level, access_code, review_role, current_period, self_eval_status) %>%
                arrange(review_role, Level, name) %>%
                setNames(c("Resident Name", "Level", "Access Code", "Review Role", "Review Period", "Self-Eval"))
            
            if (nrow(coach_residents) > 0) {
                # Create table with click handling
                return(datatable_with_click(
                    coach_residents, 
                    caption = paste0("Residents Assigned to ", input$coach_name)
                ))
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
                `Self-Eval` = character(0)
            ),
            caption = paste0("No residents found for ", input$coach_name)
        ))
    })
    
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
    # Career Planning UI
    output$career_data_ui <- renderUI({
        req(values$selected_resident)
        req(values$current_period)
        
        # Get app data
        data <- app_data()
        resident_data <- data$resident_data
        
        # Get career data using our helper function
        career_data <- process_career_data(
            resident_name = values$selected_resident$name,
            current_period = values$current_period,
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
                          "in the", values$current_period, "period.")
                )
            )
        }
        
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
                if (length(career_data$fellowship) > 0) {
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
                if (length(career_data$career_path) > 0) {
                    div(
                        class = "career-section",
                        h5("Career Path Interests", class = "text-primary"),
                        tags$ul(
                            class = "list-group list-group-flush",
                            lapply(career_data$career_path, function(career) {
                                tags$li(
                                    class = "list-group-item",
                                    icon("briefcase", class = "text-primary me-2"),
                                    career
                                )
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
                if (length(career_data$fellowship) > 0) {
                    div(
                        class = "career-section mt-4",
                        h5("Fellowship Interests", class = "text-primary"),
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
                },
                
                # Track information section
                if (!is.null(career_data$track_info) && career_data$track_info$has_track == "Yes") {
                    div(
                        class = "career-section mt-4",
                        h5("Interest in Special Tracks", class = "text-primary"),
                        p(strong("Interested in a Track: "), "Yes"),
                        
                        if (length(career_data$track_info$track_types) > 0) {
                            tagList(
                                p(strong("Track Types:")),
                                tags$ul(
                                    lapply(career_data$track_info$track_types, function(track) {
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
    output$discussion_topics_ui <- renderUI({
        req(values$selected_resident)
        req(values$current_period)
        
        # Get app data
        data <- app_data()
        resident_data <- data$resident_data
        
        # Debug
        message("Rendering discussion topics UI for: ", values$selected_resident$name)
        
        # Get discussion topics using our fixed function
        discussion_topics <- get_discussion_topics(
            resident_name = values$selected_resident$name,
            current_period = values$current_period,
            resident_data = resident_data
        )
        
        # Only display if there are discussion topics
        if (!is.null(discussion_topics) && discussion_topics != "") {
            message("Creating discussion topics UI")
            
            return(card(
                card_header("Additional Concerns from Self-Evaluation"),
                card_body(
                    # Display the discussion topics
                    div(
                        class = "p-3 bg-light border rounded mb-4",
                        style = "white-space: pre-wrap;",
                        discussion_topics
                    ),
                    
                    # Add comments input
                    textAreaInput(
                        "discussion_topics_comments", 
                        label = "Comments on Discussion Topics:",
                        rows = 4,
                        width = "100%",
                        placeholder = "Add your comments about these discussion topics..."
                    )
                )
            ))
        } else {
            message("No discussion topics found, returning NULL")
            return(NULL)
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
    
    # Second Review back button handler
    observeEvent(input$second_review_back, {
        # Hide review pages and show coach selection page
        shinyjs::hide("review-pages")
        shinyjs::show("coach-selection-page")
        
        # Reset resident selection
        values$selected_resident <- NULL
        values$review_type <- NULL
        
        # Add notification
        showNotification("Returned to coach dashboard", type = "message")
    })
}