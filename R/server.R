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
        req(values$current_period)
        
        data <- app_data()
        
        # Map the app period to milestone period format
        mile_period <- map_to_milestone_period(values$selected_resident$Level, values$current_period)
        
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
        req(values$current_period)
        
        data <- app_data()
        
        # Debugging
        message("In program_milestones_plot - Getting previous period")
        message("Current period: ", values$current_period)
        message("Resident level: ", values$selected_resident$Level)
        
        # Get the previous period in app format
        prev_app_period <- get_previous_period(
            values$current_period, 
            values$selected_resident$Level
        )
        
        message("Previous app period: ", ifelse(is.na(prev_app_period), "NA", prev_app_period))
        
        # If there's a previous period, map it to milestone format
        if (!is.na(prev_app_period)) {
            prev_mile_period <- map_to_milestone_period(values$selected_resident$Level, prev_app_period)
            message("Previous milestone period: ", ifelse(is.na(prev_mile_period), "NA", prev_mile_period))
        } else {
            prev_mile_period <- NA
            message("No previous period found")
        }
        
        # If we're dealing with "End Review" we might need a special mapping
        if (values$current_period == "End Review" && values$selected_resident$Level == "Intern") {
            message("Special case: End Review for Intern")
            prev_mile_period <- "Mid Intern"
            message("Forced previous milestone period to: Mid Intern")
        }
        
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
        req(values$current_period)
        
        # Get resident level
        resident_level <- values$selected_resident$Level
        
        # Get app data
        data <- app_data()
        
        # Create CCC notes table
        ccc_table <- tryCatch({
            create_ccc_notes_table(
                resident_name = values$selected_resident$name,
                current_period = values$current_period,
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
        req(values$current_period)
        
        # Get resident level
        resident_level <- values$selected_resident$Level
        
        # Debug
        message("Looking for previous period for current period: ", values$current_period, 
                ", resident level: ", resident_level)
        
        # Get the previous period
        prev_period <- get_previous_period(values$current_period, resident_level)
        
        # Debug
        message("Current period: ", values$current_period, 
                ", Resident level: ", resident_level,
                ", Previous period: ", ifelse(is.na(prev_period), "NA", prev_period))
        
        if (is.na(prev_period)) {
            return(div(class = "alert alert-info", 
                       "No previous ILP data available for this resident's current level and period."))
        }
        
        # Get app data
        data <- app_data()
        
        # Debug
        message("Attempting to find ILP data for resident: ", values$selected_resident$name, 
                " in period: ", prev_period)
        
        # Create ILP table
        ilp_table <- tryCatch({
            create_ilp_data_table(
                resident_name = values$selected_resident$name,
                period = prev_period,
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
                             "in the", prev_period, "period.")))
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
            h4(paste("Previous ILP from", prev_period, "Period")),
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
    
    # Tab navigation
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