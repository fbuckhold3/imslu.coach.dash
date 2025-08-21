# ============================================================================
# TABLE GENERATION COMPONENT
# R/server_components/table_generation.R
# ============================================================================

#' Initialize Table Generation Outputs
#' 
#' Sets up coach residents table and related outputs
#' @param input Shiny input object
#' @param output Shiny output object  
#' @param session Shiny session object
#' @param values Reactive values object
#' @param processed_resident_data Reactive expression for processed resident data
#' @return NULL
initialize_table_generation <- function(input, output, session, values, processed_resident_data) {
  
  # ============================================================================
  # ENHANCED COACH DASHBOARD OUTPUTS
  # ============================================================================
  output$dashboard_coach_name <- renderText({ 
    req(input$coach_name)
    input$coach_name 
  })
  
  output$coach_residents_table <- DT::renderDataTable({
    req(input$coach_name)
    req(processed_resident_data())
    
    message("=== CREATING ENHANCED RESIDENTS TABLE ===")
    
    resident_data <- processed_resident_data()
    
    # Determine which period to use
    if (input$enable_period_override && input$override_period != "auto") {
      review_period <- input$override_period
      message("Using override period: ", review_period)
    } else {
      review_period <- tryCatch({
        gmed::get_current_period()
      }, error = function(e) {
        message("Error getting current period: ", e$message)
        "Intern Intro"  # Default fallback
      })
      message("Using current period: ", review_period)
    }
    
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
        
        # Process each resident with enhanced status checking
        for (i in 1:nrow(coach_residents)) {
          res_name <- coach_residents$name[i]
          res_level <- coach_residents$level[i]
          
          message("Processing resident ", i, ": ", res_name, " (Level: ", res_level, ")")
          
          # Check if reviewed in current period using gmed function
          should_review <- TRUE
          if (!input$enable_period_override && exists("should_resident_be_reviewed", where = "package:gmed")) {
            should_review <- tryCatch({
              gmed::should_resident_be_reviewed(res_level, review_period)
            }, error = function(e) {
              message("Error checking should_resident_be_reviewed: ", e$message)
              TRUE  # Default to allowing review
            })
          }
          
          if (should_review) {
            # Get milestone period using gmed
            redcap_period <- tryCatch({
              gmed::map_to_milestone_period(res_level, review_period)
            }, error = function(e) {
              message("Error mapping milestone period: ", e$message)
              review_period  # Use original period as fallback
            })
            
            redcap_periods[i] <- ifelse(is.na(redcap_period), "No Review", as.character(redcap_period))
            
            # Enhanced completion status checking using simpler approach
            if (!is.na(redcap_period)) {
              completion <- tryCatch({
                # Use the same pattern as the working server.R
                self_eval_complete <- gmed::check_self_eval_complete(
                  app_data()$resident_data, 
                  res_name, 
                  redcap_period
                )
                
                coach_review_complete <- tryCatch({
                  if (exists("check_coach_review_complete_status", where = "package:gmed")) {
                    gmed::check_coach_review_complete_status(
                      app_data()$resident_data, 
                      res_name, 
                      redcap_period,
                      level = res_level
                    )
                  } else {
                    FALSE
                  }
                }, error = function(e) {
                  message("Error checking coach review status: ", e$message)
                  FALSE
                })
                
                list(
                  self_eval = self_eval_complete,
                  coach_review = coach_review_complete
                )
              }, error = function(e) {
                message("Error checking completion status for ", res_name, ": ", e$message)
                list(self_eval = FALSE, coach_review = FALSE)
              })
              
              # Enhanced status indicators with icons
              self_eval_statuses[i] <- ifelse(completion$self_eval, 
                                              "<span class='text-success'><i class='fas fa-check-circle'></i> Complete</span>", 
                                              "<span class='text-danger'><i class='fas fa-times-circle'></i> Pending</span>")
              primary_review_statuses[i] <- ifelse(completion$coach_review, 
                                                   "<span class='text-success'><i class='fas fa-check-circle'></i> Complete</span>", 
                                                   "<span class='text-danger'><i class='fas fa-times-circle'></i> Pending</span>")
              secondary_review_statuses[i] <- "<span class='text-secondary'><i class='fas fa-minus-circle'></i> Pending</span>"
            } else {
              self_eval_statuses[i] <- "<span class='text-muted'><i class='fas fa-minus'></i> N/A</span>"
              primary_review_statuses[i] <- "<span class='text-muted'><i class='fas fa-minus'></i> N/A</span>"
              secondary_review_statuses[i] <- "<span class='text-muted'><i class='fas fa-minus'></i> N/A</span>"
            }
          } else {
            redcap_periods[i] <- "No Review This Period"
            self_eval_statuses[i] <- "<span class='text-muted'><i class='fas fa-calendar-times'></i> Not scheduled</span>"
            primary_review_statuses[i] <- "<span class='text-muted'><i class='fas fa-calendar-times'></i> Not scheduled</span>"
            secondary_review_statuses[i] <- "<span class='text-muted'><i class='fas fa-calendar-times'></i> Not scheduled</span>"
          }
        }
        
        # Build enhanced table with better column selection
        coach_residents <- coach_residents %>%
          select(name, level, access_code, review_role) %>%
          mutate(
            redcap_period = redcap_periods,
            self_eval_status = self_eval_statuses,
            primary_review_status = primary_review_statuses,
            secondary_review_status = secondary_review_statuses
          ) %>%
          arrange(
            review_role,
            factor(level, levels = c("Intern", "PGY2", "PGY3", "Unknown")),
            name
          ) %>%
          setNames(c("Resident Name", "Level", "Access Code", "Review Role", "Review Period",
                     "Self-Eval", "Primary Review", "Secondary Review"))
        
        message("Successfully created enhanced table with ", nrow(coach_residents), " rows")
        
        # Create enhanced datatable with gmed styling
        dt <- DT::datatable(
          coach_residents,
          escape = FALSE,  # Important for HTML icons
          options = list(
            pageLength = 15,
            dom = 'ftp',
            scrollX = TRUE,
            columnDefs = list(
              list(width = "100px", targets = c(5, 6, 7)),
              list(className = "text-center", targets = c(3, 4, 5, 6, 7))
            )
          ),
          caption = paste0("Enhanced Residents for ", input$coach_name, " - ", review_period),
          rownames = FALSE,
          class = 'table table-striped table-hover gmed-datatable',
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
        
        return(dt)
      }
      
      # Return empty table if no residents found
      return(DT::datatable(
        data.frame(Message = "No residents found for this coach"),
        options = list(dom = 't'),
        rownames = FALSE
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
}