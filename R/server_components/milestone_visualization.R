# ============================================================================
# MILESTONE VISUALIZATION COMPONENT
# R/server_components/milestone_visualization.R
# ============================================================================

#' Initialize Milestone Visualization Outputs
#' 
#' Sets up all milestone plotting outputs using gmed functions
#' @param input Shiny input object
#' @param output Shiny output object  
#' @param session Shiny session object
#' @param values Reactive values object
#' @return NULL
initialize_milestone_visualization <- function(input, output, session, values) {
  
  # ============================================================================
  # ENHANCED MILESTONE VISUALIZATION WITH GMED
  # ============================================================================
  output$enhanced_milestone_plot <- plotly::renderPlotly({
    req(values$selected_resident)
    req(values$current_period)
    
    message("=== CREATING ENHANCED MILESTONE PLOT ===")
    message("Resident: ", values$selected_resident$name)
    message("Period: ", values$current_period)
    message("Record ID: ", values$selected_resident$record_id)
    
    tryCatch({
      # Get milestone data using gmed functions
      milestone_workflow <- tryCatch({
        message("Calling gmed::create_clean_milestone_workflow...")
        gmed::create_clean_milestone_workflow(
          raw_milestone_data = app_data()$resident_data,  # Use resident_data, not raw_data
          resident_data = app_data()$resident_data
        )
      }, error = function(e) {
        message("gmed milestone workflow failed: ", e$message)
        return(NULL)
      })
      
      if (!is.null(milestone_workflow)) {
        message("Milestone workflow created successfully")
        message("P_miles rows: ", ifelse(is.null(milestone_workflow$p_miles), "NULL", nrow(milestone_workflow$p_miles)))
        message("S_miles rows: ", ifelse(is.null(milestone_workflow$s_miles), "NULL", nrow(milestone_workflow$s_miles)))
        
        # Try self-assessment first, then program assessment
        milestone_data_to_use <- NULL
        plot_type <- NULL
        
        if (!is.null(milestone_workflow$s_miles) && nrow(milestone_workflow$s_miles) > 0) {
          # Filter for this resident and period
          resident_milestones <- milestone_workflow$s_miles %>%
            filter(name == values$selected_resident$name)
          
          if (nrow(resident_milestones) > 0) {
            milestone_data_to_use <- milestone_workflow$s_miles
            plot_type <- "self"
            message("Using self-assessment milestones")
          }
        }
        
        if (is.null(milestone_data_to_use) && !is.null(milestone_workflow$p_miles) && nrow(milestone_workflow$p_miles) > 0) {
          # Try program milestones
          resident_milestones <- milestone_workflow$p_miles %>%
            filter(name == values$selected_resident$name)
          
          if (nrow(resident_milestones) > 0) {
            milestone_data_to_use <- milestone_workflow$p_miles
            plot_type <- "program"
            message("Using program milestones")
          }
        }
        
        if (!is.null(milestone_data_to_use)) {
          # Create spider plot using gmed function
          spider_plot <- tryCatch({
            message("Creating spider plot with gmed function...")
            gmed::create_milestone_spider_plot_final(
              milestone_data = milestone_data_to_use,
              median_data = milestone_workflow$milestone_medians,
              resident_id = values$selected_resident$record_id,
              period_text = values$current_period,
              milestone_type = plot_type,
              resident_data = app_data()$resident_data
            )
          }, error = function(e) {
            message("gmed spider plot failed: ", e$message)
            
            # Fallback to basic milestone plot
            tryCatch({
              if (exists("miles_plot", envir = .GlobalEnv)) {
                message("Trying fallback miles_plot function...")
                
                # Convert plotly if it's a ggplot
                basic_plot <- miles_plot(milestone_data_to_use, values$selected_resident$name, values$current_period)
                if ("ggplot" %in% class(basic_plot)) {
                  plotly::ggplotly(basic_plot)
                } else {
                  basic_plot
                }
              } else {
                plotly::plot_ly() %>% 
                  plotly::add_annotations(
                    text = paste("Error loading milestone data:", e$message), 
                    x = 0.5, y = 0.5,
                    showarrow = FALSE
                  )
              }
            }, error = function(e2) {
              message("Fallback plot also failed: ", e2$message)
              plotly::plot_ly() %>% 
                plotly::add_annotations(
                  text = "Unable to create milestone visualization", 
                  x = 0.5, y = 0.5,
                  showarrow = FALSE
                )
            })
          })
          
          return(spider_plot)
          
        } else {
          message("No milestone data found for resident")
          return(plotly::plot_ly() %>% 
                   plotly::add_annotations(
                     text = paste("No milestone data available for", values$selected_resident$name), 
                     x = 0.5, y = 0.5,
                     showarrow = FALSE
                   ))
        }
      } else {
        message("Milestone workflow creation failed")
        return(plotly::plot_ly() %>% 
                 plotly::add_annotations(
                   text = "Unable to process milestone data", 
                   x = 0.5, y = 0.5,
                   showarrow = FALSE
                 ))
      }
      
    }, error = function(e) {
      message("Error creating milestone plot: ", e$message)
      
      return(plotly::plot_ly() %>%
               plotly::add_annotations(
                 text = paste("Error loading milestone data:", e$message), 
                 x = 0.5, y = 0.5,
                 showarrow = FALSE
               ))
    })
  })
  
  # ============================================================================
  # ALTERNATIVE MILESTONE PLOTS FOR COMPARISON
  # ============================================================================
  output$self_milestone_plot <- renderPlot({
    req(values$selected_resident)
    req(values$current_period)
    
    message("=== CREATING SELF MILESTONE PLOT ===")
    
    data <- app_data()
    
    if (!is.null(data$s_miles)) {
      tryCatch({
        # Use existing miles_plot function if available
        if (exists("miles_plot", envir = .GlobalEnv)) {
          miles_plot(data$s_miles, values$selected_resident$name, values$current_period)
        } else {
          # Create basic ggplot
          resident_data <- data$s_miles %>%
            filter(name == values$selected_resident$name)
          
          if (nrow(resident_data) > 0) {
            ggplot() +
              annotate("text", x = 0.5, y = 0.5,
                       label = paste("Self-assessment data found for", values$selected_resident$name),
                       color = "blue", size = 4) +
              theme_void()
          } else {
            ggplot() +
              annotate("text", x = 0.5, y = 0.5,
                       label = "No self-assessment data available",
                       color = "gray", size = 4) +
              theme_void()
          }
        }
      }, error = function(e) {
        message("Error in self milestone plot: ", e$message)
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste("Error:", e$message),
                   color = "red", size = 3) +
          theme_void()
      })
    } else {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "No self-assessment milestone data available",
                 color = "gray", size = 4) +
        theme_void()
    }
  })
  
  output$program_milestone_plot <- renderPlot({
    req(values$selected_resident)
    req(values$current_period)
    
    message("=== CREATING PROGRAM MILESTONE PLOT ===")
    
    data <- app_data()
    
    if (!is.null(data$p_miles)) {
      tryCatch({
        # Use existing miles_plot function if available
        if (exists("miles_plot", envir = .GlobalEnv)) {
          miles_plot(data$p_miles, values$selected_resident$name, values$current_period)
        } else {
          # Create basic ggplot
          resident_data <- data$p_miles %>%
            filter(name == values$selected_resident$name)
          
          if (nrow(resident_data) > 0) {
            ggplot() +
              annotate("text", x = 0.5, y = 0.5,
                       label = paste("Program assessment data found for", values$selected_resident$name),
                       color = "green", size = 4) +
              theme_void()
          } else {
            ggplot() +
              annotate("text", x = 0.5, y = 0.5,
                       label = "No program assessment data available",
                       color = "gray", size = 4) +
              theme_void()
          }
        }
      }, error = function(e) {
        message("Error in program milestone plot: ", e$message)
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste("Error:", e$message),
                   color = "red", size = 3) +
          theme_void()
      })
    } else {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "No program milestone data available",
                 color = "gray", size = 4) +
        theme_void()
    }
  })
}