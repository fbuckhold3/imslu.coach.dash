# ============================================================================
# EVALUATION DISPLAYS COMPONENT
# R/server_components/evaluation_displays.R
# ============================================================================

#' Initialize Evaluation Display Outputs
#' 
#' Sets up plus/delta displays and data visualization outputs
#' @param input Shiny input object
#' @param output Shiny output object  
#' @param session Shiny session object
#' @param values Reactive values object
#' @return NULL
initialize_evaluation_displays <- function(input, output, session, values) {
  
  # ============================================================================
  # ENHANCED PLUS/DELTA DISPLAY WITH FALLBACKS
  # ============================================================================
  output$enhanced_plus_delta_display <- renderUI({
    req(values$selected_resident)
    
    message("=== GENERATING PLUS/DELTA DATA ===")
    message("Resident: ", values$selected_resident$name)
    message("Record ID: ", values$selected_resident$record_id)
    
    # Get plus/delta data using multiple approaches
    plus_delta_data <- tryCatch({
      data <- app_data()
      
      # Method 1: Try RDM 2.0 approach first if we have record_id
      if (!is.null(values$selected_resident$record_id) && 
          values$selected_resident$record_id != "unknown" &&
          exists("generate_plus_delta_rdm2", where = "package:gmed")) {
        
        message("Trying RDM 2.0 plus/delta approach...")
        rdm2_data <- tryCatch({
          gmed::generate_plus_delta_rdm2(data$resident_data, values$selected_resident$record_id)
        }, error = function(e) {
          message("RDM 2.0 approach failed: ", e$message)
          NULL
        })
        
        if (!is.null(rdm2_data) && nrow(rdm2_data) > 0) {
          message("RDM 2.0 approach successful, found ", nrow(rdm2_data), " records")
          return(rdm2_data)
        }
      }
      
      # Method 2: Try traditional approach
      if (exists("generate_p_d", envir = .GlobalEnv)) {
        message("Trying traditional plus/delta approach...")
        traditional_data <- tryCatch({
          generate_p_d(data$resident_data, values$selected_resident$name)
        }, error = function(e) {
          message("Traditional approach failed: ", e$message)
          NULL
        })
        
        if (!is.null(traditional_data) && nrow(traditional_data) > 0) {
          message("Traditional approach successful, found ", nrow(traditional_data), " records")
          return(traditional_data)
        }
      }
      
      # Method 3: Manual extraction from resident_data
      message("Trying manual extraction...")
      manual_data <- data$resident_data %>%
        filter(name == values$selected_resident$name) %>%
        select(any_of(c("Date", "date", "Rotation", "rotation", "ass_rotation",
                        "cc_res_does_well", "ass_plus", "Plus", "plus",
                        "res_to_improve", "ass_delta", "Delta", "delta",
                        "min_giv_feedback", "Feedback", "feedback",
                        "Evaluator", "evaluator", "fac_name"))) %>%
        filter(if_any(everything(), ~ !is.na(.x) & .x != ""))
      
      if (nrow(manual_data) > 0) {
        message("Manual extraction successful, found ", nrow(manual_data), " records")
        
        # Standardize column names
        if ("cc_res_does_well" %in% names(manual_data) && !"Plus" %in% names(manual_data)) {
          manual_data$Plus <- manual_data$cc_res_does_well
        }
        if ("res_to_improve" %in% names(manual_data) && !"Delta" %in% names(manual_data)) {
          manual_data$Delta <- manual_data$res_to_improve
        }
        if ("min_giv_feedback" %in% names(manual_data) && !"Feedback" %in% names(manual_data)) {
          manual_data$Feedback <- manual_data$min_giv_feedback
        }
        if ("ass_plus" %in% names(manual_data) && !"Plus" %in% names(manual_data)) {
          manual_data$Plus <- manual_data$ass_plus
        }
        if ("ass_delta" %in% names(manual_data) && !"Delta" %in% names(manual_data)) {
          manual_data$Delta <- manual_data$ass_delta
        }
        
        return(manual_data)
      }
      
      message("No plus/delta data found with any method")
      return(NULL)
      
    }, error = function(e) {
      message("Error generating plus/delta data: ", e$message)
      return(NULL)
    })
    
    if (!is.null(plus_delta_data) && nrow(plus_delta_data) > 0) {
      message("Creating plus/delta display with ", nrow(plus_delta_data), " records")
      
      # Extract plus and delta text from the most recent entries
      plus_text <- ""
      delta_text <- ""
      
      # Look for Plus column (various possible names)
      plus_col <- intersect(c("Plus", "plus", "cc_res_does_well", "ass_plus"), names(plus_delta_data))[1]
      if (!is.na(plus_col)) {
        plus_entries <- plus_delta_data[[plus_col]][!is.na(plus_delta_data[[plus_col]]) & plus_delta_data[[plus_col]] != ""]
        if (length(plus_entries) > 0) {
          plus_text <- paste(plus_entries, collapse = " | ")
        }
      }
      
      # Look for Delta column (various possible names)
      delta_col <- intersect(c("Delta", "delta", "res_to_improve", "ass_delta"), names(plus_delta_data))[1]
      if (!is.na(delta_col)) {
        delta_entries <- plus_delta_data[[delta_col]][!is.na(plus_delta_data[[delta_col]]) & plus_delta_data[[delta_col]] != ""]
        if (length(delta_entries) > 0) {
          delta_text <- paste(delta_entries, collapse = " | ")
        }
      }
      
      message("Plus text length: ", nchar(plus_text))
      message("Delta text length: ", nchar(delta_text))
      
      # Try to use gmed plus/delta component, fallback to manual
      tryCatch({
        if (exists("gmed_plus_delta_display", where = "package:gmed")) {
          gmed::gmed_plus_delta_display(
            plus_text = plus_text,
            delta_text = delta_text
          )
        } else {
          create_manual_plus_delta_display(plus_text, delta_text)
        }
      }, error = function(e) {
        message("gmed plus/delta display failed, using fallback: ", e$message)
        create_manual_plus_delta_display(plus_text, delta_text)
      })
    } else {
      message("No plus/delta data available, showing placeholder")
      div(
        class = "alert alert-light text-center",
        icon("info-circle", class = "me-2"),
        "No evaluation feedback available yet for this resident."
      )
    }
  })
  
  # ============================================================================
  # ENHANCED UI OUTPUTS FOR STEPS WITH REAL DATA
  # ============================================================================
  
  # Knowledge topics display with real data
  output$knowledge_topics_display <- renderUI({
    req(values$selected_resident)
    
    data <- app_data()
    knowledge_data <- get_knowledge_topics_data(
      values$selected_resident$name, 
      values$current_period,
      data$resident_data
    )
    
    if (!is.null(knowledge_data) && nrow(knowledge_data) > 0) {
      div(
        class = "card border-info",
        div(
          class = "card-header bg-info text-white",
          icon("brain", class = "me-2"),
          "Knowledge Topics & Discussion Points"
        ),
        div(
          class = "card-body",
          DT::renderDataTable({
            DT::datatable(
              knowledge_data,
              options = list(
                pageLength = 5,
                dom = 't',
                scrollX = TRUE
              ),
              rownames = FALSE,
              class = 'table table-sm table-striped'
            )
          }, outputArgs = list(height = "200px"))
        )
      )
    } else {
      div(
        class = "alert alert-light",
        icon("info-circle", class = "me-2"),
        "No knowledge assessment topics found for this resident."
      )
    }
  })
  
  # Scholarship activities display with real data
  output$scholarship_activities_display <- renderUI({
    req(values$selected_resident)
    
    data <- app_data()
    scholarship_data <- get_scholarship_data(
      values$selected_resident$name,
      data$resident_data
    )
    
    if (!is.null(scholarship_data) && nrow(scholarship_data) > 0) {
      div(
        class = "card border-purple",
        div(
          class = "card-header bg-purple text-white",
          icon("graduation-cap", class = "me-2"),
          "Scholarship Activities"
        ),
        div(
          class = "card-body",
          DT::renderDataTable({
            DT::datatable(
              scholarship_data,
              options = list(
                pageLength = 5,
                dom = 't',
                scrollX = TRUE
              ),
              rownames = FALSE,
              class = 'table table-sm table-striped'
            )
          }, outputArgs = list(height = "200px"))
        )
      )
    } else {
      div(
        class = "alert alert-light",
        icon("info-circle", class = "me-2"),
        "No scholarship activities found for this resident."
      )
    }
  })
  
  # Milestone goals display with real data
  output$milestone_goals_display <- renderUI({
    req(values$selected_resident)
    
    data <- app_data()
    goals_data <- get_milestone_goals_data(
      values$selected_resident$name,
      values$current_period,
      data$resident_data,
      data$rdm_dict
    )
    
    div(
      class = "row",
      
      # PC/MK Goal
      div(
        class = "col-md-4",
        div(
          class = "card border-primary mb-3",
          div(
            class = "card-header bg-primary text-white",
            "PC/MK Goal"
          ),
          div(
            class = "card-body",
            if (!is.null(goals_data$pc_mk_goal)) {
              div(
                p(strong("Goal:"), goals_data$pc_mk_goal$value),
                if (!is.null(goals_data$pc_mk_action)) {
                  p(strong("Action Plan:"), goals_data$pc_mk_action)
                }
              )
            } else {
              p(class = "text-muted", "No PC/MK goal set")
            }
          )
        )
      ),
      
      # SBP/PBL Goal
      div(
        class = "col-md-4",
        div(
          class = "card border-success mb-3",
          div(
            class = "card-header bg-success text-white",
            "SBP/PBL Goal"
          ),
          div(
            class = "card-body",
            if (!is.null(goals_data$sbp_pbl_goal)) {
              div(
                p(strong("Goal:"), goals_data$sbp_pbl_goal$value),
                if (!is.null(goals_data$sbp_pbl_action)) {
                  p(strong("Action Plan:"), goals_data$sbp_pbl_action)
                }
              )
            } else {
              p(class = "text-muted", "No SBP/PBL goal set")
            }
          )
        )
      ),
      
      # PROF/ICS Goal
      div(
        class = "col-md-4",
        div(
          class = "card border-warning mb-3",
          div(
            class = "card-header bg-warning text-dark",
            "PROF/ICS Goal"
          ),
          div(
            class = "card-body",
            if (!is.null(goals_data$prof_ics_goal)) {
              div(
                p(strong("Goal:"), goals_data$prof_ics_goal$value),
                if (!is.null(goals_data$prof_ics_action)) {
                  p(strong("Action Plan:"), goals_data$prof_ics_action)
                }
              )
            } else {
              p(class = "text-muted", "No PROF/ICS goal set")
            }
          )
        )
      )
    )
  })
  
  # ============================================================================
  # MODAL HANDLERS FOR ENHANCED FEATURES
  # ============================================================================
  observeEvent(input$open_detailed_plus_delta, {
    req(values$selected_resident)
    
    plus_delta_data <- tryCatch({
      if (exists("generate_p_d", envir = .GlobalEnv)) {
        generate_p_d(app_data()$resident_data, values$selected_resident$name)
      } else {
        NULL
      }
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(plus_delta_data)) {
      showModal(modalDialog(
        title = tagList(icon("chart-bar"), " Detailed Plus/Delta Feedback"),
        size = "xl",
        easyClose = TRUE,
        
        # Use regular DT::datatable instead of gmed version
        DT::renderDataTable({
          DT::datatable(
            plus_delta_data,
            options = list(
              pageLength = 15,
              scrollX = TRUE,
              dom = 'frtip'
            ),
            caption = paste("Complete evaluation feedback for", values$selected_resident$name),
            escape = FALSE,
            class = 'table table-striped table-hover'
          )
        }),
        
        footer = modalButton("Close")
      ))
    }
  })
}