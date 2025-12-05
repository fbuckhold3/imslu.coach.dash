# ============================================================================
# DEBUG OUTPUT FUNCTIONS
# R/helpers/debug_outputs.R
# ============================================================================

#' Initialize Debug Output Functions
#' 
#' Sets up debug outputs for troubleshooting data issues
#' @param input Shiny input object
#' @param output Shiny output object  
#' @param session Shiny session object
#' @param values Reactive values object
#' @return NULL
initialize_debug_outputs <- function(input, output, session, values) {
  
  # ============================================================================
  # DEBUG OUTPUT FOR PLUS/DELTA DATA STRUCTURE
  # ============================================================================
  output$debug_plus_delta_structure <- renderText({
    req(values$selected_resident)
    
    data <- app_data()
    resident_data <- data$resident_data
    
    # Filter for selected resident
    resident_rows <- resident_data %>%
      filter(name == values$selected_resident$name)
    
    if (nrow(resident_rows) == 0) {
      return("No data found for resident")
    }
    
    # Look for evaluation-related columns
    eval_cols <- names(resident_rows)[grepl("plus|delta|does_well|improve|feedback|eval|ass_", names(resident_rows), ignore.case = TRUE)]
    
    debug_info <- paste(
      "Resident:", values$selected_resident$name,
      "\nRecord ID:", values$selected_resident$record_id,
      "\nRows found:", nrow(resident_rows),
      "\nEvaluation columns found:", length(eval_cols),
      "\nColumns:", paste(eval_cols, collapse = ", "),
      "\n\nSample data:",
      if (length(eval_cols) > 0) {
        paste(capture.output(print(head(resident_rows[eval_cols], 3))), collapse = "\n")
      } else {
        "No evaluation columns found"
      }
    )
    
    return(debug_info)
  })
  
  # ============================================================================
  # DEBUG OUTPUT FOR KNOWLEDGE DATA FIELDS
  # ============================================================================
  output$debug_knowledge_fields <- renderText({
    req(values$selected_resident)
    
    data <- app_data()
    resident_data <- data$resident_data
    
    resident_rows <- resident_data %>%
      filter(name == values$selected_resident$name)
    
    if (nrow(resident_rows) == 0) {
      return("No data found for resident")
    }
    
    # Look for knowledge-related columns
    knowledge_cols <- names(resident_rows)[grepl("s_e_|discussion|topic|knowledge|learn", names(resident_rows), ignore.case = TRUE)]
    
    debug_info <- paste(
      "Knowledge-related columns found:", length(knowledge_cols),
      "\nColumns:", paste(knowledge_cols, collapse = ", "),
      "\n\nSample values:",
      if (length(knowledge_cols) > 0) {
        paste(capture.output({
          for (col in knowledge_cols[1:min(3, length(knowledge_cols))]) {
            cat("\n", col, ":", head(resident_rows[[col]][!is.na(resident_rows[[col]])], 2))
          }
        }), collapse = "\n")
      } else {
        "No knowledge columns found"
      }
    )
    
    return(debug_info)
  })
  
  # ============================================================================
  # DEBUG OUTPUT FOR SCHOLARSHIP DATA FIELDS
  # ============================================================================
  output$debug_scholarship_fields <- renderText({
    req(values$selected_resident)
    
    data <- app_data()
    resident_data <- data$resident_data
    
    resident_rows <- resident_data %>%
      filter(name == values$selected_resident$name)
    
    if (nrow(resident_rows) == 0) {
      return("No data found for resident")
    }
    
    # Look for scholarship-related columns
    schol_cols <- names(resident_rows)[grepl("schol|research|project|safety|qip|scholar", names(resident_rows), ignore.case = TRUE)]
    
    debug_info <- paste(
      "Scholarship-related columns found:", length(schol_cols),
      "\nColumns:", paste(schol_cols, collapse = ", "),
      "\n\nSample values:",
      if (length(schol_cols) > 0) {
        paste(capture.output({
          for (col in schol_cols[1:min(3, length(schol_cols))]) {
            cat("\n", col, ":", head(resident_rows[[col]][!is.na(resident_rows[[col]])], 2))
          }
        }), collapse = "\n")
      } else {
        "No scholarship columns found"
      }
    )
    
    return(debug_info)
  })
  
  # ============================================================================
  # DEBUG OUTPUT FOR MILESTONE DATA STRUCTURE
  # ============================================================================
  output$debug_milestone_structure <- renderText({
    req(values$selected_resident)
    
    data <- app_data()
    
    debug_info <- paste0(
      "=== MILESTONE DATA DEBUG ===\n",
      "Resident: ", values$selected_resident$name, "\n",
      "Period: ", values$current_period, "\n",
      "Record ID: ", values$selected_resident$record_id, "\n\n"
    )
    
    # Check for milestone data in app_data
    if (!is.null(data$s_miles)) {
      s_mile_count <- nrow(data$s_miles)
      s_mile_resident_count <- nrow(data$s_miles %>% filter(name == values$selected_resident$name))
      debug_info <- paste0(debug_info,
                           "Self-assessment milestones (s_miles):\n",
                           "  Total rows: ", s_mile_count, "\n",
                           "  Rows for this resident: ", s_mile_resident_count, "\n"
      )
      
      if (s_mile_resident_count > 0) {
        resident_periods <- unique(data$s_miles$period[data$s_miles$name == values$selected_resident$name])
        debug_info <- paste0(debug_info,
                             "  Available periods: ", paste(resident_periods, collapse = ", "), "\n"
        )
      }
    } else {
      debug_info <- paste0(debug_info, "s_miles: NULL\n")
    }
    
    if (!is.null(data$p_miles)) {
      p_mile_count <- nrow(data$p_miles)
      p_mile_resident_count <- nrow(data$p_miles %>% filter(name == values$selected_resident$name))
      debug_info <- paste0(debug_info,
                           "\nProgram milestones (p_miles):\n",
                           "  Total rows: ", p_mile_count, "\n",
                           "  Rows for this resident: ", p_mile_resident_count, "\n"
      )
      
      if (p_mile_resident_count > 0) {
        resident_periods <- unique(data$p_miles$period[data$p_miles$name == values$selected_resident$name])
        debug_info <- paste0(debug_info,
                             "  Available periods: ", paste(resident_periods, collapse = ", "), "\n"
        )
      }
    } else {
      debug_info <- paste0(debug_info, "p_miles: NULL\n")
    }
    
    # Check for milestone columns in resident_data
    resident_data <- data$resident_data %>%
      filter(name == values$selected_resident$name)
    
    if (nrow(resident_data) > 0) {
      milestone_cols <- names(resident_data)[grepl("(rep_|acgme_|mile)", names(resident_data), ignore.case = TRUE)]
      debug_info <- paste0(debug_info,
                           "\nMilestone columns in resident_data: ", length(milestone_cols), "\n",
                           "Columns: ", paste(milestone_cols[1:min(10, length(milestone_cols))], collapse = ", "),
                           if (length(milestone_cols) > 10) "..." else "", "\n"
      )
    }
    
    return(debug_info)
  })
}