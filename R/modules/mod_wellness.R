# Section 1: Wellness & Progress Module
# Displays previous period wellness data and allows coach to enter new comments

mod_wellness_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Previous Period Data Section
    h4("Previous Period Responses", style = "color: #34495e; margin-top: 10px;"),
    
    wellPanel(
      style = "background-color: #f8f9fa; border-left: 4px solid #3498db;",
      
      h5("Resident Self-Assessment - Wellness", style = "color: #2980b9;"),
      uiOutput(ns("previous_wellness")),
      
      hr(),
      
      h5("Resident Self-Assessment - Progress and Assistance", style = "color: #2980b9;"),
      uiOutput(ns("previous_progress"))
    ),
    
    hr(),
    
    # Current Period Coach Entry
    h4("Coach Review - Current Period", style = "color: #34495e; margin-top: 20px;"),
    
    wellPanel(
      style = "background-color: #ffffff; border-left: 4px solid #27ae60;",
      
      div(
        style = "margin-bottom: 15px;",
        tags$label(
          "Coach Comments on Wellness:",
          style = "font-weight: bold; color: #2c3e50;"
        ),
        tags$p(
          "Provide comments on the resident's wellness, work-life balance, and any support needed.",
          style = "font-size: 12px; color: #7f8c8d; margin-top: 5px;"
        )
      ),
      
      textAreaInput(
        ns("coach_wellness"),
        label = NULL,
        value = "",
        width = "100%",
        height = "200px",
        placeholder = "Enter your wellness assessment and recommendations here..."
      ),
      
      # Character count
      div(
        style = "text-align: right; font-size: 12px; color: #95a5a6;",
        textOutput(ns("char_count"))
      )
    )
  )
}

mod_wellness_server <- function(id, resident_data, current_period, app_data) {
  moduleServer(id, function(input, output, session) {
    
    # DIAGNOSTIC: Check what data we're receiving
    observe({
      req(resident_data())
      
      data <- resident_data()
      
      cat("\n=== WELLNESS MODULE DIAGNOSTIC ===\n")
      cat("Structure of resident_data:\n")
      cat("  Names:", paste(names(data), collapse=", "), "\n")
      
      if (!is.null(data$current_period)) {
        cat("  current_period names:", paste(names(data$current_period), collapse=", "), "\n")
        
        if (!is.null(data$current_period$s_eval)) {
          cat("  s_eval rows:", nrow(data$current_period$s_eval), "\n")
          
          if (nrow(data$current_period$s_eval) > 0) {
            cat("  s_e_well value:", data$current_period$s_eval$s_e_well[1], "\n")
            cat("  s_e_prog_assist value:", data$current_period$s_eval$s_e_prog_assist[1], "\n")
          }
        }
      }
      cat("=================================\n\n")
    })

    # EXTENDED DIAGNOSTIC - Check what period was used
    observe({
      req(resident_data())
      req(current_period())
      req(app_data())
      
      data <- resident_data()
      period_val <- current_period()
      
      cat("\n=== PERIOD MATCHING DIAGNOSTIC ===\n")
      cat("Period passed to filter:", period_val, "\n")
      cat("Period class:", class(period_val), "\n")
      
      # Convert to period name if it's a number
      if (is.numeric(period_val)) {
        period_name <- get_period_name(period_val)
        cat("Period name:", period_name, "\n")
      } else {
        period_name <- period_val
      }
      
      # Check what's actually in s_eval for this resident
      resident_id <- data$resident_info$record_id[1]
      cat("Resident ID:", resident_id, "\n")
      
      # Look at raw s_eval data for this resident (before filtering)
      if (!is.null(app_data()$all_forms$s_eval)) {
        raw_s_eval <- app_data()$all_forms$s_eval %>%
          filter(record_id == resident_id)
        
        cat("Total s_eval records for this resident:", nrow(raw_s_eval), "\n")
        
        if (nrow(raw_s_eval) > 0) {
          cat("Available periods in s_eval:\n")
          print(unique(raw_s_eval$s_e_period))
          cat("Period we're filtering for:", period_name, "\n")
          cat("Does it match?", period_name %in% raw_s_eval$s_e_period, "\n")
        }
      }
      cat("=================================\n\n")
    })
    
    # Display current period wellness data
    output$previous_wellness <- renderUI({
      req(resident_data())
      
      curr_data <- resident_data()$current_period$s_eval
      
      # DIAGNOSTIC - what are we trying to render?
      cat("\n=== RENDERING WELLNESS ===\n")
      cat("curr_data is null?", is.null(curr_data), "\n")
      if (!is.null(curr_data)) {
        cat("curr_data rows:", nrow(curr_data), "\n")
        if (nrow(curr_data) > 0) {
          wellness_text <- curr_data$s_e_well[1]
          cat("wellness_text:", wellness_text, "\n")
          cat("is.na(wellness_text)?", is.na(wellness_text), "\n")
          cat("wellness_text == ''?", wellness_text == "", "\n")
        }
      }
      cat("==========================\n")
      
      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No wellness data available for current period"
          )
        )
      }
      
      wellness_text <- curr_data$s_e_well[1]
      
      if (is.na(wellness_text) || wellness_text == "") {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "Resident did not provide wellness information"
          )
        )
      }
      
      div(
        style = "padding: 10px; background-color: white; border-radius: 4px;",
        HTML(gsub("\n", "<br>", wellness_text))
      )
    })
    
    # Display current period progress/assistance data
    output$previous_progress <- renderUI({
      req(resident_data())
      
      curr_data <- resident_data()$current_period$s_eval
      
      # DIAGNOSTIC
      cat("\n=== RENDERING PROGRESS ===\n")
      cat("curr_data is null?", is.null(curr_data), "\n")
      if (!is.null(curr_data)) {
        cat("curr_data rows:", nrow(curr_data), "\n")
        if (nrow(curr_data) > 0) {
          progress_text <- curr_data$s_e_prog_assist[1]
          cat("progress_text:", progress_text, "\n")
          cat("is.na(progress_text)?", is.na(progress_text), "\n")
          cat("trimws == ''?", trimws(progress_text) == "", "\n")
        }
      }
      cat("==========================\n")
      
      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No progress data available for current period"
          )
        )
      }
      
      progress_text <- curr_data$s_e_prog_assist[1]
      
      if (is.na(progress_text) || trimws(progress_text) == "") {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "Resident did not provide progress information"
          )
        )
      }
      
      div(
        style = "padding: 10px; background-color: white; border-radius: 4px;",
        HTML(gsub("\n", "<br>", progress_text))
      )
    })
    
    # Character count for coach entry
    output$char_count <- renderText({
      char_count <- nchar(input$coach_wellness)
      sprintf("%d characters", char_count)
    })
    
    # Pre-populate if data already exists for current period
    observe({
      req(resident_data())
      
      curr_data <- resident_data()$current_period$coach_rev
      
      if (!is.null(curr_data) && nrow(curr_data) > 0) {
        existing_wellness <- curr_data$coach_wellness[1]
        
        if (!is.na(existing_wellness) && existing_wellness != "") {
          updateTextAreaInput(
            session,
            "coach_wellness",
            value = existing_wellness
          )
        }
      }
    })
    
    # Return reactive with entered data
    return(
      reactive({
        list(
          coach_wellness = input$coach_wellness,
          is_complete = !is.null(input$coach_wellness) && nchar(trimws(input$coach_wellness)) > 0
        )
      })
    )
  })  # <-- moduleServer ENDS HERE
}