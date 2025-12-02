# Section 2: Evaluations & Feedback Module
# Displays assessment visualizations, resident reflections, and coach entry fields
# PATTERN: Matches working mod_assessment_wrapper.R from self-assessment app

mod_evaluations_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Previous Period Coach Comments Section
    h4("Previous Period Coach Comments", style = "color: #34495e; margin-top: 10px;"),
    
    wellPanel(
      style = "background-color: #f8f9fa; border-left: 4px solid #3498db;",
      
      h5("Coach Comments on Plus/Delta Review", style = "color: #2980b9;"),
      uiOutput(ns("previous_pd_comments")),
      
      hr(),
      
      h5("Coach Comments on Evaluations", style = "color: #2980b9;"),
      uiOutput(ns("previous_eval_comments"))
    ),
    
    hr(),
    
    # Current Period Resident Reflections
    h4("Current Period - Resident Assessment Review", style = "color: #34495e; margin-top: 20px;"),
    
    wellPanel(
      style = "background-color: #fff8e1; border-left: 4px solid #ffc107;",
      
      h5("Resident's Reflection on Plus Feedback", style = "color: #f39c12;"),
      uiOutput(ns("current_plus_reflection")),
      
      hr(),
      
      h5("Resident's Reflection on Delta Feedback", style = "color: #f39c12;"),
      uiOutput(ns("current_delta_reflection"))
    ),
    
    hr(),

    # SIMPLIFIED FOR DEBUGGING - Assessment Visualizations
    h4("Assessment Data & Visualizations", style = "color: #34495e; margin-top: 20px;"),

    wellPanel(
      style = "background-color: #ffffff; border-left: 4px solid #9b59b6;",

      p("Assessment visualizations will appear here."),

      # TEMPORARILY COMMENTED OUT - Testing if gmed UI modules cause rendering issues
      # # Main assessment overview
      # gmed::assessment_viz_ui(ns("charts"), title = "Assessment Progress"),
      #
      # hr(),
      #
      # # Detailed assessment breakdown
      # gmed::mod_assessment_detail_custom_ui(ns("custom_detail")),
      #
      # hr(),
      #
      # # Core curriculum completion
      # gmed::mod_cc_completion_ui(ns("cc_completion")),
      #
      # hr(),
      #
      # # Plus/Delta feedback table (collapsible)
      # bslib::accordion(
      #   id = ns("plus_delta_accordion"),
      #   open = FALSE,
      #   bslib::accordion_panel(
      #     "Plus / Delta Feedback Details",
      #     gmed::mod_plus_delta_table_ui(ns("plus_delta"), title = NULL)
      #   )
      # )

      uiOutput(ns("assessment_debug"))
    ),
    
    hr(),
    
    # Current Period Coach Entry
    h4("Coach Review - Current Period", style = "color: #34495e; margin-top: 20px;"),
    
    wellPanel(
      style = "background-color: #ffffff; border-left: 4px solid #27ae60;",
      
      # Coach comments on Plus/Delta
      div(
        style = "margin-bottom: 20px;",
        tags$label(
          "Coach Comments on Plus/Delta Review:",
          style = "font-weight: bold; color: #2c3e50;"
        ),
        tags$p(
          "Provide feedback on the resident's self-assessment and reflection on faculty feedback.",
          style = "font-size: 12px; color: #7f8c8d; margin-top: 5px;"
        ),
        textAreaInput(
          ns("coach_p_d_comments"),
          label = NULL,
          value = "",
          width = "100%",
          height = "150px",
          placeholder = "Enter your comments on the resident's plus/delta reflections..."
        ),
        div(
          style = "text-align: right; font-size: 12px; color: #95a5a6;",
          textOutput(ns("pd_char_count"))
        )
      ),
      
      # Coach comments on Evaluations
      div(
        style = "margin-bottom: 15px;",
        tags$label(
          "Coach Comments on Evaluation Completion & Quality:",
          style = "font-weight: bold; color: #2c3e50;"
        ),
        tags$p(
          "Comment on assessment completion status, faculty feedback quality, and any concerns.",
          style = "font-size: 12px; color: #7f8c8d; margin-top: 5px;"
        ),
        textAreaInput(
          ns("coach_evaluations"),
          label = NULL,
          value = "",
          width = "100%",
          height = "150px",
          placeholder = "Enter your comments on evaluation status and quality..."
        ),
        div(
          style = "text-align: right; font-size: 12px; color: #95a5a6;",
          textOutput(ns("eval_char_count"))
        )
      )
    )
  )
}

mod_evaluations_server <- function(id, resident_data, current_period, app_data, data_dict) {
  moduleServer(id, function(input, output, session) {
    
    # ===== PREVIOUS PERIOD DISPLAY =====
    
    # Display previous period coach P/D comments
    output$previous_pd_comments <- renderUI({
      req(resident_data())
      
      prev_data <- resident_data()$previous_period$coach_rev
      
      if (is.null(prev_data) || nrow(prev_data) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No previous period data available"
          )
        )
      }
      
      pd_comments <- prev_data$coach_p_d_comments[1]
      
      if (is.na(pd_comments) || trimws(pd_comments) == "") {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No previous coach comments on plus/delta"
          )
        )
      }
      
      div(
        style = "padding: 10px; background-color: white; border-radius: 4px;",
        HTML(gsub("\n", "<br>", pd_comments))
      )
    })
    
    # Display previous period coach evaluation comments
    output$previous_eval_comments <- renderUI({
      req(resident_data())
      
      prev_data <- resident_data()$previous_period$coach_rev
      
      if (is.null(prev_data) || nrow(prev_data) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No previous period data available"
          )
        )
      }
      
      eval_comments <- prev_data$coach_evaluations[1]
      
      if (is.na(eval_comments) || trimws(eval_comments) == "") {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No previous coach comments on evaluations"
          )
        )
      }
      
      div(
        style = "padding: 10px; background-color: white; border-radius: 4px;",
        HTML(gsub("\n", "<br>", eval_comments))
      )
    })
    
    # ===== CURRENT PERIOD RESIDENT REFLECTIONS =====
    
    # Display current period plus reflection
    output$current_plus_reflection <- renderUI({
      req(resident_data())
      
      curr_data <- resident_data()$current_period$s_eval
      
      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No self-evaluation data for current period"
          )
        )
      }
      
      plus_text <- curr_data$s_e_plus[1]
      
      if (is.na(plus_text) || trimws(plus_text) == "") {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "Resident has not yet reflected on plus feedback"
          )
        )
      }
      
      div(
        style = "padding: 10px; background-color: white; border-radius: 4px;",
        HTML(gsub("\n", "<br>", plus_text))
      )
    })
    
    # Display current period delta reflection
    output$current_delta_reflection <- renderUI({
      req(resident_data())
      
      curr_data <- resident_data()$current_period$s_eval
      
      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No self-evaluation data for current period"
          )
        )
      }
      
      delta_text <- curr_data$s_e_delta[1]
      
      if (is.na(delta_text) || trimws(delta_text) == "") {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "Resident has not yet reflected on delta feedback"
          )
        )
      }
      
      div(
        style = "padding: 10px; background-color: white; border-radius: 4px;",
        HTML(gsub("\n", "<br>", delta_text))
      )
    })
    
    # ===== GMED VISUALIZATION MODULES =====
    # Following exact pattern from working mod_assessment_wrapper.R
    
    # Extract record_id as separate reactive
    record_id <- reactive({
      req(resident_data())
      resident_data()$resident_info$record_id
    })
    
    # Extract resident name as separate reactive
    resident_name <- reactive({
      req(resident_data())
      resident_data()$resident_info$full_name
    })
    
    # Get resident info for CC completion
    resident_info_data <- reactive({
      req(app_data(), record_id())
      
      app_data()$residents %>%
        dplyr::filter(record_id == !!record_id()) %>%
        dplyr::slice(1)
    })
    
    # Get raw assessment data for plus/delta table
    raw_assessment_data <- reactive({
      req(app_data())
      
      if ("assessment" %in% names(app_data()$all_forms)) {
        return(app_data()$all_forms$assessment)
      } else {
        return(data.frame())
      }
    })
    
    # Prepare combined assessment + questions data with source_form indicator
    combined_data <- reactive({
      req(app_data())

      # Add source_form while preserving redcap_repeat_instrument
      combined <- dplyr::bind_rows(
        app_data()$all_forms$assessment %>% dplyr::mutate(source_form = "assessment"),
        app_data()$all_forms$questions %>% dplyr::mutate(source_form = "questions")
      )

      return(combined)
    })

    # NOTE: data_dict is now passed as parameter from parent (matches working app pattern)
    # Extract the actual data_dict value safely within reactive context
    data_dict_value <- reactive({
      req(data_dict())
      data_dict()
    })

    # DEBUGGING OUTPUT - Show what data is available
    output$assessment_debug <- renderUI({
      req(resident_data())

      div(
        style = "padding: 15px; background-color: #f0f0f0; border-radius: 5px;",
        h5("Debug Info:"),
        p(strong("Resident ID:"), resident_data()$resident_info$record_id),
        p(strong("Period:"), current_period()),
        p(strong("Data Dict Rows:"), nrow(data_dict())),
        p(strong("Assessment Records:"), nrow(combined_data())),
        p("✓ Module loaded successfully - gmed visualizations temporarily disabled for debugging")
      )
    })

    # TEMPORARILY COMMENTED OUT - gmed module calls disabled for debugging
    # # Call gmed modules with exact same pattern as working app
    #
    # # Assessment charts
    # gmed::assessment_viz_server(
    #   "charts",
    #   data = combined_data,
    #   record_id = record_id,
    #   resident_name = resident_name
    # )
    #
    # # Custom detail viz - pass unwrapped data_dict using isolate
    # detail_viz_state <- gmed::mod_assessment_detail_custom_server(
    #   "custom_detail",
    #   rdm_data = combined_data,
    #   record_id = record_id,
    #   data_dict = isolate(data_dict_value())  # Use isolate to safely unwrap without creating dependency
    # )
    #
    # # CC Completion Status
    # gmed::mod_cc_completion_server(
    #   "cc_completion",
    #   rdm_data = combined_data,
    #   record_id = record_id,
    #   resident_data = resident_info_data
    # )
    #
    # # Plus/Delta table
    # gmed::mod_plus_delta_table_server(
    #   "plus_delta",
    #   rdm_data = raw_assessment_data,
    #   record_id = record_id
    # )
    
    # ===== CHARACTER COUNTS =====
    
    output$pd_char_count <- renderText({
      char_count <- nchar(input$coach_p_d_comments)
      sprintf("%d characters", char_count)
    })
    
    output$eval_char_count <- renderText({
      char_count <- nchar(input$coach_evaluations)
      sprintf("%d characters", char_count)
    })
    
    # ===== PRE-POPULATE EXISTING DATA =====
    
    observe({
      req(resident_data())
      
      curr_data <- resident_data()$current_period$coach_rev
      
      if (!is.null(curr_data) && nrow(curr_data) > 0) {
        # Pre-populate P/D comments if they exist
        existing_pd <- curr_data$coach_p_d_comments[1]
        if (!is.na(existing_pd) && existing_pd != "") {
          updateTextAreaInput(
            session,
            "coach_p_d_comments",
            value = existing_pd
          )
        }
        
        # Pre-populate evaluation comments if they exist
        existing_eval <- curr_data$coach_evaluations[1]
        if (!is.na(existing_eval) && existing_eval != "") {
          updateTextAreaInput(
            session,
            "coach_evaluations",
            value = existing_eval
          )
        }
      }
    })
    
    # ===== RETURN ENTERED DATA =====
    
    return(
      reactive({
        list(
          coach_p_d_comments = input$coach_p_d_comments,
          coach_evaluations = input$coach_evaluations,
          is_complete = (
            !is.null(input$coach_p_d_comments) && 
            nchar(trimws(input$coach_p_d_comments)) > 0
          ) || (
            !is.null(input$coach_evaluations) && 
            nchar(trimws(input$coach_evaluations)) > 0
          )
        )
      })
    )
  })
}