# Section 2: Evaluations & Feedback Module
# Displays assessment visualizations, resident reflections, and coach entry fields
# PATTERN: Matches working mod_assessment_wrapper.R from self-assessment app

mod_evaluations_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Previous Period Section
    div(
      class = "mb-4",
      h5(icon("history"), " Previous Period Review", style = "color: #7f8c8d;"),
      wellPanel(
        style = "background-color: #f8f9fa;",
        div(
          class = "row",
          div(
            class = "col-md-6",
            h6("Coach Comments on Plus/Delta", style = "color: #3498db;"),
            uiOutput(ns("prev_pd_comments"))
          ),
          div(
            class = "col-md-6",
            h6("Coach Comments on Evaluations", style = "color: #3498db;"),
            uiOutput(ns("prev_eval_comments"))
          )
        )
      )
    ),

    # Current Period - Resident Reflections
    div(
      class = "mb-4",
      h5(icon("user"), " Current Period - Resident Self-Assessment", style = "color: #2c3e50;"),
      wellPanel(
        style = "background-color: #fff8e1;",
        div(
          class = "row",
          div(
            class = "col-md-6",
            h6("Reflection on Plus Feedback", style = "color: #f39c12;"),
            uiOutput(ns("current_plus"))
          ),
          div(
            class = "col-md-6",
            h6("Reflection on Delta Feedback", style = "color: #f39c12;"),
            uiOutput(ns("current_delta"))
          )
        )
      )
    ),

    hr(),

    # Assessment Visualizations Section - Collapsible
    div(
      class = "mb-4",

      # Clickable header to toggle visualizations
      tags$a(
        `data-toggle` = "collapse",
        href = paste0("#", ns("viz_collapse")),
        class = "text-decoration-none",
        div(
          style = "background-color: #e8f5e9; padding: 12px; border-radius: 4px; border-left: 4px solid #4caf50; cursor: pointer; margin-bottom: 10px;",
          h5(
            style = "margin: 0; color: #2c3e50;",
            icon("chart-bar"), " Assessment Data & Visualizations ",
            tags$small(
              style = "float: right; color: #666;",
              icon("chevron-down"), " Click to expand/collapse"
            )
          )
        )
      ),

      # Collapsible content (starts collapsed)
      div(
        id = ns("viz_collapse"),
        class = "collapse",
        div(
          style = "border: 1px solid #e0e0e0; border-radius: 4px; padding: 15px; background-color: #fafafa;",

          # Assessment progress charts
          gmed::assessment_viz_ui(ns("charts"), title = "Assessment Progress"),

          hr(),

          # Custom detail viz from gmed
          gmed::mod_assessment_detail_custom_ui(ns("custom_detail")),

          hr(),

          # Custom data display for selected evaluation
          gmed::mod_assessment_data_display_ui(ns("data_display")),

          hr(),

          # CC Completion Status
          gmed::mod_cc_completion_ui(ns("cc_completion")),

          hr(),

          # Conference attendance/questions
          gmed::mod_questions_viz_ui(ns("questions"), title = "Conference Attendance by Rotation"),

          hr(),

          # Plus/Delta feedback table - collapsible (starts open for visibility)
          div(
            class = "mb-3",
            h5(
              style = "color: #9b59b6; margin-bottom: 15px;",
              icon("comments"), " Plus / Delta Feedback Details"
            ),
            p(
              class = "text-muted",
              style = "font-size: 14px; margin-bottom: 10px;",
              "Click on any evaluation below to view the detailed feedback comments."
            ),
            bslib::accordion(
              id = ns("plus_delta_accordion"),
              open = TRUE,  # Start open so users can see the table
              bslib::accordion_panel(
                "Faculty Feedback Table (Click to expand/collapse)",
                gmed::mod_plus_delta_table_ui(ns("plus_delta"), title = NULL)
              )
            )
          )
        )
      )
    ),

    hr(),

    # Coach Entry for Current Period
    div(
      class = "mb-3",
      h5(icon("edit"), " Your Coach Review - Current Period", style = "color: #27ae60;"),

      wellPanel(
        style = "background-color: #ffffff;",

        # Coach P/D Comments
        div(
          class = "mb-3",
          tags$label(
            "Comments on Resident's Plus/Delta Reflections:",
            style = "font-weight: bold;"
          ),
          tags$p(
            "Review the resident's self-assessment and provide feedback on their insights.",
            style = "font-size: 12px; color: #7f8c8d; margin-bottom: 5px;"
          ),
          textAreaInput(
            ns("coach_p_d_comments"),
            label = NULL,
            value = "",
            width = "100%",
            height = "120px",
            placeholder = "Enter your feedback on the resident's plus/delta reflections..."
          )
        ),

        # Coach Evaluations Comments
        div(
          class = "mb-3",
          tags$label(
            "Comments on Assessment Completion & Quality:",
            style = "font-weight: bold;"
          ),
          tags$p(
            "Comment on assessment completion status, faculty feedback quality, and any concerns.",
            style = "font-size: 12px; color: #7f8c8d; margin-bottom: 5px;"
          ),
          textAreaInput(
            ns("coach_evaluations"),
            label = NULL,
            value = "",
            width = "100%",
            height = "120px",
            placeholder = "Enter your comments on evaluation status and quality..."
          )
        )
      )
    )
  )
}

mod_evaluations_server <- function(id, resident_data, current_period, app_data, app_data_rv) {
  moduleServer(id, function(input, output, session) {

    # Create reactive for data_dict from reactiveValues - will update when data loads
    data_dict <- reactive({
      dd <- app_data_rv$data_dict
      message("DEBUG [mod_evaluations]: data_dict reactive, is.null = ", is.null(dd),
              ", nrow = ", if(!is.null(dd)) nrow(dd) else "NULL")
      dd
    })

    # ===== PREVIOUS PERIOD DISPLAY =====

    output$prev_pd_comments <- renderUI({
      req(resident_data())

      prev_data <- resident_data()$previous_period$coach_rev

      if (is.null(prev_data) || nrow(prev_data) == 0) {
        return(
          p(style = "font-style: italic; color: #95a5a6;",
            "No previous period data available")
        )
      }

      pd_comments <- prev_data$coach_p_d_comments[1]

      if (is.na(pd_comments) || trimws(pd_comments) == "") {
        return(
          p(style = "font-style: italic; color: #95a5a6;",
            "No previous comments on plus/delta")
        )
      }

      div(
        style = "padding: 10px; background-color: white; border-radius: 4px;",
        HTML(gsub("\n", "<br>", pd_comments))
      )
    })

    output$prev_eval_comments <- renderUI({
      req(resident_data())

      prev_data <- resident_data()$previous_period$coach_rev

      if (is.null(prev_data) || nrow(prev_data) == 0) {
        return(
          p(style = "font-style: italic; color: #95a5a6;",
            "No previous period data available")
        )
      }

      eval_comments <- prev_data$coach_evaluations[1]

      if (is.na(eval_comments) || trimws(eval_comments) == "") {
        return(
          p(style = "font-style: italic; color: #95a5a6;",
            "No previous comments on evaluations")
        )
      }

      div(
        style = "padding: 10px; background-color: white; border-radius: 4px;",
        HTML(gsub("\n", "<br>", eval_comments))
      )
    })

    # ===== PREPARE DATA FOR GMED MODULES =====

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
        assessment_data <- app_data()$all_forms$assessment

        # Debug: Check assessment data structure for plus/delta display
        req(record_id())
        resident_assessments <- assessment_data %>% dplyr::filter(record_id == !!record_id())

        message(sprintf("DEBUG [mod_evaluations]: Assessment data for plus/delta table (resident %s):", record_id()))
        message(sprintf("  Total assessment records: %d", nrow(resident_assessments)))
        if (nrow(resident_assessments) > 0) {
          # Check for required fields
          required_fields <- c("ass_date", "ass_level", "ass_plus", "ass_delta", "ass_faculty")
          missing_fields <- setdiff(required_fields, names(resident_assessments))
          if (length(missing_fields) > 0) {
            message(sprintf("  WARNING: Missing required fields: %s", paste(missing_fields, collapse = ", ")))
          } else {
            message("  All required fields present")
          }

          # Count non-empty plus/delta records
          has_feedback <- resident_assessments %>%
            dplyr::filter(
              !(is.na(ass_plus) | ass_plus == "") |
              !(is.na(ass_delta) | ass_delta == "")
            )
          message(sprintf("  Records with plus/delta feedback: %d", nrow(has_feedback)))
        }

        return(assessment_data)
      } else {
        message("WARNING: No assessment form found in app_data")
        return(data.frame())
      }
    })

    # Prepare combined assessment + questions data
    combined_data <- reactive({
      req(app_data())

      # Add source_form while preserving redcap_repeat_instrument
      # CRITICAL: assessment_viz_server expects source_form = "faculty_evaluation" for counts to display
      # Also map assessment field names to faculty_evaluation field names for gmed compatibility
      assessment_data <- app_data()$all_forms$assessment %>%
        dplyr::mutate(
          source_form = "faculty_evaluation",
          # Map assessment fields to faculty_evaluation fields expected by gmed
          fac_eval_level = if("ass_level" %in% names(.)) ass_level else NA_character_,
          fac_eval_date = if("ass_date" %in% names(.)) ass_date else NA_character_,
          # Keep original field names too for compatibility
        )

      questions_data <- app_data()$all_forms$questions %>%
        dplyr::mutate(source_form = "questions")

      combined <- dplyr::bind_rows(assessment_data, questions_data)

      # NOTE: Don't filter out empty ass_level - some assessments/questions legitimately don't have levels
      # The gmed modules should handle these appropriately

      # DEBUG: Check data for current resident
      req(record_id())
      resident_data <- combined %>% dplyr::filter(record_id == !!record_id())

      message(sprintf("DEBUG [mod_evaluations]: Combined data for resident %s:", record_id()))
      message(sprintf("  Total rows: %d", nrow(resident_data)))
      message(sprintf("  Faculty evaluation rows: %d", sum(resident_data$source_form == "faculty_evaluation", na.rm = TRUE)))
      message(sprintf("  Questions rows: %d", sum(resident_data$source_form == "questions", na.rm = TRUE)))

      if (nrow(resident_data) > 0) {
        message(sprintf("  Has fac_eval_level: %s", "fac_eval_level" %in% names(resident_data)))
        message(sprintf("  Has ass_level: %s", "ass_level" %in% names(resident_data)))
        if ("fac_eval_level" %in% names(resident_data)) {
          message(sprintf("  fac_eval_level values: %s", paste(unique(resident_data$fac_eval_level), collapse = ", ")))
          message(sprintf("  Empty fac_eval_level count: %d", sum(is.na(resident_data$fac_eval_level) | resident_data$fac_eval_level == "")))
        }
      }

      return(combined)
    })

    # ===== CALL GMED MODULES =====

    # Assessment charts - this one expects data as reactive and calls it internally
    gmed::assessment_viz_server(
      "charts",
      data = combined_data,
      record_id = record_id,
      resident_name = resident_name
    )

    # CC Completion Status - doesn't use data_dict
    gmed::mod_cc_completion_server(
      "cc_completion",
      rdm_data = combined_data,
      record_id = record_id,
      resident_data = resident_info_data
    )

    # Plus/Delta table - doesn't use data_dict
    gmed::mod_plus_delta_table_server(
      "plus_delta",
      rdm_data = raw_assessment_data,
      record_id = record_id
    )

    # Wrap gmed modules that need data_dict as data frame (not reactive) in observe()
    # These modules use filter() directly on data_dict, so they expect a data frame
    observe({
      req(data_dict())  # Ensure data_dict has loaded

      dd <- data_dict()  # Extract the actual data frame once

      message("DEBUG [mod_evaluations observe]: Extracted data_dict for gmed modules, nrow = ", nrow(dd))

      # Custom detail viz from gmed - pass extracted data frame
      detail_viz_state <- gmed::mod_assessment_detail_custom_server(
        "custom_detail",
        rdm_data = combined_data,
        record_id = record_id,
        data_dict = dd  # Pass data frame, not reactive
      )

      # Custom data display for selected evaluation - pass extracted data frame
      gmed::mod_assessment_data_display_server(
        "data_display",
        selected_category = detail_viz_state$selected_category,
        category_data = detail_viz_state$category_data,
        data_dict = dd  # Pass data frame, not reactive
      )

      # Questions/conference attendance - pass extracted data frame
      gmed::mod_questions_viz_server(
        "questions",
        rdm_data = combined_data,
        record_id = record_id,
        data_dict = dd  # Pass data frame, not reactive
      )
    })

    # ===== CURRENT PERIOD RESIDENT REFLECTIONS =====

    output$current_plus <- renderUI({
      req(resident_data())

      curr_data <- resident_data()$current_period$s_eval

      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(
          p(style = "font-style: italic; color: #95a5a6;",
            "No self-evaluation data for current period")
        )
      }

      plus_text <- curr_data$s_e_plus[1]

      if (is.na(plus_text) || trimws(plus_text) == "") {
        return(
          p(style = "font-style: italic; color: #95a5a6;",
            "Resident has not yet reflected on plus feedback")
        )
      }

      div(
        style = "padding: 10px; background-color: white; border-radius: 4px;",
        HTML(gsub("\n", "<br>", plus_text))
      )
    })

    output$current_delta <- renderUI({
      req(resident_data())

      curr_data <- resident_data()$current_period$s_eval

      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(
          p(style = "font-style: italic; color: #95a5a6;",
            "No self-evaluation data for current period")
        )
      }

      delta_text <- curr_data$s_e_delta[1]

      if (is.na(delta_text) || trimws(delta_text) == "") {
        return(
          p(style = "font-style: italic; color: #95a5a6;",
            "Resident has not yet reflected on delta feedback")
        )
      }

      div(
        style = "padding: 10px; background-color: white; border-radius: 4px;",
        HTML(gsub("\n", "<br>", delta_text))
      )
    })

    # ===== PRE-POPULATE EXISTING COACH DATA =====

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
            nchar(trimws(input$coach_p_d_comments)) > 0 &&
            !is.null(input$coach_evaluations) &&
            nchar(trimws(input$coach_evaluations)) > 0
          )
        )
      })
    )
  })
}