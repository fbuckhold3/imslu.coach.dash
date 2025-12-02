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

mod_evaluations_server <- function(id, resident_data, current_period, app_data, data_dict) {
  moduleServer(id, function(input, output, session) {

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