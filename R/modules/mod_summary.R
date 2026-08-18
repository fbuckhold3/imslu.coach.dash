# Section 8: Summary & Submission Module
# Displays completion checklist and submission buttons
#
# P7 (Entering Residency) adds one more thing: an overall "Summary" coach
# textarea (coach_summary) as the LAST step in the whole P7 flow — the
# earlier per-topic notes (background, coping, skills/topics/styles) live in
# mod_intern_intro.R; this is the closing, whole-review summary.
#
# NOTE: this module previously checked `current_period() == 7L` for P6/P7
# detection — WRONG. This app's local period numbering (PERIOD_NAMES /
# get_period_number() in globals.R) is 0-based, with Entering Residency = 0
# (gmed's/REDCap's own scheme uses 7 — see mod_review_interface.R's
# .coach_sections_for_period() for the full explanation). Fixed to check 0L.

mod_summary_ui <- function(id, period_num = NULL) {
  ns <- NS(id)
  is_p7 <- identical(suppressWarnings(as.integer(period_num)), 0L)

  tagList(
    # P7: Overall Summary comes FIRST — the closing note right after
    # Milestones, before the completion checklist/preview clutter below.
    if (is_p7) tagList(
      h4("Overall Summary", style = "color: #34495e; margin-top: 10px;"),
      wellPanel(
        style = "background-color: #ffffff; border-left: 4px solid #3498db;",
        tags$label("Summary:", style = "font-weight: bold; color: #2c3e50;"),
        tags$p("Overall summary of this entering-residency review.",
               style = "font-size: 12px; color: #7f8c8d; margin-top: 5px;"),
        textAreaInput(
          ns("coach_summary"), label = NULL, value = "", width = "100%", height = "150px",
          placeholder = "Summarize this entering-residency review..."
        ),
        div(
          style = "text-align: right; font-size: 12px; color: #95a5a6;",
          textOutput(ns("char_count_summary"))
        )
      ),
      hr()
    ),

    h4("Review Completion Checklist", style = "color: #34495e; margin-top: 10px;"),
    p(style = "color: #7f8c8d;", "Verify all sections are complete before submitting the review."),

    wellPanel(
      style = "background-color: #f8f9fa; border-left: 4px solid #3498db;",
      uiOutput(ns("completion_checklist"))
    ),

    hr(),

    # Validation messages
    uiOutput(ns("validation_messages")),

    hr(),

    h4("Review Summary", style = "color: #34495e; margin-top: 20px;"),
    p(style = "color: #7f8c8d;", "Preview of your coaching review entries:"),

    wellPanel(
      style = "background-color: #ffffff; border-left: 4px solid #27ae60;",
      uiOutput(ns("review_summary"))
    )
  )
}

mod_summary_server <- function(id, resident_data, wellness_data, evaluations_data, learning_data,
                               scholarship_data, career_data, milestones_data, goals_data,
                               grad_plan_data = reactive(NULL), intro_data = reactive(NULL),
                               current_period = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    # P6 (graduation) drops Wellness/Learning/Career/Goals and adds grad_plan;
    # P7 (intern intro) drops Wellness/Evaluations/Scholarship/Learning
    # (Learning's topics/styles moved into mod_intern_intro.R for P7).
    is_p6 <- reactive(identical(as.integer(current_period() %||% NA_integer_), 6L))
    is_p7 <- reactive(identical(as.integer(current_period() %||% NA_integer_), 0L))

    # Calculate overall completion status
    completion_status <- reactive({
      base <- list(
        wellness    = if (!is.null(wellness_data()))    wellness_data()$is_complete    else FALSE,
        evaluations = if (!is.null(evaluations_data())) evaluations_data()$is_complete else FALSE,
        learning    = if (!is.null(learning_data()))    learning_data()$is_complete    else FALSE,
        scholarship = if (!is.null(scholarship_data())) scholarship_data()$is_complete else FALSE,
        career      = if (!is.null(career_data()))      career_data()$is_complete      else FALSE,
        milestones  = if (!is.null(milestones_data()))  milestones_data()$is_complete  else FALSE,
        goals       = if (!is.null(goals_data()))       goals_data()$is_complete       else FALSE
      )
      if (is_p6()) {
        keep <- setdiff(names(base), c("wellness", "learning", "career", "goals"))
        base <- base[keep]
        base$grad_plan <- if (!is.null(grad_plan_data())) grad_plan_data()$is_complete else FALSE
        base
      } else if (is_p7()) {
        base <- base[setdiff(names(base), c("wellness", "evaluations", "scholarship", "learning"))]
        base$intro <- if (!is.null(intro_data())) intro_data()$is_complete else FALSE
        base$coach_summary <- !is.null(input$coach_summary) &&
                               nchar(trimws(input$coach_summary %||% "")) > 0
        base
      } else base
    })

    # Display completion checklist
    output$completion_checklist <- renderUI({
      status <- completion_status()

      create_checkbox_item <- function(label, is_complete) {
        icon_html <- if (is_complete) {
          tags$span(
            style = "color: #27ae60; font-size: 18px; margin-right: 10px;",
            icon("check-circle")
          )
        } else {
          tags$span(
            style = "color: #e74c3c; font-size: 18px; margin-right: 10px;",
            icon("times-circle")
          )
        }

        div(
          style = "margin-bottom: 10px; padding: 8px; background-color: white; border-radius: 4px;",
          icon_html,
          tags$span(
            label,
            style = if (is_complete) "color: #2c3e50;" else "color: #e74c3c; font-weight: bold;"
          )
        )
      }

      labels <- c(
        wellness      = "Wellness & Progress",
        evaluations   = "Evaluations & Feedback",
        learning      = "Learning & Board Preparation",
        scholarship   = "Scholarship",
        career        = "Career Planning",
        milestones    = "Milestones",
        goals         = "Goals & ILP (including ILP Final Summary)",
        grad_plan     = "Graduation Plan & Alumni",
        intro         = "Background, Coping, Skills/Topics/Learning Styles",
        coach_summary = "Overall Summary"
      )

      items <- lapply(names(status), function(nm) {
        create_checkbox_item(labels[[nm]], status[[nm]])
      })

      tagList(
        items,

        hr(),

        div(
          style = "text-align: center; margin-top: 15px;",
          if (all(unlist(status))) {
            tags$p(
              style = "color: #27ae60; font-size: 16px; font-weight: bold;",
              icon("check-circle"),
              " All sections complete! Ready to submit."
            )
          } else {
            tags$p(
              style = "color: #e67e22; font-size: 16px; font-weight: bold;",
              icon("exclamation-triangle"),
              " Please complete all required sections before submitting."
            )
          }
        )
      )
    })

    # Display validation messages
    output$validation_messages <- renderUI({
      status <- completion_status()

      incomplete_sections <- names(status)[!unlist(status)]

      if (length(incomplete_sections) == 0) {
        return(
          div(
            class = "alert alert-success",
            style = "margin-top: 20px;",
            icon("check-circle"),
            " All required sections are complete. You may now submit the review."
          )
        )
      }

      section_labels <- c(
        wellness      = "Wellness & Progress",
        evaluations   = "Evaluations & Feedback",
        learning      = "Learning & Board Preparation",
        scholarship   = "Scholarship",
        career        = "Career Planning",
        milestones    = "Milestones",
        goals         = "Goals & ILP",
        grad_plan     = "Graduation Plan & Alumni",
        intro         = "Background, Coping, Skills/Topics/Learning Styles",
        coach_summary = "Overall Summary"
      )

      incomplete_labels <- sapply(incomplete_sections, function(x) section_labels[x])

      div(
        class = "alert alert-warning",
        style = "margin-top: 20px;",
        icon("exclamation-triangle"),
        " Please complete the following sections:",
        tags$ul(
          lapply(incomplete_labels, function(label) {
            tags$li(label)
          })
        )
      )
    })

    # Display review summary
    output$review_summary <- renderUI({

      # Helper to truncate text
      truncate_text <- function(text, max_chars = 200) {
        if (is.null(text) || is.na(text) || text == "") return("")
        if (nchar(text) <= max_chars) return(text)
        paste0(substr(text, 1, max_chars), "...")
      }

      create_summary_section <- function(title, content) {
        if (is.null(content) || is.na(content) || content == "") {
          content_html <- tags$em(
            style = "color: #95a5a6;",
            "No comments entered"
          )
        } else {
          content_html <- p(
            style = "color: #34495e;",
            truncate_text(content)
          )
        }

        div(
          style = "margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 4px; border-left: 3px solid #3498db;",
          h5(title, style = "color: #2980b9; margin-top: 0;"),
          content_html
        )
      }

      tagList(
        if (!is.null(wellness_data()) && !is_p7()) {
          create_summary_section(
            "Wellness & Progress",
            wellness_data()$coach_wellness
          )
        },

        if (!is.null(evaluations_data()) && !is_p7()) {
          tagList(
            create_summary_section(
              "Evaluations",
              evaluations_data()$coach_evaluations
            ),
            create_summary_section(
              "Plus/Delta Comments",
              evaluations_data()$coach_p_d_comments
            )
          )
        },

        if (!is.null(learning_data()) && !is_p7()) {
          create_summary_section(
            "Learning & Board Preparation",
            learning_data()$coach_step_board
          )
        },

        if (!is.null(scholarship_data()) && !is_p7()) {
          create_summary_section(
            "Scholarship",
            scholarship_data()$coach_scholarship
          )
        },

        if (!is.null(career_data()) && !is_p6() && !is_p7()) {
          create_summary_section(
            "Career Planning",
            career_data()$coach_career
          )
        },

        if (is_p7() && !is.null(career_data())) {
          create_summary_section(
            "Career Planning",
            career_data()$coach_career
          )
        },

        if (is_p7() && !is.null(intro_data())) {
          tagList(
            create_summary_section("Background Information", intro_data()$coach_intro_back),
            create_summary_section("Coping and Adjustment", intro_data()$coach_coping),
            create_summary_section("Skills, Topics & Learning Styles", intro_data()$coach_ls_and_topic)
          )
        },

        if (!is.null(milestones_data())) {
          div(
            style = "margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 4px; border-left: 3px solid #e74c3c;",
            h5("Milestones", style = "color: #c0392b; margin-top: 0;"),
            p(
              style = "color: #34495e;",
              if (is_p7()) {
                "Self-assessment reviewed (no coach ratings for Entering Residency)"
              } else if (milestones_data()$is_complete) {
                "Milestone ratings have been entered"
              } else {
                tags$em(style = "color: #e74c3c;", "Milestone ratings incomplete")
              }
            )
          )
        },

        if (!is.null(goals_data()) && !is_p6() && !is_p7()) {
          tagList(
            create_summary_section(
              "Goals Assessment",
              goals_data()$coach_mile_goal
            ),
            create_summary_section(
              "ILP Final Summary",
              goals_data()$coach_ilp_final
            )
          )
        },

        if (!is.null(goals_data()) && is_p7()) {
          create_summary_section(
            "Goals Assessment",
            goals_data()$coach_mile_goal
          )
        },

        if (is_p6() && !is.null(grad_plan_data())) {
          create_summary_section(
            "Graduation Summary & Transition Notes",
            grad_plan_data()$coach_ilp_final
          )
        }
      )
    })

    # ----- Overall Summary (P7 only): character count -----
    output$char_count_summary <- renderText({
      sprintf("%d characters", nchar(input$coach_summary %||% ""))
    })

    # ----- Load existing coach_summary when resident changes (P7 only) -----
    current_resident_id <- reactiveVal(NULL)

    observe({
      req(resident_data())
      new_resident_id <- resident_data()$resident_info$record_id[1]

      if (is.null(current_resident_id()) || current_resident_id() != new_resident_id) {
        current_resident_id(new_resident_id)

        if (isTRUE(is_p7())) {
          curr_data <- resident_data()$current_period$coach_rev
          existing <- if (!is.null(curr_data) && nrow(curr_data) > 0 && "coach_summary" %in% names(curr_data)) {
            curr_data$coach_summary[1]
          } else NA
          updateTextAreaInput(
            session, "coach_summary",
            value = if (!is.na(existing) && nzchar(existing)) existing else ""
          )
        }
      }
    })

    # Return completion status + entered data for parent module
    return(
      reactive({
        status <- completion_status()
        list(
          all_complete = all(unlist(status)),
          completion_status = status,
          coach_summary = input$coach_summary
        )
      })
    )
  })
}
