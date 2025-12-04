# Section 8: Summary & Submission Module
# Displays completion checklist and submission buttons

mod_summary_ui <- function(id) {
  ns <- NS(id)

  tagList(
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

mod_summary_server <- function(id, wellness_data, evaluations_data, learning_data,
                               scholarship_data, career_data, milestones_data, goals_data) {
  moduleServer(id, function(input, output, session) {

    # Calculate overall completion status
    completion_status <- reactive({
      list(
        wellness = if (!is.null(wellness_data())) wellness_data()$is_complete else FALSE,
        evaluations = if (!is.null(evaluations_data())) evaluations_data()$is_complete else FALSE,
        learning = if (!is.null(learning_data())) learning_data()$is_complete else FALSE,
        scholarship = if (!is.null(scholarship_data())) scholarship_data()$is_complete else FALSE,
        career = if (!is.null(career_data())) career_data()$is_complete else FALSE,
        milestones = if (!is.null(milestones_data())) milestones_data()$is_complete else FALSE,
        goals = if (!is.null(goals_data())) goals_data()$is_complete else FALSE
      )
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

      tagList(
        create_checkbox_item("Section 1: Wellness & Progress", status$wellness),
        create_checkbox_item("Section 2: Evaluations & Feedback", status$evaluations),
        create_checkbox_item("Section 3: Learning & Board Preparation", status$learning),
        create_checkbox_item("Section 4: Scholarship", status$scholarship),
        create_checkbox_item("Section 5: Career Planning", status$career),
        create_checkbox_item("Section 6: Milestones", status$milestones),
        create_checkbox_item("Section 7: Goals & ILP (including ILP Final Summary)", status$goals),

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
        wellness = "Wellness & Progress",
        evaluations = "Evaluations & Feedback",
        learning = "Learning & Board Preparation",
        scholarship = "Scholarship",
        career = "Career Planning",
        milestones = "Milestones",
        goals = "Goals & ILP"
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
        if (!is.null(wellness_data())) {
          create_summary_section(
            "Wellness & Progress",
            wellness_data()$coach_wellness
          )
        },

        if (!is.null(evaluations_data())) {
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

        if (!is.null(learning_data())) {
          create_summary_section(
            "Learning & Board Preparation",
            learning_data()$coach_step_board
          )
        },

        if (!is.null(scholarship_data())) {
          create_summary_section(
            "Scholarship",
            scholarship_data()$coach_scholarship
          )
        },

        if (!is.null(career_data())) {
          create_summary_section(
            "Career Planning",
            career_data()$coach_career
          )
        },

        if (!is.null(milestones_data())) {
          div(
            style = "margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 4px; border-left: 3px solid #e74c3c;",
            h5("Milestones", style = "color: #c0392b; margin-top: 0;"),
            p(
              style = "color: #34495e;",
              if (milestones_data()$is_complete) {
                "Milestone ratings have been entered"
              } else {
                tags$em(style = "color: #e74c3c;", "Milestone ratings incomplete")
              }
            )
          )
        },

        if (!is.null(goals_data())) {
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
        }
      )
    })

    # Return completion status for parent module
    return(
      reactive({
        status <- completion_status()
        list(
          all_complete = all(unlist(status)),
          completion_status = status
        )
      })
    )
  })
}
