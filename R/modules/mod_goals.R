# Section 7: Goals & ILP Module
# Displays previous and current period ILP goals with achievement status and allows coach to enter comments

mod_goals_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Previous Coach ILP Summary
    h4("Previous Coach ILP Summary", style = "color: #34495e; margin-top: 10px;"),
    p(style = "color: #7f8c8d;", "Review the comprehensive ILP summary from the previous coaching period."),

    wellPanel(
      style = "background-color: #e8f4f8; border-left: 4px solid #3498db;",
      uiOutput(ns("previous_coach_ilp"))
    ),

    hr(),

    # Previous Period Goals & Achievement
    h4("Previous Period Goals & Achievement", style = "color: #34495e; margin-top: 10px;"),
    p(style = "color: #7f8c8d;", "Review the goals set in the previous period and the resident's assessment of their progress."),

    wellPanel(
      style = "background-color: #f8f9fa; border-left: 4px solid #27ae60;",
      uiOutput(ns("previous_goals"))
    ),

    hr(),

    # Current Period Goals Section
    h4("Current Period Goals", style = "color: #34495e; margin-top: 20px;"),
    p(style = "color: #7f8c8d;", "Review the goals set for the current period by the resident."),

    wellPanel(
      style = "background-color: #fff8e1; border-left: 4px solid #f39c12;",
      uiOutput(ns("current_goals"))
    ),

    hr(),

    # Coach Entry for Goals Assessment
    h4("Coach Assessment of Goals", style = "color: #34495e; margin-top: 20px;"),

    wellPanel(
      style = "background-color: #ffffff; border-left: 4px solid #27ae60;",

      div(
        style = "margin-bottom: 15px;",
        tags$label(
          "Coach Comments on Goals:",
          style = "font-weight: bold; color: #2c3e50;"
        ),
        tags$p(
          "Provide feedback on the resident's goal selection, progress on previous goals, and support needed for goal achievement.",
          style = "font-size: 12px; color: #7f8c8d; margin-top: 5px;"
        )
      ),

      textAreaInput(
        ns("coach_mile_goal"),
        label = NULL,
        value = "",
        width = "100%",
        height = "200px",
        placeholder = "Enter your assessment of the resident's goals and progress here..."
      ),

      # Character count
      div(
        style = "text-align: right; font-size: 12px; color: #95a5a6;",
        textOutput(ns("char_count"))
      )
    ),

    hr(),

    # ILP Final Summary
    h4("Comprehensive ILP Summary", style = "color: #34495e; margin-top: 20px;"),

    wellPanel(
      style = "background-color: #ffffff; border-left: 4px solid #3498db;",

      div(
        style = "margin-bottom: 15px;",
        tags$label(
          "Finalized ILP Summary:",
          style = "font-weight: bold; color: #2c3e50;"
        ),
        tags$p(
          "Provide a comprehensive ILP summary that considers: career plans, scholarly work, areas of improvement, exam scores and preparation, and current milestone goals.",
          style = "font-size: 12px; color: #7f8c8d; margin-top: 5px;"
        ),
        tags$ul(
          style = "font-size: 12px; color: #7f8c8d; margin-top: 5px;",
          tags$li("Career planning and development"),
          tags$li("Scholarly activities and research"),
          tags$li("Areas identified for improvement"),
          tags$li("Board exam preparation and scores"),
          tags$li("Current milestone goals and progress")
        )
      ),

      textAreaInput(
        ns("coach_ilp_final"),
        label = NULL,
        value = "",
        width = "100%",
        height = "350px",
        placeholder = "Enter your comprehensive ILP summary here, addressing career plans, scholarly work, areas of improvement, exam preparation, and milestone goals..."
      ),

      # Character count for ILP final
      div(
        style = "text-align: right; font-size: 12px; color: #95a5a6;",
        textOutput(ns("char_count_ilp"))
      )
    )
  )
}

mod_goals_server <- function(id, resident_data, current_period, app_data) {
  moduleServer(id, function(input, output, session) {

    # Milestone subcompetency labels mapping (for goal translation)
    # Using correct labels from gmed::milestone_short_labels()
    milestone_labels <- c(
      "1" = "PC1: History",
      "2" = "PC2: Physical Exam",
      "3" = "PC3: Clinical Reasoning",
      "4" = "PC4: Mgmt-Inpatient",
      "5" = "PC5: Mgmt-Outpatient",
      "6" = "PC6: Digital Health",
      "7" = "MK1: Applied Sciences",
      "8" = "MK2: Therapeutics",
      "9" = "MK3: Diagnostics"
    )

    milestone_labels_sbp_pbli <- c(
      "1" = "SBP1: Safety & QI",
      "2" = "SBP2: Navigation",
      "3" = "SBP3: Physician Role",
      "4" = "PBL1: Evidence-Based",
      "5" = "PBL2: Reflective"
    )

    milestone_labels_prof_ics <- c(
      "1" = "PROF1: Behavior",
      "2" = "PROF2: Ethics",
      "3" = "PROF3: Accountability",
      "4" = "PROF4: Well-Being",
      "5" = "ICS1: Patient Comm",
      "6" = "ICS2: Team Comm",
      "7" = "ICS3: Documentation"
    )

    # Milestone level descriptions
    milestone_levels <- c(
      "1" = "Level 1",
      "2" = "Level 2",
      "3" = "Level 3",
      "4" = "Level 4",
      "5" = "Level 5"
    )

    # Translation helper functions
    translate_goal_pcmk <- function(code) {
      if (is.null(code) || is.na(code) || code == "") return("")
      label <- milestone_labels[as.character(code)]
      if (is.na(label)) return(paste("Code:", code))
      return(label)
    }

    translate_goal_sbppbl <- function(code) {
      if (is.null(code) || is.na(code) || code == "") return("")
      label <- milestone_labels_sbp_pbli[as.character(code)]
      if (is.na(label)) return(paste("Code:", code))
      return(label)
    }

    translate_goal_profics <- function(code) {
      if (is.null(code) || is.na(code) || code == "") return("")
      label <- milestone_labels_prof_ics[as.character(code)]
      if (is.na(label)) return(paste("Code:", code))
      return(label)
    }

    translate_level <- function(code) {
      if (is.null(code) || is.na(code) || code == "") return("")
      label <- milestone_levels[as.character(code)]
      if (is.na(label)) return(paste("Level", code))
      return(label)
    }

    # Display previous coach ILP final summary
    output$previous_coach_ilp <- renderUI({
      req(resident_data())

      prev_coach_data <- resident_data()$previous_period$coach_rev

      if (is.null(prev_coach_data) || nrow(prev_coach_data) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No previous coach ILP summary available"
          )
        )
      }

      ilp_final_text <- prev_coach_data$coach_ilp_final[1]

      if (is.na(ilp_final_text) || trimws(ilp_final_text) == "") {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No previous ILP summary provided by coach"
          )
        )
      }

      div(
        style = "padding: 15px; background-color: white; border-radius: 4px;",
        HTML(gsub("\n", "<br>", ilp_final_text))
      )
    })

    # Display previous period goals with achievement status
    output$previous_goals <- renderUI({
      req(resident_data())

      prev_data <- resident_data()$previous_period$ilp

      if (is.null(prev_data) || nrow(prev_data) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No previous period goals available"
          )
        )
      }

      goals_html <- tagList()

      # Helper function to create goal display with achievement
      create_goal_display <- function(domain_name, goal_field, level_field, how_field,
                                      achievement_field, reflection_not_met_field,
                                      reflection_met_field, translate_fn) {

        goal_code <- prev_data[[goal_field]][1]
        goal_level <- prev_data[[level_field]][1]
        how_text <- prev_data[[how_field]][1]
        achievement <- prev_data[[achievement_field]][1]

        # Handle NULL/NA/empty
        if (is.null(goal_code) || is.na(goal_code) || goal_code == "") {
          return(NULL)
        }

        # Translate goal code to readable text
        goal_text <- translate_fn(goal_code)
        level_text <- translate_level(goal_level)

        # Determine achievement status - handle NULL/NA
        goal_met <- !is.null(achievement) && !is.na(achievement) &&
                   (achievement == "1" || tolower(achievement) == "yes")

        # Get appropriate reflection text - handle NULL/NA
        reflection_text <- ""
        if (goal_met) {
          met_text <- prev_data[[reflection_met_field]][1]
          if (!is.null(met_text) && !is.na(met_text) && trimws(met_text) != "") {
            reflection_text <- met_text
          }
        } else {
          not_met_text <- prev_data[[reflection_not_met_field]][1]
          if (!is.null(not_met_text) && !is.na(not_met_text) && trimws(not_met_text) != "") {
            reflection_text <- not_met_text
          }
        }

        # Color coding based on achievement
        border_color <- if (goal_met) "#27ae60" else "#e67e22"
        achievement_icon <- if (goal_met) {
          tags$span(style = "color: #27ae60; font-size: 18px;", icon("check-circle"), " Goal Met")
        } else {
          tags$span(style = "color: #e67e22; font-size: 18px;", icon("exclamation-circle"), " Goal Not Yet Met")
        }

        div(
          style = sprintf("margin-bottom: 20px; padding: 15px; background-color: white; border-radius: 4px; border-left: 4px solid %s;", border_color),
          h5(domain_name, style = "color: #2980b9; margin-top: 0;"),

          div(style = "margin-bottom: 10px;", achievement_icon),

          p(strong("Selected Goal: "), goal_text),
          p(strong("Target Milestone Level: "), level_text),

          if (!is.null(how_text) && !is.na(how_text) && how_text != "") {
            tagList(
              p(strong("How to achieve: ")),
              p(style = "padding-left: 15px; color: #34495e;", HTML(gsub("\n", "<br>", how_text)))
            )
          },

          if (!is.null(reflection_text) && !is.na(reflection_text) && trimws(reflection_text) != "") {
            tagList(
              hr(style = "margin: 10px 0; border-top: 1px solid #ecf0f1;"),
              p(strong("Resident's Reflection: "), style = "color: #7f8c8d;"),
              p(style = "padding-left: 15px; font-style: italic; color: #555;",
                HTML(gsub("\n", "<br>", reflection_text)))
            )
          }
        )
      }

      # PC/MK Domain Goal
      pcmk_goal <- create_goal_display(
        "Patient Care / Medical Knowledge Goal",
        "goal_pcmk", "goal_level_pcmk", "how_pcmk",
        "prior_goal_pcmk",
        "review_q_pcmk", "review_q2_pcmk",
        translate_goal_pcmk
      )
      if (!is.null(pcmk_goal)) goals_html <- tagList(goals_html, pcmk_goal)

      # SBP/PBLI Domain Goal
      sbppbl_goal <- create_goal_display(
        "Systems-Based Practice / Practice-Based Learning Goal",
        "goal_sbppbl", "goal_level_sbppbl", "how_sbppbl",
        "prior_goal_sbppbl",
        "review_q_sbppbl", "review_q2_sbppbl",
        translate_goal_sbppbl
      )
      if (!is.null(sbppbl_goal)) goals_html <- tagList(goals_html, sbppbl_goal)

      # PROF/ICS Domain Goal
      profics_goal <- create_goal_display(
        "Professionalism / Interpersonal Communication Goal",
        "goal_subcomp_profics", "goal_level_profics", "how_profics",
        "prior_goal_profics",
        "review_q_profics", "review_q2_profics",
        translate_goal_profics
      )
      if (!is.null(profics_goal)) goals_html <- tagList(goals_html, profics_goal)

      if (length(goals_html) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No previous goals found"
          )
        )
      }

      goals_html
    })

    # Display current period goals
    output$current_goals <- renderUI({
      req(resident_data())

      curr_data <- resident_data()$current_period$ilp

      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No current period goals available"
          )
        )
      }

      goals_html <- tagList()

      # Helper function to create current goal display
      create_current_goal_display <- function(domain_name, goal_field, level_field, how_field, translate_fn) {
        goal_code <- curr_data[[goal_field]][1]
        goal_level <- curr_data[[level_field]][1]
        how_text <- curr_data[[how_field]][1]

        # Handle NULL/NA/empty
        if (is.null(goal_code) || is.na(goal_code) || goal_code == "") {
          return(NULL)
        }

        # Translate goal code to readable text
        goal_text <- translate_fn(goal_code)
        level_text <- translate_level(goal_level)

        div(
          style = "margin-bottom: 20px; padding: 15px; background-color: white; border-radius: 4px; border-left: 4px solid #f39c12;",
          h5(domain_name, style = "color: #e67e22; margin-top: 0;"),
          p(strong("Selected Goal: "), goal_text),
          p(strong("Target Milestone Level: "), level_text),
          if (!is.null(how_text) && !is.na(how_text) && how_text != "") {
            tagList(
              p(strong("How to achieve: ")),
              p(style = "padding-left: 15px; color: #34495e;", HTML(gsub("\n", "<br>", how_text)))
            )
          }
        )
      }

      # PC/MK Domain Goal
      pcmk_goal <- create_current_goal_display(
        "Patient Care / Medical Knowledge Goal",
        "goal_pcmk", "goal_level_pcmk", "how_pcmk",
        translate_goal_pcmk
      )
      if (!is.null(pcmk_goal)) goals_html <- tagList(goals_html, pcmk_goal)

      # SBP/PBLI Domain Goal
      sbppbl_goal <- create_current_goal_display(
        "Systems-Based Practice / Practice-Based Learning Goal",
        "goal_sbppbl", "goal_level_sbppbl", "how_sbppbl",
        translate_goal_sbppbl
      )
      if (!is.null(sbppbl_goal)) goals_html <- tagList(goals_html, sbppbl_goal)

      # PROF/ICS Domain Goal
      profics_goal <- create_current_goal_display(
        "Professionalism / Interpersonal Communication Goal",
        "goal_subcomp_profics", "goal_level_profics", "how_profics",
        translate_goal_profics
      )
      if (!is.null(profics_goal)) goals_html <- tagList(goals_html, profics_goal)

      if (length(goals_html) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No current goals found"
          )
        )
      }

      goals_html
    })

    # Character count for coach entry
    output$char_count <- renderText({
      char_count <- nchar(input$coach_mile_goal)
      sprintf("%d characters", char_count)
    })

    # Character count for ILP final
    output$char_count_ilp <- renderText({
      char_count <- nchar(input$coach_ilp_final)
      sprintf("%d characters", char_count)
    })

    # Pre-populate if data already exists for current period
    observe({
      req(resident_data())

      curr_data <- resident_data()$current_period$coach_rev

      if (!is.null(curr_data) && nrow(curr_data) > 0) {
        # Pre-populate goal comments
        existing_goals <- curr_data$coach_mile_goal[1]
        if (!is.na(existing_goals) && existing_goals != "") {
          updateTextAreaInput(
            session,
            "coach_mile_goal",
            value = existing_goals
          )
        }

        # Pre-populate ILP final
        existing_ilp <- curr_data$coach_ilp_final[1]
        if (!is.na(existing_ilp) && existing_ilp != "") {
          updateTextAreaInput(
            session,
            "coach_ilp_final",
            value = existing_ilp
          )
        }
      }
    })

    # Return reactive with entered data
    return(
      reactive({
        list(
          coach_mile_goal = input$coach_mile_goal,
          coach_ilp_final = input$coach_ilp_final,
          is_complete = !is.null(input$coach_mile_goal) && nchar(trimws(input$coach_mile_goal)) > 0 &&
                       !is.null(input$coach_ilp_final) && nchar(trimws(input$coach_ilp_final)) > 0
        )
      })
    )
  })
}
