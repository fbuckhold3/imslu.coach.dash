# Section 7: Goals & ILP Module
# Displays previous and current period ILP goals and allows coach to enter comments

mod_goals_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Previous Period Goals Section
    h4("Previous Period Goals", style = "color: #34495e; margin-top: 10px;"),
    p(style = "color: #7f8c8d;", "Review the goals set in the previous period and their progress."),

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

    # Coach Entry for Current Period
    h4("Coach Review - Goal Assessment", style = "color: #34495e; margin-top: 20px;"),

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
    h4("ILP Final Summary", style = "color: #34495e; margin-top: 20px;"),

    wellPanel(
      style = "background-color: #ffffff; border-left: 4px solid #3498db;",

      div(
        style = "margin-bottom: 15px;",
        tags$label(
          "Finalized ILP Summary:",
          style = "font-weight: bold; color: #2c3e50;"
        ),
        tags$p(
          "Provide a comprehensive ILP summary that synthesizes all sections of the review.",
          style = "font-size: 12px; color: #7f8c8d; margin-top: 5px;"
        )
      ),

      textAreaInput(
        ns("coach_ilp_final"),
        label = NULL,
        value = "",
        width = "100%",
        height = "300px",
        placeholder = "Enter your comprehensive ILP summary here..."
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

    # Milestone competency labels for goal display
    milestone_labels <- c(
      "PC1" = "PC1: Gathers and synthesizes essential information",
      "PC2" = "PC2: Prioritizes differential diagnosis",
      "PC3" = "PC3: Manages patients with progressive responsibility",
      "PC4" = "PC4: Demonstrates skill in performing procedures",
      "PC5" = "PC5: Requests consultations effectively",
      "PC6" = "PC6: Provides appropriate role modeling",
      "MK1" = "MK1: Core knowledge for effective patient care",
      "MK2" = "MK2: Knowledge of diagnostic testing and procedures",
      "MK3" = "MK3: Scholarly activities",
      "SBP1" = "SBP1: Works effectively within healthcare system",
      "SBP2" = "SBP2: Coordinates care with other healthcare professionals",
      "SBP3" = "SBP3: Incorporates cost-awareness",
      "PBLI1" = "PBLI1: Identifies strengths and gaps in knowledge",
      "PBLI2" = "PBLI2: Uses information technology for learning",
      "PROF1" = "PROF1: Demonstrates compassion and respect",
      "PROF2" = "PROF2: Demonstrates accountability to patients and society",
      "PROF3" = "PROF3: Manages conflicts of interest",
      "PROF4" = "PROF4: Demonstrates self-awareness and help-seeking",
      "ICS1" = "ICS1: Communicates effectively with patients and families",
      "ICS2" = "ICS2: Maintains comprehensive, accurate records",
      "ICS3" = "ICS3: Communicates effectively with healthcare team"
    )

    # Display previous period goals
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

      # Extract goal fields from previous period
      goals_html <- tagList()

      # PC/MK Domain Goal
      if (!is.na(prev_data$goal_pcmk[1]) && prev_data$goal_pcmk[1] != "") {
        goals_html <- tagList(
          goals_html,
          div(
            style = "margin-bottom: 20px; padding: 15px; background-color: white; border-radius: 4px;",
            h5("Patient Care / Medical Knowledge Goal:", style = "color: #2980b9; margin-top: 0;"),
            p(strong("Selected Milestone: "), prev_data$goal_pcmk[1]),
            if (!is.na(prev_data$how_pcmk[1]) && prev_data$how_pcmk[1] != "") {
              tagList(
                p(strong("How to achieve: ")),
                p(style = "padding-left: 15px;", HTML(gsub("\n", "<br>", prev_data$how_pcmk[1])))
              )
            }
          )
        )
      }

      # SBP/PBLI Domain Goal
      if (!is.na(prev_data$goal_sbppbl[1]) && prev_data$goal_sbppbl[1] != "") {
        goals_html <- tagList(
          goals_html,
          div(
            style = "margin-bottom: 20px; padding: 15px; background-color: white; border-radius: 4px;",
            h5("Systems-Based Practice / Practice-Based Learning Goal:", style = "color: #2980b9; margin-top: 0;"),
            p(strong("Selected Milestone: "), prev_data$goal_sbppbl[1]),
            if (!is.na(prev_data$how_sbppbl[1]) && prev_data$how_sbppbl[1] != "") {
              tagList(
                p(strong("How to achieve: ")),
                p(style = "padding-left: 15px;", HTML(gsub("\n", "<br>", prev_data$how_sbppbl[1])))
              )
            }
          )
        )
      }

      # PROF/ICS Domain Goal
      if (!is.na(prev_data$goal_profics[1]) && prev_data$goal_profics[1] != "") {
        goals_html <- tagList(
          goals_html,
          div(
            style = "margin-bottom: 20px; padding: 15px; background-color: white; border-radius: 4px;",
            h5("Professionalism / Interpersonal Communication Goal:", style = "color: #2980b9; margin-top: 0;"),
            p(strong("Selected Milestone: "), prev_data$goal_profics[1]),
            if (!is.na(prev_data$how_profics[1]) && prev_data$how_profics[1] != "") {
              tagList(
                p(strong("How to achieve: ")),
                p(style = "padding-left: 15px;", HTML(gsub("\n", "<br>", prev_data$how_profics[1])))
              )
            }
          )
        )
      }

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

      # Extract goal fields from current period
      goals_html <- tagList()

      # PC/MK Domain Goal
      if (!is.na(curr_data$goal_pcmk[1]) && curr_data$goal_pcmk[1] != "") {
        goals_html <- tagList(
          goals_html,
          div(
            style = "margin-bottom: 20px; padding: 15px; background-color: white; border-radius: 4px;",
            h5("Patient Care / Medical Knowledge Goal:", style = "color: #e67e22; margin-top: 0;"),
            p(strong("Selected Milestone: "), curr_data$goal_pcmk[1]),
            if (!is.na(curr_data$how_pcmk[1]) && curr_data$how_pcmk[1] != "") {
              tagList(
                p(strong("How to achieve: ")),
                p(style = "padding-left: 15px;", HTML(gsub("\n", "<br>", curr_data$how_pcmk[1])))
              )
            }
          )
        )
      }

      # SBP/PBLI Domain Goal
      if (!is.na(curr_data$goal_sbppbl[1]) && curr_data$goal_sbppbl[1] != "") {
        goals_html <- tagList(
          goals_html,
          div(
            style = "margin-bottom: 20px; padding: 15px; background-color: white; border-radius: 4px;",
            h5("Systems-Based Practice / Practice-Based Learning Goal:", style = "color: #e67e22; margin-top: 0;"),
            p(strong("Selected Milestone: "), curr_data$goal_sbppbl[1]),
            if (!is.na(curr_data$how_sbppbl[1]) && curr_data$how_sbppbl[1] != "") {
              tagList(
                p(strong("How to achieve: ")),
                p(style = "padding-left: 15px;", HTML(gsub("\n", "<br>", curr_data$how_sbppbl[1])))
              )
            }
          )
        )
      }

      # PROF/ICS Domain Goal
      if (!is.na(curr_data$goal_profics[1]) && curr_data$goal_profics[1] != "") {
        goals_html <- tagList(
          goals_html,
          div(
            style = "margin-bottom: 20px; padding: 15px; background-color: white; border-radius: 4px;",
            h5("Professionalism / Interpersonal Communication Goal:", style = "color: #e67e22; margin-top: 0;"),
            p(strong("Selected Milestone: "), curr_data$goal_profics[1]),
            if (!is.na(curr_data$how_profics[1]) && curr_data$how_profics[1] != "") {
              tagList(
                p(strong("How to achieve: ")),
                p(style = "padding-left: 15px;", HTML(gsub("\n", "<br>", curr_data$how_profics[1])))
              )
            }
          )
        )
      }

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
