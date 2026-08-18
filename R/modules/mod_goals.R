# Section 7: Goals & ILP Module
# Display logic for periods 1-6 lives in gmed::mod_ilp_display. This module
# owns the coach-entry textareas (coach_mile_goal, coach_ilp_final) and
# submission wiring, and mounts the gmed display module for the read-only
# data — unchanged for periods 1-6.
#
# P7 (Entering Residency) has no ilp record at all (that form doesn't exist
# for this period), so gmed::mod_ilp_display was rendering garbage against
# empty/missing data. For P7, show the resident's actual answers instead:
# s_e_ume_goal1/2/3 ("Goals for First 6 Months"). Coach entry
# (coach_mile_goal / coach_ilp_final) is unchanged for every period.
# `period_num` is the app's local 0-based period number (0 = Entering
# Residency), passed once at UI-build time from mod_review_interface.R's
# sections_ui renderUI.

mod_goals_ui <- function(id, period_num = NULL) {
  ns <- NS(id)
  is_p7 <- identical(suppressWarnings(as.integer(period_num)), 0L)

  tagList(
    if (is_p7) {
      wellPanel(
        style = "background-color: #f8f9fa; border-left: 4px solid #3498db;",
        uiOutput(ns("p7_goals_display"))
      )
    } else {
      # Read-only ILP display (gmed):
      #   1. previous coach ILP summary (coach_mile_goal + coach_ilp_final)
      #   2. previous-period goals + resident achievement reflection
      #   3. current-period goals
      gmed::mod_ilp_display_ui(ns("ilp"))
    },

    hr(),

    # Coach entry: per-goal / milestone comments
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

      div(
        style = "text-align: right; font-size: 12px; color: #95a5a6;",
        textOutput(ns("char_count"))
      )
    ),

    # Comprehensive ILP summary — periods 1-6 only. P7 has no ILP; the
    # overall summary for entering residents lives in mod_summary.R instead.
    if (!is_p7) tagList(
      hr(),

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

        div(
          style = "text-align: right; font-size: 12px; color: #95a5a6;",
          textOutput(ns("char_count_ilp"))
        )
      )
    )
  )
}

mod_goals_server <- function(id, resident_data, current_period, app_data, app_data_rv) {
  moduleServer(id, function(input, output, session) {

    is_p7 <- reactive({ identical(as.integer(current_period() %||% NA_integer_), 0L) })

    # data_dict is held in app_data_rv (a reactiveValues); extract as a reactive
    # so mod_ilp_display_server can call it when the data finishes loading.
    data_dict_r <- reactive({
      if (shiny::is.reactivevalues(app_data_rv)) {
        app_data_rv$data_dict
      } else if (is.function(app_data_rv)) {
        dd <- app_data_rv()
        if (is.list(dd) && !is.null(dd$data_dict)) dd$data_dict else dd
      } else {
        app_data_rv
      }
    })

    # ----- Read-only ILP display via gmed (periods 1-6 only) -----
    # Always registered (cheap, reactive) — for P7 the corresponding UI
    # container isn't present in mod_goals_ui(), so this is a no-op there.
    gmed::mod_ilp_display_server(
      "ilp",
      current_ilp = reactive({
        rd <- resident_data()
        if (is.null(rd)) NULL else rd$current_period$ilp
      }),
      previous_ilp = reactive({
        rd <- resident_data()
        if (is.null(rd)) NULL else rd$previous_period$ilp
      }),
      previous_coach_rev = reactive({
        rd <- resident_data()
        if (is.null(rd)) NULL else rd$previous_period$coach_rev
      }),
      data_dict = data_dict_r
    )

    # ----- P7 goals display: s_e_ume_goal1/2/3 (read-only) -----
    output$p7_goals_display <- renderUI({
      rd <- resident_data(); if (is.null(rd)) return(NULL)
      row <- rd$current_period$s_eval
      if (is.null(row) || nrow(row) == 0) {
        return(p(class = "text-muted", "No self-evaluation on file yet."))
      }

      goals <- c(row$s_e_ume_goal1[1], row$s_e_ume_goal2[1], row$s_e_ume_goal3[1])
      goals <- goals[!is.na(goals) & nzchar(trimws(as.character(goals)))]
      if (length(goals) == 0) {
        return(p(class = "text-muted", "No goals provided."))
      }

      tagList(
        h5("Goals for First 6 Months", style = "color: #2c3e50;"),
        tags$ol(lapply(goals, tags$li))
      )
    })

    # ----- Character counts -----
    output$char_count <- renderText({
      sprintf("%d characters", nchar(input$coach_mile_goal %||% ""))
    })
    output$char_count_ilp <- renderText({
      sprintf("%d characters", nchar(input$coach_ilp_final %||% ""))
    })

    # ----- Load existing coach entries when resident changes -----
    current_resident_id <- reactiveVal(NULL)

    observe({
      req(resident_data())
      res_info <- resident_data()$resident_info
      new_resident_id <- res_info$record_id[1]

      if (is.null(current_resident_id()) || current_resident_id() != new_resident_id) {
        current_resident_id(new_resident_id)

        curr_data <- resident_data()$current_period$coach_rev

        if (!is.null(curr_data) && nrow(curr_data) > 0) {
          existing_goals <- curr_data$coach_mile_goal[1]

          updateTextAreaInput(session, "coach_mile_goal",
                              value = if (!is.na(existing_goals) && existing_goals != "")
                                        existing_goals else "")

          if (!isTRUE(is_p7())) {
            existing_ilp <- curr_data$coach_ilp_final[1]
            updateTextAreaInput(session, "coach_ilp_final",
                                value = if (!is.na(existing_ilp) && existing_ilp != "")
                                          existing_ilp else "")
          }
          message(sprintf("Loaded existing goals data for resident %s", new_resident_id))
        } else {
          updateTextAreaInput(session, "coach_mile_goal", value = "")
          if (!isTRUE(is_p7())) updateTextAreaInput(session, "coach_ilp_final", value = "")
          message(sprintf("Cleared goals forms for new resident %s (no coach_rev record)",
                          new_resident_id))
        }
      }
    })

    # ----- Return entered data for submission -----
    reactive({
      if (isTRUE(is_p7())) {
        list(
          coach_mile_goal = input$coach_mile_goal,
          coach_ilp_final = "",
          is_complete = !is.null(input$coach_mile_goal) &&
                        nchar(trimws(input$coach_mile_goal %||% "")) > 0
        )
      } else {
        list(
          coach_mile_goal = input$coach_mile_goal,
          coach_ilp_final = input$coach_ilp_final,
          is_complete = !is.null(input$coach_mile_goal) &&
                        nchar(trimws(input$coach_mile_goal)) > 0 &&
                        !is.null(input$coach_ilp_final) &&
                        nchar(trimws(input$coach_ilp_final)) > 0
        )
      }
    })
  })
}
