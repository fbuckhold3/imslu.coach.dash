# Section 1: Wellness & Progress Module
# Displays previous period wellness data and allows coach to enter new comments

mod_wellness_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Resident self-reflection (read-only) \u2014 full reflection block from s_eval:
    # plus / delta / wellness / mentor-discussion topics / program-assistance.
    # Previous period text is shown as a reference panel above each field.
    # Duty hours sit beside the resident's own wellness answers so the coach
    # reads "how I'm doing" next to the hours behind it (stacks on narrow
    # screens).
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      div(
        h4("Resident Self-Reflection", style = "margin-top: 10px;"),
        wellPanel(
          gmed::mod_seval_reflection_display_ui(
            ns("reflection"),
            title  = NULL,
            fields = c("well", "discussion", "prog_assist")
          )
        )
      ),
      mod_workload_ui(ns("workload"))
    ),

    hr(),
    
    # Current Period Coach Entry
    h4("Coach Review - Current Period", style = "margin-top: 20px;"),

    mod_prev_notes_ui(ns("prev_notes")),

    wellPanel(
      
      div(
        style = "margin-bottom: 15px;",
        tags$label(
          "Coach Comments on Wellness:",
          style = "font-weight: bold;"
        ),
        tags$p(
          "Provide comments on the resident's wellness, work-life balance, and any support needed.",
          style = "font-size: 12px; color: var(--roundsui-ink-muted); margin-top: 5px;"
        )
      ),
      
      coach_note_ui(ns, "coach_wellness"),
      
      # Character count
      div(
        style = "text-align: right; font-size: 12px; color: var(--roundsui-ink-faint);",
        textOutput(ns("char_count"))
      )
    )
  )
}

mod_wellness_server <- function(id, resident_data, current_period, app_data) {
  moduleServer(id, function(input, output, session) {

    # Resident self-reflection (read-only) \u2014 gmed display module renders all
    # five s_eval reflection fields (plus, delta, well, discussion, prog_assist)
    # with previous-period reference text where present.
    gmed::mod_seval_reflection_display_server(
      "reflection",
      current_row  = reactive({
        rd <- resident_data()
        if (is.null(rd)) NULL else rd$current_period$s_eval
      }),
      previous_row = reactive({
        rd <- resident_data()
        if (is.null(rd)) NULL else rd$previous_period$s_eval
      }),
      fields = c("well", "discussion", "prog_assist")
    )

    # Previous coach wellness notes + duty hours since the last review
    mod_prev_notes_server("prev_notes", resident_data, current_period,
                          fields = c(coach_wellness = "Wellness"))
    mod_workload_server("workload", resident_data, current_period)

    # Character count for coach entry
    output$char_count <- renderText({
      char_count <- nchar(input$coach_wellness)
      sprintf("%d characters", char_count)
    })

    # Coach note textarea, rendered pre-filled from the saved coach_rev
    # value (see coach_note_server in R/helpers/section_shell.R).
    coach_note_server(output, session, "coach_wellness", resident_data,
                      width = "100%", height = "200px", placeholder = "Enter your wellness assessment and recommendations here...")

    # Return reactive with entered data
    return(
      reactive({
        list(
          coach_wellness = input$coach_wellness,
          is_complete = !is.null(input$coach_wellness) && nchar(trimws(input$coach_wellness)) > 0
        )
      })
    )
  })
}