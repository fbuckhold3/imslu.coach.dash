# Section 4: Scholarship Module
# Resident scholarship (shared gmed view, read-only) + coach verify and note

mod_scholarship_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Same list the resident sees in ind.dash (gmed), read-only here.
    gmed::mod_scholarship_view_ui(ns("schol_view")),

    # QI / patient safety / committee answers from the self-evaluation.
    h5("Quality Improvement, Patient Safety & Committees",
       style = "color: var(--roundsui-ink); margin-top: 16px;"),
    div(style = "padding: 10px 14px; border: 1px solid var(--roundsui-border); border-radius: 8px; background: var(--roundsui-surface);",
        uiOutput(ns("qi_ps"))),

    hr(),

    # Coach verification — confirm the scholarship record is complete and accurate.
    wellPanel(
      style = "background-color: var(--roundsui-surface); border-left: 4px solid var(--roundsui-accent);",
      tags$label(
        "Verify Scholarship Record",
        style = "font-weight: bold; color: var(--roundsui-ink);"
      ),
      tags$p(
        "Review the scholarship list above with the resident. Confirm it includes ",
        tags$strong("everything"), ": posters, QI projects, research, publications, ",
        "noon conference and other residency presentations, regional/national presentations, ",
        "committee work, and any other scholarly activity. We need accurate data — ",
        "missing entries should be added by the resident in their dashboard before you continue.",
        style = "font-size: 13px; color: var(--roundsui-ink-muted); margin-top: 5px; margin-bottom: 12px;"
      ),
      checkboxInput(
        ns("scholarship_verified"),
        label = "I have reviewed this with the resident and confirm the scholarship record is complete and up to date.",
        value = FALSE
      )
    ),

    # Coach notes on scholarship (coach_scholarship). Optional.
    mod_prev_notes_ui(ns("prev_notes")),
    wellPanel(
      tags$label("Coach Comments on Scholarship:", style = "font-weight: bold;"),
      tags$p(
        "Projects in progress, opportunities or mentors to connect with, presentation or publication plans.",
        style = "font-size: 12px; color: var(--roundsui-ink-muted); margin-top: 5px;"
      ),
      coach_note_ui(ns, "coach_scholarship"),
      div(style = "text-align: right; font-size: 12px; color: var(--roundsui-ink-faint);",
          textOutput(ns("char_count")))
    )
  )
}

mod_scholarship_server <- function(id, resident_data, current_period, app_data) {
  moduleServer(id, function(input, output, session) {

    # This resident's scholarship rows -> shared gmed view, no edit buttons.
    schol_rows <- reactive({
      req(resident_data(), app_data())
      rid <- resident_data()$resident_info$record_id
      sch <- app_data()$all_forms$scholarship
      if (is.null(sch) || nrow(sch) == 0) return(NULL)
      sch[as.character(sch$record_id) == as.character(rid), , drop = FALSE]
    })
    gmed::mod_scholarship_view_server("schol_view", schol_data = schol_rows,
                                      title = "Scholarship Activities",
                                      show_intro = FALSE, actions = FALSE)

    output$qi_ps <- renderUI({
      req(resident_data(), app_data())
      rid <- resident_data()$resident_info$record_id
      se <- app_data()$all_forms$s_eval
      if (!is.null(se) && nrow(se) > 0) se <- se[as.character(se$record_id) == as.character(rid), , drop = FALSE]
      dd <- tryCatch(app_data()$data_dict, error = function(e) NULL)
      gmed::seval_qi_ps_ui(se, get_period_name(current_period()), data_dict = dd)
    })

    # Previous period's scholarship notes + the coach note box, pre-filled
    # from the saved coach_rev value (see coach_note_server).
    mod_prev_notes_server("prev_notes", resident_data, current_period,
                          fields = c(coach_scholarship = "Scholarship"))
    coach_note_server(output, session, "coach_scholarship", resident_data,
                      width = "100%", height = "150px",
                      placeholder = "Scholarship discussion, next steps...")
    output$char_count <- renderText(sprintf("%d characters", nchar(input$coach_scholarship %||% "")))

    # Return reactive with completion status — coach must verify the
    # scholarship record is complete before this section is marked done.
    # The note is optional.
    return(
      reactive({
        list(
          coach_scholarship = input$coach_scholarship %||% "",
          scholarship_verified = isTRUE(input$scholarship_verified),
          is_complete = isTRUE(input$scholarship_verified)
        )
      })
    )
  })
}
