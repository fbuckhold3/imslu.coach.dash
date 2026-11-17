# =============================================================================
# Touch-base: ILP check-in (periods 1-5)
#
# The semi-annual review is split into two coach forms (mod_review_interface.R):
#   - Evaluation & ILP: milestones, ILP, completion -> marks coach_rev complete
#   - Touch-base: a later meeting -> Wellness, Scholarship, Career + this
#     section, the ILP check-in.
#
# Both save into the same coach_rev instance for the period. The ILP check-in
# (coach_ilp_checkin) is the touch-base's takeaway and, with coach_touch_date,
# shows up in the resident thread and roadmap. No second review.
# =============================================================================

mod_touch_base_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h5("This period's ILP goals", style = "color: var(--roundsui-ink); margin-top: 4px;"),
    uiOutput(ns("ilp_context")),
    hr(),
    h5("ILP check-in", style = "color: var(--roundsui-ink);"),
    p(class = "text-muted small",
      "How is the resident progressing toward the ILP goals? This is the takeaway ",
      "from the touch-base and appears in the resident's history."),
    uiOutput(ns("date_slot")),
    coach_note_ui(ns, "coach_ilp_checkin"),
    div(style = "text-align: right; font-size: 12px; color: var(--roundsui-ink-faint);",
        textOutput(ns("char_count")))
  )
}

#' @param resident_data Reactive get_resident_period_data() output.
#' @param data_dict Optional reactive REDCap data dictionary (milestone-level
#'   descriptions on the goal cards; bare level numbers without it).
mod_touch_base_server <- function(id, resident_data, data_dict = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # This period's goals as the Evaluation showed them (last period | this
    # period per domain), each with the coach's note from the Evaluation,
    # then the Finalized ILP.
    output$ilp_context <- renderUI({
      rd <- resident_data(); req(rd)
      cur_ilp  <- rd$current_period$ilp
      prev_ilp <- rd$previous_period$ilp
      ilp <- coach_note_saved_value(rd, "coach_ilp_final")
      dd <- if (is.function(data_dict)) tryCatch(data_dict(), error = function(e) NULL)
      if (!is.data.frame(dd)) dd <- NULL
      has_goals <- is.data.frame(cur_ilp) && nrow(cur_ilp) > 0
      .block <- function(label, txt) if (nzchar(trimws(txt))) div(
        class = "mb-2",
        tags$strong(label),
        div(style = "white-space: pre-wrap; background: var(--roundsui-surface-2); padding: 8px; border-radius: 4px;",
            txt))

      # Older reviews have one combined goal note instead of three.
      have_new <- any(vapply(.COACH_GOAL_FIELDS, function(fl)
        nzchar(trimws(coach_note_saved_value(rd, fl))), logical(1)))
      old_goal <- if (!have_new) coach_note_saved_value(rd, "coach_mile_goal") else ""

      cards <- if (has_goals) lapply(names(.COACH_GOAL_FIELDS), function(dom) {
        note <- coach_note_saved_value(rd, .COACH_GOAL_FIELDS[[dom]])
        div(
          class = "mb-4",
          gmed::ilp_compare_row_ui(dom, prev_ilp = prev_ilp, cur_ilp = cur_ilp, data_dict = dd),
          if (have_new) div(
            style = paste("margin-top: -1.5rem; padding: 10px 14px;",
                          "border: 1px solid var(--roundsui-border); border-top: none;",
                          "border-left: 4px solid var(--roundsui-accent);",
                          "border-radius: 0 0 8px 8px; background: var(--roundsui-surface);"),
            tags$strong(paste0("Your note on the ", .COACH_GOAL_TAGS[[dom]], " goal (Evaluation): ")),
            if (nzchar(trimws(note))) div(style = "white-space: pre-wrap;", note)
            else tags$em(class = "text-muted", "none written"))
        )
      })

      tagList(
        if (!has_goals)
          div(class = "alert alert-warning mb-3", style = "font-size: 0.9rem;",
              icon("triangle-exclamation"),
              " The resident hasn't set ILP goals for this period yet."),
        cards,
        .block("Coach notes on goals (Evaluation)", old_goal),
        if (nzchar(trimws(ilp))) .block("Finalized ILP", ilp)
        else div(class = "text-muted fst-italic small",
                 "No finalized ILP saved yet. Submit the Evaluation & ILP form first, ",
                 "or note the goals discussed in the check-in.")
      )
    })

    output$date_slot <- renderUI({
      saved <- suppressWarnings(as.Date(coach_note_saved_value(resident_data(), "coach_touch_date")))
      dateInput(ns("coach_touch_date"), "Touch-base date",
                value = if (length(saved) == 1 && !is.na(saved)) saved else Sys.Date(),
                max = Sys.Date(), width = "220px")
    })

    coach_note_server(output, session, "coach_ilp_checkin", resident_data,
                      placeholder = "Progress on each goal, barriers, changes agreed...",
                      rows = 6, width = "100%")

    output$char_count <- renderText(sprintf("%d characters", nchar(input$coach_ilp_checkin %||% "")))

    reactive({
      txt <- input$coach_ilp_checkin %||% ""
      dt  <- input$coach_touch_date
      list(
        coach_ilp_checkin = txt,
        coach_touch_date  = if (length(dt) == 1 && !is.na(dt)) format(as.Date(dt), "%Y-%m-%d") else "",
        is_complete       = nzchar(trimws(txt)) && length(dt) == 1 && !is.na(dt)
      )
    })
  })
}
