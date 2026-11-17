# Section 7: Goals & ILP Module
# 2026-11: P1-6 coaches write one note per ILP goal (coach_goal_pcmk /
# coach_goal_sbppbl / coach_goal_profics). The three are also combined into
# coach_mile_goal on save, which gmed's ILP history, the Touch-base context
# and the CCC dashboard still read. The Finalized ILP summary
# (coach_ilp_final) moved to its own section after Milestones
# (mod_ilp_summary.R).
# Periods 1-6 lay out one group per domain: gmed::ilp_compare_row_ui (last
# period's goal with the did-it-move card | this period's goal), then the
# coach's note on that goal. gmed::mod_ilp_display shows last period's coach
# comments above them.
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
        style = "background-color: var(--roundsui-surface-2); border: 1px solid var(--roundsui-border);",
        uiOutput(ns("p7_goals_display"))
      )
    } else {
      tagList(
        # Last period's coach ILP comments (gmed), for context.
        gmed::mod_ilp_display_ui(ns("ilp"), sections = "prev_coach")
      )
    },

    hr(),

    # Coach entry. P1-6: one note per ILP goal. P7 (no ILP): one general box.
    if (is_p7) tagList(
      h4("Coach Assessment of Goals", style = "margin-top: 20px;"),
      wellPanel(
        style = "background-color: var(--roundsui-surface); border-left: 4px solid var(--roundsui-accent);",
        div(
          style = "margin-bottom: 15px;",
          tags$label("Coach Comments on Goals:", style = "font-weight: bold; color: var(--roundsui-ink);"),
          tags$p("Provide feedback on the resident's goal selection, progress on previous goals, and support needed for goal achievement.",
                 style = "font-size: 12px; color: var(--roundsui-ink-muted); margin-top: 5px;")
        ),
        coach_note_ui(ns, "coach_mile_goal"),
        div(style = "text-align: right; font-size: 12px; color: var(--roundsui-ink-faint);",
            textOutput(ns("char_count")))
      )
    ) else tagList(
      # One group per domain: last period's goal (+ did it move?), this
      # period's goal, then the coach's note on that goal.
      h4("Goals by Domain", style = "margin-top: 10px;"),
      tags$p("For each goal: is it the right goal, is the target realistic, and what will help the resident get there?",
             style = "font-size: 12px; color: var(--roundsui-ink-muted);"),
      uiOutput(ns("old_general_note")),
      lapply(names(.COACH_GOAL_FIELDS), function(dom) {
        div(
          class = "mb-4",
          # The goal card re-renders with live milestone ratings; the note
          # slot sits outside it so typing is never reset.
          uiOutput(ns(paste0("goal_cmp_", dom))),
          div(
            style = paste("margin-top: -1.5rem; padding: 12px 14px;",
                          "border: 1px solid var(--roundsui-border); border-top: none;",
                          "border-left: 4px solid var(--roundsui-accent);",
                          "border-radius: 0 0 8px 8px; background: var(--roundsui-surface);"),
            tags$label(paste0("Your note on the ", .COACH_GOAL_TAGS[[dom]], " goal"),
                       style = "font-weight: bold; color: var(--roundsui-ink);"),
            coach_note_ui(ns, .COACH_GOAL_FIELDS[[dom]])
          )
        )
      })
    )
  )
}

# This period's ILP goals, one entry per domain: label, goal text, level,
# the resident's why / how. Shared with mod_ilp_summary.R.
coach_ilp_goals <- function(curr_ilp) {
  lapply(stats::setNames(names(.CTX_GOAL_DOMAINS), names(.CTX_GOAL_DOMAINS)), function(dom) {
    f <- .CTX_GOAL_DOMAINS[[dom]]
    sub <- ctx_goal_subcomp(dom, ctx_val(curr_ilp, f$goal))
    list(domain = dom, domain_label = f$label,
         goal  = if (is.na(sub)) "" else unname(.CTX_SUBCOMP_LABELS[sub]),
         level = ctx_val(curr_ilp, f$level),
         how   = ctx_val(curr_ilp, paste0("how_", dom)))
  })
}

.COACH_GOAL_FIELDS <- c(pcmk = "coach_goal_pcmk", sbppbl = "coach_goal_sbppbl", profics = "coach_goal_profics")
.COACH_GOAL_TAGS   <- c(pcmk = "PC/MK", sbppbl = "SBP/PBLI", profics = "PROF/ICS")

mod_goals_server <- function(id, resident_data, current_period, app_data, app_data_rv,
                             live_coach_row = NULL) {
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
      data_dict = data_dict_r,
      sections = "prev_coach"
    )

    # ----- Last period's goals: target vs ratings (periods 1-6) -----
    # "Now" prefers ACGME, then the coach's milestone ratings: the live
    # entries from the Milestones section when the coach has started them
    # (live_coach_row), otherwise whatever is already saved for this period.
    goal_period_r <- reactive({
      unname(get_previous_period(get_period_name(current_period())))
    })
    followthrough_r <- reactive({
      rd <- resident_data(); req(rd)
      gp <- goal_period_r()
      if (is.na(gp)) return(NULL)
      live <- if (is.function(live_coach_row)) tryCatch(live_coach_row(), error = function(e) NULL)
      curr_coach <- if (!is.null(live) && nrow(live) > 0) live else rd$current_period$milestone_entry
      ctx_goal_followthrough(
        prev_ilp   = rd$previous_period$ilp,
        curr_ilp   = rd$current_period$ilp,
        prev_acgme = rd$previous_period$acgme_miles,
        prev_coach = rd$previous_period$milestone_entry,
        curr_acgme = rd$current_period$acgme_miles,
        curr_coach = curr_coach,
        curr_self  = rd$current_period$milestone_selfevaluation,
        goal_period_name = gp
      )
    })

    # ----- Per-domain goal card: last period | this period (gmed) -----
    lapply(names(.COACH_GOAL_FIELDS), function(dom) {
      output[[paste0("goal_cmp_", dom)]] <- renderUI({
        rd <- resident_data(); req(rd)
        ft <- tryCatch(followthrough_r(), error = function(e) NULL)
        ft_row <- if (is.data.frame(ft) && nrow(ft) > 0 && dom %in% ft$domain) ft[ft$domain == dom, ][1, ]
        dd <- tryCatch(data_dict_r(), error = function(e) NULL)
        gmed::ilp_compare_row_ui(
          dom,
          prev_ilp   = rd$previous_period$ilp,
          cur_ilp    = rd$current_period$ilp,
          data_dict  = if (is.data.frame(dd)) dd else NULL,
          prev_extra = if (!is.null(ft_row))
            div(class = "mt-3", goal_followthrough_card(ft_row, goal_period_r(), compact = TRUE))
        )
      })
    })

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
        h5("Goals for First 6 Months", style = "color: var(--roundsui-ink);"),
        tags$ol(lapply(goals, tags$li))
      )
    })

    # ----- Character counts -----
    output$char_count <- renderText({
      sprintf("%d characters", nchar(input$coach_mile_goal %||% ""))
    })

    # Coach note textareas, rendered pre-filled from the saved coach_rev
    # value (see coach_note_server in R/helpers/section_shell.R).
    coach_note_server(output, session, "coach_mile_goal", resident_data,
                      width = "100%", height = "200px", placeholder = "Enter your assessment of the resident's goals and progress here...")
    for (dom in names(.COACH_GOAL_FIELDS))
      coach_note_server(output, session, .COACH_GOAL_FIELDS[[dom]], resident_data,
                        width = "100%", height = "110px",
                        placeholder = "Your notes on this goal...")

    goals_r <- reactive({
      rd <- resident_data(); if (is.null(rd)) return(NULL)
      coach_ilp_goals(rd$current_period$ilp)
    })

    # A review saved before per-goal notes: show its general comment.
    output$old_general_note <- renderUI({
      rd <- resident_data(); req(rd)
      old <- coach_note_saved_value(rd, "coach_mile_goal")
      have_new <- any(vapply(.COACH_GOAL_FIELDS, function(fl)
        nzchar(coach_note_saved_value(rd, fl)), logical(1)))
      if (!nzchar(trimws(old)) || have_new) return(NULL)
      div(class = "alert alert-light py-2 px-3 mb-3", style = "font-size:0.85rem;",
          tags$strong("Earlier general comment on goals: "),
          div(style = "white-space:pre-wrap;", old))
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
        notes <- vapply(.COACH_GOAL_FIELDS, function(fl) trimws(input[[fl]] %||% ""), character(1))
        g <- tryCatch(goals_r(), error = function(e) NULL)
        has_goal <- if (is.null(g)) rep(FALSE, 3) else vapply(g, function(x) nzchar(x$goal), logical(1))
        # Combined copy for coach_mile_goal (read by gmed / CCC / Touch-base).
        lbl <- vapply(names(.COACH_GOAL_FIELDS), function(d) {
          gl <- if (!is.null(g)) g[[d]]$goal else ""
          paste0(.COACH_GOAL_TAGS[[d]], if (nzchar(gl)) paste0(" (", gl, ")"), ": ")
        }, character(1))
        combined <- paste(paste0(lbl, notes)[nzchar(notes)], collapse = "\n\n")
        out <- as.list(stats::setNames(notes, unname(.COACH_GOAL_FIELDS)))
        out$coach_mile_goal <- combined
        # Done when every goal the resident set has a note (or, with no goals
        # on file yet, when at least one note is written).
        out$is_complete <- if (any(has_goal)) all(nzchar(notes[has_goal])) else any(nzchar(notes))
        out
      }
    })
  })
}
