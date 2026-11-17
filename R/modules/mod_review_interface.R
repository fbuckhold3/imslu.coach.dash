# Review Interface Module
# Displays accordion with 7 review sections when resident is selected
#
# FIXES APPLIED:
# 1. Added "Back to Resident Table" button that works
# 2. Added "Change Coach" button to return to coach selection
# 3. Improved header layout with both navigation options
# 4. Added preview page before submission with milestone spider plot

# Helper function to create review preview with milestone spider plot
create_review_preview <- function(review_data, resident_data, current_period, session_ns) {

  res_info <- resident_data()$resident_info
  period_num <- current_period()
  period_name <- get_period_name(period_num)

  tagList(
    # Header info
    div(
      class = "card mb-3",
      div(
        class = "card-header",
        h4(class = "mb-0", "Review Summary")
      ),
      div(
        class = "card-body",
        p(
          tags$strong("Resident: "), res_info$full_name, br(),
          tags$strong("Level: "), res_info$Level, br(),
          tags$strong("Period: "), PERIOD_NAMES[period_num + 1], br(),
          tags$strong("Date: "), format(Sys.Date(), "%B %d, %Y")
        )
      )
    ),

    # Milestone Spider Plot Visualization
    div(
      class = "card mb-3",
      div(
        class = "card-header",
        h5(class = "mb-0", icon("chart-line"), " Milestone Ratings Visualization")
      ),
      div(
        class = "card-body",
        p(class = "text-info mb-3",
          icon("info-circle"),
          " Your milestone ratings compared to program medians for this period"
        ),
        plotly::plotlyOutput(session_ns("preview_spider_plot"), height = "500px")
      )
    ),

    # Coach Review Text Fields — only render sections that have data
    div(
      class = "card mb-3",
      div(
        class = "card-header",
        h5(class = "mb-0", icon("clipboard"), " Coach Review Entries")
      ),
      div(
        class = "card-body",

        if (!is.null(review_data$wellness)) tagList(
          div(
            class = "mb-3 p-2 border-start border-primary border-3",
            h6(icon("heart"), " Wellness & Progress"),
            p(class = "text-muted small", review_data$wellness$coach_wellness)
          ),
          hr()
        ),

        if (!is.null(review_data$evaluations)) tagList(
          div(
            class = "mb-3 p-2 border-start border-purple border-3",
            h6(icon("clipboard"), " Evaluations & Feedback"),
            div(class = "mb-2",
                strong("Evaluations: "),
                p(class = "text-muted small", review_data$evaluations$coach_evaluations)),
            div(strong("Plus/Delta Comments: "),
                p(class = "text-muted small", review_data$evaluations$coach_p_d_comments))
          ),
          hr()
        ),

        if (!is.null(review_data$learning)) tagList(
          div(
            class = "mb-3 p-2 border-start border-warning border-3",
            h6(icon("book"), " Learning & Board Preparation"),
            div(class = "mb-2",
                strong("Learning Topics & Styles: "),
                p(class = "text-muted small", review_data$learning$coach_ls_and_topic)),
            div(strong("Board Preparation: "),
                p(class = "text-muted small", review_data$learning$coach_step_board))
          ),
          hr()
        ),

        if (!is.null(review_data$career)) tagList(
          div(
            class = "mb-3 p-2 border-start border-info border-3",
            h6(icon("briefcase"), " Career Planning"),
            p(class = "text-muted small", review_data$career$coach_career)
          ),
          hr()
        ),

        if (!is.null(review_data$goals)) tagList(
          div(
            class = "mb-3 p-2 border-start border-success border-3",
            h6(icon("bullseye"), " Goals & ILP"),
            div(class = "mb-2",
                strong("Milestone Goals: "),
                p(class = "text-muted small", review_data$goals$coach_mile_goal)),
            div(strong("ILP Final Comments: "),
                p(class = "text-muted small", review_data$goals$coach_ilp_final))
          )
        ),

        if (!is.null(review_data$grad_plan)) {
          div(
            class = "mb-3 p-2 border-start border-success border-3",
            h6(icon("mortarboard"), " Graduation Plan & Alumni"),
            div(strong("Graduation Summary: "),
                p(class = "text-muted small", review_data$grad_plan$coach_ilp_final))
          )
        },

        if (!is.null(review_data$intro)) {
          div(
            class = "mb-3 p-2 border-start border-primary border-3",
            h6(icon("user-graduate"), " Entering Residency"),
            div(class = "mb-2",
                strong("Background: "),
                p(class = "text-muted small", review_data$intro$coach_intro_back)),
            div(class = "mb-2",
                strong("Coping & Adjustment: "),
                p(class = "text-muted small", review_data$intro$coach_coping)),
            div(strong("Skills, Topics & Learning Styles: "),
                p(class = "text-muted small", review_data$intro$coach_ls_and_topic))
          )
        },

        if (!is.null(review_data$summary) && !is.null(review_data$summary$coach_summary)) {
          div(
            class = "mb-3 p-2 border-start border-primary border-3",
            h6(icon("clipboard-check"), " Overall Summary"),
            p(class = "text-muted small", review_data$summary$coach_summary)
          )
        }
      )
    ),

    # Confirmation message
    div(
      class = "alert alert-warning",
      icon("exclamation-triangle"),
      strong(" Please review all information carefully. "),
      "Once submitted, this review will be saved to REDCap."
    )
  )
}

mod_review_interface_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Inject the small JS handler used by the shell to scroll on advance
    coach_scroll_js(),

    # Header with resident info and navigation buttons
    fluidRow(
      column(
        width = 12,
        div(
          style = "margin-bottom: 12px; display: flex; flex-wrap: wrap; gap: 10px; justify-content: space-between; align-items: center;",

          div(
            style = "display: flex; flex-wrap: wrap; gap: 10px;",
            actionButton(
              ns("back_to_table"),
              "\u2190 Back to Residents",
              class = "btn-secondary",
              icon = icon("table")
            ),
            actionButton(
              ns("change_coach"),
              "Change Coach",
              class = "btn-outline-secondary",
              icon = icon("user-tie")
            )
          ),

          div(
            uiOutput(ns("resident_header"))
          )
        )
      )
    ),

    # CCC monitoring banner — only renders when the latest flagged CCC
    # review is still open (or a quiet note once it's resolved).
    uiOutput(ns("ccc_banner")),

    # Review architecture Phase 3: the self-eval's frozen data + gate
    # override, and the interim-note / CCC thread. No completion badges, so
    # they don't change how sections are marked done.
    div(
      style = "max-width: 1400px; margin: 0 auto;",
      mod_roadmap_ui(ns("roadmap")),
      mod_gate_override_ui(ns("gate_data")),
      mod_interim_note_ui(ns("thread")),
      # P1-5: which coach form this sitting is (Evaluation & ILP / Touch-base)
      uiOutput(ns("form_picker"))
    ),

    # Sticky progress bar \u2014 sections-as-pills + percentage + bar
    coach_progress_bar_ui(ns("progress_bar")),

    # Sequential layout \u2014 sections rendered server-side based on the
    # current period (P1-5 standard, P6 graduating, P7 intern intro).
    div(
      style = "max-width: 1400px; margin: 0 auto; padding-top: 12px;",
      uiOutput(ns("sections_ui"))
    ),

    hr(),

    # Final submission row \u2014 visible always, but only meaningful once
    # the coach has completed enough sections to submit.
    fluidRow(
      column(
        width = 12,
        style = "margin-top: 20px; margin-bottom: 20px;",
        uiOutput(ns("submit_row"))
      )
    )
  )
}

# Period -> ordered list of section names + display labels + icons.
# Names must match the moduleServer ids wired below in the server function.
.coach_sections_for_period <- function(period_num) {
  pn <- suppressWarnings(as.integer(period_num))
  if (is.na(pn)) pn <- 1L

  std <- list(
    wellness    = list(label = "Wellness & Progress",        icon = "heart"),
    evaluations = list(label = "Evaluations & Feedback",     icon = "clipboard"),
    learning    = list(label = "Learning & Board Prep",      icon = "book"),
    scholarship = list(label = "Scholarship",                icon = "mortarboard"),
    career      = list(label = "Career Planning",            icon = "briefcase"),
    goals       = list(label = "Goals & ILP",                icon = "bullseye"),
    milestones  = list(label = "Milestones",                 icon = "graph-up"),
    ilp_summary = list(label = "ILP at a Glance",            icon = "clipboard-check"),
    summary     = list(label = "Summary",                    icon = "list-check")
  )

  if (pn == 6L) {
    # Graduation: drop Wellness, Learning, Career, and Goals — graduating
    # residents don't submit those self-eval blocks. The grad_plan section
    # consolidates career path / chief / contact info AND boards / Step 3
    # (which graduates DO submit) plus the coach summary (coach_ilp_final).
    sec <- std[c("evaluations", "scholarship", "milestones", "summary")]
    sec$grad_plan <- list(label = "Graduation Plan & Alumni",
                          icon  = "mortarboard-fill")
    sec <- sec[c("evaluations", "scholarship", "milestones", "grad_plan",
                 "summary")]
    return(sec)
  }

  if (pn == 0L) {
    # Intern Intro / "Entering Residency". NOTE: this app's local period
    # numbering (PERIOD_NAMES/get_period_number() in globals.R) is 0-based
    # with Entering Residency = 0 — DIFFERENT from gmed's / REDCap's own
    # 7-based scheme (s_e_period, coach_period: "7, Entering Residency").
    # `current_period()` here always carries the LOCAL 0-based number, so
    # this branch (and is_p7 below) must check pn==0L, not 7L. Periods 1-6
    # happen to agree between both schemes, which is why only P7 drifted.
    # When writing coach_period/prog_mile_period back to REDCap, 0 must be
    # translated to the REDCap raw code 7 — see is_p7 handling below.
    #
    # Intern Intro: Background/Concerns/Skills-Topics-Styles (intro_stub) +
    # Career + Goals (s_e_ume_goal1-3, no ILP) + Milestones (self-assessment
    # only, no coach entry) + Summary (adds the overall coach_summary box).
    # No Wellness, Evaluations, or Scholarship. Learning is NOT its own
    # section for P7 — its topics/learning-styles content and comment field
    # are folded into intro_stub instead (see mod_intern_intro.R).
    sec <- std[c("career", "goals", "milestones", "summary")]
    sec$intro_stub <- list(label = "Skills Review & Concerns",
                            icon  = "person-arms-up")
    sec <- sec[c("intro_stub", "career", "goals", "milestones", "summary")]
    return(sec)
  }

  # P1-5 standard: both coach forms; .coach_sections_for_form() splits them.
  std$touch <- list(label = "ILP Check-in", icon = "bullseye")
  std
}

# P1-5 split the review into two coach forms. Touch-base sections; every
# other section belongs to the Evaluation & ILP form.
.COACH_TOUCH_SECTIONS <- c("wellness", "scholarship", "career", "touch")

.coach_is_split <- function(period_num) {
  pn <- suppressWarnings(as.integer(period_num))
  !is.na(pn) && pn %in% 1:5
}

.coach_form_of <- function(section) {
  if (section %in% .COACH_TOUCH_SECTIONS) "touch" else "eval"
}

# Sections of one form ("eval" / "touch"); P6/P7 have a single form.
.coach_sections_for_form <- function(period_num, form) {
  sec <- .coach_sections_for_period(period_num)
  if (!.coach_is_split(period_num)) return(sec)
  sec[vapply(names(sec), .coach_form_of, character(1)) == form]
}

# coach_rev instance for a resident's period (scheduled review).
.coach_rev_instance <- function(res_info, period_num) {
  gmed::get_redcap_instance(level = res_info$Level,
                            period = get_period_name(period_num),
                            review_type = "scheduled")
}

# Render placeholder body for a section that doesn't have its own module
# yet (P7 intern intro). Phase E replaced grad_stub with mod_grad_plan.
.coach_period_stub_body <- function(period_num, kind) {
  msg <- switch(
    kind,
    intro_stub = "Period 7 skills-preparedness ratings, learning goals (s_e_ume_goal1-3), and entering-residency concerns \u2014 coming soon. Wellness / Learning / Milestones are live below.",
    "Coming soon."
  )
  div(
    class = "alert alert-info mb-0",
    style = "font-size: 0.9rem;",
    icon("info-circle"), " ", msg
  )
}

mod_review_interface_server <- function(id, selected_resident, rdm_data, current_period, app_data_rv) {
  moduleServer(id, function(input, output, session) {

    # Local namespace helper (used by renderUI calls below)
    ns <- session$ns

    # Don't extract data_dict here - pass app_data_rv to child modules
    # They will create reactive for data_dict that updates when data loads

    # Reactive to get resident data
    resident_data <- reactive({
      req(selected_resident())
      req(rdm_data())
      req(current_period())

      # Extract values from reactives BEFORE passing to helper
      resident_id <- selected_resident()$record_id
      period_number <- current_period()
      data <- rdm_data()

      # Call helper function with actual values, not reactives
      get_resident_period_data(
        rdm_data = data,           # Actual data, not reactive
        record_id = resident_id,
        current_period = period_number,
        include_previous = TRUE
      )
    })

    # Display resident header
    output$resident_header <- renderUI({
      req(resident_data())
      req(current_period())

      res_data <- resident_data()$resident_info
      period_num <- current_period()

      # Deep link to the resident's own imslu.ind.dash, pre-authed via their
      # access_code — same ?code= pattern imslu.ccc.dashboard uses.
      ac_val <- if ("access_code" %in% names(res_data) &&
                    !is.na(res_data$access_code[1]) &&
                    nzchar(trimws(res_data$access_code[1]))) {
        trimws(res_data$access_code[1])
      } else ""
      ind_dash_url <- paste0(
        "https://fbuckhold3-imsluresidentdashboard.share.connect.posit.cloud",
        if (nzchar(ac_val)) paste0("?code=", ac_val) else ""
      )

      # Name the period under review (and the one "previous" notes come from)
      # up front -- the resident panel's period label alone was easy to miss.
      period_name <- PERIOD_NAMES[period_num + 1]
      prev_name <- unname(get_previous_period(period_name))

      tagList(
      div(
        style = "margin-bottom: 8px;",
        tags$h3(
          style = "margin: 0; font-weight: 700;",
          sprintf("%s review", period_name)
        ),
        tags$div(
          style = "font-size: 0.9rem; color: var(--roundsui-ink-muted, #5B6472);",
          if (is.na(prev_name)) "First review period (no previous period)"
          else sprintf("Previous period shown for comparison: %s", prev_name)
        )
      ),
      div(
        style = "display: flex; align-items: center; gap: 12px; flex-wrap: wrap;",
        roundsui::roundsui_resident_panel(
          resident_name = res_data$full_name,
          level         = res_data$Level,
          period        = period_name
        ),
        tags$a(
          href = ind_dash_url,
          target = "_blank",
          style = "font-size: 0.8rem; font-weight: 600; white-space: nowrap;",
          icon("arrow-up-right-from-square"), " Individual Dashboard"
        )
      )
      )
    })

    # CCC monitoring banner (coach_context.R / coach_context_ui.R). Reads all
    # of the resident's ccc_review records, not just the previous period's.
    output$ccc_banner <- renderUI({
      req(selected_resident(), rdm_data())
      dd <- if (shiny::is.reactivevalues(app_data_rv)) app_data_rv$data_dict else NULL
      ccc_monitoring_banner(
        ctx_ccc_status(rdm_data()$all_forms, selected_resident()$record_id, dd)
      )
    })

    # Self-eval data + gate override; interim notes + CCC thread.
    coach_default_r <- reactive({
      ri <- selected_resident()$resident_info
      v <- if (!is.null(ri) && "coach" %in% names(ri)) as.character(ri$coach[1]) else NA_character_
      if (length(v) == 0 || is.na(v) || !nzchar(v)) NA_character_ else v
    })
    rid_r <- reactive({ req(selected_resident()); as.character(selected_resident()$record_id) })
    mod_gate_override_server("gate_data", record_id_r = rid_r,
                             period_r = reactive({ req(current_period()); current_period() }),
                             coach_name_r = coach_default_r)
    # Bumped after a touch-base save so the thread and roadmap re-read REDCap.
    refresh_rv <- reactiveVal(0)
    thread_forms <- mod_interim_note_server("thread", record_id_r = rid_r,
                                            coach_name_r = coach_default_r,
                                            refresh_r = refresh_rv)
    mod_roadmap_server("roadmap", forms_r = thread_forms, record_id_r = rid_r,
                       current_period = current_period)

    # Touch-base ILP check-in (P1-5)
    touch_data <- mod_touch_base_server(
      "touch", resident_data,
      # Data dictionary, for the milestone-level descriptions on the goal cards.
      data_dict = reactive(if (shiny::is.reactivevalues(app_data_rv)) app_data_rv$data_dict)
    )

    # ---- Which coach form (P1-5) -------------------------------------------
    is_split <- reactive({ req(current_period()); .coach_is_split(current_period()) })
    # Evaluation & ILP is in for this period: form marked complete (raw "2" or
    # the translated label) or a finalized ILP saved -- same rule the roadmap uses.
    eval_done <- reactive({
      rd <- resident_data()
      coach_note_saved_value(rd, "coach_rev_complete") %in% c("2", "Complete") ||
        nzchar(trimws(coach_note_saved_value(rd, "coach_ilp_final")))
    })
    output$form_picker <- renderUI({
      if (!is_split()) return(NULL)
      done <- eval_done()
      div(class = "card mb-3",
          div(class = "card-body", style = "padding: 10px 14px;",
              tags$strong("Coach form for this meeting"),
              radioButtons(ns("form"), NULL, inline = TRUE,
                           choices = c("Evaluation & ILP" = "eval", "Touch-base" = "touch"),
                           selected = if (done) "touch" else "eval"),
              tags$div(class = "text-muted small",
                       if (done) "Evaluation & ILP is submitted for this period. Touch-base is the follow-up meeting: wellness, scholarship, career and an ILP check-in."
                       else "Evaluation & ILP comes first (milestones, ILP, completion; the second review reads it). Touch-base is the later follow-up meeting.")))
    })
    form_r <- reactive({
      if (!is_split()) return("eval")
      f <- input$form
      if (is.null(f) || !f %in% c("eval", "touch")) { if (eval_done()) "touch" else "eval" } else f
    })

    # Call Section 1 module
    wellness_data <- mod_wellness_server("wellness", resident_data, current_period, rdm_data)

    # Call Section 2 module - pass app_data_rv for reactive data_dict access
    evaluations_data <- mod_evaluations_server("evaluations", resident_data, current_period, rdm_data, app_data_rv)

    # Call Section 3 module - pass app_data_rv for reactive data_dict access
    learning_data <- mod_learning_server("learning", resident_data, current_period, rdm_data, app_data_rv)

    # Call Section 4 module
    scholarship_data <- mod_scholarship_server("scholarship", resident_data, current_period, rdm_data)

    # Call Section 5 module
    career_data <- mod_career_server("career", resident_data, current_period, rdm_data)

    # Call Section 6 module (Goals & ILP - moved before Milestones)
    # live_coach_row: the coach's in-progress milestone ratings as a one-row
    # milestone_entry-shaped data frame, so the goal follow-through can use
    # them before submission. milestones_data is created further down; the
    # reactive only looks it up when evaluated, after it exists.
    live_coach_milestones <- reactive({
      md <- tryCatch(milestones_data(), error = function(e) NULL)
      sc <- md$milestone_ratings$scores
      if (is.null(sc) || length(sc) == 0) return(NULL)
      field_map <- unlist(gmed::get_milestone_field_mapping_rdm2("milestone_entry"))
      vals <- unlist(sc)
      keep <- names(vals) %in% names(field_map)
      if (!any(keep)) return(NULL)
      as.data.frame(as.list(stats::setNames(as.character(vals[keep]), field_map[names(vals)[keep]])),
                    stringsAsFactors = FALSE)
    })
    goals_raw <- mod_goals_server("goals", resident_data, current_period, rdm_data, app_data_rv,
                                  live_coach_row = live_coach_milestones)
    # ILP at a Glance section (P1-5, after Milestones): display only (goals,
    # spider, notes). coach_ilp_final is entered in the Summary section.
    # Live inputs for its "At a Glance" row: this review's milestone ratings
    # and the coach's notes from the other Evaluation & ILP sections
    # (wellness / scholarship / career are on the Touch-base form). milestones_data and the
    # section reactives are only looked up when these evaluate.
    ilp_summary_data <- mod_ilp_summary_server(
      "ilp_summary", resident_data, goals_raw,
      milestone_scores = reactive({
        md <- tryCatch(milestones_data(), error = function(e) NULL)
        md$milestone_ratings$scores
      }),
      section_notes = reactive({
        .v <- function(r, f) {
          x <- tryCatch(r(), error = function(e) NULL)
          v <- x[[f]]; if (is.null(v) || length(v) == 0 || is.na(v[1])) "" else as.character(v[1])
        }
        c("Plus/delta reflections"  = .v(evaluations_data, "coach_p_d_comments"),
          "Assessments"             = .v(evaluations_data, "coach_evaluations"),
          "Learning topics & styles" = .v(learning_data, "coach_ls_and_topic"),
          "Boards & ITE"            = .v(learning_data, "coach_step_board"))
      })
    )
    # Downstream (save, preview) reads goals_data()$coach_ilp_final as
    # before; merge it in from the Summary section.
    goals_data <- reactive({
      g <- goals_raw(); if (is.null(g)) return(NULL)
      pn <- suppressWarnings(as.integer(current_period()))
      if (!is.na(pn) && pn %in% 1:5) g$coach_ilp_final <- summary_data()$coach_ilp_final %||% ""
      g
    })

    # P6 Graduation Plan & Alumni — uses gmed grad_plan + boards displays;
    # writes coach_ilp_final (which surfaces in CCC dashboard).
    grad_plan_data <- mod_grad_plan_server("grad_plan", resident_data, current_period,
                                           rdm_data, app_data_rv)

    # P7 Entering Residency — preparedness ratings, first-6-months goals,
    # concerns; writes coach_intro_back + coach_coping.
    intro_data <- mod_intern_intro_server("intro_stub", resident_data, current_period,
                                          rdm_data, app_data_rv)

    # Call Section 7 module (Milestones - moved after ILP)
    milestones_data <- mod_milestones_server("milestones", resident_data, current_period, rdm_data, app_data_rv)

    # Section 8: Summary checklist + review preview (was orphaned; now wired)
    summary_data <- mod_summary_server(
      "summary",
      resident_data,
      wellness_data, evaluations_data, learning_data,
      # goals_raw, not goals_data: goals_data takes coach_ilp_final from
      # this module, so passing it back in would make a reactive loop.
      scholarship_data, career_data, milestones_data, goals_raw,
      grad_plan_data, intro_data,
      current_period = current_period
    )

    # \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
    # Sequential-section shell: progress bar + per-period section list
    # \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

    # Every section of the period (rendered once per period) and the
    # sections of the form being filled in (drives progress + shell state).
    all_sections_r <- reactive({
      req(current_period())
      .coach_sections_for_period(current_period())
    })
    sections_r <- reactive({
      req(current_period())
      .coach_sections_for_form(current_period(), form_r())
    })

    # Shell state: tracks which section is "active" + per-section completion.
    # We rebuild the state when the section list changes (period switch).
    shell_state <- reactiveVal(NULL)
    observeEvent(sections_r(), {
      shell_state(coach_shell_state(names(sections_r())))
    }, ignoreNULL = FALSE)

    # Map each section name to the reactive `is_complete` it exposes.
    section_reactives <- reactive({
      list(
        wellness    = wellness_data,
        evaluations = evaluations_data,
        learning    = learning_data,
        scholarship = scholarship_data,
        career      = career_data,
        goals       = goals_raw,
        milestones  = milestones_data,
        ilp_summary = ilp_summary_data,
        grad_plan   = grad_plan_data,
        summary     = NULL,        # validation-only; complete iff all others
        intro_stub  = intro_data,  # P7 Entering Residency (mod_intern_intro)
        touch       = touch_data   # P1-5 touch-base ILP check-in
      )
    })

    # Push each section module's `is_complete` into shell_state$complete.
    observe({
      st <- shell_state(); req(st)
      reacts <- section_reactives()
      for (nm in names(sections_r())) {
        r <- reacts[[nm]]
        if (is.null(r)) next
        val <- tryCatch(r(), error = function(e) NULL)
        if (!is.null(val) && !is.null(val$is_complete)) {
          st$set_complete(nm, isTRUE(val$is_complete))
        }
      }
    })

    # Render sections sequentially. Section bodies are emitted ONCE per
    # period change \u2014 the textareas inside live across reactive updates so
    # typing isn't lost. Per-section badge + Continue-button warning text
    # are routed through dedicated uiOutput slots so they can update
    # reactively without re-rendering the body.
    output$sections_ui <- renderUI({
      secs <- all_sections_r()                   # only re-render on period change
      split <- .coach_is_split(current_period())
      tagList(lapply(names(secs), function(nm) {
        meta <- secs[[nm]]

        body <- switch(
          nm,
          wellness    = mod_wellness_ui(ns("wellness")),
          evaluations = mod_evaluations_ui(ns("evaluations")),
          learning    = mod_learning_ui(ns("learning")),
          scholarship = mod_scholarship_ui(ns("scholarship")),
          career      = mod_career_ui(ns("career")),
          goals       = mod_goals_ui(ns("goals"), period_num = current_period()),
          milestones  = mod_milestones_ui(ns("milestones"), period_num = current_period()),
          ilp_summary = mod_ilp_summary_ui(ns("ilp_summary")),
          grad_plan   = mod_grad_plan_ui(ns("grad_plan")),
          summary     = mod_summary_ui(ns("summary"), period_num = current_period()),
          intro_stub  = mod_intern_intro_ui(ns("intro_stub")),
          touch       = mod_touch_base_ui(ns("touch")),
          tags$em("Unknown section")
        )

        card <- coach_sec_card(
          name       = nm,
          title      = meta$label,
          icon       = meta$icon,
          status     = "incomplete",   # consumed; badge actually driven by badge_slot
          badge_slot = uiOutput(ns(paste0("badge_", nm)),
                                inline = TRUE),
          body,
          # Skip continue button on the last section of each form
          if (!nm %in% c("summary", "touch"))
            coach_continue_btn(ns, paste0("continue_", nm),
                               label     = "Save & Continue",
                               warn_slot = uiOutput(ns(paste0("warn_", nm)),
                                                    inline = TRUE))
        )
        # P1-5: show only the sections of the selected form. Rendered once
        # and hidden client-side, so typed notes survive switching forms.
        if (!split) return(card)
        frm <- .coach_form_of(nm)
        conditionalPanel(
          sprintf("(input['%s'] || '%s') == '%s'", ns("form"),
                  if (isolate(eval_done())) "touch" else "eval", frm),
          card)
      }))
    })

    # Submit row: Evaluation & ILP -> review + submit; Touch-base -> save.
    output$submit_row <- renderUI({
      if (identical(form_r(), "touch")) {
        div(style = "text-align: center;",
            actionButton(ns("save_touch"), "Save touch-base", class = "btn-primary btn-lg",
                         style = "padding: 10px 30px;", icon = icon("floppy-disk")),
            br(), br(),
            p(style = "color: var(--roundsui-ink-muted); font-size: 14px;",
              "Saves wellness, scholarship and career notes and the ILP check-in to this period's coach review."))
      } else {
        div(style = "text-align: center;",
            actionButton(ns("submit_review"), "Go to Final Review", class = "btn-primary btn-lg",
                         style = "padding: 10px 30px;", icon = icon("eye")),
            br(), br(),
            p(style = "color: var(--roundsui-ink-muted); font-size: 14px;",
              "Complete all required sections, then review and submit"))
      }
    })

    # Per-section reactive renderers for badge + Continue-button warning.
    # These are the ONLY pieces that re-render when shell_state changes,
    # so the textareas in the section bodies keep their state on every
    # keystroke.
    observe({
      secs <- sections_r()
      st   <- shell_state(); req(st)
      for (nm in names(secs)) local({
        sec_name <- nm
        is_active <- identical(sec_name, names(secs)[st$step])

        output[[paste0("badge_", sec_name)]] <- renderUI({
          done <- isTRUE(unname(shell_state()$complete[sec_name]))  # NA when the section is on the other form
          if (done) {
            roundsui::roundsui_status_badge("Complete", status = "complete")
          } else {
            roundsui::roundsui_status_badge("In progress", status = "pending")
          }
        })

        output[[paste0("warn_", sec_name)]] <- renderUI({
          done <- isTRUE(unname(shell_state()$complete[sec_name]))  # NA when the section is on the other form
          if (done) return(NULL)
          tags$span(class = "text-warning ms-2",
            style = "font-size:0.78rem;",
            icon("triangle-exclamation"),
            " Section incomplete \u2014 you can continue, but please return later")
        })
      })
    })

    # Render the sticky progress bar.
    output$progress_bar <- renderUI({
      st <- shell_state(); req(st)
      secs <- sections_r()
      labels <- setNames(vapply(secs, `[[`, character(1), "label"), names(secs))
      active <- names(secs)[st$step]
      render_coach_progress(labels, st$complete, active)
    })

    # Continue-button observers: bump active step + scroll to next section.
    # We register one observer per *possible* section name (matches input ids).
    .all_section_names <- c("wellness", "evaluations", "learning",
                            "scholarship", "career", "goals", "milestones",
                            "ilp_summary", "grad_plan", "intro_stub")
    lapply(.all_section_names, function(nm) {
      observeEvent(input[[paste0("continue_", nm)]], {
        st <- shell_state(); req(st)
        secs <- sections_r()
        if (!nm %in% names(secs)) return()
        idx <- match(nm, names(secs))
        if (is.na(idx)) return()
        # Advance only if the user is on (or past) this section.
        if (idx >= st$step) st$advance()
        nxt_idx <- min(idx + 1L, length(secs))
        nxt <- names(secs)[nxt_idx]
        session$sendCustomMessage("coach_scroll_to", nxt)
      }, ignoreInit = TRUE)
    })

    # Back to table button - returns reactive that triggers navigation
    back_to_table_clicked <- reactive({
      input$back_to_table
    })

    # Change coach button - returns reactive that triggers navigation to coach select
    change_coach_clicked <- reactive({
      input$change_coach
    })

    # Submit button
    submit_clicked <- reactive({
      input$submit_review
    })

    # Signal fired when user clicks "Return to Resident Table" inside the
    # post-submission success modal.  The parent app observes this and
    # handles navigation + data refresh.
    post_submit_return_rv <- reactiveVal(0)
    
    # Handle back to table
    observeEvent(input$back_to_table, {
      message(sprintf(
        "[%s] Back to table button clicked",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ))
      
      showNotification(
        "Returning to resident table",
        type = "message",
        duration = 2
      )
    })
    
    # Handle change coach
    observeEvent(input$change_coach, {
      message(sprintf(
        "[%s] Change coach button clicked",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ))
      
      showNotification(
        "Returning to coach selection",
        type = "message",
        duration = 2
      )
    })
    
    # Handle submission
    observeEvent(input$submit_review, {
      req(resident_data())
      req(current_period())

      pn <- as.integer(current_period())
      is_p6 <- identical(pn, 6L)
      is_p7 <- identical(pn, 0L)  # local scheme: 0 = Entering Residency (see .coach_sections_for_period)

      # Required sections vary by period; only req() what we'll actually
      # consume so P6/P7 don't get stuck waiting on dropped sections.
      if (!is_p6 && !is_p7) {
        # Wellness / Scholarship / Career are on the Touch-base form now.
        req(evaluations_data()); req(learning_data())
        req(milestones_data()); req(goals_data())
      } else if (is_p6) {
        req(evaluations_data()); req(scholarship_data())
        req(milestones_data()); req(grad_plan_data())
      } else {
        req(wellness_data()); req(learning_data()); req(milestones_data())
        req(goals_data()); req(intro_data())
      }

      # Collect data from all sections (NULL-safe for dropped sections)
      review_data <- list(
        wellness    = if (!is.null(wellness_data()))    wellness_data()    else NULL,
        evaluations = if (!is.null(evaluations_data())) evaluations_data() else NULL,
        learning    = if (!is.null(learning_data()))    learning_data()    else NULL,
        scholarship = if (!is.null(scholarship_data())) scholarship_data() else NULL,
        career      = if (!is.null(career_data()))      career_data()      else NULL,
        goals       = if (!is.null(goals_data()))       goals_data()       else NULL,
        milestones  = if (!is.null(milestones_data()))  milestones_data()  else NULL,
        grad_plan   = if (!is.null(grad_plan_data()))   grad_plan_data()   else NULL,
        intro       = if (!is.null(intro_data()))       intro_data()       else NULL,
        summary     = if (!is.null(summary_data()))     summary_data()     else NULL
      )
      # P1-5: the Touch-base form owns these; not part of this submission.
      if (.coach_is_split(pn)) review_data[c("wellness", "scholarship", "career")] <- list(NULL)

      # Validate per-period required sections
      incomplete_sections <- c()
      if (is_p6) {
        if (!isTRUE(review_data$evaluations$is_complete)) incomplete_sections <- c(incomplete_sections, "Evaluations")
        if (!isTRUE(review_data$milestones$is_complete))  incomplete_sections <- c(incomplete_sections, "Milestones")
        if (!isTRUE(review_data$grad_plan$is_complete))   incomplete_sections <- c(incomplete_sections, "Graduation Plan")
      } else if (is_p7) {
        # No Wellness/Learning for P7 (neither section is shown — Learning's
        # topics/styles moved into intro_stub). Milestones has no coach
        # entry for P7 (always is_complete=TRUE — see mod_milestones.R).
        if (!isTRUE(review_data$intro$is_complete)) incomplete_sections <- c(incomplete_sections, "Entering Residency")
      } else {
        if (!isTRUE(review_data$evaluations$is_complete)) incomplete_sections <- c(incomplete_sections, "Evaluations")
        if (!isTRUE(review_data$learning$is_complete))    incomplete_sections <- c(incomplete_sections, "Learning")
        if (!isTRUE(review_data$milestones$is_complete))  incomplete_sections <- c(incomplete_sections, "Milestones")
      }

      if (length(incomplete_sections) > 0) {
        showNotification(
          paste("Please complete the following sections before submitting:", paste(incomplete_sections, collapse = ", ")),
          type = "warning",
          duration = 5
        )
        return()
      }

      # Show preview modal with all data before submission
      showModal(modalDialog(
        title = tagList(
          icon("eye"),
          " Review Before Submitting"
        ),
        size = "xl",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            session$ns("confirm_submit"),
            "Submit All",
            class = "btn-primary",
            icon = icon("check-circle")
          )
        ),

        # Preview content
        create_review_preview(
          review_data = review_data,
          resident_data = resident_data,
          current_period = current_period,
          session_ns = session$ns
        )
      ))

      # Render the spider plot for the preview
      output$preview_spider_plot <- plotly::renderPlotly({
        req(review_data$milestones$milestone_ratings)

        milestone_ratings <- review_data$milestones$milestone_ratings

        # Debug logging
        message("=== Preview Spider Plot Debug ===")
        message("milestone_ratings structure: ", paste(names(milestone_ratings), collapse = ", "))
        message("Has scores: ", !is.null(milestone_ratings$scores))

        # Check if we have scores
        if (is.null(milestone_ratings$scores) || length(milestone_ratings$scores) == 0) {
          message("ERROR: No milestone scores available")
          return(plotly::plotly_empty() %>%
                   plotly::add_annotations(
                     text = "No milestone ratings entered yet",
                     x = 0.5, y = 0.5, showarrow = FALSE,
                     font = list(size = 16, color = "gray")
                   ))
        }

        tryCatch({
          # Get the current period name
          period_name <- get_period_name(current_period())
          record_id <- resident_data()$resident_info$record_id

          message("Creating spider plot from entered scores...")
          message("Number of scores: ", length(milestone_ratings$scores))

          # Convert gmed field names to display names
          field_mapping <- c(
            "PC_1" = "PC1: History",
            "PC_2" = "PC2: Physical Exam",
            "PC_3" = "PC3: Clinical Reasoning",
            "PC_4" = "PC4: Mgmt-Inpatient",
            "PC_5" = "PC5: Mgmt-Outpatient",
            "PC_6" = "PC6: Digital Health",
            "MK_1" = "MK1: Applied Sciences",
            "MK_2" = "MK2: Therapeutics",
            "MK_3" = "MK3: Diagnostics",
            "SBP_1" = "SBP1: Safety & QI",
            "SBP_2" = "SBP2: Navigation",
            "SBP_3" = "SBP3: Physician Role",
            "PBLI_1" = "PBLI1: Evidence-Based",
            "PBLI_2" = "PBLI2: Reflective",
            "PROF_1" = "PROF1: Behavior",
            "PROF_2" = "PROF2: Ethics",
            "PROF_3" = "PROF3: Accountability",
            "PROF_4" = "PROF4: Well-Being",
            "ICS_1" = "ICS1: Patient Comm",
            "ICS_2" = "ICS2: Team Comm",
            "ICS_3" = "ICS3: Documentation"
          )

          # Extract scores and create vectors for plotting
          categories <- character()
          values <- numeric()

          for (field in names(milestone_ratings$scores)) {
            if (field %in% names(field_mapping)) {
              categories <- c(categories, field_mapping[[field]])
              values <- c(values, as.numeric(milestone_ratings$scores[[field]]))
            }
          }

          message("Categories: ", paste(categories, collapse = ", "))
          message("Values: ", paste(values, collapse = ", "))

          # Create spider plot using plotly
          plot <- plotly::plot_ly(
            type = 'scatterpolar',
            mode = 'lines+markers',
            fill = 'toself'
          ) %>%
            plotly::add_trace(
              r = values,
              theta = categories,
              name = 'Your Ratings',
              fillcolor = plotly::toRGB(roundsui::roundsui_colors()$accent, 0.25),
              line = list(color = roundsui::roundsui_colors()$accent, width = 2),
              marker = list(size = 8, color = roundsui::roundsui_colors()$accent)
            ) %>%
            plotly::layout(
              polar = list(
                radialaxis = list(
                  visible = TRUE,
                  range = c(0, 9),
                  tickmode = 'linear',
                  tick0 = 0,
                  dtick = 1
                )
              ),
              title = paste("Milestone Ratings -", period_name),
              showlegend = TRUE
            ) %>%
            roundsui::roundsui_plotly_layout()

          message("Spider plot created successfully")
          return(plot)

        }, error = function(e) {
          message("ERROR creating preview spider plot: ", e$message)
          return(plotly::plotly_empty() %>%
                   plotly::add_annotations(
                     text = paste("Unable to create visualization:", e$message),
                     x = 0.5, y = 0.5, showarrow = FALSE,
                     font = list(size = 14, color = "orange")
                   ))
        })
      })
    })

    # Handle confirmed submission
    observeEvent(input$confirm_submit, {
      req(resident_data())
      req(current_period())

      pn <- as.integer(current_period())
      is_p6 <- identical(pn, 6L)
      is_p7 <- identical(pn, 0L)  # local scheme: 0 = Entering Residency (see .coach_sections_for_period)

      # Close preview modal
      removeModal()

      # Collect data from all sections (NULL-safe per-period)
      review_data <- list(
        wellness    = if (!is.null(wellness_data()))    wellness_data()    else NULL,
        evaluations = if (!is.null(evaluations_data())) evaluations_data() else NULL,
        learning    = if (!is.null(learning_data()))    learning_data()    else NULL,
        scholarship = if (!is.null(scholarship_data())) scholarship_data() else NULL,
        career      = if (!is.null(career_data()))      career_data()      else NULL,
        goals       = if (!is.null(goals_data()))       goals_data()       else NULL,
        milestones  = if (!is.null(milestones_data()))  milestones_data()  else NULL,
        grad_plan   = if (!is.null(grad_plan_data()))   grad_plan_data()   else NULL,
        intro       = if (!is.null(intro_data()))       intro_data()       else NULL,
        summary     = if (!is.null(summary_data()))     summary_data()     else NULL
      )
      # P1-5: the Touch-base form owns these; not part of this submission.
      if (.coach_is_split(pn)) review_data[c("wellness", "scholarship", "career")] <- list(NULL)

      # Show processing notification
      notification_id <- showNotification(
        tagList(icon("spinner", class = "fa-spin"), " Submitting coaching review..."),
        duration = NULL
      )

      # Build REDCap payload
      tryCatch({
        res_info <- resident_data()$resident_info
        period_num <- current_period()
        period_name <- get_period_name(period_num)

        # REDCap's own period dropdowns (coach_period, prog_mile_period,
        # s_e_period) use "7" for Entering Residency; this app's local
        # period_num is 0-based ("Entering Residency" = 0). Translate ONLY
        # at the REDCap-write boundary — don't change period_num itself,
        # since get_period_name()/PERIOD_NAMES indexing depend on the local
        # 0-based value.
        period_redcap_code <- if (identical(suppressWarnings(as.integer(period_num)), 0L)) 7L else period_num

        # gmed::get_redcap_instance keys on string level ("Intern"/"PGY2"/"PGY3")
        # — pass res_info$Level directly so the direct mapping resolves
        # (e.g., PGY3 + "Graduating" -> instance 6, not the default fallback 8).
        level_str <- res_info$Level

        # Calculate instance for coach_rev form
        instance <- gmed::get_redcap_instance(
          level = level_str,
          period = period_name,
          review_type = "scheduled"
        )

        # Build coach_rev submission record (raw format, date and period included)
        coach_rev_record <- data.frame(
          record_id = res_info$record_id,
          redcap_repeat_instrument = "coach_rev",
          redcap_repeat_instance = instance,
          coach_date = format(Sys.Date(), "%Y-%m-%d"),  # REDCap date format
          coach_period = as.character(period_redcap_code), # REDCap raw code (7=Entering Residency)
          coach_wellness = as.character(review_data$wellness$coach_wellness %||% ""),
          coach_evaluations = as.character(review_data$evaluations$coach_evaluations %||% ""),
          coach_p_d_comments = as.character(review_data$evaluations$coach_p_d_comments %||% ""),
          # P7 folds Topics/Learning Styles comments into intro_stub instead
          # of the (unshown) Learning section.
          coach_ls_and_topic = as.character(
            if (is_p7) (review_data$intro$coach_ls_and_topic %||% "")
            else (review_data$learning$coach_ls_and_topic %||% "")
          ),
          coach_step_board = as.character(review_data$learning$coach_step_board %||% ""),
          coach_career = as.character(review_data$career$coach_career %||% ""),
          coach_scholarship = as.character(review_data$scholarship$coach_scholarship %||% ""),
          coach_mile_goal = as.character(review_data$goals$coach_mile_goal %||% ""),
          coach_goal_pcmk    = as.character(review_data$goals$coach_goal_pcmk    %||% ""),
          coach_goal_sbppbl  = as.character(review_data$goals$coach_goal_sbppbl  %||% ""),
          coach_goal_profics = as.character(review_data$goals$coach_goal_profics %||% ""),
          coach_ilp_final = as.character(
            if (is_p6) (review_data$grad_plan$coach_ilp_final %||% "")
            else (review_data$goals$coach_ilp_final %||% "")
          ),
          coach_intro_back = as.character(
            if (is_p7) (review_data$intro$coach_intro_back %||% "") else ""
          ),
          coach_coping = as.character(
            if (is_p7) (review_data$intro$coach_coping %||% "") else ""
          ),
          # Overall summary lives on mod_summary.R's P7-only box now (the
          # last step in the flow), not intro_stub.
          coach_summary = as.character(
            if (is_p7) (review_data$summary$coach_summary %||% "") else ""
          ),
          coach_rev_complete = "2",  # Raw format (string)
          stringsAsFactors = FALSE
        )
        # P1-5: wellness and career are written by the Touch-base save; leave
        # whatever it saved alone.
        if (.coach_is_split(period_num)) {
          coach_rev_record$coach_wellness <- NULL
          coach_rev_record$coach_career   <- NULL
          coach_rev_record$coach_scholarship <- NULL
        }

        # Submit coach_rev to REDCap using REDCapR
        coach_result <- tryCatch({
          result <- REDCapR::redcap_write_oneshot(
            ds = coach_rev_record,
            redcap_uri = REDCAP_CONFIG$url,
            token = REDCAP_CONFIG$rdm_token
          )

          list(
            success = result$success,
            message = if (result$success) "Coach review submitted" else result$outcome_message
          )
        }, error = function(e) {
          list(success = FALSE, message = e$message)
        })

        # Build milestone_entry submission record (raw format) — skipped
        # entirely for P7: coaches don't rate baseline milestones for
        # incoming interns (mod_milestones.R shows self-assessment only, no
        # entry UI), so there's nothing to submit.
        if (is_p7) {
          milestone_result <- list(success = TRUE, message = "No coach milestone entry for Entering Residency")
        } else {
        milestone_ratings <- review_data$milestones$milestone_ratings

        # Helper function to convert gmed field names to REDCap field names
        # Converts: PC_1 -> rep_pc1, MK_2 -> rep_mk2, PROF_1 -> rep_prof1, PBLI_1 -> rep_pbl1
        convert_to_redcap_field <- function(gmed_field) {
          # Convert from format like "PC_1" or "PROF_1" to "rep_pc1" or "rep_prof1"
          redcap_field <- tolower(gmed_field)  # Convert to lowercase

          # Special case: PBLI should become PBL in REDCap
          redcap_field <- gsub("pbli", "pbl", redcap_field)

          redcap_field <- gsub("_", "", redcap_field)  # Remove underscore
          redcap_field <- paste0("rep_", redcap_field)  # Add rep_ prefix
          return(redcap_field)
        }

        milestone_record <- data.frame(
          record_id = res_info$record_id,
          redcap_repeat_instrument = "milestone_entry",
          redcap_repeat_instance = instance,
          prog_mile_date = format(Sys.Date(), "%Y-%m-%d"),  # REDCap date format
          prog_mile_period = as.character(period_redcap_code), # REDCap raw code — NOTE: dictionary labels choice 7 "Error", flagged separately
          stringsAsFactors = FALSE
        )

        # Add milestone scores and descriptions if available
        if (!is.null(milestone_ratings$scores) && length(milestone_ratings$scores) > 0) {
          for (field_name in names(milestone_ratings$scores)) {
            # Convert field name to REDCap format
            redcap_field <- convert_to_redcap_field(field_name)
            milestone_record[[redcap_field]] <- as.character(milestone_ratings$scores[[field_name]])
          }
        }

        if (!is.null(milestone_ratings$descriptions) && length(milestone_ratings$descriptions) > 0) {
          for (field_name in names(milestone_ratings$descriptions)) {
            # Convert field name to REDCap format and add _desc suffix
            redcap_field <- paste0(convert_to_redcap_field(field_name), "_desc")
            milestone_record[[redcap_field]] <- as.character(milestone_ratings$descriptions[[field_name]])
          }
        }

        # Add milestone completion status (raw format)
        milestone_record$milestone_entry_complete <- "2"

        # Submit milestone_entry to REDCap using REDCapR
        milestone_result <- tryCatch({
          result <- REDCapR::redcap_write_oneshot(
            ds = milestone_record,
            redcap_uri = REDCAP_CONFIG$url,
            token = REDCAP_CONFIG$rdm_token
          )

          list(
            success = result$success,
            message = if (result$success) "Milestones submitted" else result$outcome_message
          )
        }, error = function(e) {
          list(success = FALSE, message = e$message)
        })
        }

        # Remove processing notification
        removeNotification(notification_id)

        # Check results
        if (coach_result$success && milestone_result$success) {
          # Show success modal with summary
          showModal(modalDialog(
            title = tagList(
              icon("check-circle", class = "text-success"),
              sprintf(" Review Submitted Successfully")
            ),
            size = "l",
            easyClose = TRUE,
            footer = tagList(
              modalButton("Close"),
              actionButton(
                session$ns("return_to_table"),
                "Return to Resident Table",
                class = "btn-primary"
              )
            ),

            # Summary of submitted data
            div(
              h4("Submitted Coaching Review"),
              p(
                tags$strong("Resident: "), res_info$full_name, br(),
                tags$strong("Period: "), PERIOD_NAMES[period_num + 1], br(),
                tags$strong("Date: "), format(Sys.Date(), "%B %d, %Y"), br(),
                tags$strong("Instance: "), instance
              ),

              hr(),

              local({
                .nl <- function(x) gsub("\n", "<br>", as.character(x %||% ""))
                .well_div <- function(...) div(
                  class = "well",
                  style = "background-color: var(--roundsui-surface-2); padding: 10px; max-height: 150px; overflow-y: auto;",
                  ...
                )
                tagList(
                  if (!is.null(review_data$wellness)) tagList(
                    h5(icon("heart"), " Wellness & Progress"),
                    .well_div(HTML(.nl(review_data$wellness$coach_wellness)))
                  ),
                  if (!is.null(review_data$evaluations)) tagList(
                    h5(icon("clipboard"), " Evaluations & Feedback"),
                    .well_div(
                      tags$strong("Evaluations: "),
                      HTML(.nl(review_data$evaluations$coach_evaluations)),
                      br(), br(),
                      tags$strong("Plus/Delta Comments: "),
                      HTML(.nl(review_data$evaluations$coach_p_d_comments))
                    )
                  ),
                  if (!is.null(review_data$learning)) tagList(
                    h5(icon("book"), " Learning & Board Preparation"),
                    .well_div(
                      tags$strong("Learning Topics & Styles: "),
                      HTML(.nl(review_data$learning$coach_ls_and_topic)),
                      br(), br(),
                      tags$strong("Board Preparation: "),
                      HTML(.nl(review_data$learning$coach_step_board))
                    )
                  ),
                  if (!is.null(review_data$career)) tagList(
                    h5(icon("briefcase"), " Career Planning"),
                    .well_div(HTML(.nl(review_data$career$coach_career)))
                  ),
                  if (!is.null(review_data$goals)) tagList(
                    h5(icon("bullseye"), " Goals & ILP"),
                    .well_div(
                      tags$strong("Milestone Goals: "),
                      HTML(.nl(review_data$goals$coach_mile_goal)),
                      br(), br(),
                      tags$strong("ILP Final Comments: "),
                      HTML(.nl(review_data$goals$coach_ilp_final))
                    )
                  ),
                  if (!is.null(review_data$grad_plan)) tagList(
                    h5(icon("mortarboard"), " Graduation Plan & Alumni"),
                    .well_div(HTML(.nl(review_data$grad_plan$coach_ilp_final)))
                  )
                )
              }),

              hr(),

              div(
                class = "alert alert-info",
                icon("info-circle"),
                " Milestone ratings have also been submitted successfully."
              )
            )
          ))

          # Handle return to table button in modal.
          # Removes the modal and increments post_submit_return_rv so the
          # parent app's observeEvent fires and handles navigation + refresh.
          observeEvent(input$return_to_table, {
            removeModal()
            post_submit_return_rv(post_submit_return_rv() + 1)
          })

        } else {
          error_msg <- c()
          if (!coach_result$success) error_msg <- c(error_msg, paste("Coach Review:", coach_result$message))
          if (!milestone_result$success) error_msg <- c(error_msg, paste("Milestones:", milestone_result$message))

          showNotification(
            tagList(
              icon("times-circle"),
              " Submission failed: ",
              paste(error_msg, collapse = "; ")
            ),
            type = "error",
            duration = 15
          )
        }

      }, error = function(e) {
        removeNotification(notification_id)
        showNotification(
          tagList(
            icon("times-circle"),
            " Submission error: ",
            e$message
          ),
          type = "error",
          duration = 15
        )
      })
    })
    
    # ---- Touch-base save (P1-5) ---------------------------------------------
    # Same coach_rev instance as the Evaluation & ILP form. Writes only the
    # touch-base fields and never coach_rev_complete (that's the evaluation's).
    observeEvent(input$save_touch, {
      req(resident_data(), current_period(), is_split())
      td <- touch_data()
      if (!isTRUE(td$is_complete)) {
        showNotification("Add the ILP check-in and the touch-base date before saving.",
                         type = "warning", duration = 6)
        return()
      }
      res_info <- resident_data()$resident_info
      period_num <- current_period()
      .val <- function(r, f) as.character((if (is.null(r)) NULL else r[[f]]) %||% "")
      rec <- data.frame(
        record_id                = res_info$record_id,
        redcap_repeat_instrument = "coach_rev",
        redcap_repeat_instance   = .coach_rev_instance(res_info, period_num),
        coach_period             = coach_period_code(period_num),
        coach_wellness           = .val(wellness_data(), "coach_wellness"),
        coach_scholarship        = .val(scholarship_data(), "coach_scholarship"),
        coach_career             = .val(career_data(), "coach_career"),
        coach_ilp_checkin        = td$coach_ilp_checkin,
        coach_touch_date         = td$coach_touch_date,
        stringsAsFactors = FALSE
      )
      res <- tryCatch(
        REDCapR::redcap_write_oneshot(ds = rec, redcap_uri = REDCAP_CONFIG$url,
                                      token = REDCAP_CONFIG$rdm_token, verbose = FALSE),
        error = function(e) list(success = FALSE, outcome_message = conditionMessage(e)))
      if (!isTRUE(res$success)) {
        showNotification(paste("Touch-base not saved:", res$outcome_message %||% "unknown error"),
                         type = "error", duration = 15)
        return()
      }
      message(sprintf("[touch-base] saved record=%s instance=%s", rec$record_id,
                      rec$redcap_repeat_instance))
      refresh_rv(refresh_rv() + 1)
      showModal(modalDialog(
        title = tagList(icon("check-circle", class = "text-success"), " Touch-base saved"),
        easyClose = TRUE,
        footer = tagList(modalButton("Close"),
                         actionButton(ns("touch_return"), "Return to Resident Table",
                                      class = "btn-primary")),
        p(tags$strong("Resident: "), res_info$full_name, br(),
          tags$strong("Period: "), PERIOD_NAMES[period_num + 1], br(),
          tags$strong("Date: "), td$coach_touch_date),
        tags$strong("ILP check-in"),
        div(style = "white-space: pre-wrap;", td$coach_ilp_checkin)))
    })
    observeEvent(input$touch_return, {
      removeModal()
      post_submit_return_rv(post_submit_return_rv() + 1)
    })

    # Return reactive values
    return(
      list(
        back_to_table_clicked  = back_to_table_clicked,
        change_coach_clicked   = change_coach_clicked,
        submit_clicked         = submit_clicked,
        post_submit_return     = post_submit_return_rv
      )
    )
  })
}