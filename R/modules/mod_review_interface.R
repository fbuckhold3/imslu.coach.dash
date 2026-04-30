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
        class = "card-header bg-primary text-white",
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
        class = "card-header bg-info text-white",
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
        class = "card-header bg-success text-white",
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
          style = "margin-bottom: 12px; display: flex; justify-content: space-between; align-items: center;",

          div(
            style = "display: flex; gap: 10px;",
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
            style = "font-size: 16px; font-weight: bold;",
            uiOutput(ns("resident_header"))
          )
        )
      )
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
        div(
          style = "text-align: center;",
          actionButton(
            ns("submit_review"),
            "Go to Final Review",
            class = "btn-primary btn-lg",
            style = "padding: 10px 30px;",
            icon = icon("eye")
          ),
          br(),
          br(),
          p(
            style = "color: #666; font-size: 14px;",
            "Complete all required sections, then review and submit"
          )
        )
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

  if (pn == 7L) {
    # Intern Intro: Skills Review + Concerns + initial Goals; no
    # Evaluations / Scholarship / Career.
    sec <- std[c("wellness", "learning", "goals", "milestones", "summary")]
    sec$intro_stub <- list(label = "Skills Review & Concerns",
                            icon  = "person-arms-up")
    sec <- sec[c("intro_stub", "wellness", "learning", "goals",
                 "milestones", "summary")]
    return(sec)
  }

  # P1-5 standard
  std
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
    style = "border-left: 4px solid #0d6efd; font-size: 0.9rem;",
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

      HTML(sprintf(
        "<span style='color: #2c3e50;'>%s</span> | <span style='color: #7f8c8d;'>%s | Period: %s</span>",
        res_data$full_name,
        res_data$Level,
        PERIOD_NAMES[period_num + 1]
      ))
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
    goals_data <- mod_goals_server("goals", resident_data, current_period, rdm_data, app_data_rv)

    # P6 Graduation Plan & Alumni — uses gmed grad_plan + boards displays;
    # writes coach_ilp_final (which surfaces in CCC dashboard).
    grad_plan_data <- mod_grad_plan_server("grad_plan", resident_data, current_period,
                                           rdm_data, app_data_rv)

    # Call Section 7 module (Milestones - moved after ILP)
    milestones_data <- mod_milestones_server("milestones", resident_data, current_period, rdm_data, app_data_rv)

    # Section 8: Summary checklist + review preview (was orphaned; now wired)
    summary_data <- mod_summary_server(
      "summary",
      wellness_data, evaluations_data, learning_data,
      scholarship_data, career_data, milestones_data, goals_data,
      grad_plan_data,
      current_period = current_period
    )

    # \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
    # Sequential-section shell: progress bar + per-period section list
    # \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

    # Period-aware section list \u2014 recomputed when the period changes.
    sections_r <- reactive({
      req(current_period())
      .coach_sections_for_period(current_period())
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
        goals       = goals_data,
        milestones  = milestones_data,
        grad_plan   = grad_plan_data,
        summary     = NULL,        # validation-only; complete iff all others
        intro_stub  = NULL         # placeholder, soft-skip (P7)
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
      secs <- sections_r()                       # only re-render on period change
      tagList(lapply(names(secs), function(nm) {
        meta <- secs[[nm]]

        body <- switch(
          nm,
          wellness    = mod_wellness_ui(ns("wellness")),
          evaluations = mod_evaluations_ui(ns("evaluations")),
          learning    = mod_learning_ui(ns("learning")),
          scholarship = mod_scholarship_ui(ns("scholarship")),
          career      = mod_career_ui(ns("career")),
          goals       = mod_goals_ui(ns("goals")),
          milestones  = mod_milestones_ui(ns("milestones")),
          grad_plan   = mod_grad_plan_ui(ns("grad_plan")),
          summary     = mod_summary_ui(ns("summary")),
          intro_stub  = .coach_period_stub_body(current_period(), "intro_stub"),
          tags$em("Unknown section")
        )

        coach_sec_card(
          name       = nm,
          title      = meta$label,
          icon       = meta$icon,
          status     = "incomplete",   # consumed; badge actually driven by badge_slot
          badge_slot = uiOutput(ns(paste0("badge_", nm)),
                                inline = TRUE),
          body,
          # Skip continue button on the final summary section
          if (!identical(nm, "summary"))
            coach_continue_btn(ns, paste0("continue_", nm),
                               label     = "Save & Continue",
                               warn_slot = uiOutput(ns(paste0("warn_", nm)),
                                                    inline = TRUE))
        )
      }))
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
          done <- isTRUE(shell_state()$complete[[sec_name]])
          if (done) {
            tags$span(class = "badge",
              style = "background:#198754; color:#fff; font-weight:600;",
              tags$i(class = "bi bi-check-circle-fill me-1"), "Complete")
          } else {
            tags$span(class = "badge",
              style = "background:#e5e7eb; color:#475569; font-weight:600;",
              "In progress")
          }
        })

        output[[paste0("warn_", sec_name)]] <- renderUI({
          done <- isTRUE(shell_state()$complete[[sec_name]])
          if (done) return(NULL)
          tags$span(class = "text-warning ms-2",
            style = "font-size:0.78rem;",
            tags$i(class = "bi bi-exclamation-triangle-fill me-1"),
            "Section incomplete \u2014 you can continue, but please return later")
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
                            "grad_plan", "intro_stub")
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
      is_p7 <- identical(pn, 7L)

      # Required sections vary by period; only req() what we'll actually
      # consume so P6/P7 don't get stuck waiting on dropped sections.
      if (!is_p6 && !is_p7) {
        req(wellness_data()); req(evaluations_data()); req(learning_data())
        req(scholarship_data()); req(career_data()); req(milestones_data())
        req(goals_data())
      } else if (is_p6) {
        req(evaluations_data()); req(scholarship_data())
        req(milestones_data()); req(grad_plan_data())
      } else {
        req(wellness_data()); req(learning_data()); req(milestones_data())
        req(goals_data())
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
        grad_plan   = if (!is.null(grad_plan_data()))   grad_plan_data()   else NULL
      )

      # Validate per-period required sections
      incomplete_sections <- c()
      if (is_p6) {
        if (!isTRUE(review_data$evaluations$is_complete)) incomplete_sections <- c(incomplete_sections, "Evaluations")
        if (!isTRUE(review_data$milestones$is_complete))  incomplete_sections <- c(incomplete_sections, "Milestones")
        if (!isTRUE(review_data$grad_plan$is_complete))   incomplete_sections <- c(incomplete_sections, "Graduation Plan")
      } else if (is_p7) {
        if (!isTRUE(review_data$wellness$is_complete))   incomplete_sections <- c(incomplete_sections, "Wellness")
        if (!isTRUE(review_data$learning$is_complete))   incomplete_sections <- c(incomplete_sections, "Learning")
        if (!isTRUE(review_data$milestones$is_complete)) incomplete_sections <- c(incomplete_sections, "Milestones")
      } else {
        if (!isTRUE(review_data$wellness$is_complete))    incomplete_sections <- c(incomplete_sections, "Wellness")
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
              fillcolor = 'rgba(31, 119, 180, 0.3)',
              line = list(color = 'rgb(31, 119, 180)', width = 2),
              marker = list(size = 8, color = 'rgb(31, 119, 180)')
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
            )

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
        grad_plan   = if (!is.null(grad_plan_data()))   grad_plan_data()   else NULL
      )

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

        # Extract numeric level from Level field (e.g., "PGY2" -> 2, "Intern" -> 1)
        level_num <- case_when(
          res_info$Level == "Intern" ~ 1,
          res_info$Level == "PGY2" ~ 2,
          res_info$Level == "PGY3" ~ 3,
          grepl("PGY[0-9]", res_info$Level) ~ as.numeric(gsub("[^0-9]", "", res_info$Level)),
          TRUE ~ NA_real_
        )

        # Calculate instance for coach_rev form
        instance <- gmed::get_redcap_instance(
          level = level_num,
          period = period_name,
          review_type = "scheduled"
        )

        # Build coach_rev submission record (raw format, date and period included)
        coach_rev_record <- data.frame(
          record_id = res_info$record_id,
          redcap_repeat_instrument = "coach_rev",
          redcap_repeat_instance = instance,
          coach_date = format(Sys.Date(), "%Y-%m-%d"),  # REDCap date format
          coach_period = as.character(period_num),       # Raw format (numeric as string)
          coach_wellness = as.character(review_data$wellness$coach_wellness %||% ""),
          coach_evaluations = as.character(review_data$evaluations$coach_evaluations %||% ""),
          coach_p_d_comments = as.character(review_data$evaluations$coach_p_d_comments %||% ""),
          coach_ls_and_topic = as.character(review_data$learning$coach_ls_and_topic %||% ""),
          coach_step_board = as.character(review_data$learning$coach_step_board %||% ""),
          coach_career = as.character(review_data$career$coach_career %||% ""),
          coach_mile_goal = as.character(review_data$goals$coach_mile_goal %||% ""),
          coach_ilp_final = as.character(
            if (is_p6) (review_data$grad_plan$coach_ilp_final %||% "")
            else (review_data$goals$coach_ilp_final %||% "")
          ),
          coach_rev_complete = "2",  # Raw format (string)
          stringsAsFactors = FALSE
        )

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

        # Build milestone_entry submission record (raw format)
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
          prog_mile_period = as.character(period_num),       # Raw format
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
                  style = "background-color: #f8f9fa; padding: 10px; max-height: 150px; overflow-y: auto;",
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

          # Handle return to table button in modal
          observeEvent(input$return_to_table, {
            removeModal()
            # Trigger back to table navigation
            # The parent app should handle this
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
    
    # Return reactive values
    return(
      list(
        back_to_table_clicked = back_to_table_clicked,
        change_coach_clicked = change_coach_clicked,
        submit_clicked = submit_clicked
      )
    )
  })
}