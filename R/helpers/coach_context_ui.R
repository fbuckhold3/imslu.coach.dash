# Coach context display (pass 1)
#
# UI for the data prepared in coach_context.R:
#   - ccc_monitoring_banner()        review header, when the CCC is monitoring
#   - mod_prev_notes_ui/server       previous coach notes above an entry box
#   - goal_followthrough_ui()        last period's goals vs the ratings
#   - mod_workload_ui/server         duty hours beside the wellness answers
#
# Styling uses roundsui components and --roundsui-* tokens so these pieces
# match the new shell.

.ctx_muted <- "color: var(--roundsui-ink-muted, #5B6472);"

# ── CCC monitoring banner ────────────────────────────────────────────────────

#' @param status Output of ctx_ccc_status().
ccc_monitoring_banner <- function(status) {
  if (is.null(status) || identical(status$state, "none")) return(NULL)

  if (identical(status$state, "resolved")) {
    return(div(
      class = "coach-ccc-note",
      style = paste("padding: 8px 14px; margin-bottom: 12px; border-radius: 6px;",
                    "background: var(--roundsui-surface-2, #FBFBFD);",
                    "border: 1px solid var(--roundsui-border, #E2E5EC); font-size: 0.85rem;", .ctx_muted),
      icon("circle-check"), " Previously monitored by the CCC",
      if (nzchar(status$date)) paste0(" (last update ", status$date, ")"),
      " — marked resolved."
    ))
  }

  line <- function(label, value) {
    if (length(value) == 0 || !any(nzchar(value))) return(NULL)
    div(style = "margin-top: 4px;",
        tags$strong(paste0(label, ": ")), paste(value, collapse = ", "))
  }

  div(
    class = "coach-ccc-banner",
    role = "status",
    style = paste("padding: 12px 16px; margin-bottom: 14px; border-radius: 10px;",
                  "background: var(--roundsui-danger-tint, #FBEAE8);",
                  "border: 1px solid var(--roundsui-danger, #D33A2C);",
                  "color: var(--roundsui-ink, #13161F); font-size: 0.9rem;"),
    div(
      style = "display: flex; align-items: center; gap: 10px; flex-wrap: wrap;",
      tags$strong(style = "color: var(--roundsui-danger, #D33A2C);",
                  icon("triangle-exclamation"), " Monitored by the CCC"),
      roundsui::roundsui_status_badge(
        if (length(status$status)) paste(status$status, collapse = ", ") else "Open",
        status = "warning"),
      tags$span(style = .ctx_muted,
                paste0(status$type, " review",
                       if (nzchar(status$date)) paste0(", ", status$date) else "",
                       if (nzchar(status$session)) paste0(" (", status$session, ")") else ""))
    ),
    line("Competency", status$competencies),
    line("Actions", status$actions),
    line("Responsible", status$person),
    line("Follow-up", status$follow_up),
    line("Progress notes", status$notes)
  )
}

# ── Previous coach notes ─────────────────────────────────────────────────────

.prev_note_text <- function(row) {
  div(
    div(style = paste("font-size: 0.75rem; font-weight: 600; text-transform: uppercase;",
                      "letter-spacing: .04em;", .ctx_muted),
        row$period, if (nzchar(row$date)) paste0(" · ", row$date)),
    div(style = "white-space: pre-wrap; margin-top: 2px;", row$text)
  )
}

#' Render previous coach notes for one field: the most recent in full,
#' older ones inside a collapsed <details>.
prev_notes_block <- function(notes, label) {
  if (is.null(notes) || nrow(notes) == 0) {
    return(div(style = paste("font-size: 0.85rem; margin-bottom: 10px;", .ctx_muted),
               icon("clock-rotate-left"), " No earlier coach note for ", tolower(label), "."))
  }
  older <- if (nrow(notes) > 1) notes[-1, , drop = FALSE] else NULL
  div(
    class = "coach-prev-notes",
    style = paste("padding: 10px 14px; margin-bottom: 12px; border-radius: 8px;",
                  "background: var(--roundsui-surface-2, #FBFBFD);",
                  "border: 1px solid var(--roundsui-border, #E2E5EC); font-size: 0.88rem;"),
    div(style = "font-weight: 600; margin-bottom: 6px;",
        icon("clock-rotate-left"), paste0(" Last coach note — ", label)),
    .prev_note_text(notes[1, , drop = FALSE]),
    if (!is.null(older)) tags$details(
      style = "margin-top: 8px;",
      tags$summary(style = paste("cursor: pointer; font-size: 0.8rem;", .ctx_muted),
                   sprintf("%d earlier note%s", nrow(older), if (nrow(older) > 1) "s" else "")),
      lapply(seq_len(nrow(older)), function(i)
        div(style = "margin-top: 8px;", .prev_note_text(older[i, , drop = FALSE])))
    )
  )
}

#' Previous-notes slot. Place directly above the coach entry box.
mod_prev_notes_ui <- function(id) {
  uiOutput(NS(id, "notes"))
}

#' @param fields Named character vector: names = coach_rev field,
#'   values = display label (e.g. c(coach_wellness = "Wellness")).
mod_prev_notes_server <- function(id, resident_data, current_period, fields) {
  moduleServer(id, function(input, output, session) {
    output$notes <- renderUI({
      rd <- resident_data(); req(rd)
      period_name <- get_period_name(current_period())
      hist <- rd$coach_rev_history
      tagList(lapply(names(fields), function(f) {
        prev_notes_block(ctx_previous_notes(hist, f, period_name), fields[[f]])
      }))
    })
  })
}

# ── Last period's goals ──────────────────────────────────────────────────────

.GOAL_VERDICT_STATUS <- c(
  "Met" = "success", "Progressing" = "in-progress", "No change" = "warning",
  "Declined" = "danger", "Not met" = "danger", "Awaiting faculty rating" = "pending"
)

.fmt_rating <- function(v, src) {
  if (is.na(v)) return("—")
  paste0(format(v), if (nzchar(src)) paste0(" (", src, ")") else "")
}

#' @param rows Output of ctx_goal_followthrough().
#' @param goal_period_name Period the goals were set in.
goal_followthrough_ui <- function(rows, goal_period_name) {
  if (is.null(rows) || nrow(rows) == 0) {
    return(roundsui::roundsui_empty_state(
      "No ILP goals were recorded last period, so there is nothing to follow up.",
      icon = icon("bullseye"), compact = TRUE))
  }

  cards <- lapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i, ]
    stat <- .GOAL_VERDICT_STATUS[[r$verdict]]
    notes <- c(
      if (isTRUE(r$mismatch))
        sprintf("Resident says “%s” but the rating says “%s”.", r$resident_says, r$verdict),
      if (isTRUE(r$below_expected))
        sprintf("Target (%s) was below the program expectation for %s (%s).",
                r$target_rating, goal_period_name, r$expected_rating),
      if (isTRUE(r$met_when_set))
        "Already at target when the goal was set."
    )
    div(
      class = "coach-goal-card",
      style = paste("flex: 1 1 260px; padding: 12px 14px; border-radius: 10px;",
                    "background: var(--roundsui-surface, #fff);",
                    "border: 1px solid var(--roundsui-border, #E2E5EC);"),
      div(style = paste("font-size: 0.72rem; text-transform: uppercase; letter-spacing: .04em;", .ctx_muted),
          r$domain_label),
      div(style = "font-weight: 700; margin: 2px 0 8px;", r$subcomp_label),
      roundsui::roundsui_status_badge(r$verdict, status = stat),
      tags$table(
        style = "width: 100%; margin-top: 10px; font-size: 0.85rem;",
        tags$tbody(
          tags$tr(tags$td(style = .ctx_muted, "Target"),
                  tags$td(if (is.na(r$target_rating)) "—"
                          else sprintf("%s (goal level %s)", r$target_rating, r$goal_level))),
          tags$tr(tags$td(style = .ctx_muted, "When set"), tags$td(.fmt_rating(r$baseline, r$baseline_source))),
          tags$tr(tags$td(style = .ctx_muted, "Now"), tags$td(.fmt_rating(r$current, r$current_source))),
          tags$tr(tags$td(style = .ctx_muted, "Self-rating now"), tags$td(.fmt_rating(r$self_current, ""))),
          tags$tr(tags$td(style = .ctx_muted, "Resident says"),
                  tags$td(if (nzchar(r$resident_says)) r$resident_says else "—"))
        )
      ),
      if (length(notes)) div(
        style = paste("margin-top: 8px; font-size: 0.8rem; color: var(--roundsui-warning, #C2760A);"),
        lapply(notes, function(n) div(icon("circle-info"), " ", n))
      )
    )
  })

  tagList(
    p(style = paste("font-size: 0.85rem;", .ctx_muted),
      "Goals set in ", tags$strong(goal_period_name, .noWS = "after"),
      ". Ratings use ACGME when available, otherwise the coach rating. ",
      "Goal level 1–5 maps to rating 1, 3, 5, 7, 9."),
    div(style = "display: flex; flex-wrap: wrap; gap: 12px;", cards)
  )
}

# ── Duty hours beside wellness ───────────────────────────────────────────────
#
# build_duty_hour_summary() is program-wide and slow (Amion year fetch + RDM
# crosswalk + duty log), so it runs once per server process in a background
# future and is shared by every session for DUTY_CACHE_MAX_AGE_HOURS.

DUTY_CACHE_MAX_AGE_HOURS <- 6
.DUTY_CACHE <- new.env(parent = emptyenv())

duty_cache_get <- function() {
  if (is.null(.DUTY_CACHE$data) || is.null(.DUTY_CACHE$time)) return(NULL)
  age <- as.numeric(difftime(Sys.time(), .DUTY_CACHE$time, units = "hours"))
  if (age > DUTY_CACHE_MAX_AGE_HOURS) return(NULL)
  .DUTY_CACHE$data
}

duty_cache_set <- function(data) {
  .DUTY_CACHE$data <- data
  .DUTY_CACHE$time <- Sys.time()
  invisible(data)
}

# Runs inside the future worker: fetch, then keep only what the panel needs.
.fetch_duty_summary <- function(redcap_url, rdm_token) {
  httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
  d <- amiontools::build_duty_hour_summary(rdm_token = rdm_token, redcap_url = redcap_url)
  list(
    weekly             = d$weekly,
    weekly_by_category = d$weekly_by_category,
    rest_gap_flags     = d$rest_gap_flags,
    duty_blocks        = d$duty_blocks[d$duty_blocks$source == "day_off",
                                       c("record_id", "Date", "source")]
  )
}

mod_workload_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "coach-workload",
    style = paste("padding: 12px 14px; border-radius: 10px; height: 100%;",
                  "background: var(--roundsui-surface-2, #FBFBFD);",
                  "border: 1px solid var(--roundsui-border, #E2E5EC);"),
    div(style = "font-weight: 700; margin-bottom: 6px;", icon("clock"), " Workload since last review"),
    uiOutput(ns("body"))
  )
}

mod_workload_server <- function(id, resident_data, current_period) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    has_amion <- requireNamespace("amiontools", quietly = TRUE)
    duty_r <- reactiveVal(duty_cache_get())

    task <- ExtendedTask$new(function(url, token) {
      promises::future_promise(.fetch_duty_summary(url, token), seed = TRUE)
    })

    observeEvent(input$load, {
      cached <- duty_cache_get()
      if (!is.null(cached)) { duty_r(cached); return() }
      task$invoke(REDCAP_CONFIG$url, REDCAP_CONFIG$rdm_token)
    })

    observe({
      st <- task$status()
      if (identical(st, "success")) duty_r(duty_cache_set(task$result()))
    })

    window_r <- reactive({
      rd <- resident_data(); req(rd)
      # Window: last coach review (or 1 July, whichever is later — Amion
      # data only covers the current academic year) to this review or today.
      ay_start <- as.Date(paste0(
        as.integer(format(Sys.Date(), "%Y")) - (format(Sys.Date(), "%m") < "07"), "-07-01"))
      since <- ctx_last_review_date(rd$coach_rev_history, get_period_name(current_period()))
      if (is.na(since) || since < ay_start) since <- ay_start
      until <- suppressWarnings(as.Date(ctx_val(rd$current_period$coach_rev, "coach_date")))
      if (is.na(until)) until <- Sys.Date()
      list(since = since, until = until)
    })

    output$body <- renderUI({
      d <- duty_r()
      if (is.null(d) && !has_amion) {
        return(p(style = .ctx_muted, "Duty hours need the amiontools package, which isn't installed on this server."))
      }
      if (is.null(d)) {
        st <- task$status()
        if (identical(st, "running")) {
          return(roundsui::roundsui_loading_state("Loading schedule and duty hours…", compact = TRUE))
        }
        return(tagList(
          if (identical(st, "error"))
            roundsui::roundsui_inline_error("Couldn't load duty hours from Amion. Try again."),
          p(style = paste("font-size: 0.85rem;", .ctx_muted),
            "Estimated from the Amion schedule plus hours the resident confirmed. ",
            "The first load takes a little while; after that it's shared across reviews."),
          actionButton(ns("load"), "Load duty hours", icon = icon("clock"), class = "btn-sm btn-outline-primary")
        ))
      }

      rd <- resident_data(); req(rd)
      w <- window_r()
      s <- ctx_duty_window(d, rd$resident_info$record_id[1], w$since, w$until)
      if (is.null(s)) {
        return(roundsui::roundsui_empty_state(
          "No Amion schedule found for this resident in this window.", compact = TRUE))
      }

      stat <- function(label, value, flag = FALSE) {
        div(style = "flex: 1 1 110px;",
            div(style = paste("font-size: 0.72rem; text-transform: uppercase;", .ctx_muted), label),
            div(style = paste0("font-size: 1.15rem; font-weight: 700;",
                               if (flag) " color: var(--roundsui-danger, #D33A2C);" else ""),
                value))
      }
      tagList(
        div(style = paste("font-size: 0.8rem; margin-bottom: 8px;", .ctx_muted),
            format(s$since, "%b %d"), " – ", format(s$until, "%b %d, %Y"),
            sprintf(" · %d weeks", s$n_weeks)),
        div(style = "display: flex; flex-wrap: wrap; gap: 10px; margin-bottom: 8px;",
            stat("Avg / week", sprintf("%.0f h", s$avg_weekly)),
            stat("Weeks > 80h (4-wk avg)", s$weeks_over_80, s$weeks_over_80 > 0),
            stat("Short rest gaps", sprintf("%d hard · %d soft", s$gaps_hard, s$gaps_soft), s$gaps_hard > 0),
            stat("Days off", sprintf("%d (class %.1f)", s$days_off, s$class_days_off))),
        div(style = paste("font-size: 0.82rem; margin-bottom: 6px;", .ctx_muted),
            sprintf("Heaviest stretch: %.0f h/week 4-week average, week of %s%s.",
                    s$peak_4wk_avg, format(s$peak_week, "%b %d"),
                    if (nzchar(s$peak_category)) paste0(" (", s$peak_category, ")") else ""),
            if (!is.na(s$verified_pct))
              sprintf(" %.0f%% of days confirmed by the resident; the rest are schedule estimates.",
                      s$verified_pct)),
        plotly::plotlyOutput(ns("chart"), height = "180px"),
        div(style = paste("font-size: 0.75rem; margin-top: 4px;", .ctx_muted),
            "Estimates for discussion, not a compliance record.")
      )
    })

    output$chart <- plotly::renderPlotly({
      d <- duty_r(); req(d)
      rd <- resident_data(); req(rd)
      w <- window_r()
      s <- ctx_duty_window(d, rd$resident_info$record_id[1], w$since, w$until)
      req(s)
      wk <- s$weekly
      col <- roundsui::roundsui_colors()
      p <- plotly::plot_ly(wk, x = ~week_start) |>
        plotly::add_bars(y = ~Total_Hours, name = "Weekly hours",
                         marker = list(color = ifelse(wk$flag_80h, col$danger, col$accent)),
                         hovertemplate = "Week of %{x|%b %d}: %{y:.0f} h<extra></extra>") |>
        plotly::add_lines(y = ~rolling_4wk_avg_hours, name = "4-week average",
                          line = list(color = col$ink, width = 2),
                          hovertemplate = "4-week avg: %{y:.0f} h<extra></extra>") |>
        plotly::add_lines(y = rep(80, nrow(wk)), name = "80 h",
                          line = list(color = col$danger, dash = "dash", width = 1),
                          hoverinfo = "skip")
      roundsui::roundsui_plotly_layout(
        p, showlegend = FALSE, margin = list(l = 30, r = 10, t = 5, b = 25),
        yaxis = list(title = "", rangemode = "tozero"), xaxis = list(title = "")
      ) |> plotly::config(displayModeBar = FALSE)
    })
  })
}
