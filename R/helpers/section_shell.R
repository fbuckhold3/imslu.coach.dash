# Sequential-section shell for coach.dash review interface.
#
# Adapts the resident self-evaluation single-page UX (one scrolling page,
# progress bar at top, sections rendered in order) to coach review. Uses
# SOFT gating: every section renders regardless of completeness, the user
# can scroll/jump freely, and the progress bar reflects each section's
# self-reported completion state.
#
# Each section module is responsible for:
#   1. Rendering its own UI inside coach_sec_card()
#   2. Returning a reactive `is_complete` boolean
#   3. (Optional) calling session$sendCustomMessage("coach_advance", id) on
#      save to scroll the next section into view.


# ── UI ────────────────────────────────────────────────────────────────────

#' Progress bar for the coach review shell
#'
#' @param id Output id (the server uses `renderUI` here)
coach_progress_bar_ui <- function(id) {
  shiny::div(
    class = "coach-progress-wrap sticky-top bg-white",
    style = paste(
      "padding: 10px 18px;",
      "border-bottom: 1px solid #e5e7eb;",
      "z-index: 100;"),
    shiny::uiOutput(id)
  )
}

#' Render the progress bar (server side)
#'
#' @param sections Named character vector: name = display label
#' @param status   Named logical vector: name -> is_complete
#' @param active   Character. Currently active section name.
render_coach_progress <- function(sections, status, active = NULL) {
  n_total <- length(sections)
  n_done  <- sum(isTRUE_safe(status))
  pct     <- if (n_total > 0) round(100 * n_done / n_total) else 0

  pills <- lapply(names(sections), function(nm) {
    label <- sections[[nm]]
    done  <- isTRUE(status[[nm]])
    is_active <- !is.null(active) && identical(nm, active)
    bg <- if (done) "#198754" else if (is_active) "#003d5c" else "#e5e7eb"
    fg <- if (done || is_active) "#fff" else "#475569"
    icon <- if (done) "bi-check-circle-fill" else if (is_active) "bi-pencil-fill" else "bi-circle"
    shiny::tags$a(
      href = paste0("#sec_", nm),
      class = "coach-progress-pill",
      style = sprintf(
        paste(
          "display:inline-flex; align-items:center; gap:6px;",
          "padding:4px 10px; border-radius:999px;",
          "font-size:0.78rem; font-weight:600;",
          "background:%s; color:%s;",
          "text-decoration:none; margin-right:6px;"),
        bg, fg),
      shiny::tags$i(class = paste("bi", icon)),
      label
    )
  })

  shiny::tagList(
    shiny::div(
      class = "d-flex align-items-center justify-content-between mb-1",
      shiny::tags$strong(
        style = "color:#003d5c; font-size:0.88rem;",
        sprintf("Review progress: %d of %d sections", n_done, n_total)),
      shiny::tags$span(
        style = "color:#64748b; font-size:0.8rem;",
        sprintf("%d%%", pct))
    ),
    shiny::div(
      class = "progress mb-2",
      style = "height:6px; background:#f1f5f9;",
      shiny::div(
        class = "progress-bar",
        role  = "progressbar",
        style = sprintf("width:%d%%; background:#198754;", pct),
        `aria-valuenow` = pct, `aria-valuemin` = 0, `aria-valuemax` = 100
      )
    ),
    shiny::div(class = "coach-progress-pills", pills)
  )
}

#' Section card — title, status badge, body
#'
#' @param name     Internal name (used for anchor + DOM id).
#' @param title    Display title.
#' @param icon     Bootstrap icon name (without "bi-" prefix).
#' @param status   "incomplete" | "complete" | "skipped".
#' @param ...      Body content.
coach_sec_card <- function(name, title, icon = "card-text",
                           status = "incomplete", ...) {
  badge <- switch(
    status,
    complete   = shiny::tags$span(
      class = "badge",
      style = "background:#198754; color:#fff; font-weight:600;",
      shiny::tags$i(class = "bi bi-check-circle-fill me-1"),
      "Complete"),
    skipped    = shiny::tags$span(
      class = "badge",
      style = "background:#f59e0b; color:#fff; font-weight:600;",
      shiny::tags$i(class = "bi bi-skip-end-fill me-1"),
      "Skipped"),
    shiny::tags$span(
      class = "badge",
      style = "background:#e5e7eb; color:#475569; font-weight:600;",
      "In progress")
  )

  shiny::div(
    class = "card border-0 shadow-sm mb-4",
    id    = paste0("sec_", name),
    style = "border-radius: 8px; scroll-margin-top: 110px;",
    shiny::div(
      class = "card-header border-0 d-flex align-items-center justify-content-between",
      style = paste(
        "background:#f8fafc; border-radius:8px 8px 0 0;",
        "padding:14px 20px;"),
      shiny::div(
        class = "d-flex align-items-center gap-2",
        shiny::tags$i(
          class = paste0("bi bi-", icon),
          style = "color:#003d5c; font-size:1.1rem;"),
        shiny::tags$span(
          style = "font-weight:700; color:#003d5c; font-size:1rem;",
          title)),
      badge
    ),
    shiny::div(
      class = "card-body",
      style = "padding: 18px 20px;",
      ...
    )
  )
}

#' Continue button — bumps the active section but does not block on
#' incompleteness.
#'
#' @param ns      Module namespace function.
#' @param id      Input id (e.g., "continue_wellness").
#' @param label   Button label.
#' @param is_complete Logical. Drives the visual state.
coach_continue_btn <- function(ns, id, label = "Save & Continue",
                               is_complete = FALSE) {
  warn_text <- if (!isTRUE(is_complete)) {
    shiny::tags$span(
      class = "text-warning ms-2",
      style = "font-size:0.78rem;",
      shiny::tags$i(class = "bi bi-exclamation-triangle-fill me-1"),
      "Section incomplete \u2014 you can continue, but please return later")
  } else NULL

  shiny::div(
    class = "d-flex align-items-center gap-2 mt-3 pt-3 border-top",
    shiny::actionButton(
      ns(id), label,
      class = "btn btn-sm",
      style = paste(
        "background:#003d5c; color:#fff; border:none;",
        "padding:7px 22px; font-weight:600;")),
    shiny::uiOutput(ns(paste0(id, "_status"))),
    warn_text
  )
}


# ── Server-side state ─────────────────────────────────────────────────────

#' Reactive state container for the coach shell
#'
#' @param section_names Character vector of section names (in render order).
#' @return A reactiveValues list with:
#'   - `step`       : integer, current 1-based active section
#'   - `complete`   : named logical vector keyed by section name
#'   - `set_complete(name, val)` : function to update `complete`
#'   - `advance()`  : function, bumps `step` to the next section
coach_shell_state <- function(section_names) {
  rv <- shiny::reactiveValues(
    step     = 1L,
    complete = setNames(rep(FALSE, length(section_names)), section_names)
  )
  rv$names    <- section_names
  rv$set_complete <- function(name, val = TRUE) {
    if (name %in% names(rv$complete)) {
      cur <- rv$complete
      cur[[name]] <- isTRUE(val)
      rv$complete <- cur
    }
  }
  rv$advance <- function() {
    if (rv$step < length(section_names)) rv$step <- rv$step + 1L
  }
  rv
}


# ── Internal utils ────────────────────────────────────────────────────────

isTRUE_safe <- function(x) {
  if (length(x) == 0) return(logical(0))
  vapply(x, isTRUE, logical(1))
}


# ── Browser-side helper (registered by the shell once at startup) ─────────
#
# Sends a custom message "coach_scroll_to" with section name; the JS scrolls
# that section into view smoothly. Call from server on continue/save.

coach_scroll_js <- function() {
  shiny::tags$script(shiny::HTML("
    Shiny.addCustomMessageHandler('coach_scroll_to', function(name) {
      var el = document.getElementById('sec_' + name);
      if (el) el.scrollIntoView({behavior:'smooth', block:'start'});
    });
  "))
}
