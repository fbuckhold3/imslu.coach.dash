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


# \u2500\u2500 UI \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

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
    bg <- if (done) "var(--roundsui-success, #16A34A)"
          else if (is_active) "var(--roundsui-accent, #3355D8)"
          else "var(--roundsui-border, #E2E5EC)"
    fg <- if (done || is_active) "#fff" else "var(--roundsui-ink-muted, #5B6472)"
    icon <- if (done) "circle-check" else if (is_active) "pen" else "circle"
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
      shiny::icon(icon, class = if (identical(icon, "circle")) "fa-regular" else NULL),
      label
    )
  })

  shiny::tagList(
    shiny::div(
      class = "d-flex align-items-center justify-content-between mb-1",
      shiny::tags$strong(
        style = "font-size:0.88rem;",
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
        style = sprintf("width:%d%%; background:var(--roundsui-success, #16A34A);", pct),
        `aria-valuenow` = pct, `aria-valuemin` = 0, `aria-valuemax` = 100
      )
    ),
    shiny::div(class = "coach-progress-pills", pills)
  )
}

#' Section card \u2014 title, status badge, body
#'
#' @param name        Internal name (used for anchor + DOM id).
#' @param title       Display title.
#' @param icon        Icon name; legacy Bootstrap Icons names are mapped to
#'                    Font Awesome by coach_fa_icon().
#' @param status      "incomplete" | "complete" | "skipped". Used only when
#'                    `badge_slot` is NULL.
#' @param badge_slot  Optional Shiny tag (typically `uiOutput(...)`) that
#'                    replaces the static badge \u2014 lets the badge update
#'                    reactively without re-rendering the card body.
#' @param ...         Body content.
coach_sec_card <- function(name, title, icon = "card-text",
                           status = "incomplete",
                           badge_slot = NULL, ...) {
  badge <- if (!is.null(badge_slot)) badge_slot else switch(
    status,
    complete = roundsui::roundsui_status_badge("Complete", status = "complete"),
    skipped  = roundsui::roundsui_status_badge("Skipped", status = "warning"),
    roundsui::roundsui_status_badge("In progress", status = "pending")
  )

  shiny::div(
    class = "card coach-sec-card mb-4",
    id    = paste0("sec_", name),
    style = "scroll-margin-top: 110px;",
    shiny::div(
      class = "card-header d-flex align-items-center justify-content-between",
      shiny::div(
        class = "d-flex align-items-center gap-2",
        shiny::span(class = "coach-sec-card__icon", shiny::icon(coach_fa_icon(icon))),
        shiny::tags$span(
          style = "font-weight:700; font-size:1rem;",
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

#' Continue button \u2014 bumps the active section but does not block on
#' incompleteness.
#'
#' @param ns      Module namespace function.
#' @param id      Input id (e.g., "continue_wellness").
#' @param label   Button label.
#' @param is_complete Logical. Drives the visual state when `warn_slot`
#'                    is NULL.
#' @param warn_slot   Optional Shiny tag (typically `uiOutput(...)`) that
#'                    replaces the static "Section incomplete" warning so
#'                    the warning can update reactively without re-rendering
#'                    the parent card body.
coach_continue_btn <- function(ns, id, label = "Save & Continue",
                               is_complete = FALSE,
                               warn_slot   = NULL) {
  warn_text <- if (!is.null(warn_slot)) warn_slot
               else if (!isTRUE(is_complete)) {
    shiny::tags$span(
      class = "text-warning ms-2",
      style = "font-size:0.78rem;",
      shiny::icon("triangle-exclamation"),
      " Section incomplete \u2014 you can continue, but please return later")
  } else NULL

  shiny::div(
    class = "d-flex align-items-center gap-2 mt-3 pt-3 border-top",
    shiny::actionButton(
      ns(id), label,
      class = "btn btn-sm btn-primary",
      style = "padding:7px 22px;"),
    shiny::uiOutput(ns(paste0(id, "_status"))),
    warn_text
  )
}


# \u2500\u2500 Server-side state \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

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


# Section icons were written as Bootstrap Icons names, but the app loads
# Font Awesome (bundled with Shiny/bslib, and what roundsui standardizes on).
coach_fa_icon <- function(name) {
  map <- c(
    "heart" = "heart", "clipboard" = "clipboard", "book" = "book",
    "mortarboard" = "graduation-cap", "mortarboard-fill" = "graduation-cap",
    "briefcase" = "briefcase", "bullseye" = "bullseye", "graph-up" = "chart-line",
    "list-check" = "list-check", "person-arms-up" = "user-graduate",
    "card-text" = "file-lines"
  )
  if (!is.null(name) && name %in% names(map)) map[[name]] else name
}

# \u2500\u2500 Internal utils \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

isTRUE_safe <- function(x) {
  if (length(x) == 0) return(logical(0))
  vapply(x, isTRUE, logical(1))
}


# \u2500\u2500 Browser-side helper (registered by the shell once at startup) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
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


# -- Coach note text areas ----------------------------------------------------
#
# The review screen is rebuilt every time it is opened (app.R view router and
# sections_ui renderUI), so an updateTextAreaInput() sent from a prefill
# observer can arrive before the textarea exists and be dropped -- reopened
# reviews then showed blank coach notes. Rendering the textarea server-side
# with the saved value means it always appears pre-filled, the same way
# mod_milestone_entry draws its sliders from initial_scores.

#' Placeholder slot for a coach note textarea (module UI side)
#'
#' @param ns       Module namespace function
#' @param input_id Input id of the textarea (unchanged from before)
coach_note_ui <- function(ns, input_id) {
  shiny::uiOutput(ns(paste0(input_id, "_slot")))
}

#' Saved value of a current-period field, "" when missing
#'
#' @param form Current-period form holding the field (coach_rev by default;
#'             second_review for mod_second_review)
coach_note_saved_value <- function(rd, field, form = "coach_rev") {
  curr <- if (!is.null(rd)) rd$current_period[[form]] else NULL
  if (is.null(curr) || !is.data.frame(curr) || nrow(curr) == 0 ||
      !field %in% names(curr)) return("")
  v <- curr[[field]][1]
  if (is.null(v) || is.na(v)) "" else as.character(v)
}

#' Render a coach note textarea pre-filled from the saved coach_rev value
#'
#' Re-renders when resident_data() changes (new resident, period, or a data
#' reload after save) but not on typing.
#'
#' @param output,session Module output / session
#' @param input_id       Textarea input id
#' @param field          coach_rev field holding the saved text (defaults to
#'                       input_id; differs in mod_learning)
#' @param form           Form holding the saved value (default coach_rev)
#' @param resident_data  Reactive returning get_resident_period_data() output
#' @param ...            Passed to textAreaInput (placeholder, height, ...)
coach_note_server <- function(output, session, input_id, resident_data,
                              field = input_id, form = "coach_rev", ...) {
  args <- list(...)
  output[[paste0(input_id, "_slot")]] <- shiny::renderUI({
    value <- coach_note_saved_value(resident_data(), field, form)
    do.call(shiny::textAreaInput,
            c(list(inputId = session$ns(input_id), label = NULL, value = value), args))
  })
}
