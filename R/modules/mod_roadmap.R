# =============================================================================
# Coaching roadmap: every expected coach encounter for a resident, in order,
# with its date and takeaway once done. Interim notes slot in by date.
#
# Cadence (REDCap period codes):
#   7 Entering Residency check-in (Jul-Sep)   single form, takeaway coach_summary
#   1-5 Mid Intern ... Mid PGY3               Evaluation & ILP (coach_ilp_final),
#                                             then Touch-base (coach_ilp_checkin)
#   6 Graduation                              single form, takeaway coach_ilp_final
#
# coach_rev arrives either with raw codes ("1") or, from the app's loaded
# data, with labels ("Mid Intern", "Graduating"); both are normalized.
# =============================================================================

.RM_ORDER <- c("7", "1", "2", "3", "4", "5", "6")
.RM_NAME  <- c("7" = "Entering Residency", "1" = "Mid Intern", "2" = "End Intern",
               "3" = "Mid PGY2", "4" = "End PGY2", "5" = "Mid PGY3", "6" = "Graduation")
.RM_ALIAS <- c("Entering Residency" = "7", "Intern Intro" = "7", "Mid Intern" = "1",
               "End Intern" = "2", "Mid PGY2" = "3", "End PGY2" = "4", "Mid PGY3" = "5",
               "Graduation" = "6", "Graduating" = "6", "Interim" = "8")

.rm_code <- function(x) {
  x <- trimws(as.character(x))
  out <- ifelse(x %in% names(.RM_ALIAS), unname(.RM_ALIAS[x]), x)
  ifelse(is.na(x) | !nzchar(x), NA_character_, out)
}
.rm_txt <- function(df, f) {
  if (is.null(df) || !f %in% names(df)) return(rep(NA_character_, NROW(df)))
  v <- trimws(as.character(df[[f]]))
  ifelse(is.na(v) | !nzchar(v), NA_character_, v)
}
.rm_date <- function(x) suppressWarnings(as.Date(substr(as.character(x), 1, 10)))

# Touch-bases started with the two-form coach review. An earlier review with
# no touch-base isn't a missed meeting, so it isn't listed.
TOUCH_BASE_START <- as.Date("2026-11-14")

#' Local (0-based, 0 = Entering Residency) period number -> REDCap code
coach_period_code <- function(local_period) {
  p <- suppressWarnings(as.integer(local_period))
  if (length(p) != 1 || is.na(p)) return(NA_character_)
  if (p == 0L) "7" else as.character(p)
}

#' Expected encounters for one resident with status, date and takeaway
#'
#' @param coach_rev    coach_rev rows (any residents; filtered by record_id)
#' @param coach_interim coach_interim rows, or NULL
#' @param record_id    Resident record_id
#' @param current_code REDCap code of the resident's current period ("7", "1".."6")
#' @return data.frame: kind (review/touch/interim), period, label, status
#'   (done/due/missed/upcoming/interim), date, takeaway
coach_roadmap_steps <- function(coach_rev, coach_interim, record_id, current_code) {
  rid <- as.character(record_id)
  cr <- if (!is.null(coach_rev) && nrow(coach_rev) > 0)
    coach_rev[as.character(coach_rev$record_id) == rid, , drop = FALSE] else NULL
  if (!is.null(cr) && "redcap_repeat_instrument" %in% names(cr))
    cr <- cr[is.na(cr$redcap_repeat_instrument) | cr$redcap_repeat_instrument == "coach_rev", , drop = FALSE]
  code <- if (!is.null(cr)) .rm_code(cr$coach_period) else character()
  cur_i <- match(as.character(current_code), .RM_ORDER)

  row_for <- function(cd) {
    if (is.null(cr) || !any(code %in% cd)) return(NULL)
    cr[which(code %in% cd)[1], , drop = FALSE]
  }

  steps <- list()
  add <- function(kind, cd, label, done, date, takeaway, ready = TRUE) {
    i <- match(cd, .RM_ORDER)
    status <- if (done) "done"
              else if (is.na(cur_i)) "upcoming"
              else if (i < cur_i) "missed"
              else if (i == cur_i && ready) "due"
              else "upcoming"
    steps[[length(steps) + 1]] <<- data.frame(
      kind = kind, period = cd, label = label, status = status,
      date = if (done) date else as.Date(NA), takeaway = if (done) takeaway else NA_character_,
      stringsAsFactors = FALSE)
  }

  for (cd in .RM_ORDER) {
    r <- row_for(cd)
    nm <- unname(.RM_NAME[cd])
    complete <- !is.null(r) && "coach_rev_complete" %in% names(r) &&
                as.character(r$coach_rev_complete[1]) %in% c("2", "Complete")
    summ <- if (!is.null(r)) .rm_txt(r, "coach_summary")[1] else NA_character_
    ilp  <- if (!is.null(r)) .rm_txt(r, "coach_ilp_final")[1] else NA_character_
    take <- if (!is.na(ilp) && !is.na(summ)) paste0(summ, "\n\n", ilp)
            else if (!is.na(ilp)) ilp else summ
    rev_done <- complete || !is.na(take)
    rev_date <- if (!is.null(r)) .rm_date(r$coach_date[1]) else as.Date(NA)

    if (cd == "7") {
      add("review", cd, "Entering Residency check-in", rev_done, rev_date, take)
    } else if (cd == "6") {
      add("review", cd, "Graduation review", rev_done, rev_date, take)
    } else {
      add("review", cd, paste(nm, "evaluation & ILP"), rev_done, rev_date, take)
      chk <- if (!is.null(r)) .rm_txt(r, "coach_ilp_checkin")[1] else NA_character_
      tdt <- if (!is.null(r)) .rm_date(.rm_txt(r, "coach_touch_date")[1]) else as.Date(NA)
      past <- !is.na(cur_i) && match(cd, .RM_ORDER) < cur_i
      pre_launch <- is.na(rev_date) || rev_date < TOUCH_BASE_START
      # Not listed: a past touch-base that predates the two-form review.
      if (!(is.na(chk) && past && pre_launch)) {
        # Due only once that period's evaluation is in.
        add("touch", cd, paste(nm, "touch-base"), !is.na(chk), tdt, chk, ready = rev_done)
      }
    }
  }
  out <- do.call(rbind, steps)

  # Interim notes: coach_interim plus older interim coach_rev rows (period 8).
  ints <- list()
  ci <- if (!is.null(coach_interim) && nrow(coach_interim) > 0)
    coach_interim[as.character(coach_interim$record_id) == rid, , drop = FALSE] else NULL
  if (!is.null(ci) && nrow(ci) > 0) {
    ints[[1]] <- data.frame(kind = "interim", period = NA_character_, label = "Interim note",
                            status = "interim", date = .rm_date(ci$ci_date),
                            takeaway = .rm_txt(ci, "ci_notes"), stringsAsFactors = FALSE)
  }
  if (!is.null(cr) && any(code %in% "8")) {
    r8 <- cr[code %in% "8", , drop = FALSE]
    ints[[length(ints) + 1]] <- data.frame(kind = "interim", period = NA_character_,
                            label = "Interim review", status = "interim",
                            date = .rm_date(r8$coach_date),
                            takeaway = .rm_txt(r8, "coach_summary"), stringsAsFactors = FALSE)
  }
  if (length(ints) == 0) return(out)
  ints <- do.call(rbind, ints)
  ints <- ints[order(ints$date), , drop = FALSE]

  # Each interim goes right after the last done step dated on or before it.
  pos <- vapply(seq_len(nrow(ints)), function(j) {
    d <- ints$date[j]
    if (is.na(d)) return(nrow(out))
    hit <- which(out$status == "done" & !is.na(out$date) & out$date <= d)
    if (length(hit)) max(hit) else 0L
  }, numeric(1))
  res <- out[0, , drop = FALSE]
  if (any(pos == 0)) res <- rbind(res, ints[pos == 0, , drop = FALSE])
  for (i in seq_len(nrow(out))) {
    res <- rbind(res, out[i, , drop = FALSE])
    if (any(pos == i)) res <- rbind(res, ints[pos == i, , drop = FALSE])
  }
  rownames(res) <- NULL
  res
}

#' Label of the next expected encounter ("Mid Intern touch-base"), or NA
coach_next_step <- function(steps) {
  if (is.null(steps) || nrow(steps) == 0) return(NA_character_)
  due <- steps$label[steps$status == "due"]
  if (length(due)) return(due[1])
  up <- steps$label[steps$status == "upcoming"]
  if (length(up)) paste0(up[1], " (upcoming)") else NA_character_
}

mod_roadmap_ui <- function(id) {
  ns <- NS(id)
  div(class = "card mb-3",
      div(class = "card-header", tags$strong("Coaching roadmap")),
      div(class = "card-body", style = "padding: 10px 14px;", uiOutput(ns("roadmap"))))
}

#' @param forms_r        Reactive list of fresh forms (coach_rev, coach_interim)
#' @param record_id_r    Reactive record_id
#' @param current_period Reactive local period number (0 = Entering Residency)
mod_roadmap_server <- function(id, forms_r, record_id_r, current_period) {
  moduleServer(id, function(input, output, session) {
    output$roadmap <- renderUI({
      f <- forms_r()
      if (is.null(f)) return(tags$em(class = "text-muted", "Loading..."))
      st <- coach_roadmap_steps(f$coach_rev, f$coach_interim, record_id_r(),
                                coach_period_code(current_period()))
      .style <- c(done = "var(--roundsui-success)", due = "var(--roundsui-accent)",
                  missed = "var(--roundsui-ink-faint)", upcoming = "var(--roundsui-ink-faint)",
                  interim = "var(--roundsui-ink-muted)")
      .icon <- c(done = "circle-check", due = "circle-dot", missed = "circle-minus",
                 upcoming = "circle", interim = "comment")
      .word <- c(done = "", due = "Due now", missed = "Not recorded", upcoming = "Upcoming",
                 interim = "")
      items <- lapply(seq_len(nrow(st)), function(i) {
        s <- st[i, ]
        date <- if (!is.na(s$date)) trimws(format(s$date, "%b %e, %Y")) else ""
        head <- div(
          style = sprintf("display:flex; gap:8px; align-items:baseline;%s%s",
                          if (s$status == "due") " font-weight:700;" else "",
                          if (s$kind == "interim") " padding-left:22px; font-size:0.88rem;" else ""),
          tags$span(style = sprintf("color:%s;", .style[[s$status]]), icon(.icon[[s$status]])),
          tags$span(style = sprintf("color:%s;", if (s$status %in% c("missed", "upcoming"))
                                      "var(--roundsui-ink-muted)" else "var(--roundsui-ink)"), s$label),
          tags$span(class = "text-muted small", paste(c(date, .word[[s$status]])[nzchar(c(date, .word[[s$status]]))],
                                                    collapse = " · ")))
        if (!is.na(s$takeaway)) {
          tags$details(style = "margin: 2px 0;",
                       tags$summary(style = "cursor:pointer; list-style:none; margin-left:14px;",
                                    tagAppendChild(head, tags$span(class = "text-muted small",
                                                                   icon("chevron-down"), " takeaway"))),
                       div(style = "white-space: pre-wrap; margin: 4px 0 6px 22px; padding: 6px 8px; background: var(--roundsui-surface-2); border-radius: 4px; font-size: 0.88rem;",
                           s$takeaway))
        } else div(style = "margin: 2px 0 2px 14px;", head)
      })
      tagList(items)
    })
  })
}
