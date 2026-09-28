# Coach context helpers (pure functions, no reactivity)
#
# Data preparation behind the pass-1 coaching views:
#   - previous coach notes per section field (coach_rev history)
#   - CCC monitoring status (ccc_review)
#   - last period's ILP goals vs what the milestone ratings did
#   - duty-hour workload since the last review (amiontools output)
#
# Every function takes plain data frames / lists and returns plain data, so
# it can be unit tested without Shiny, REDCap or gmed (see
# tests/testthat/test-coach_context.R). Display lives in coach_context_ui.R.
#
# Period fields (coach_period, ccc_session, prog_mile_period, ...) arrive
# already translated to period NAMES by load_coaching_data() in globals.R,
# so everything here orders periods by .CTX_PERIOD_ORDER.

.CTX_PERIOD_ORDER <- c("Entering Residency", "Mid Intern", "End Intern",
                       "Mid PGY2", "End PGY2", "Mid PGY3", "Graduating")

# ── Small utilities ──────────────────────────────────────────────────────────

# TRUE for NULL / NA / "" / "NA" / whitespace.
ctx_blank <- function(x) {
  if (is.null(x) || length(x) == 0) return(TRUE)
  x <- x[1]
  is.na(x) || !nzchar(trimws(as.character(x))) || identical(trimws(as.character(x)), "NA")
}

# First value of a column as trimmed character, "" when missing.
ctx_val <- function(df, col, i = 1) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) < i || !col %in% names(df)) return("")
  v <- df[[col]][i]
  if (ctx_blank(v)) "" else trimws(as.character(v))
}

ctx_num <- function(df, col, i = 1) {
  v <- suppressWarnings(as.numeric(ctx_val(df, col, i)))
  if (length(v) == 0) NA_real_ else v
}

ctx_period_index <- function(period_name) {
  match(as.character(period_name), .CTX_PERIOD_ORDER)
}

#' Parse a REDCap choices string ("1, Label | 2, Label") into a named vector
#' (names = codes, values = labels) for one field of the data dictionary.
#' Handles both API-style and CSV-style dictionary column names.
ctx_field_choices <- function(data_dict, field_name) {
  if (is.null(data_dict) || !is.data.frame(data_dict) || nrow(data_dict) == 0) {
    return(character(0))
  }
  fn_col <- intersect(c("field_name", "Variable / Field Name", "Variable...Field.Name"),
                      names(data_dict))
  ch_col <- intersect(c("select_choices_or_calculations",
                        "Choices, Calculations, OR Slider Labels",
                        "Choices..Calculations..OR.Slider.Labels"),
                      names(data_dict))
  if (!length(fn_col) || !length(ch_col)) return(character(0))
  row <- data_dict[data_dict[[fn_col[1]]] == field_name, , drop = FALSE]
  if (nrow(row) == 0 || ctx_blank(row[[ch_col[1]]][1])) return(character(0))
  parts <- trimws(strsplit(as.character(row[[ch_col[1]]][1]), "\\|")[[1]])
  codes <- trimws(sub(",.*$", "", parts))
  labels <- trimws(sub("^[^,]*,", "", parts))
  stats::setNames(labels, codes)
}

# Labels of the checked options of a checkbox field (field___N == "1").
# `fallback` is a named vector used when the dictionary has no entry.
ctx_checked_labels <- function(row, field_name, data_dict = NULL, fallback = NULL) {
  if (is.null(row) || nrow(row) == 0) return(character(0))
  cols <- grep(paste0("^", field_name, "___"), names(row), value = TRUE)
  checked <- cols[vapply(cols, function(cl) identical(ctx_val(row, cl), "1"), logical(1))]
  if (length(checked) == 0) return(character(0))
  codes <- sub(paste0("^", field_name, "___"), "", checked)
  choices <- ctx_field_choices(data_dict, field_name)
  if (length(choices) == 0 && !is.null(fallback)) choices <- fallback
  unname(ifelse(codes %in% names(choices), choices[codes], paste("Option", codes)))
}

# ── Previous coach notes ─────────────────────────────────────────────────────

#' All coach_rev rows for one resident, oldest period first.
ctx_coach_rev_history <- function(all_forms, record_id) {
  cr <- all_forms$coach_rev
  if (is.null(cr) || !is.data.frame(cr) || nrow(cr) == 0) return(data.frame())
  cr <- cr[as.character(cr$record_id) == as.character(record_id), , drop = FALSE]
  if ("redcap_repeat_instrument" %in% names(cr)) {
    cr <- cr[!is.na(cr$redcap_repeat_instrument) &
               cr$redcap_repeat_instrument == "coach_rev", , drop = FALSE]
  }
  if (nrow(cr) == 0) return(data.frame())
  per_idx <- if ("coach_period" %in% names(cr)) ctx_period_index(cr$coach_period) else NA_integer_
  dates <- if ("coach_date" %in% names(cr)) suppressWarnings(as.Date(cr$coach_date)) else as.Date(NA)
  cr[order(per_idx, dates, na.last = TRUE), , drop = FALSE]
}

#' Previous coach notes for one field: one row per earlier period that has
#' text, most recent first. Periods at or after `current_period_name` are
#' excluded (the current period's note is the one being written).
ctx_previous_notes <- function(history, field, current_period_name) {
  empty <- data.frame(period = character(0), date = character(0),
                      text = character(0), stringsAsFactors = FALSE)
  if (is.null(history) || nrow(history) == 0 || !field %in% names(history)) return(empty)
  cur_idx <- ctx_period_index(current_period_name)
  per <- if ("coach_period" %in% names(history)) as.character(history$coach_period) else rep(NA, nrow(history))
  idx <- ctx_period_index(per)
  keep <- !vapply(history[[field]], ctx_blank, logical(1)) &
    (is.na(cur_idx) | (!is.na(idx) & idx < cur_idx))
  if (!any(keep)) return(empty)
  h <- history[keep, , drop = FALSE]
  out <- data.frame(
    period = ifelse(is.na(per[keep]), "Unknown period", per[keep]),
    date   = if ("coach_date" %in% names(h)) ifelse(is.na(h$coach_date), "", as.character(h$coach_date)) else "",
    text   = trimws(as.character(h[[field]])),
    stringsAsFactors = FALSE
  )
  out[rev(seq_len(nrow(out))), , drop = FALSE]
}

#' Date of the most recent coach review before the current period, or NA.
ctx_last_review_date <- function(history, current_period_name) {
  prev <- ctx_previous_notes(history, "coach_date", current_period_name)
  if (nrow(prev) == 0) return(as.Date(NA))
  suppressWarnings(as.Date(prev$text[1]))
}

# ── CCC monitoring status ────────────────────────────────────────────────────

# Fallback labels, mirroring imslu.ccc.dashboard/R/redcap_submission.R.
.CCC_ACTION_LABELS <- c(
  "1" = "Remediation plan", "2" = "Probation", "3" = "Referral for professionalism",
  "4" = "Coach follow up", "5" = "Meet with PD/CCC Chair", "6" = "Meet with Chiefs",
  "7" = "Other (see notes)", "8" = "Referral to behavioral health"
)
.CCC_STATUS_LABELS <- c("1" = "Initiation", "2" = "Ongoing", "3" = "Resolved", "4" = "Recurring")

#' CCC monitoring status for one resident, from their most recent ccc_review
#' record that raised something (concern = yes, an action checked, or an
#' interim review).
#'
#' @return list(state = "none" | "active" | "resolved", plus date, session,
#'   type, competencies, actions, status, person, follow_up, notes)
ctx_ccc_status <- function(all_forms, record_id, data_dict = NULL) {
  none <- list(state = "none")
  cc <- all_forms$ccc_review
  if (is.null(cc) || !is.data.frame(cc) || nrow(cc) == 0) return(none)
  cc <- cc[as.character(cc$record_id) == as.character(record_id), , drop = FALSE]
  if ("redcap_repeat_instrument" %in% names(cc)) {
    cc <- cc[!is.na(cc$redcap_repeat_instrument) &
               cc$redcap_repeat_instrument == "ccc_review", , drop = FALSE]
  }
  if (nrow(cc) == 0) return(none)

  flag_row <- function(i) {
    r <- cc[i, , drop = FALSE]
    identical(ctx_val(r, "ccc_concern"), "1") ||
      identical(ctx_val(r, "ccc_rev_type"), "2") ||
      length(ctx_checked_labels(r, "ccc_action", fallback = .CCC_ACTION_LABELS)) > 0
  }
  flagged <- cc[vapply(seq_len(nrow(cc)), flag_row, logical(1)), , drop = FALSE]
  if (nrow(flagged) == 0) return(none)

  d <- if ("ccc_date" %in% names(flagged)) suppressWarnings(as.Date(flagged$ccc_date)) else as.Date(rep(NA, nrow(flagged)))
  inst <- if ("redcap_repeat_instance" %in% names(flagged)) suppressWarnings(as.numeric(flagged$redcap_repeat_instance)) else 0
  latest <- flagged[order(d, inst, decreasing = TRUE, na.last = TRUE)[1], , drop = FALSE]

  status <- ctx_checked_labels(latest, "ccc_action_status", data_dict, .CCC_STATUS_LABELS)
  resolved <- length(status) > 0 && all(status == .CCC_STATUS_LABELS[["3"]])

  list(
    state        = if (resolved) "resolved" else "active",
    date         = ctx_val(latest, "ccc_date"),
    session      = ctx_val(latest, "ccc_session"),
    type         = if (identical(ctx_val(latest, "ccc_rev_type"), "2")) "Interim" else "Scheduled",
    concern      = identical(ctx_val(latest, "ccc_concern"), "1"),
    competencies = ctx_checked_labels(latest, "ccc_competency", data_dict),
    actions      = ctx_checked_labels(latest, "ccc_action", data_dict, .CCC_ACTION_LABELS),
    status       = status,
    person       = ctx_val(latest, "ccc_fu_resp"),
    follow_up    = ctx_val(latest, "ccc_issues_follow_up"),
    notes        = ctx_val(latest, "ccc_interim")
  )
}

# ── Last period's goals: target vs what the ratings did ──────────────────────

# ILP domain -> field names (mirrors gmed:::.ilp_domain_fields; note the
# profics goal field's historical asymmetry).
.CTX_GOAL_DOMAINS <- list(
  pcmk    = list(label = "Patient Care / Medical Knowledge", goal = "goal_pcmk",
                 level = "goal_level_pcmk",    prior = "prior_goal_pcmk"),
  sbppbl  = list(label = "Systems-Based Practice / Practice-Based Learning", goal = "goal_sbppbl",
                 level = "goal_level_sbppbl",  prior = "prior_goal_sbppbl"),
  profics = list(label = "Professionalism / Interpersonal Communication", goal = "goal_subcomp_profics",
                 level = "goal_level_profics", prior = "prior_goal_profics")
)

.CTX_SUBCOMP_LABELS <- c(
  pc1 = "PC1: History", pc2 = "PC2: Physical Exam", pc3 = "PC3: Clinical Reasoning",
  pc4 = "PC4: Mgmt-Inpatient", pc5 = "PC5: Mgmt-Outpatient", pc6 = "PC6: Digital Health",
  mk1 = "MK1: Applied Sciences", mk2 = "MK2: Therapeutics", mk3 = "MK3: Diagnostics",
  sbp1 = "SBP1: Safety & QI", sbp2 = "SBP2: Navigation", sbp3 = "SBP3: Physician Role",
  pbl1 = "PBL1: Evidence-Based", pbl2 = "PBL2: Reflective",
  prof1 = "PROF1: Behavior", prof2 = "PROF2: Ethics", prof3 = "PROF3: Accountability",
  prof4 = "PROF4: Well-Being", ics1 = "ICS1: Patient Comm", ics2 = "ICS2: Team Comm",
  ics3 = "ICS3: Documentation"
)

#' ILP goal code -> milestone field stem ("pc3", "pbl1", ...).
ctx_goal_subcomp <- function(domain, code) {
  n <- suppressWarnings(as.integer(code))
  if (length(n) == 0 || is.na(n) || n < 1) return(NA_character_)
  switch(domain,
    pcmk    = if (n <= 6) paste0("pc", n)   else if (n <= 9) paste0("mk", n - 6)   else NA_character_,
    sbppbl  = if (n <= 3) paste0("sbp", n)  else if (n <= 5) paste0("pbl", n - 3)  else NA_character_,
    profics = if (n <= 4) paste0("prof", n) else if (n <= 7) paste0("ics", n - 4)  else NA_character_,
    NA_character_
  )
}

#' Goal level (1-5) -> milestone rating (1-9): 1=1, 2=3, 3=5, 4=7, 5=9.
ctx_goal_level_to_rating <- function(level) {
  lv <- suppressWarnings(as.numeric(level))
  ifelse(is.na(lv) | lv < 1 | lv > 5, NA_real_, 2 * lv - 1)
}

#' Program's expected rating for the stage a goal was set in:
#' intern 3, late intern / early PGY2 5, late PGY2 / PGY3 7.
ctx_expected_rating <- function(period_name) {
  switch(as.character(period_name),
    "Entering Residency" = 3, "Mid Intern" = 3,
    "End Intern" = 5, "Mid PGY2" = 5,
    "End PGY2" = 7, "Mid PGY3" = 7, "Graduating" = 7,
    NA_real_
  )
}

# Best available faculty rating for one subcompetency at one period:
# ACGME first, then coach (milestone_entry). CCC-entered ratings are not
# stored as a separate rating form yet, so they aren't in the hierarchy.
ctx_best_rating <- function(acgme_row, coach_row, subcomp) {
  a <- ctx_num(acgme_row, paste0("acgme_", subcomp))
  if (!is.na(a)) return(list(value = a, source = "ACGME"))
  c <- ctx_num(coach_row, paste0("rep_", subcomp))
  if (!is.na(c)) return(list(value = c, source = "Coach"))
  list(value = NA_real_, source = "")
}

#' One row per ILP domain comparing last period's goal with the ratings.
#'
#' @param prev_ilp ilp row(s) from the period the goals were set in.
#' @param curr_ilp ilp row(s) from the current period (holds prior_goal_*,
#'   the resident's own "did I meet it?").
#' @param prev_acgme,prev_coach acgme_miles / milestone_entry rows at the
#'   goal-setting period (baseline).
#' @param curr_acgme,curr_coach,curr_self the same forms for the current
#'   period, plus milestone_selfevaluation_c33c (used only when no faculty
#'   rating exists yet).
#' @param goal_period_name Period the goals were set in (for the
#'   stage-expectation check).
ctx_goal_followthrough <- function(prev_ilp, curr_ilp,
                                   prev_acgme, prev_coach,
                                   curr_acgme, curr_coach, curr_self,
                                   goal_period_name) {
  rows <- lapply(names(.CTX_GOAL_DOMAINS), function(dom) {
    f <- .CTX_GOAL_DOMAINS[[dom]]
    code <- ctx_val(prev_ilp, f$goal)
    sub <- ctx_goal_subcomp(dom, code)
    if (is.na(sub)) return(NULL)

    level  <- suppressWarnings(as.numeric(ctx_val(prev_ilp, f$level)))
    target <- ctx_goal_level_to_rating(level)
    expected <- ctx_expected_rating(goal_period_name)

    base <- ctx_best_rating(prev_acgme, prev_coach, sub)
    now  <- ctx_best_rating(curr_acgme, curr_coach, sub)
    self_now <- ctx_num(curr_self, paste0("rep_", sub, "_self"))

    verdict <- if (is.na(now$value)) {
      "Awaiting faculty rating"
    } else if (!is.na(target) && now$value >= target) {
      "Met"
    } else if (!is.na(base$value) && now$value > base$value) {
      "Progressing"
    } else if (!is.na(base$value) && now$value == base$value) {
      "No change"
    } else if (!is.na(base$value) && now$value < base$value) {
      "Declined"
    } else {
      "Not met"
    }

    says <- ctx_val(curr_ilp, f$prior)
    resident_says <- if (says == "1") "Met" else if (says %in% c("0", "2")) "Not met" else ""
    mismatch <- nzchar(resident_says) && verdict != "Awaiting faculty rating" &&
      ((resident_says == "Met") != (verdict == "Met"))

    data.frame(
      domain          = dom,
      domain_label    = f$label,
      subcomp         = sub,
      subcomp_label   = unname(.CTX_SUBCOMP_LABELS[sub]),
      goal_level      = level,
      target_rating   = target,
      expected_rating = expected,
      below_expected  = !is.na(target) && !is.na(expected) && target < expected,
      met_when_set    = !is.na(target) && !is.na(base$value) && base$value >= target,
      baseline        = base$value,
      baseline_source = base$source,
      current         = now$value,
      current_source  = now$source,
      self_current    = self_now,
      resident_says   = resident_says,
      verdict         = verdict,
      mismatch        = mismatch,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

# ── Duty-hour workload window ────────────────────────────────────────────────

#' Summarize one resident's duty hours between `since` and `until` from a
#' build_duty_hour_summary() result (amiontools).
#'
#' @return list(n_weeks, avg_weekly, peak_4wk_avg, peak_week, peak_category,
#'   weeks_over_80, days_per_week, low_days, gaps_hard, gaps_soft,
#'   days_off, class_days_off, verified_pct, weekly) or NULL if no data.
ctx_duty_window <- function(duty, record_id, since, until = Sys.Date()) {
  if (is.null(duty) || is.null(duty$weekly) || nrow(duty$weekly) == 0) return(NULL)
  rid <- as.character(record_id)
  since <- as.Date(since); until <- as.Date(until)

  wk <- duty$weekly[as.character(duty$weekly$record_id) == rid, , drop = FALSE]
  wk <- wk[wk$week_start >= since - 6 & wk$week_start <= until, , drop = FALSE]
  if (nrow(wk) == 0) return(NULL)
  wk <- wk[order(wk$week_start), , drop = FALSE]
  level <- as.character(wk$Level[1])

  peak_i <- which.max(wk$rolling_4wk_avg_hours)
  peak_week <- wk$week_start[peak_i]

  peak_category <- ""
  wbc <- duty$weekly_by_category
  if (!is.null(wbc) && nrow(wbc) > 0) {
    pc <- wbc[as.character(wbc$record_id) == rid & wbc$week_start == peak_week &
                wbc$super_category != "Time Off", , drop = FALSE]
    if (nrow(pc) > 0) {
      agg <- tapply(pc$Hours, pc$super_category, sum)
      peak_category <- names(agg)[which.max(agg)]
    }
  }

  in_window <- function(d) !is.na(d) & as.Date(d) >= since & as.Date(d) <= until

  gaps <- duty$rest_gap_flags
  gaps <- if (!is.null(gaps) && nrow(gaps) > 0) {
    gaps[as.character(gaps$record_id) == rid & in_window(as.Date(gaps$to_start)), , drop = FALSE]
  } else data.frame(severity = character(0))

  # Days off = dates whose duty block was zeroed as a real day off.
  off_counts <- function(ids) {
    db <- duty$duty_blocks
    if (is.null(db) || nrow(db) == 0) return(stats::setNames(numeric(0), character(0)))
    db <- db[db$source == "day_off" & in_window(db$Date) & as.character(db$record_id) %in% ids, , drop = FALSE]
    n <- tapply(db$Date, as.character(db$record_id), function(x) length(unique(x)))
    out <- stats::setNames(rep(0, length(ids)), ids)
    out[names(n)] <- as.numeric(n)
    out
  }
  class_ids <- unique(as.character(duty$weekly$record_id[as.character(duty$weekly$Level) == level]))
  class_off <- off_counts(union(class_ids, rid))

  verified_pct <- NA_real_
  if (!is.null(wbc) && nrow(wbc) > 0) {
    v <- wbc[as.character(wbc$record_id) == rid & wbc$week_start >= since - 6 &
               wbc$week_start <= until, , drop = FALSE]
    if (nrow(v) > 0 && sum(v$Days) > 0) verified_pct <- 100 * sum(v$Days[v$verified]) / sum(v$Days)
  }

  last <- wk[nrow(wk), , drop = FALSE]
  list(
    level          = level,
    since          = since,
    until          = until,
    n_weeks        = nrow(wk),
    avg_weekly     = mean(wk$Total_Hours, na.rm = TRUE),
    peak_4wk_avg   = wk$rolling_4wk_avg_hours[peak_i],
    peak_week      = peak_week,
    peak_category  = peak_category,
    weeks_over_80  = sum(wk$flag_80h, na.rm = TRUE),
    days_per_week  = last$rolling_days_per_week,
    low_days       = isTRUE(last$flag_low_days),
    gaps_hard      = sum(gaps$severity == "hard"),
    gaps_soft      = sum(gaps$severity == "soft"),
    days_off       = unname(class_off[rid]),
    class_days_off = if (length(class_off) > 0) mean(class_off) else NA_real_,
    verified_pct   = verified_pct,
    weekly         = wk[, intersect(c("week_start", "Total_Hours", "rolling_4wk_avg_hours", "flag_80h"),
                                    names(wk)), drop = FALSE]
  )
}
