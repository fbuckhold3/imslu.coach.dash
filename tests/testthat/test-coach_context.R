# Unit tests for R/helpers/coach_context.R (pure helpers, synthetic data).
# Run from the repo root: Rscript -e 'testthat::test_dir("tests/testthat")'

source(file.path(testthat::test_path("..", ".."), "R", "helpers", "coach_context.R"))

dd <- data.frame(
  field_name = c("ccc_competency", "ccc_action_status"),
  select_choices_or_calculations = c("1, Patient Care | 2, Medical Knowledge | 3, Professionalism",
                                     "1, Initiation | 2, Ongoing | 3, Resolved | 4, Recurring"),
  stringsAsFactors = FALSE
)

test_that("choices parser handles codes and labels with commas", {
  ch <- ctx_field_choices(data.frame(field_name = "x",
    select_choices_or_calculations = "1, A | 2, B, with comma", stringsAsFactors = FALSE), "x")
  expect_equal(unname(ch), c("A", "B, with comma"))
  expect_equal(names(ch), c("1", "2"))
  expect_length(ctx_field_choices(dd, "missing"), 0)
})

test_that("previous notes exclude current and later periods, newest first", {
  cr <- data.frame(
    record_id = "10", redcap_repeat_instrument = "coach_rev",
    coach_period = c("End Intern", "Mid Intern", "Mid PGY2", "Entering Residency"),
    coach_date = c("2026-05-01", "2025-12-01", "2026-12-01", "2025-08-01"),
    coach_wellness = c("Tired but ok", "Adjusting well", "Current draft", ""),
    stringsAsFactors = FALSE
  )
  hist <- ctx_coach_rev_history(list(coach_rev = cr), "10")
  expect_equal(hist$coach_period, c("Entering Residency", "Mid Intern", "End Intern", "Mid PGY2"))
  notes <- ctx_previous_notes(hist, "coach_wellness", "Mid PGY2")
  expect_equal(notes$period, c("End Intern", "Mid Intern"))
  expect_equal(notes$text[1], "Tired but ok")
  expect_equal(nrow(ctx_previous_notes(hist, "no_such_field", "Mid PGY2")), 0)
  expect_equal(nrow(ctx_coach_rev_history(list(), "10")), 0)
})

test_that("CCC status picks latest flagged review and detects resolution", {
  cc <- data.frame(
    record_id = "10", redcap_repeat_instrument = "ccc_review",
    redcap_repeat_instance = c(1, 2, 3),
    ccc_date = c("2025-12-10", "2026-03-01", "2026-06-10"),
    ccc_session = c("Mid Intern", "End Intern", "End Intern"),
    ccc_rev_type = c("1", "2", "1"),
    ccc_concern = c("1", "1", "0"),
    ccc_competency___1 = c("0", "1", "0"), ccc_competency___3 = c("1", "0", "0"),
    ccc_action___1 = c("1", "0", "0"), ccc_action___4 = c("0", "1", "0"),
    ccc_action_status___1 = c("1", "0", "0"), ccc_action_status___2 = c("0", "1", "0"),
    ccc_action_status___3 = c("0", "0", "0"),
    ccc_fu_resp = c("PD", "Coach", ""), ccc_issues_follow_up = c("", "Weekly check-ins", ""),
    stringsAsFactors = FALSE
  )
  st <- ctx_ccc_status(list(ccc_review = cc), "10", dd)
  expect_equal(st$state, "active")
  expect_equal(st$date, "2026-03-01")           # row 3 raised nothing
  expect_equal(st$type, "Interim")
  expect_equal(st$competencies, "Patient Care")
  expect_equal(st$actions, "Coach follow up")
  expect_equal(st$status, "Ongoing")
  expect_equal(st$follow_up, "Weekly check-ins")

  cc$ccc_action_status___2[2] <- "0"; cc$ccc_action_status___3[2] <- "1"
  expect_equal(ctx_ccc_status(list(ccc_review = cc), "10", dd)$state, "resolved")
  expect_equal(ctx_ccc_status(list(ccc_review = cc), "99", dd)$state, "none")
  expect_equal(ctx_ccc_status(list(), "10")$state, "none")
})

test_that("goal code, level and stage mappings", {
  expect_equal(ctx_goal_subcomp("pcmk", "3"), "pc3")
  expect_equal(ctx_goal_subcomp("pcmk", "8"), "mk2")
  expect_equal(ctx_goal_subcomp("sbppbl", "4"), "pbl1")
  expect_equal(ctx_goal_subcomp("profics", "7"), "ics3")
  expect_true(is.na(ctx_goal_subcomp("pcmk", "")))
  expect_true(is.na(ctx_goal_subcomp("pcmk", "10")))
  expect_equal(ctx_goal_level_to_rating(1:5), c(1, 3, 5, 7, 9))
  expect_true(is.na(ctx_goal_level_to_rating(6)))
  expect_equal(ctx_expected_rating("Mid Intern"), 3)
  expect_equal(ctx_expected_rating("Mid PGY2"), 5)
  expect_equal(ctx_expected_rating("Mid PGY3"), 7)
})

test_that("goal follow-through verdicts, source hierarchy and mismatch", {
  prev_ilp <- data.frame(goal_pcmk = "3", goal_level_pcmk = "3",        # PC3, target 5
                         goal_sbppbl = "1", goal_level_sbppbl = "2",    # SBP1, target 3
                         goal_subcomp_profics = "", stringsAsFactors = FALSE)
  curr_ilp <- data.frame(prior_goal_pcmk = "1", prior_goal_sbppbl = "1", stringsAsFactors = FALSE)
  prev_acgme <- data.frame(acgme_pc3 = "4", stringsAsFactors = FALSE)
  prev_coach <- data.frame(rep_pc3 = "2", rep_sbp1 = "3", stringsAsFactors = FALSE)
  curr_acgme <- data.frame()
  curr_coach <- data.frame(rep_pc3 = "5", rep_sbp1 = "3", stringsAsFactors = FALSE)
  curr_self  <- data.frame(rep_pc3_self = "6", stringsAsFactors = FALSE)

  g <- ctx_goal_followthrough(prev_ilp, curr_ilp, prev_acgme, prev_coach,
                              curr_acgme, curr_coach, curr_self, "End Intern")
  expect_equal(nrow(g), 2)                       # profics goal blank -> skipped
  pc <- g[g$subcomp == "pc3", ]
  expect_equal(pc$baseline, 4); expect_equal(pc$baseline_source, "ACGME")
  expect_equal(pc$current, 5);  expect_equal(pc$current_source, "Coach")
  expect_equal(pc$verdict, "Met"); expect_false(pc$mismatch)
  expect_false(pc$below_expected)               # target 5 == expected 5

  sb <- g[g$subcomp == "sbp1", ]
  expect_equal(sb$verdict, "Met")                # 3 >= target 3
  expect_true(sb$below_expected)                 # target 3 < expected 5 at End Intern
  expect_true(sb$met_when_set)                   # baseline already 3

  # No faculty rating yet -> awaiting, never a mismatch
  g2 <- ctx_goal_followthrough(prev_ilp, curr_ilp, prev_acgme, prev_coach,
                               data.frame(), data.frame(), curr_self, "End Intern")
  expect_equal(g2$verdict[g2$subcomp == "pc3"], "Awaiting faculty rating")
  expect_equal(g2$self_current[g2$subcomp == "pc3"], 6)
  expect_false(any(g2$mismatch))

  # Resident says met, rating fell -> mismatch
  g3 <- ctx_goal_followthrough(prev_ilp, curr_ilp, prev_acgme, prev_coach,
                               data.frame(), data.frame(rep_pc3 = "3"), curr_self, "End Intern")
  expect_equal(g3$verdict[g3$subcomp == "pc3"], "Declined")
  expect_true(g3$mismatch[g3$subcomp == "pc3"])

  expect_equal(nrow(ctx_goal_followthrough(data.frame(), curr_ilp, NULL, NULL,
                                           NULL, NULL, NULL, "End Intern")), 0)
})

test_that("duty window summarises hours, flags, days off and verification", {
  weeks <- as.Date("2026-08-02") + 7 * 0:5
  weekly <- rbind(
    data.frame(record_id = "10", name = "A", Level = "PGY2", week_start = weeks,
               Total_Hours = c(60, 70, 85, 90, 88, 50),
               rolling_4wk_avg_hours = c(60, 65, 71.7, 76.3, 83.3, 78.3),
               flag_80h = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
               rolling_days_per_week = 5.8, flag_low_days = FALSE),
    data.frame(record_id = "11", name = "B", Level = "PGY2", week_start = weeks,
               Total_Hours = 55, rolling_4wk_avg_hours = 55, flag_80h = FALSE,
               rolling_days_per_week = 5.5, flag_low_days = FALSE)
  )
  wbc <- data.frame(record_id = "10", week_start = weeks[5],
                    super_category = c("Inpatient", "Continuity Clinic"),
                    verified = c(TRUE, FALSE), needs_entry = FALSE,
                    Hours = c(80, 8), Days = c(5, 1))
  blocks <- data.frame(record_id = c("10", "10", "11", "11", "11"),
                       Date = as.Date(c("2026-08-09", "2026-08-16", "2026-08-09", "2026-08-10", "2026-08-11")),
                       source = c("day_off", "day_off", "day_off", "day_off", "default"))
  gaps <- data.frame(record_id = "10", to_start = as.POSIXct("2026-08-20 07:00", tz = "UTC"),
                     severity = c("hard", "soft"))
  duty <- list(weekly = weekly, weekly_by_category = wbc, duty_blocks = blocks, rest_gap_flags = gaps)

  s <- ctx_duty_window(duty, "10", since = as.Date("2026-08-02"), until = as.Date("2026-09-12"))
  expect_equal(s$n_weeks, 6)
  expect_equal(s$weeks_over_80, 1)
  expect_equal(s$peak_4wk_avg, 83.3)
  expect_equal(s$peak_category, "Inpatient")
  expect_equal(s$gaps_hard, 1); expect_equal(s$gaps_soft, 1)
  expect_equal(s$days_off, 2)
  expect_equal(s$class_days_off, 2)              # (2 + 2) / 2
  expect_equal(round(s$verified_pct), 83)        # 5 of 6 days
  expect_null(ctx_duty_window(duty, "99", as.Date("2026-08-02")))
  expect_null(ctx_duty_window(NULL, "10", as.Date("2026-08-02")))
})
