# Manual smoke test for the pass-1 coach context wiring.
# Runs the changed Shiny modules with shiny::testServer() against fake data
# (no REDCap). Needs gmed + roundsui installed. From the repo root:
#   RDM_TOKEN=x ACCESS_CODE=x Rscript tests/manual/smoke_pass1_modules.R

suppressMessages(source("R/globals.R"))
library(testthat)

rd <- list(
  resident_info = data.frame(record_id = "10", full_name = "Test Resident", Level = "PGY2",
                             access_code = "abc", stringsAsFactors = FALSE),
  current_period = list(
    s_eval = data.frame(record_id = "10", s_e_well = "Tired", stringsAsFactors = FALSE),
    coach_rev = data.frame(),
    ilp = data.frame(prior_goal_pcmk = "1", stringsAsFactors = FALSE),
    milestone_entry = data.frame(),
    milestone_selfevaluation = data.frame(rep_pc3_self = "6", stringsAsFactors = FALSE),
    acgme_miles = data.frame()
  ),
  previous_period = list(
    s_eval = data.frame(),
    coach_rev = data.frame(coach_career = "Consider cards", stringsAsFactors = FALSE),
    ilp = data.frame(goal_pcmk = "3", goal_level_pcmk = "3", stringsAsFactors = FALSE),
    acgme_miles = data.frame(acgme_pc3 = "4", stringsAsFactors = FALSE),
    milestone_entry = data.frame(),
    ccc_review = data.frame()
  ),
  coach_rev_history = data.frame(
    record_id = "10", redcap_repeat_instrument = "coach_rev",
    coach_period = c("Mid Intern", "End Intern"), coach_date = c("2025-12-01", "2026-05-01"),
    coach_wellness = c("Settling in", "Busy spring"), coach_career = c("", "Consider cards"),
    coach_ls_and_topic = c("Likes cases", ""), coach_step_board = c("", "Step 3 in May"),
    coach_p_d_comments = "PD note", coach_evaluations = "",
    stringsAsFactors = FALSE)
)
resident_data <- reactive(rd)
current_period <- reactive(3)   # local numbering: 3 = Mid PGY2

test_that("wellness renders previous notes and the workload panel", {
  testServer(mod_wellness_server, args = list(resident_data = resident_data,
                                              current_period = current_period,
                                              app_data = reactive(NULL)), {
    session$flushReact()
    notes <- output$`prev_notes-notes`$html
    expect_match(notes, "Busy spring")
    expect_match(notes, "1 earlier note")
    expect_match(output$`workload-body`$html, "amiontools|Load duty hours|Workload")
  })
})

test_that("goals follow-through prefers the live coach rating", {
  live <- reactiveVal(data.frame(rep_pc3 = "5", stringsAsFactors = FALSE))
  testServer(mod_goals_server, args = list(resident_data = resident_data,
                                           current_period = current_period,
                                           app_data = reactive(NULL),
                                           app_data_rv = reactiveValues(data_dict = NULL),
                                           live_coach_row = live), {
    session$flushReact()
    html <- output$followthrough$html
    expect_match(html, "PC3: Clinical Reasoning")
    expect_match(html, "End Intern")
    expect_match(html, "Met")
    live(NULL)                     # no live ratings, nothing saved -> awaiting
    session$flushReact()
    expect_match(output$followthrough$html, "Awaiting faculty rating")
  })
})

test_that("career and learning show all earlier notes", {
  testServer(mod_career_server, args = list(resident_data = resident_data,
                                            current_period = current_period,
                                            app_data = reactive(NULL)), {
    session$flushReact()
    expect_match(output$previous_coach_comments$html, "Consider cards")
  })
  testServer(mod_learning_server, args = list(resident_data = resident_data,
                                              current_period = current_period,
                                              app_data = reactive(NULL),
                                              app_data_rv = reactiveValues(data_dict = NULL)), {
    session$flushReact()
    html <- output$previous_coach_comments$html
    expect_match(html, "Likes cases")
    expect_match(html, "Step 3 in May")
  })
})

test_that("review UI builds with the roundsui shell pieces", {
  ui <- mod_review_interface_ui("review")
  html <- as.character(ui)
  expect_match(html, "review-ccc_banner")
  for (m in list(mod_wellness_ui("w"), mod_goals_ui("g", period_num = 3),
                 mod_learning_ui("l"), mod_career_ui("c"), mod_evaluations_ui("e"))) {
    expect_true(nchar(as.character(m)) > 0)
  }
})


test_that("review interface: header panel and CCC banner from all_forms", {
  forms <- list(
    coach_rev = rd$coach_rev_history,
    ccc_review = data.frame(
      record_id = "10", redcap_repeat_instrument = "ccc_review", redcap_repeat_instance = 1,
      ccc_date = "2026-03-01", ccc_session = "End Intern", ccc_rev_type = "2", ccc_concern = "1",
      ccc_action___4 = "1", ccc_action_status___2 = "1", ccc_fu_resp = "Coach",
      ccc_issues_follow_up = "Weekly check-ins", stringsAsFactors = FALSE),
    ilp = data.frame(record_id = "10", redcap_repeat_instrument = "ilp",
                     year_resident = "End Intern", goal_pcmk = "3", goal_level_pcmk = "3",
                     stringsAsFactors = FALSE)
  )
  rdm <- list(residents = data.frame(record_id = "10", full_name = "Test Resident", Level = "PGY2",
                                     access_code = "abc", stringsAsFactors = FALSE),
              all_forms = forms)
  # suppressWarnings: gmed's plotly charts warn on the tiny fake dataset.
  suppressWarnings(testServer(mod_review_interface_server, args = list(
    selected_resident = reactive(list(record_id = "10", role = "primary")),
    rdm_data = reactive(rdm),
    current_period = reactive(3),
    app_data_rv = reactiveValues(data_dict = NULL)), {
    session$flushReact()
    expect_match(output$resident_header$html, "Test Resident")
    expect_match(output$resident_header$html, "roundsui-resident-panel")
    banner <- output$ccc_banner$html
    expect_match(banner, "Monitored by the CCC")
    expect_match(banner, "Coach follow up")
    expect_match(banner, "Ongoing")
    expect_match(output$`goals-followthrough`$html, "PC3")
  }))
})

test_that("workload panel renders from a cached duty summary", {
  weeks <- Sys.Date() - 7 * 5:0
  duty_cache_set(list(
    weekly = data.frame(record_id = "10", name = "T", Level = "PGY2", week_start = weeks,
                        Total_Hours = c(60, 70, 85, 90, 88, 50),
                        rolling_4wk_avg_hours = c(60, 65, 71.7, 76.3, 83.3, 78.3),
                        flag_80h = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
                        rolling_days_per_week = 5.8, flag_low_days = FALSE),
    weekly_by_category = data.frame(record_id = "10", week_start = weeks[5], super_category = "Inpatient",
                                    verified = TRUE, needs_entry = FALSE, Hours = 88, Days = 6),
    rest_gap_flags = data.frame(record_id = character(0), to_start = as.POSIXct(character(0)),
                                severity = character(0)),
    duty_blocks = data.frame(record_id = "10", Date = weeks[2], source = "day_off")
  ))
  testServer(mod_workload_server, args = list(resident_data = resident_data,
                                              current_period = current_period), {
    session$flushReact()
    html <- output$body$html
    expect_match(html, "Weeks &gt; 80h|Weeks > 80h")
    expect_match(html, "Inpatient")
    expect_match(html, "1 \\(class 1.0\\)")
    expect_true(!is.null(output$chart))
  })
  .DUTY_CACHE$data <- NULL
})

cat("\nPass-1 module smoke test finished.\n")
