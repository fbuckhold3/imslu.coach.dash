# =============================================================================
# Self-evaluation data card (resident review architecture, Phase 3).
#
# Shows the snapshot ind.dash froze when the resident submitted the
# self-eval (s_e_snapshot_json), the gate result, and any gate override.
# "Compare with today" / "Check today's data" builds a live snapshot in a
# future worker (all of RDM + Amion, ~30 s), only when clicked.
#
# "Clear gate" (Fred, 2026-11-10/11): any coach can clear the gate for an
# open, unsubmitted period, with a required reason and name. It writes the
# four s_e_gate_override* fields onto the EXISTING s_eval instance for that
# period (instance = period number, as ind.dash saves it), never a new one.
# coach.dash has one shared access code, so the name is self-reported.
#
# The resident's s_eval rows are read fresh from REDCap (one small call) when
# the card opens: the app's main data is loaded once per server process, so
# a self-eval submitted after that load wouldn't show otherwise.
# =============================================================================

# Gate periods: Mid Intern .. Graduation. Entering Residency (local 0 /
# REDCap 7) has no gate.
.GATE_PERIODS <- 1:6

.read_resident_forms <- function(record_id, forms) {
  res <- tryCatch(
    REDCapR::redcap_read_oneshot(
      redcap_uri = REDCAP_CONFIG$url, token = REDCAP_CONFIG$rdm_token,
      records = as.character(record_id), forms = forms, fields = "record_id",
      raw_or_label = "raw", verbose = FALSE),
    error = function(e) list(success = FALSE, outcome_message = conditionMessage(e)))
  if (!isTRUE(res$success)) stop(res$outcome_message %||% "REDCap read failed")
  d <- res$data
  out <- lapply(forms, function(f) {
    if (!"redcap_repeat_instrument" %in% names(d)) return(d[0, , drop = FALSE])
    d[!is.na(d$redcap_repeat_instrument) & d$redcap_repeat_instrument == f, , drop = FALSE]
  })
  stats::setNames(out, forms)
}

.seval_row <- function(seva, period) {
  if (is.null(seva) || nrow(seva) == 0 || !"s_e_period" %in% names(seva)) return(NULL)
  r <- seva[as.character(seva$s_e_period) == as.character(period), , drop = FALSE]
  if (nrow(r) == 0) NULL else r[1, , drop = FALSE]
}

.chr1 <- function(r, f) {
  v <- if (!is.null(r) && f %in% names(r)) as.character(r[[f]][1]) else NA_character_
  if (is.na(v) || !nzchar(trimws(v))) NA_character_ else trimws(v)
}

mod_gate_override_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "card mb-3",
    div(class = "card-header d-flex justify-content-between align-items-center",
        tags$strong("Self-evaluation data"),
        uiOutput(ns("override_btn"), inline = TRUE)),
    div(class = "card-body",
        uiOutput(ns("status")),
        reviewkit::mod_snapshot_panel_ui(ns("panel")))
  )
}

#' @param record_id_r Reactive record_id.
#' @param period_r Reactive local period number (0 = Entering Residency).
#' @param coach_name_r Reactive default for "Cleared by".
mod_gate_override_server <- function(id, record_id_r, period_r, coach_name_r = reactive(NA)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    seva <- reactiveVal(NULL)
    load_error <- reactiveVal(NULL)
    live <- reactiveVal(NULL)
    live_error <- reactiveVal(NULL)

    token <- REDCAP_CONFIG$rdm_token
    url   <- REDCAP_CONFIG$url

    reload <- function() {
      rid <- isolate(record_id_r())
      if (is.null(rid) || !nzchar(as.character(rid))) return(invisible())
      tryCatch({
        seva(.read_resident_forms(rid, "s_eval")$s_eval)
        load_error(NULL)
      }, error = function(e) {
        seva(NULL); load_error(conditionMessage(e))
      })
      invisible()
    }

    observeEvent(list(record_id_r(), period_r()), {
      live(NULL); live_error(NULL)
      reload()
    })

    period_code <- reactive({
      p <- suppressWarnings(as.integer(period_r()))
      if (length(p) == 0 || is.na(p)) NA_integer_ else p
    })
    gated <- reactive(isTRUE(period_code() %in% .GATE_PERIODS))
    row <- reactive(.seval_row(seva(), period_code()))

    snapshot <- reactive({
      j <- .chr1(row(), "s_e_snapshot_json")
      if (is.na(j)) return(NULL)
      tryCatch(reviewkit::snapshot_from_json(j), error = function(e) NULL)
    })
    override <- reactive({
      r <- row()
      if (is.null(r) || !(.chr1(r, "s_e_gate_override") %in% c("1", "Yes"))) return(NULL)
      list(reason = .chr1(r, "s_e_gate_override_reason"),
           by = .chr1(r, "s_e_gate_override_by"),
           date = .chr1(r, "s_e_gate_override_date"))
    })

    # Live snapshot, only on click.
    task <- ExtendedTask$new(function(rid, token, url) {
      promises::future_promise({
        inputs <- reviewkit::fetch_review_inputs(token, url, record_ids = rid)
        reviewkit::build_review_snapshot(rid, inputs$forms, inputs$activity,
                                         reviewkit::review_window())
      })
    })
    observeEvent(task$status(), {
      st <- task$status()
      if (st == "success") { live(task$result()); live_error(NULL) }
      if (st == "error") {
        live_error(tryCatch({ task$result(); "" }, error = function(e) conditionMessage(e)))
      }
    })
    live_status <- reactive({
      if (task$status() == "running") "running"
      else if (!is.null(live_error())) paste("Couldn't load today's data:", live_error())
      else "idle"
    })

    panel <- reviewkit::mod_snapshot_panel_server(
      "panel", snapshot_r = snapshot, live_r = live, override_r = override,
      live_status_r = live_status)
    observeEvent(panel$compare(), {
      rid <- as.character(record_id_r())
      if (task$status() != "running" && nzchar(rid)) task$invoke(rid, token, url)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    output$status <- renderUI({
      if (!is.null(load_error())) {
        return(div(class = "alert alert-danger mb-2",
                   "Couldn't read the self-evaluation from REDCap: ", load_error()))
      }
      if (!gated()) {
        return(p(class = "text-muted mb-2",
                 "Entering Residency has no data gate; numbers below are from today only if you check them."))
      }
      NULL
    })

    # "Clear gate": open, gated period, not submitted, not already cleared.
    can_clear <- reactive(gated() && is.null(snapshot()) && is.null(override()) &&
                            is.null(load_error()))
    output$override_btn <- renderUI({
      if (!can_clear()) return(NULL)
      actionButton(ns("clear"), "Clear gate", class = "btn-outline-secondary btn-sm",
                   icon = icon("unlock"))
    })

    observeEvent(input$clear, {
      who <- coach_name_r()
      showModal(modalDialog(
        title = "Clear the data gate",
        p("The resident will be able to submit this period's self-evaluation ",
          "without finishing duty hours and conference logging. The snapshot ",
          "still records what is missing."),
        p(class = "text-muted small",
          "Use this for leave, an Amion error, or another reason the data can't be completed."),
        textAreaInput(ns("ov_reason"), "Reason (required)", width = "100%", rows = 3),
        textInput(ns("ov_by"), "Cleared by (required)",
                  value = if (is.na(who %||% NA)) "" else who, width = "100%"),
        uiOutput(ns("ov_msg")),
        footer = tagList(modalButton("Cancel"),
                         actionButton(ns("ov_save"), "Clear gate", class = "btn-primary")),
        easyClose = FALSE
      ))
    })

    ov_msg <- reactiveVal(NULL)
    output$ov_msg <- renderUI(ov_msg())

    observeEvent(input$ov_save, {
      reason <- trimws(input$ov_reason %||% "")
      by <- trimws(input$ov_by %||% "")
      if (!nzchar(reason) || !nzchar(by)) {
        ov_msg(div(class = "text-danger small", "Enter a reason and your name."))
        return()
      }
      p <- period_code()
      rid <- as.character(record_id_r())
      req(p %in% .GATE_PERIODS, nzchar(rid))

      rec <- data.frame(
        record_id = rid, redcap_repeat_instrument = "s_eval",
        redcap_repeat_instance = p, s_e_period = as.character(p),
        s_e_gate_override = "1", s_e_gate_override_reason = reason,
        s_e_gate_override_by = by,
        s_e_gate_override_date = format(Sys.Date(), "%Y-%m-%d"),
        stringsAsFactors = FALSE)
      res <- tryCatch(
        REDCapR::redcap_write_oneshot(ds = rec, redcap_uri = url, token = token, verbose = FALSE),
        error = function(e) list(success = FALSE, outcome_message = conditionMessage(e)))
      if (!isTRUE(res$success)) {
        ov_msg(div(class = "text-danger small", "REDCap didn't save it: ",
                   res$outcome_message %||% "unknown error"))
        return()
      }
      ov_msg(NULL)
      removeModal()
      reload()
      showNotification("Gate cleared. The resident can submit now.", type = "message")
    })
  })
}
