# =============================================================================
# Resident thread + interim notes (resident review architecture, Phase 3).
#
# One dated thread per resident, newest first: coach interim notes
# (coach_interim), CCC reviews (ccc_review, scheduled and interim, with
# concern / actions / status) and each review's coach_summary. Built by
# reviewkit::build_resident_thread(), so second review and the CCC show the
# same entries.
#
# "Add interim note" saves a NEW coach_interim instance. It is not tied to a
# review period and is available any time. The thread's three forms are read
# fresh from REDCap for this resident when the card opens and after a save
# (the app's main data is loaded once per server process).
# =============================================================================

.CI_REASONS <- c("Resident-initiated" = "1", "CCC follow-up" = "2",
                 "Concern" = "3", "Other" = "4")

mod_interim_note_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "card mb-3",
    div(class = "card-header d-flex justify-content-between align-items-center",
        tags$strong("Interim notes and CCC thread"),
        actionButton(ns("add"), "Add interim note", class = "btn-outline-primary btn-sm",
                     icon = icon("plus"))),
    div(class = "card-body",
        uiOutput(ns("error")),
        reviewkit::mod_thread_ui(ns("thread")))
  )
}

#' @param record_id_r Reactive record_id.
#' @param coach_name_r Reactive default for the note's coach name.
mod_interim_note_server <- function(id, record_id_r, coach_name_r = reactive(NA)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    forms <- reactiveVal(NULL)
    load_error <- reactiveVal(NULL)

    reload <- function() {
      rid <- isolate(record_id_r())
      if (is.null(rid) || !nzchar(as.character(rid))) return(invisible())
      tryCatch({
        forms(.read_resident_forms(rid, c("coach_interim", "ccc_review", "coach_rev")))
        load_error(NULL)
      }, error = function(e) {
        forms(NULL); load_error(conditionMessage(e))
      })
      invisible()
    }
    observeEvent(record_id_r(), { forms(NULL); reload() })

    thread <- reactive({
      f <- forms()
      if (is.null(f)) {
        if (is.null(load_error())) return(NULL)   # still loading
        return(reviewkit::build_resident_thread(list(), as.character(record_id_r())))
      }
      reviewkit::build_resident_thread(f, as.character(record_id_r()))
    })
    reviewkit::mod_thread_server("thread", thread)

    output$error <- renderUI({
      if (is.null(load_error())) return(NULL)
      div(class = "alert alert-danger mb-2", "Couldn't read the thread from REDCap: ", load_error())
    })

    observeEvent(input$add, {
      who <- coach_name_r()
      showModal(modalDialog(
        title = "Add interim note",
        dateInput(ns("ci_date"), "Meeting date", value = Sys.Date(), max = Sys.Date()),
        textInput(ns("ci_coach"), "Coach",
                  value = if (is.na(who %||% NA)) "" else who, width = "100%"),
        radioButtons(ns("ci_reason"), "Reason for meeting", choices = .CI_REASONS,
                     selected = character(0)),
        conditionalPanel(sprintf("input['%s'] == '4'", ns("ci_reason")),
                         textInput(ns("ci_reason_other"), "Other reason", width = "100%")),
        textAreaInput(ns("ci_notes"), "Notes", rows = 5, width = "100%"),
        checkboxInput(ns("ci_ccc_flag"), "Flag for the CCC", value = FALSE),
        dateInput(ns("ci_followup_date"), "Follow-up date (optional)", value = NA,
                  min = Sys.Date()),
        uiOutput(ns("msg")),
        footer = tagList(modalButton("Cancel"),
                         actionButton(ns("save"), "Save note", class = "btn-primary")),
        easyClose = FALSE, size = "l"
      ))
    })

    msg <- reactiveVal(NULL)
    output$msg <- renderUI(msg())

    observeEvent(input$save, {
      notes  <- trimws(input$ci_notes %||% "")
      reason <- input$ci_reason %||% ""
      coach  <- trimws(input$ci_coach %||% "")
      d      <- input$ci_date
      if (!nzchar(notes) || !nzchar(reason) || !nzchar(coach) || length(d) == 0 || is.na(d)) {
        msg(div(class = "text-danger small", "Enter the date, coach, reason and notes."))
        return()
      }
      rid <- as.character(record_id_r()); req(nzchar(rid))

      # Next instance from a fresh read, so two coaches saving the same
      # resident don't collide on a stale count.
      existing <- tryCatch(.read_resident_forms(rid, "coach_interim")$coach_interim,
                           error = function(e) NULL)
      if (is.null(existing)) {
        msg(div(class = "text-danger small", "Couldn't reach REDCap. Try again."))
        return()
      }
      inst <- suppressWarnings(max(c(0L, as.integer(existing$redcap_repeat_instance)), na.rm = TRUE)) + 1L

      fu <- input$ci_followup_date
      rec <- data.frame(
        record_id = rid, redcap_repeat_instrument = "coach_interim",
        redcap_repeat_instance = inst,
        ci_date = format(as.Date(d), "%Y-%m-%d"),
        ci_coach = coach, ci_reason = reason,
        ci_reason_other = if (reason == "4") trimws(input$ci_reason_other %||% "") else "",
        ci_notes = notes,
        ci_ccc_flag = if (isTRUE(input$ci_ccc_flag)) "1" else "0",
        ci_followup_date = if (length(fu) == 0 || is.na(fu)) "" else format(as.Date(fu), "%Y-%m-%d"),
        coach_interim_complete = "2",
        stringsAsFactors = FALSE)
      res <- tryCatch(
        REDCapR::redcap_write_oneshot(ds = rec, redcap_uri = REDCAP_CONFIG$url,
                                      token = REDCAP_CONFIG$rdm_token, verbose = FALSE),
        error = function(e) list(success = FALSE, outcome_message = conditionMessage(e)))
      if (!isTRUE(res$success)) {
        msg(div(class = "text-danger small", "REDCap didn't save it: ",
                res$outcome_message %||% "unknown error"))
        return()
      }
      msg(NULL)
      removeModal()
      reload()
      showNotification("Interim note saved.", type = "message")
    })
  })
}
