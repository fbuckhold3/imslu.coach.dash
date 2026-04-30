# Section: Graduation Plan & Alumni (P6 only)
#
# Read-only display of the resident's graduation submission lives in
# gmed::mod_grad_plan_display + gmed::mod_seval_boards_display. Coach also
# verifies the resident-submitted info; if incorrect, an inline edit form
# writes corrections back to the resident_data form. Coach summary is
# captured in coach_ilp_final (which surfaces in CCC dashboard).

mod_grad_plan_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # 1) Career path / chief / contact (gmed) — section card already labels it
    gmed::mod_grad_plan_display_ui(ns("plan"), title = NULL),

    # 2) Verification gate
    wellPanel(
      style = "background:#fff8e1; border-left:4px solid #f59e0b; margin-top:14px;",
      tags$label(
        "Is the graduation information above correct?",
        style = "font-weight:600; color:#2c3e50; font-size:0.95rem;"
      ),
      radioButtons(
        ns("verify_grad"),
        label    = NULL,
        choices  = c("Yes, information is accurate" = "1",
                     "No, needs correction"         = "0"),
        selected = character(0),
        inline   = FALSE
      ),
      uiOutput(ns("edit_panel")),
      uiOutput(ns("save_status"))
    ),

    hr(),

    # 3) Boards / Step 3 / MKSAP (gmed) — graduates still need Step 3
    h4("Boards & Step 3 Status", style = "color: #34495e; margin-top: 10px;"),
    gmed::mod_seval_boards_display_ui(ns("boards")),

    hr(),

    # 4) Coach summary (writes to coach_ilp_final)
    h4("Graduation Summary & Transition Notes",
       style = "color: #34495e; margin-top: 20px;"),

    wellPanel(
      style = "background-color: #ffffff; border-left: 4px solid #6610f2;",
      div(
        style = "margin-bottom: 15px;",
        tags$label(
          "Coach Summary on Graduation:",
          style = "font-weight: bold; color: #2c3e50;"
        ),
        tags$p(
          "Comprehensive note on the resident's transition out of residency.",
          style = "font-size: 12px; color: #7f8c8d; margin-top: 5px;"
        ),
        tags$ul(
          style = "font-size: 12px; color: #7f8c8d; margin-top: 5px;",
          tags$li("Post-residency plans and readiness"),
          tags$li("Board preparation status and Step 3 timeline"),
          tags$li("Areas of strength and areas for ongoing development"),
          tags$li("Alumni connection / staying in touch"),
          tags$li("Any concerns or follow-up items")
        )
      ),
      textAreaInput(
        ns("coach_ilp_final"),
        label = NULL,
        value = "",
        width = "100%",
        height = "300px",
        placeholder = paste(
          "Enter your graduation summary covering transition plans, board",
          "readiness, strengths, ongoing development areas, and alumni",
          "follow-up..."
        )
      ),
      div(
        style = "text-align: right; font-size: 12px; color: #95a5a6;",
        textOutput(ns("char_count"))
      )
    )
  )
}

mod_grad_plan_server <- function(id, resident_data, current_period,
                                 app_data, app_data_rv = NULL) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    data_dict <- reactive({
      if (is.null(app_data_rv)) return(NULL)
      app_data_rv$data_dict
    })

    # Local override of resident_info fields (after coach saves edits) so
    # the gmed display reflects the new values without a full data reload.
    overrides <- reactiveVal(list())

    res_row <- reactive({
      rd <- resident_data(); if (is.null(rd)) return(NULL)
      r  <- rd$resident_info
      ov <- overrides()
      if (length(ov)) for (nm in names(ov)) r[[nm]] <- ov[[nm]]
      r
    })

    # ----- Graduation plan display (career path / chief / contact) -----
    gmed::mod_grad_plan_display_server(
      "plan",
      seval_row = reactive({
        rd <- resident_data(); if (is.null(rd)) NULL else rd$current_period$s_eval
      }),
      resident_row = res_row,
      data_dict = data_dict
    )

    # ----- Boards / Step 3 / MKSAP (reused from mod_learning) -----
    gmed::mod_seval_boards_display_server(
      "boards",
      current_s_eval = reactive({
        rd <- resident_data(); if (is.null(rd)) NULL else rd$current_period$s_eval
      }),
      resident_info  = res_row,
      test_data = reactive({
        ad <- app_data(); rd <- resident_data()
        if (is.null(ad) || is.null(rd)) return(NULL)
        td <- ad$all_forms$test_data
        rid <- rd$resident_info$record_id[1]
        if (is.null(td) || !nrow(td)) NULL else td[td$record_id == rid, , drop = FALSE]
      }),
      pgy = reactive({
        rd <- resident_data(); if (is.null(rd)) return(NA_integer_)
        lvl <- rd$resident_info$Level
        dplyr::case_when(
          lvl == "Intern" ~ 1L,
          lvl == "PGY2"   ~ 2L,
          lvl == "PGY3"   ~ 3L,
          grepl("PGY[0-9]", lvl) ~ as.integer(gsub("[^0-9]", "", lvl)),
          TRUE ~ NA_integer_
        )
      }),
      data_dict = data_dict
    )

    # ----- Character count -----
    output$char_count <- renderText({
      sprintf("%d characters", nchar(input$coach_ilp_final %||% ""))
    })

    # ----- Resident-change handling: clear verify radio + load coach_ilp_final
    current_resident_id <- reactiveVal(NULL)

    observe({
      req(resident_data())
      new_id <- resident_data()$resident_info$record_id[1]

      if (is.null(current_resident_id()) || current_resident_id() != new_id) {
        current_resident_id(new_id)
        overrides(list())
        updateRadioButtons(session, "verify_grad", selected = character(0))

        curr <- resident_data()$current_period$coach_rev
        existing <- if (!is.null(curr) && nrow(curr) > 0) curr$coach_ilp_final[1] else NA

        updateTextAreaInput(
          session, "coach_ilp_final",
          value = if (!is.na(existing) && nzchar(existing)) existing else ""
        )
      }
    })

    # ----- Edit panel: shown only when coach answers "No" -----
    output$edit_panel <- renderUI({
      if (!identical(input$verify_grad, "0")) return(NULL)

      r  <- res_row(); if (is.null(r)) return(NULL)
      dd <- data_dict()

      .v <- function(fld) {
        if (!fld %in% names(r)) return("")
        x <- r[[fld]][1]; if (is.null(x) || is.na(x)) "" else as.character(x)
      }

      yn_choices <- c("Yes" = "1", "No" = "0")

      grad_spec_choices <- .grad_spec_choices_from_dd(dd)

      div(
        class = "mt-3 p-3",
        style = "background:#ffffff; border:1px solid #f0c463; border-radius:6px;",
        h6("Edit graduation information",
           style = "color:#92400e; font-weight:700;"),
        p(class = "small text-muted",
          "These fields write to resident_data when you click Save. To",
          "change the post-residency path itself (Primary Care / Hospitalist",
          "/ Fellowship / Other), the resident must update their P6",
          "self-evaluation in the resident dashboard."),

        fluidRow(
          column(6, radioButtons(ns("edit_chief"),
            "Staying as Chief Resident?",
            choices = yn_choices, selected = .v("chief"), inline = TRUE)),
          column(6, textInput(ns("edit_grad_email"),
            "Graduate email", value = .v("grad_email")))
        ),
        fluidRow(
          column(6, textInput(ns("edit_grad_phone"),
            "Graduate phone", value = .v("grad_phone"))),
          column(6, selectInput(ns("edit_grad_spec"),
            "Fellowship specialty (if applicable)",
            choices  = c("—" = "", grad_spec_choices),
            selected = .v("grad_spec")))
        ),
        hr(),
        h6("Practice details (Primary Care / Hospitalist / Other)",
           style = "color:#475569;"),
        fluidRow(
          column(8, textInput(ns("edit_res_alumni_position"),
            "Position / planned position",
            value = .v("res_alumni_position"))),
          column(4, radioButtons(ns("edit_res_alumni_academic"),
            "Academic medicine?",
            choices = yn_choices, selected = .v("res_alumni_academic"),
            inline = TRUE))
        ),
        fluidRow(
          column(3, radioButtons(ns("edit_ssm"),
            "Within SSM Health?",
            choices = yn_choices, selected = .v("ssm"), inline = TRUE)),
          column(3, radioButtons(ns("edit_mo_prac"),
            "Practicing in MO?",
            choices = yn_choices, selected = .v("mo_prac"), inline = TRUE)),
          column(3, radioButtons(ns("edit_rural"),
            "Rural setting?",
            choices = yn_choices, selected = .v("rural"), inline = TRUE)),
          column(3, radioButtons(ns("edit_und_urban"),
            "Underserved / urban?",
            choices = yn_choices, selected = .v("und_urban"), inline = TRUE))
        ),
        div(style = "text-align:right; margin-top:8px;",
          actionButton(ns("save_grad"), "Save corrections",
                       class = "btn-warning",
                       icon = icon("floppy-disk")))
      )
    })

    # ----- Save corrections to REDCap (resident_data form) -----
    save_state <- reactiveVal(NULL)
    output$save_status <- renderUI({
      st <- save_state(); if (is.null(st)) return(NULL)
      cls <- if (isTRUE(st$success)) "alert alert-success mt-2" else "alert alert-danger mt-2"
      div(class = cls, style = "padding:8px 12px; font-size:0.85rem;", st$message)
    })

    observeEvent(input$save_grad, {
      rd <- resident_data(); req(rd)
      rid <- rd$resident_info$record_id[1]

      payload <- list(
        record_id            = rid,
        chief                = input$edit_chief                %||% "",
        grad_email           = input$edit_grad_email           %||% "",
        grad_phone           = input$edit_grad_phone           %||% "",
        grad_spec            = input$edit_grad_spec            %||% "",
        res_alumni_position  = input$edit_res_alumni_position  %||% "",
        res_alumni_academic  = input$edit_res_alumni_academic  %||% "",
        ssm                  = input$edit_ssm                  %||% "",
        mo_prac              = input$edit_mo_prac              %||% "",
        rural                = input$edit_rural                %||% "",
        und_urban            = input$edit_und_urban            %||% ""
      )
      df <- as.data.frame(payload, stringsAsFactors = FALSE)

      result <- tryCatch({
        REDCapR::redcap_write_oneshot(
          ds = df,
          redcap_uri = REDCAP_CONFIG$url,
          token      = REDCAP_CONFIG$rdm_token
        )
      }, error = function(e) list(success = FALSE, outcome_message = e$message))

      if (isTRUE(result$success)) {
        # Apply locally so the display refreshes without a full reload.
        ov <- payload; ov$record_id <- NULL
        overrides(ov)
        save_state(list(success = TRUE,
                        message = "Saved. Set verification to Yes to continue."))
        updateRadioButtons(session, "verify_grad", selected = character(0))
      } else {
        save_state(list(success = FALSE,
                        message = paste("Save failed:",
                                        result$outcome_message %||% "unknown error")))
      }
    })

    # ----- Verified flag drives section completion -----
    verified <- reactive(identical(input$verify_grad, "1"))

    # ----- Return data for submission -----
    reactive({
      list(
        coach_ilp_final = input$coach_ilp_final,
        verified        = verified(),
        is_complete = verified() &&
                      !is.null(input$coach_ilp_final) &&
                      nchar(trimws(input$coach_ilp_final)) > 0
      )
    })
  })
}

# Internal: parse grad_spec choices from the data dictionary.
.grad_spec_choices_from_dd <- function(dd) {
  if (is.null(dd)) return(NULL)
  ddv <- if (is.function(dd)) dd() else dd
  if (is.null(ddv) || !is.data.frame(ddv)) return(NULL)
  col_var <- intersect(c("Variable / Field Name", "field_name"), names(ddv))[1]
  col_ch  <- intersect(
    c("Choices, Calculations, OR Slider Labels",
      "select_choices_or_calculations"),
    names(ddv))[1]
  if (is.na(col_var) || is.na(col_ch)) return(NULL)
  s <- ddv[[col_ch]][ddv[[col_var]] == "grad_spec"]
  if (length(s) == 0 || is.na(s[1]) || !nzchar(s[1])) return(NULL)
  parts <- strsplit(s[1], "\\|")[[1]]
  vals  <- vapply(parts, function(p) {
    kv <- strsplit(trimws(p), ",", fixed = FALSE)[[1]]
    trimws(kv[1])
  }, character(1))
  labs <- vapply(parts, function(p) {
    kv <- strsplit(trimws(p), ",", fixed = FALSE)[[1]]
    if (length(kv) < 2) NA_character_
    else paste(trimws(kv[-1]), collapse = ",")
  }, character(1))
  setNames(vals, labs)
}
