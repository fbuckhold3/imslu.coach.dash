# Section: Entering Residency / Skills Review & Concerns (P7 only)
#
# Flow, top to bottom (per Fred's spec):
#   1. Background Questions (hs_mo/college_mo/med_mo/slusom, resident_data)
#      + coach's "Background Information" note (coach_intro_back)
#   2. Concerns Coming Into Residency (s_e_ume_concern, resident-entered)
#      + coach's "Coping and Adjustment" note (coach_coping)
#   3. Skills/Topics/Learning Styles review, ONE combined group:
#      preparedness ratings (s_e_prep_1-17) + topics (s_e_topic_sel) +
#      learning styles (s_e_learn_style) — no ITE/Step 3/board content.
#   4. ONE consolidated coach comment on all three: coach_ls_and_topic
#      (reused field — was previously written from mod_learning.R, which is
#      no longer part of the P7 flow at all; its display content for topics/
#      learning styles is folded in here instead).
#
# Preparedness labels/scale are hardcoded to match imslu.ind.dash's
# .PREP_LABELS / .PREP_SCALE (R/modules/mod_self_eval.R) EXACTLY — the RDM
# data dictionary's field_label text for s_e_prep_* is off-by-one from what
# residents actually see and answer against, so it can't be used as the
# source of truth here. Keep these two lists in sync with ind.dash if it
# ever changes.
#
# NOT shown here (kept elsewhere, no duplication):
#   - Goals (s_e_ume_goal1-3) + coach_mile_goal — mod_goals.R's P7 branch
#     (no Finalized ILP Summary for P7).
#   - Career Planning — mod_career.R, its own section, unchanged.
#   - Milestones — mod_milestones.R's P7 branch (self-assessment only).
#   - Wellness — dropped entirely for P7.
#   - Overall Summary (coach_summary) — moved to mod_summary.R's P7 branch,
#     as the last step in the whole P7 flow.
#
# Coach entry writes coach_intro_back + coach_coping + coach_ls_and_topic on
# coach_rev — see the is_p7 branch in mod_review_interface.R's confirm_submit
# handler for how coach_ls_and_topic is sourced from here (not mod_learning)
# for P7.
#
# `parse_choices_safe()` (checkbox choice-string parser) is defined in
# mod_learning.R and reused here as-is.

# Must match imslu.ind.dash/R/modules/mod_self_eval.R .PREP_LABELS exactly.
.INTRO_PREP_LABELS <- c(
  "1"  = "Answer questions from interdisciplinary services",
  "2"  = "Comfort with junior medical students",
  "3"  = "Obtaining consent for procedures",
  "4"  = "Personal organization / day-to-day tasks",
  "5"  = "Writing orders and prescriptions",
  "6"  = "Looking up evidence-based recommendations",
  "7"  = "Presenting patients (organized, hypothesis-driven)",
  "8"  = "Documenting encounters efficiently",
  "9"  = "Providing and receiving handoffs",
  "10" = "Recognizing urgent/emergent care needs",
  "11" = "Delivering bad news / challenging communication",
  "12" = "Recognizing when to ask for help",
  "13" = "Managing ICU patients (vents, pressors)",
  "14" = "Managing inpatient patients",
  "15" = "Managing primary care clinic patients",
  "16" = "Calling consults",
  "17" = "Completing documentation on time"
)

# Must match imslu.ind.dash/R/modules/mod_self_eval.R .PREP_SCALE exactly.
.INTRO_PREP_SCALE <- c(
  "1" = "Not at all prepared", "2" = "Slightly", "3" = "Moderately",
  "4" = "Very", "5" = "Extremely prepared"
)

mod_intern_intro_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # ----- 1. Background -----
    h4("Background", style = "color: #34495e; margin-top: 10px;"),
    wellPanel(
      style = "background-color: #f8f9fa; border-left: 4px solid #3498db;",
      uiOutput(ns("background_display"))
    ),
    wellPanel(
      style = "background-color: #ffffff; border-left: 4px solid #27ae60;",
      tags$label("Background Information:", style = "font-weight: bold; color: #2c3e50;"),
      tags$p("Where is this resident from, what are they excited about?",
             style = "font-size: 12px; color: #7f8c8d; margin-top: 5px;"),
      textAreaInput(
        ns("coach_intro_back"), label = NULL, value = "", width = "100%", height = "150px",
        placeholder = "Document background information about the resident..."
      )
    ),

    hr(),

    # ----- 2. Concerns + Coping and Adjustment -----
    h4("Concerns & Adjustment", style = "color: #34495e; margin-top: 10px;"),
    wellPanel(
      style = "background-color: #f8f9fa; border-left: 4px solid #3498db;",
      uiOutput(ns("concern_display"))
    ),
    wellPanel(
      style = "background-color: #ffffff; border-left: 4px solid #27ae60;",
      tags$label("Coping and Adjustment:", style = "font-weight: bold; color: #2c3e50;"),
      tags$p("How is the resident adjusting to residency?",
             style = "font-size: 12px; color: #7f8c8d; margin-top: 5px;"),
      textAreaInput(
        ns("coach_coping"), label = NULL, value = "", width = "100%", height = "150px",
        placeholder = "Document how the resident is coping and adjusting..."
      )
    ),

    hr(),

    # ----- 3. Skills / Topics / Learning Styles (combined review) -----
    h4("Skills, Topics & Learning Styles", style = "color: #34495e; margin-top: 10px;"),
    wellPanel(
      style = "background-color: #f8f9fa; border-left: 4px solid #e67e22;",
      uiOutput(ns("prep_display")),
      fluidRow(
        column(
          width = 6,
          h5("Topics Least Comfortable With", style = "color: #e67e22;"),
          uiOutput(ns("learning_topics"))
        ),
        column(
          width = 6,
          h5("Preferred Learning Styles", style = "color: #e67e22;"),
          uiOutput(ns("learning_styles"))
        )
      )
    ),

    # ----- 4. Comments on learning styles, topics, and skills -----
    wellPanel(
      style = "background-color: #ffffff; border-left: 4px solid #27ae60;",
      tags$label("Comments on Learning Styles, Topics, and Skills:",
                 style = "font-weight: bold; color: #2c3e50;"),
      tags$p("Provide feedback on the resident's preparedness, identified learning needs, and preferred learning approaches.",
             style = "font-size: 12px; color: #7f8c8d; margin-top: 5px;"),
      textAreaInput(
        ns("coach_ls_and_topic"), label = NULL, value = "", width = "100%", height = "150px",
        placeholder = "Enter your comments about skills, topics, and learning styles..."
      ),
      div(
        style = "text-align: right; font-size: 12px; color: #95a5a6;",
        textOutput(ns("char_count"))
      )
    )
  )
}

mod_intern_intro_server <- function(id, resident_data, current_period,
                                    app_data, app_data_rv = NULL) {
  moduleServer(id, function(input, output, session) {

    current_s_eval <- reactive({
      rd <- resident_data(); if (is.null(rd)) NULL else rd$current_period$s_eval
    })

    # ----- Background questions display (resident_data, answered once) -----
    output$background_display <- renderUI({
      rd <- resident_data(); if (is.null(rd)) return(NULL)
      r <- rd$resident_info; if (is.null(r) || nrow(r) == 0) return(NULL)

      yn <- function(fld) {
        if (!fld %in% names(r)) return("—")
        v <- r[[fld]][1]
        if (is.null(v) || is.na(v) || !nzchar(as.character(v))) return("—")
        switch(as.character(v), "1" = "Yes", "0" = "No", as.character(v))
      }

      tags$p(
        style = "font-size: 0.85rem; color: #37474f; margin-bottom: 4px;",
        "High school in MO: ", tags$strong(yn("hs_mo")), " • ",
        "College in MO: ", tags$strong(yn("college_mo")), " • ",
        "Med school in MO: ", tags$strong(yn("med_mo")), " • ",
        "SLUSOM grad: ", tags$strong(yn("slusom"))
      )
    })

    # ----- Concerns display -----
    output$concern_display <- renderUI({
      row <- current_s_eval()
      if (is.null(row) || nrow(row) == 0) {
        return(p(class = "text-muted", "No self-evaluation on file yet."))
      }

      concern <- row$s_e_ume_concern[1]
      if (is.null(concern) || is.na(concern) || !nzchar(trimws(as.character(concern)))) {
        return(p(class = "text-muted", "No concerns noted by resident."))
      }

      tagList(
        h5("Concerns Coming Into Residency", style = "color: #2c3e50;"),
        p(concern)
      )
    })

    # ----- Per-field preparedness labels + rating scale (hardcoded, see header) -----

    # ----- Preparedness display, sorted least-prepared first -----
    output$prep_display <- renderUI({
      row <- current_s_eval()
      if (is.null(row) || nrow(row) == 0) return(NULL)

      fields <- grep("^s_e_prep_[0-9]+$", names(row), value = TRUE)

      items <- lapply(fields, function(f) {
        raw_val <- row[[f]][1]
        if (is.null(raw_val) || is.na(raw_val) || !nzchar(as.character(raw_val))) return(NULL)
        raw_val <- as.character(raw_val)
        n <- sub("^s_e_prep_", "", f)
        list(
          field = f,
          label = .INTRO_PREP_LABELS[[n]] %||% f,
          raw = raw_val,
          display = .INTRO_PREP_SCALE[[raw_val]] %||% raw_val
        )
      })
      items <- Filter(Negate(is.null), items)
      if (length(items) == 0) {
        return(p(class = "text-muted", "No preparedness ratings provided."))
      }

      items <- items[order(vapply(items, function(x) as.numeric(x$raw), numeric(1)))]

      tagList(
        h5("Preparedness Self-Assessment", style = "color: #e67e22;"),
        tags$p("Sorted least-prepared first.", style = "font-size: 12px; color: #7f8c8d;"),
        tags$ul(
          style = "list-style: none; padding-left: 0;",
          lapply(items, function(it) {
            tags$li(
              style = "padding: 6px 0; border-bottom: 1px solid #eee;",
              tags$strong(it$label), " — ", it$display
            )
          })
        )
      )
    })

    # ----- Topics display (same source/logic as mod_learning.R) -----
    output$learning_topics <- renderUI({
      req(resident_data(), app_data())
      curr_data <- resident_data()$current_period$s_eval

      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(div(style = "font-style: italic; color: #95a5a6;", "No learning assessment available"))
      }

      data_dict <- app_data()$data_dict
      topic_field_info <- data_dict %>% dplyr::filter(field_name == "s_e_topic_sel")
      if (nrow(topic_field_info) == 0) {
        return(div(style = "font-style: italic; color: #95a5a6;", "Topic field configuration not found"))
      }

      choices_str <- topic_field_info$select_choices_or_calculations[1]
      topic_choices <- parse_choices_safe(choices_str)
      if (nrow(topic_choices) == 0) {
        return(div(style = "font-style: italic; color: #95a5a6;", "No topics configured"))
      }

      topic_cols <- grep("^s_e_topic_sel___", names(curr_data), value = TRUE)
      selected_topics <- c()
      for (col in topic_cols) {
        code <- sub("^s_e_topic_sel___", "", col)
        val <- curr_data[[col]][1]
        if (!is.na(val) && val == "1") {
          label <- topic_choices$label[topic_choices$code == code]
          if (length(label) > 0) selected_topics <- c(selected_topics, label[1])
        }
      }

      if (length(selected_topics) == 0) {
        return(div(style = "font-style: italic; color: #95a5a6;", "No topics identified"))
      }

      tags$ul(
        class = "list-unstyled",
        lapply(selected_topics, function(topic) {
          tags$li(style = "padding: 5px 0;", icon("exclamation-circle", class = "text-warning"), " ", topic)
        })
      )
    })

    # ----- Learning styles display (same source/logic as mod_learning.R) -----
    output$learning_styles <- renderUI({
      req(resident_data(), app_data())
      curr_data <- resident_data()$current_period$s_eval

      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(div(style = "font-style: italic; color: #95a5a6;", "No learning preferences available"))
      }

      data_dict <- app_data()$data_dict
      style_field_info <- data_dict %>% dplyr::filter(field_name == "s_e_learn_style")
      if (nrow(style_field_info) == 0) {
        return(div(style = "font-style: italic; color: #95a5a6;", "Learning style field configuration not found"))
      }

      choices_str <- style_field_info$select_choices_or_calculations[1]
      style_choices <- parse_choices_safe(choices_str)
      if (nrow(style_choices) == 0) {
        return(div(style = "font-style: italic; color: #95a5a6;", "No learning styles configured"))
      }

      style_cols <- grep("^s_e_learn_style___", names(curr_data), value = TRUE)
      selected_styles <- c()
      for (col in style_cols) {
        code <- sub("^s_e_learn_style___", "", col)
        val <- curr_data[[col]][1]
        if (!is.na(val) && val == "1") {
          label <- style_choices$label[style_choices$code == code]
          if (length(label) > 0) selected_styles <- c(selected_styles, label[1])
        }
      }

      if (length(selected_styles) == 0) {
        return(div(style = "font-style: italic; color: #95a5a6;", "No learning styles identified"))
      }

      tags$ul(
        class = "list-unstyled",
        lapply(selected_styles, function(style) {
          tags$li(style = "padding: 5px 0;", icon("check-circle", class = "text-success"), " ", style)
        })
      )
    })

    # ----- Character count -----
    output$char_count <- renderText({
      total <- nchar(input$coach_intro_back %||% "") +
               nchar(input$coach_coping %||% "") +
               nchar(input$coach_ls_and_topic %||% "")
      sprintf("%d characters", total)
    })

    # ----- Load or clear coach entries on resident change -----
    current_resident_id <- reactiveVal(NULL)

    observe({
      req(resident_data())
      new_id <- resident_data()$resident_info$record_id[1]

      if (is.null(current_resident_id()) || current_resident_id() != new_id) {
        current_resident_id(new_id)

        curr <- resident_data()$current_period$coach_rev
        get_val <- function(fld) {
          if (is.null(curr) || nrow(curr) == 0 || !fld %in% names(curr)) return("")
          v <- curr[[fld]][1]
          if (is.null(v) || is.na(v)) "" else as.character(v)
        }

        updateTextAreaInput(session, "coach_intro_back", value = get_val("coach_intro_back"))
        updateTextAreaInput(session, "coach_coping", value = get_val("coach_coping"))
        updateTextAreaInput(session, "coach_ls_and_topic", value = get_val("coach_ls_and_topic"))
      }
    })

    # ----- Return data for submission -----
    reactive({
      list(
        coach_intro_back   = input$coach_intro_back,
        coach_coping       = input$coach_coping,
        coach_ls_and_topic = input$coach_ls_and_topic,
        is_complete = !is.null(input$coach_intro_back) &&
                      nchar(trimws(input$coach_intro_back %||% "")) > 0 &&
                      !is.null(input$coach_coping) &&
                      nchar(trimws(input$coach_coping %||% "")) > 0
      )
    })
  })
}
