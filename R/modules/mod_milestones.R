# Section 6: Milestones Module
# Displays milestone visualizations and allows coach to enter milestone ratings
#
# P7 (Entering Residency): coaches don't rate baseline milestones — this
# section shows ONLY the resident's self-assessment, no coach entry UI at
# all. Unlike periods 1-6 (which show descriptions only — the interesting
# thing to review at that point is what changed/was flagged), P7 shows the
# FULL self-rating table (all 21 subcompetencies) alongside any description,
# because a baseline self-assessment is almost always all-ratings/no-text
# (descriptions are conventionally only written for standout ratings) — the
# descriptions-only view would show "nothing" for nearly every new intern.
# `period_num` is the app's local 0-based period number (0 = Entering
# Residency), passed once at UI-build time from mod_review_interface.R's
# sections_ui renderUI.

mod_milestones_ui <- function(id, period_num = NULL) {
  ns <- NS(id)
  is_p7 <- identical(suppressWarnings(as.integer(period_num)), 0L)

  tagList(
    if (is_p7) tagList(
      h4("Resident Self-Assessment", style = "color: #34495e; margin-top: 10px;"),
      p(style = "color: #7f8c8d;", "Baseline self-ratings across all 21 ACGME subcompetencies."),
      wellPanel(
        style = "background-color: #f8f9fa; border-left: 4px solid #3498db;",
        plotly::plotlyOutput(ns("self_assessment_spider"), height = "500px")
      ),
      wellPanel(
        style = "background-color: #fff8e1; border-left: 4px solid #f39c12;",
        uiOutput(ns("self_ratings_table"))
      )
    ) else tagList(
      # Resident Milestone Descriptions from current period
      h4("Resident Milestone Descriptions", style = "color: #34495e; margin-top: 10px;"),
      p(style = "color: #7f8c8d;", "Review resident's self-assessment descriptions for specific milestones."),

      wellPanel(
        style = "background-color: #fff8e1; border-left: 4px solid #f39c12;",
        uiOutput(ns("milestone_descriptions"))
      ),

      hr(),
      # Coach Milestone Entry (local module with visualizations)
      h4("Coach Milestone Ratings", style = "color: #34495e; margin-top: 20px;"),

      wellPanel(
        style = "background-color: #ffffff; border-left: 4px solid #27ae60;",
        mod_milestone_entry_ui(ns("milestone_entry"))
      )
    )
  )
}

mod_milestones_server <- function(id, resident_data, current_period, app_data, data_dict) {
  moduleServer(id, function(input, output, session) {

    is_p7 <- reactive({ identical(as.integer(current_period() %||% NA_integer_), 0L) })

    # Convert period number to period name for milestone entry module
    period_name <- reactive({
      period_num <- current_period()
      if (is.null(period_num) || is.na(period_num)) return(NA)
      get_period_name(period_num)
    })

    # Milestone competency labels mapping
    milestone_labels <- c(
      "m_pc1" = "PC1: Gathers and synthesizes essential information",
      "m_pc2" = "PC2: Prioritizes differential diagnosis",
      "m_pc3" = "PC3: Manages patients with progressive responsibility",
      "m_pc4" = "PC4: Demonstrates skill in performing procedures",
      "m_pc5" = "PC5: Requests consultations effectively",
      "m_pc6" = "PC6: Provides appropriate role modeling",
      "m_mk1" = "MK1: Core knowledge for effective patient care",
      "m_mk2" = "MK2: Knowledge of diagnostic testing and procedures",
      "m_mk3" = "MK3: Scholarly activities",
      "m_sbp1" = "SBP1: Works effectively within healthcare system",
      "m_sbp2" = "SBP2: Coordinates care with other healthcare professionals",
      "m_sbp3" = "SBP3: Incorporates cost-awareness",
      # NOTE: field names use "pbl" (rep_pbl1_self/rep_pbl2_self), not "pbli" —
      # keys here must match the field-derived code, not the ACGME abbreviation.
      "m_pbl1" = "PBLI1: Identifies strengths and gaps in knowledge",
      "m_pbl2" = "PBLI2: Uses information technology for learning",
      "m_prof1" = "PROF1: Demonstrates compassion and respect",
      "m_prof2" = "PROF2: Demonstrates accountability to patients and society",
      "m_prof3" = "PROF3: Manages conflicts of interest",
      "m_prof4" = "PROF4: Demonstrates self-awareness and help-seeking",
      "m_ics1" = "ICS1: Communicates effectively with patients and families",
      "m_ics2" = "ICS2: Maintains comprehensive, accurate records",
      "m_ics3" = "ICS3: Communicates effectively with healthcare team"
    )

    # ----- P7: parse all 21 self-ratings once, shared by table + spider plot -----
    self_rating_rows <- reactive({
      rd <- resident_data(); req(rd)
      curr_data <- rd$current_period$milestone_selfevaluation
      if (is.null(curr_data) || nrow(curr_data) == 0) return(NULL)

      rating_fields <- grep("^rep_.*_self$", names(curr_data), value = TRUE)
      rows <- lapply(rating_fields, function(field) {
        val <- curr_data[[field]][1]
        if (is.null(val) || is.na(val) || !nzchar(as.character(val))) return(NULL)

        base <- sub("^rep_", "", sub("_self$", "", field))
        code <- paste0("m_", base)
        label <- if (code %in% names(milestone_labels)) milestone_labels[[code]] else toupper(base)

        desc_field <- paste0(field, "_desc")
        # A couple of RDM fields have a stray "1" suffix (rep_sbp1_self_desc1) —
        # fall back to that if the plain _desc column isn't present.
        if (!desc_field %in% names(curr_data)) desc_field <- paste0(desc_field, "1")
        desc <- if (desc_field %in% names(curr_data)) curr_data[[desc_field]][1] else NA

        list(code = toupper(base), label = label, rating = as.character(val),
             desc = if (!is.na(desc) && nzchar(trimws(as.character(desc)))) as.character(desc) else NULL)
      })
      rows <- Filter(Negate(is.null), rows)
      if (length(rows) == 0) NULL else rows
    })

    # ----- P7: self-assessment spider plot -----
    # gmed::create_milestone_spider_plot_final() filters internally on the raw
    # numeric period code, but this app's data pipeline translates that field
    # to label text ("Entering Residency") before it reaches this module —
    # calling it directly would just show "No data". Reusing the same
    # self-contained plotly approach already proven to work for the coach-
    # ratings preview spider (mod_review_interface.R's preview_spider_plot).
    output$self_assessment_spider <- plotly::renderPlotly({
      rows <- self_rating_rows()
      if (is.null(rows)) {
        return(plotly::plotly_empty() %>%
                 plotly::add_annotations(
                   text = "No milestone self-assessment available",
                   x = 0.5, y = 0.5, showarrow = FALSE,
                   font = list(size = 16, color = "gray")
                 ))
      }

      categories <- vapply(rows, function(r) r$code, character(1))
      values <- vapply(rows, function(r) as.numeric(r$rating), numeric(1))

      plotly::plot_ly(type = 'scatterpolar', mode = 'lines+markers', fill = 'toself') %>%
        plotly::add_trace(
          r = values,
          theta = categories,
          name = 'Self-Assessment',
          fillcolor = 'rgba(243, 156, 18, 0.3)',
          line = list(color = 'rgb(243, 156, 18)', width = 2),
          marker = list(size = 8, color = 'rgb(243, 156, 18)')
        ) %>%
        plotly::layout(
          polar = list(
            radialaxis = list(visible = TRUE, range = c(0, 9), tickmode = 'linear', tick0 = 0, dtick = 1)
          ),
          title = "Milestone Self-Assessment - Entering Residency",
          showlegend = TRUE
        )
    })

    # ----- P7: full self-rating table (all 21 subcompetencies) -----
    output$self_ratings_table <- renderUI({
      rows <- self_rating_rows()
      if (is.null(rows)) {
        return(p(style = "font-style: italic; color: #95a5a6;", "No milestone self-assessment available"))
      }

      tags$table(
        class = "table table-striped table-bordered",
        style = "background-color: white;",
        tags$thead(
          tags$tr(
            tags$th(style = "width: 45%;", "Milestone"),
            tags$th(style = "width: 15%;", "Self-Rating"),
            tags$th("Description")
          )
        ),
        tags$tbody(
          lapply(rows, function(r) {
            tags$tr(
              tags$td(style = "vertical-align: top; font-weight: bold;", r$label),
              tags$td(style = "vertical-align: top;", r$rating),
              tags$td(if (!is.null(r$desc)) r$desc else tags$em(style = "color: #95a5a6;", "—"))
            )
          })
        )
      )
    })

    # Display milestone descriptions from resident
    output$milestone_descriptions <- renderUI({
      req(resident_data(), app_data())

      curr_data <- resident_data()$current_period$milestone_selfevaluation

      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(
          p(style = "font-style: italic; color: #95a5a6;",
            "No milestone self-assessment available")
        )
      }

      # Get all _self_desc fields that have content (resident self-assessment descriptions)
      desc_fields <- grep("^rep_.*_self_desc$", names(curr_data), value = TRUE)

      descriptions <- list()
      for (field in desc_fields) {
        value <- curr_data[[field]][1]
        if (!is.na(value) && !is.null(value) && trimws(value) != "") {
          # Get milestone code (e.g., "m_pc1" from "rep_pc1_self_desc")
          # Remove "rep_" prefix and "_self_desc" suffix, add "m_" prefix
          milestone_base <- sub("^rep_", "", field)
          milestone_base <- sub("_self_desc$", "", milestone_base)
          milestone_code <- paste0("m_", milestone_base)

          # Get label
          label <- if (milestone_code %in% names(milestone_labels)) {
            milestone_labels[milestone_code]
          } else {
            toupper(milestone_base)
          }

          descriptions[[length(descriptions) + 1]] <- list(
            label = label,
            description = value
          )
        }
      }

      if (length(descriptions) == 0) {
        return(
          p(style = "font-style: italic; color: #95a5a6;",
            "No milestone descriptions provided by resident")
        )
      }

      # Build table
      tagList(
        p(style = "color: #7f8c8d; margin-bottom: 15px;",
          "The resident provided the following descriptions for specific milestones:"),
        tags$table(
          class = "table table-striped table-bordered",
          style = "background-color: white;",
          tags$thead(
            tags$tr(
              tags$th(style = "width: 30%;", "Milestone"),
              tags$th("Description")
            )
          ),
          tags$tbody(
            lapply(descriptions, function(desc) {
              tags$tr(
                tags$td(style = "vertical-align: top; font-weight: bold;", desc$label),
                tags$td(desc$description)
              )
            })
          )
        )
      )
    })

    # Call local milestone entry module (periods 1-6 only — the UI container
    # doesn't exist for P7, so P7 skips this entirely: no coach ratings, no
    # milestone_entry submission; see the is_p7 branch in mod_review_interface.R).
    record_id <- reactive({
      req(resident_data())
      resident_data()$resident_info$record_id
    })

    .ms_field_map <- gmed::get_milestone_field_mapping_rdm2("milestone_entry")

    existing_scores <- reactive({
      rd <- resident_data()
      if (is.null(rd)) return(NULL)
      df <- rd$current_period$milestone_entry
      if (is.null(df) || nrow(df) == 0) return(NULL)
      out <- list()
      for (k in names(.ms_field_map)) {
        fld <- .ms_field_map[[k]]
        if (fld %in% names(df)) {
          v <- df[[fld]][1]
          if (!is.na(v) && nzchar(as.character(v))) {
            iv <- suppressWarnings(as.integer(v))
            if (!is.na(iv)) out[[k]] <- iv
          }
        }
      }
      if (length(out) == 0) NULL else out
    })

    existing_descs <- reactive({
      rd <- resident_data()
      if (is.null(rd)) return(NULL)
      df <- rd$current_period$milestone_entry
      if (is.null(df) || nrow(df) == 0) return(NULL)
      out <- list()
      for (k in names(.ms_field_map)) {
        fld <- paste0(.ms_field_map[[k]], "_desc")
        if (fld %in% names(df)) {
          v <- df[[fld]][1]
          if (!is.na(v) && nzchar(as.character(v))) out[[k]] <- as.character(v)
        }
      }
      if (length(out) == 0) NULL else out
    })

    # Always mount the entry module (cheap, reactive) — for P7 its UI
    # container isn't present in mod_milestones_ui(), so it's a no-op there.
    # This mirrors mod_learning.R's approach for mod_seval_boards_display.
    milestone_entry_data <- mod_milestone_entry_server(
      "milestone_entry",
      rdm_data = app_data,
      record_id = record_id,
      current_period = period_name,  # Pass period name, not number
      data_dict = reactive(isolate(data_dict())),
      initial_scores = existing_scores,
      initial_descs  = existing_descs
    )

    # Return reactive with entered data
    return(
      reactive({
        if (isTRUE(is_p7())) {
          # No coach milestone ratings for P7 — nothing required to submit.
          list(
            milestone_ratings = list(scores = NULL, descriptions = NULL, milestone_results = NULL),
            is_complete = TRUE
          )
        } else {
          milestone_data <- milestone_entry_data()
          list(
            milestone_ratings = list(
              scores = milestone_data$scores,
              descriptions = milestone_data$descriptions,
              milestone_results = milestone_data$milestone_results
            ),
            is_complete = milestone_data$is_complete
          )
        }
      })
    )
  })
}
