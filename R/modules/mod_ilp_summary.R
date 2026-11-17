# =============================================================================
# ILP at a Glance (periods 1-5, Evaluation & ILP form): placed after Milestones.
#
# Display only. Lists the resident's three ILP goals with their why / how
# plan and the coach's per-goal note (live from the Goals & ILP section),
# then "At a Glance": a spider of this review's milestone ratings (live from
# the Milestones section) over the resident's self-ratings, and the coach's
# notes from the other sections.
#
# The Finalized ILP summary entry (coach_ilp_final) lives in the last section,
# with the checklist and preview (mod_summary.R).
# =============================================================================

mod_ilp_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("This Period's ILP", style = "margin-top: 10px;"),
    uiOutput(ns("goals_list")),
    hr(),
    h4("At a Glance"),
    div(
      class = "row g-3 mb-3",
      div(class = "col-lg-6",
          plotly::plotlyOutput(ns("spider"), height = "420px")),
      div(class = "col-lg-6",
          div(style = paste("font-size:0.78rem; font-weight:700; text-transform:uppercase;",
                            "letter-spacing:.04em; color: var(--roundsui-ink-muted); margin-bottom:6px;"),
              "Your notes from this review"),
          div(style = paste("max-height: 400px; overflow-y: auto; padding-right: 6px;"),
              uiOutput(ns("notes_recap"))))
    )
  )
}

#' @param goals_data Reactive from mod_goals_server (per-goal coach notes).
#' @param milestone_scores Reactive: the coach's live ratings from the
#'   Milestones section, a named list keyed PC_1, MK_1, ... (or NULL).
#' @param section_notes Reactive: named character vector, section label ->
#'   the coach's note text so far.
mod_ilp_summary_server <- function(id, resident_data, goals_data,
                                   milestone_scores = NULL, section_notes = NULL) {
  moduleServer(id, function(input, output, session) {

    output$goals_list <- renderUI({
      rd <- resident_data(); req(rd)
      g <- coach_ilp_goals(rd$current_period$ilp)
      gd <- tryCatch(goals_data(), error = function(e) NULL)
      if (!any(vapply(g, function(x) nzchar(x$goal), logical(1))))
        return(div(class = "text-muted fst-italic", "No ILP goals on file for this period yet."))
      tagList(lapply(g, function(x) {
        note <- trimws(gd[[.COACH_GOAL_FIELDS[[x$domain]]]] %||% "")
        div(class = "mb-3 pb-2", style = "border-bottom: 1px solid var(--roundsui-border);",
          div(style = "font-size:0.78rem; font-weight:700; text-transform:uppercase; letter-spacing:.04em; color: var(--roundsui-ink-muted);",
              x$domain_label),
          if (nzchar(x$goal))
            div(style = "font-weight:700; margin:2px 0;",
                x$goal, if (nzchar(x$level)) paste0(" · target level ", x$level))
          else div(class = "text-muted fst-italic", "No goal set."),
          if (nzchar(x$how))
            div(style = "font-size:0.88rem; white-space:pre-wrap;",
                tags$span(class = "text-muted", "Resident's plan: "), x$how),
          div(style = "font-size:0.88rem; white-space:pre-wrap; margin-top:2px;",
              tags$span(class = "text-muted", "Your note: "),
              if (nzchar(note)) note else tags$em(class = "text-muted", "not written yet (Goals & ILP section)")))
      }))
    })

    # ----- Spider: this review's ratings over the resident's self-ratings -----
    output$spider <- plotly::renderPlotly({
      rd <- resident_data(); req(rd)
      map <- unlist(gmed::get_milestone_field_mapping_rdm2("milestone_entry"))
      sc <- if (is.function(milestone_scores)) tryCatch(milestone_scores(), error = function(e) NULL)
      self <- rd$current_period$milestone_selfevaluation
      coach_v <- vapply(names(map), function(k) {
        v <- sc[[k]]; if (is.null(v)) NA_real_ else suppressWarnings(as.numeric(v))
      }, numeric(1))
      self_v <- vapply(unname(map), function(f) {
        fl <- paste0(f, "_self")
        if (is.null(self) || !is.data.frame(self) || nrow(self) == 0 || !fl %in% names(self)) return(NA_real_)
        suppressWarnings(as.numeric(self[[fl]][1]))
      }, numeric(1))
      ilp_milestone_spider(sub("_", "", names(map)), coach_v, self_v)
    })

    # ----- Recap of the coach's notes elsewhere in this review -----
    output$notes_recap <- renderUI({
      notes <- if (is.function(section_notes)) tryCatch(section_notes(), error = function(e) NULL)
      if (is.null(notes) || length(notes) == 0) return(NULL)
      tagList(lapply(names(notes), function(lbl) {
        txt <- trimws(notes[[lbl]] %||% "")
        div(class = "mb-2 pb-2", style = "border-bottom: 1px solid var(--roundsui-border);",
            div(style = "font-weight:700; font-size:0.85rem; color: var(--roundsui-ink);", lbl),
            if (nzchar(txt))
              div(style = "font-size:0.85rem; white-space:pre-wrap; color: var(--roundsui-ink);", txt)
            else div(style = "font-size:0.85rem; font-style:italic; color: var(--roundsui-ink-faint);",
                     "not written yet"))
      }))
    })

    # Viewing this section is all it asks.
    reactive(list(is_complete = TRUE))
  })
}

# Milestone spider for the ILP Summary: the coach's ratings (filled) over the
# resident's self-ratings (dashed). All 21 axes always show, so the shape
# reads the same when only some milestones are rated (gaps are left open).
ilp_milestone_spider <- function(codes, coach, self) {
  if (all(is.na(coach)) && all(is.na(self))) {
    return(plotly::plotly_empty() %>%
             plotly::add_annotations(text = "No milestone ratings yet", x = 0.5, y = 0.5,
                                     showarrow = FALSE, font = list(size = 15, color = "gray")))
  }
  cl <- roundsui::roundsui_colors()
  # Close the loop so the polygon joins up.
  close <- function(x) c(x, x[1])
  p <- plotly::plot_ly(type = "scatterpolar")
  if (any(!is.na(self)))
    p <- p %>% plotly::add_trace(
      r = close(self), theta = close(codes), mode = "lines+markers", name = "Resident self-rating",
      line = list(color = cl$ink_muted, width = 2, dash = "dash"),
      marker = list(size = 6, color = cl$ink_muted))
  if (any(!is.na(coach)))
    p <- p %>% plotly::add_trace(
      r = close(coach), theta = close(codes), mode = "lines+markers", fill = "toself",
      name = "Your ratings (this review)",
      fillcolor = plotly::toRGB(cl$accent, 0.2),
      line = list(color = cl$accent, width = 2),
      marker = list(size = 7, color = cl$accent))
  p %>%
    plotly::layout(
      polar = list(radialaxis = list(visible = TRUE, range = c(0, 9), dtick = 1)),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.08),
      margin = list(t = 30, b = 30, l = 40, r = 40)
    ) %>%
    roundsui::roundsui_plotly_layout()
}
