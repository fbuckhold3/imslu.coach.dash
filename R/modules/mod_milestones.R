# Section 6: Milestones Module
# Displays milestone visualizations and allows coach to enter milestone ratings

mod_milestones_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Milestone Spider Plots
    h4("Milestone Comparisons", style = "color: #34495e; margin-top: 10px;"),

    wellPanel(
      style = "background-color: #f8f9fa; border-left: 4px solid #e74c3c;",

      fluidRow(
        column(
          width = 6,
          h5("Resident Self-Assessment (Current Period)", style = "color: #e74c3c;"),
          uiOutput(ns("self_eval_plot"))
        ),
        column(
          width = 6,
          h5("ACGME Ratings (Previous Period)", style = "color: #e74c3c;"),
          uiOutput(ns("acgme_plot"))
        )
      )
    ),

    hr(),

    # Resident Milestone Descriptions
    h4("Resident Milestone Descriptions", style = "color: #34495e; margin-top: 20px;"),

    wellPanel(
      style = "background-color: #fff8e1; border-left: 4px solid #f39c12;",
      uiOutput(ns("milestone_descriptions"))
    ),

    hr(),

    # Coach Milestone Entry (from gmed)
    h4("Coach Milestone Ratings - Current Period", style = "color: #34495e; margin-top: 20px;"),

    wellPanel(
      style = "background-color: #ffffff; border-left: 4px solid #27ae60;",
      gmed::mod_milestone_entry_ui(ns("milestone_entry"))
    )
  )
}

mod_milestones_server <- function(id, resident_data, current_period, app_data, data_dict) {
  moduleServer(id, function(input, output, session) {

    # Milestone competency labels mapping
    milestone_labels <- c(
      "m_pc1" = "PC1: Gathers and synthesizes essential information",
      "m_pc2" = "PC2: Prioritizes differential diagnosis",
      "m_pc3" = "PC3: Manages patients with progressive responsibility",
      "m_pc4" = "PC4: Demonstrates skill in performing procedures",
      "m_pc5" = "PC5: Requests consultations effectively",
      "m_mk1" = "MK1: Core knowledge for effective patient care",
      "m_sbp1" = "SBP1: Works effectively within healthcare system",
      "m_sbp2" = "SBP2: Coordinates care with other healthcare professionals",
      "m_sbp3" = "SBP3: Incorporates cost-awareness",
      "m_pbli1" = "PBLI1: Identifies strengths and gaps in knowledge",
      "m_pbli2" = "PBLI2: Uses information technology for learning",
      "m_pbli3" = "PBLI3: Analyzes clinical experience systematically",
      "m_prof1" = "PROF1: Demonstrates compassion and respect",
      "m_prof2" = "PROF2: Demonstrates accountability to patients and society",
      "m_prof3" = "PROF3: Manages conflicts of interest",
      "m_ics1" = "ICS1: Communicates effectively with patients and families",
      "m_ics2" = "ICS2: Maintains comprehensive, accurate records",
      "m_ics3" = "ICS3: Communicates effectively with healthcare team"
    )

    # Display resident self-evaluation spider plot
    output$self_eval_plot <- renderUI({
      req(resident_data(), app_data())

      curr_data <- resident_data()$current_period$milestone_selfevaluation

      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6; padding: 20px; text-align: center;",
            icon("info-circle", style = "font-size: 48px; margin-bottom: 10px;"),
            br(),
            "No milestone self-assessment available"
          )
        )
      }

      # Use gmed spider plot function
      tryCatch({
        gmed::mod_milestone_spider_ui(session$ns("self_spider"))
      }, error = function(e) {
        div(
          style = "padding: 20px; text-align: center;",
          p("Spider plot visualization coming soon")
        )
      })
    })

    # Display ACGME spider plot
    output$acgme_plot <- renderUI({
      req(resident_data(), app_data())

      prev_data <- resident_data()$previous_period$acgme_miles

      if (is.null(prev_data) || nrow(prev_data) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6; padding: 20px; text-align: center;",
            icon("info-circle", style = "font-size: 48px; margin-bottom: 10px;"),
            br(),
            "No previous ACGME ratings available"
          )
        )
      }

      # Use gmed spider plot function
      tryCatch({
        gmed::mod_milestone_spider_ui(session$ns("acgme_spider"))
      }, error = function(e) {
        div(
          style = "padding: 20px; text-align: center;",
          p("Spider plot visualization coming soon")
        )
      })
    })

    # Call spider plot servers if functions exist
    observe({
      req(resident_data(), app_data())

      # Self-evaluation spider
      curr_data <- resident_data()$current_period$milestone_selfevaluation
      if (!is.null(curr_data) && nrow(curr_data) > 0) {
        tryCatch({
          if (exists("mod_milestone_spider_server", where = "package:gmed")) {
            gmed::mod_milestone_spider_server(
              "self_spider",
              milestone_data = reactive(curr_data),
              record_id = reactive(resident_data()$resident_info$record_id)
            )
          }
        }, error = function(e) {
          # Spider plot server not available
        })
      }

      # ACGME spider
      prev_data <- resident_data()$previous_period$acgme_miles
      if (!is.null(prev_data) && nrow(prev_data) > 0) {
        tryCatch({
          if (exists("mod_milestone_spider_server", where = "package:gmed")) {
            gmed::mod_milestone_spider_server(
              "acgme_spider",
              milestone_data = reactive(prev_data),
              record_id = reactive(resident_data()$resident_info$record_id)
            )
          }
        }, error = function(e) {
          # Spider plot server not available
        })
      }
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

      # Get all _desc fields that have content
      desc_fields <- grep("^m_.*_desc$", names(curr_data), value = TRUE)

      descriptions <- list()
      for (field in desc_fields) {
        value <- curr_data[[field]][1]
        if (!is.na(value) && !is.null(value) && trimws(value) != "") {
          # Get milestone code (e.g., "m_pc1" from "m_pc1_desc")
          milestone_code <- sub("_desc$", "", field)

          # Get label
          label <- if (milestone_code %in% names(milestone_labels)) {
            milestone_labels[milestone_code]
          } else {
            toupper(milestone_code)
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

    # Call gmed milestone entry module
    record_id <- reactive({
      req(resident_data())
      resident_data()$resident_info$record_id
    })

    milestone_entry_data <- gmed::mod_milestone_entry_server(
      "milestone_entry",
      rdm_data = app_data,
      record_id = record_id,
      current_period = current_period,
      data_dict = reactive(isolate(data_dict()))
    )

    # Return reactive with entered data
    return(
      reactive({
        milestone_data <- milestone_entry_data()

        list(
          milestone_ratings = milestone_data$ratings,
          is_complete = milestone_data$is_complete
        )
      })
    )
  })
}
