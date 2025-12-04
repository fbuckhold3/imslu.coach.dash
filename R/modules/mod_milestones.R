# Section 6: Milestones Module
# Displays milestone visualizations and allows coach to enter milestone ratings

mod_milestones_ui <- function(id) {
  ns <- NS(id)

  tagList(
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
}

mod_milestones_server <- function(id, resident_data, current_period, app_data, data_dict) {
  moduleServer(id, function(input, output, session) {

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
      "m_pbli1" = "PBLI1: Identifies strengths and gaps in knowledge",
      "m_pbli2" = "PBLI2: Uses information technology for learning",
      "m_prof1" = "PROF1: Demonstrates compassion and respect",
      "m_prof2" = "PROF2: Demonstrates accountability to patients and society",
      "m_prof3" = "PROF3: Manages conflicts of interest",
      "m_prof4" = "PROF4: Demonstrates self-awareness and help-seeking",
      "m_ics1" = "ICS1: Communicates effectively with patients and families",
      "m_ics2" = "ICS2: Maintains comprehensive, accurate records",
      "m_ics3" = "ICS3: Communicates effectively with healthcare team"
    )

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

    # Call local milestone entry module
    record_id <- reactive({
      req(resident_data())
      resident_data()$resident_info$record_id
    })

    milestone_entry_data <- mod_milestone_entry_server(
      "milestone_entry",
      rdm_data = app_data,
      record_id = record_id,
      current_period = period_name,  # Pass period name, not number
      data_dict = reactive(isolate(data_dict()))
    )

    # Return reactive with entered data
    return(
      reactive({
        milestone_data <- milestone_entry_data()

        list(
          milestone_ratings = list(
            scores = milestone_data$scores,
            descriptions = milestone_data$descriptions,
            milestone_results = milestone_data$milestone_results
          ),
          is_complete = milestone_data$is_complete
        )
      })
    )
  })
}
