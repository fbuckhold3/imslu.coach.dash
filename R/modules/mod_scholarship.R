# Section 4: Scholarship Module
# Displays resident scholarship activities using gmed package visualization

mod_scholarship_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Scholarship Display from gmed
    h4("Scholarship Activities", style = "color: #34495e; margin-top: 10px;"),

    wellPanel(
      style = "background-color: #f8f9fa; border-left: 4px solid #9b59b6;",

      # Use gmed scholarship display module
      gmed::mod_scholarship_display_ui(ns("scholarship_table"))
    )
  )
}

mod_scholarship_server <- function(id, resident_data, current_period, app_data) {
  moduleServer(id, function(input, output, session) {

    # Extract record_id as separate reactive
    record_id <- reactive({
      req(resident_data())
      resident_data()$resident_info$record_id
    })

    # Get scholarship data for gmed module
    scholarship_data <- reactive({
      req(app_data())

      if ("scholarship" %in% names(app_data()$all_forms)) {
        return(app_data()$all_forms$scholarship)
      } else {
        return(data.frame())
      }
    })

    # Call gmed scholarship display module
    gmed::mod_scholarship_display_server(
      "scholarship_table",
      rdm_data = scholarship_data,
      record_id = record_id
    )

    # Return reactive with completion status
    # No coach entry required for this section
    return(
      reactive({
        list(
          is_complete = TRUE  # Always complete since no coach entry required
        )
      })
    )
  })
}
