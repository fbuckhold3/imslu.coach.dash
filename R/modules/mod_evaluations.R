# Section 2: Evaluations & Feedback Module
# Displays assessment visualizations, resident reflections, and coach entry fields
# PATTERN: Matches working mod_assessment_wrapper.R from self-assessment app

mod_evaluations_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Ultra-minimal UI for debugging
    h4("Section 2: Evaluations & Feedback"),

    p("This section will display assessment data and coach entry fields."),

    wellPanel(
      h5("Previous Period"),
      p("Previous period data will appear here.")
    ),

    wellPanel(
      h5("Current Period"),
      p("Current period data will appear here.")
    ),

    wellPanel(
      h5("Coach Entry"),
      textAreaInput(
        ns("coach_p_d_comments"),
        "Coach Comments on Plus/Delta:",
        value = "",
        width = "100%",
        height = "100px"
      ),
      textAreaInput(
        ns("coach_evaluations"),
        "Coach Comments on Evaluations:",
        value = "",
        width = "100%",
        height = "100px"
      )
    )
  )
}

mod_evaluations_server <- function(id, resident_data, current_period, app_data, data_dict) {
  moduleServer(id, function(input, output, session) {

    # Ultra-minimal server - just return the input values
    return(
      reactive({
        list(
          coach_p_d_comments = input$coach_p_d_comments,
          coach_evaluations = input$coach_evaluations,
          is_complete = FALSE
        )
      })
    )
  })
}