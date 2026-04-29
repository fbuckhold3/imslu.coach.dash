# Section 1: Wellness & Progress Module
# Displays previous period wellness data and allows coach to enter new comments

mod_wellness_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Resident self-reflection (read-only) — full reflection block from s_eval:
    # plus / delta / wellness / mentor-discussion topics / program-assistance.
    # Previous period text is shown as a reference panel above each field.
    h4("Resident Self-Reflection", style = "color: #34495e; margin-top: 10px;"),

    wellPanel(
      style = "background-color: #f8f9fa; border-left: 4px solid #3498db;",
      gmed::mod_seval_reflection_display_ui(
        ns("reflection"),
        title  = NULL,
        fields = c("well", "discussion", "prog_assist")
      )
    ),

    hr(),
    
    # Current Period Coach Entry
    h4("Coach Review - Current Period", style = "color: #34495e; margin-top: 20px;"),
    
    wellPanel(
      style = "background-color: #ffffff; border-left: 4px solid #27ae60;",
      
      div(
        style = "margin-bottom: 15px;",
        tags$label(
          "Coach Comments on Wellness:",
          style = "font-weight: bold; color: #2c3e50;"
        ),
        tags$p(
          "Provide comments on the resident's wellness, work-life balance, and any support needed.",
          style = "font-size: 12px; color: #7f8c8d; margin-top: 5px;"
        )
      ),
      
      textAreaInput(
        ns("coach_wellness"),
        label = NULL,
        value = "",
        width = "100%",
        height = "200px",
        placeholder = "Enter your wellness assessment and recommendations here..."
      ),
      
      # Character count
      div(
        style = "text-align: right; font-size: 12px; color: #95a5a6;",
        textOutput(ns("char_count"))
      )
    )
  )
}

mod_wellness_server <- function(id, resident_data, current_period, app_data) {
  moduleServer(id, function(input, output, session) {

    # Track current resident to detect changes
    current_resident_id <- reactiveVal(NULL)

    # Resident self-reflection (read-only) — gmed display module renders all
    # five s_eval reflection fields (plus, delta, well, discussion, prog_assist)
    # with previous-period reference text where present.
    gmed::mod_seval_reflection_display_server(
      "reflection",
      current_row  = reactive({
        rd <- resident_data()
        if (is.null(rd)) NULL else rd$current_period$s_eval
      }),
      previous_row = reactive({
        rd <- resident_data()
        if (is.null(rd)) NULL else rd$previous_period$s_eval
      }),
      fields = c("well", "discussion", "prog_assist")
    )

    # Character count for coach entry
    output$char_count <- renderText({
      char_count <- nchar(input$coach_wellness)
      sprintf("%d characters", char_count)
    })

    # Load or clear data when resident changes
    observe({
      req(resident_data())

      # Get the current resident's record_id
      res_info <- resident_data()$resident_info
      new_resident_id <- res_info$record_id[1]

      # Check if resident has changed
      if (is.null(current_resident_id()) || current_resident_id() != new_resident_id) {
        # Update tracked resident ID
        current_resident_id(new_resident_id)

        # Try to load existing coach review data
        curr_data <- resident_data()$current_period$coach_rev

        if (!is.null(curr_data) && nrow(curr_data) > 0) {
          existing_wellness <- curr_data$coach_wellness[1]

          # Populate with existing data if available
          if (!is.na(existing_wellness) && existing_wellness != "") {
            updateTextAreaInput(
              session,
              "coach_wellness",
              value = existing_wellness
            )
            message(sprintf("Loaded existing wellness data for resident %s", new_resident_id))
          } else {
            # Clear form if no existing data
            updateTextAreaInput(
              session,
              "coach_wellness",
              value = ""
            )
            message(sprintf("Cleared wellness form for new resident %s (no existing data)", new_resident_id))
          }
        } else {
          # Clear form if no coach_rev record exists
          updateTextAreaInput(
            session,
            "coach_wellness",
            value = ""
          )
          message(sprintf("Cleared wellness form for new resident %s (no coach_rev record)", new_resident_id))
        }
      }
    })

    # Return reactive with entered data
    return(
      reactive({
        list(
          coach_wellness = input$coach_wellness,
          is_complete = !is.null(input$coach_wellness) && nchar(trimws(input$coach_wellness)) > 0
        )
      })
    )
  })
}