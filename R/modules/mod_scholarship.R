# Section 4: Scholarship Module
# Displays resident scholarship activities

mod_scholarship_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Scholarship Display
    h4("Scholarship Activities", style = "color: #34495e; margin-top: 10px;"),

    wellPanel(
      style = "background-color: #f8f9fa; border-left: 4px solid #9b59b6;",
      uiOutput(ns("scholarship_table"))
    )
  )
}

mod_scholarship_server <- function(id, resident_data, current_period, app_data) {
  moduleServer(id, function(input, output, session) {

    # Display scholarship table
    output$scholarship_table <- renderUI({
      req(resident_data(), app_data())

      resident_info <- resident_data()$resident_info
      record_id <- resident_info$record_id

      # Get scholarship data for this resident
      if (!"scholarship" %in% names(app_data()$all_forms)) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6; padding: 15px;",
            "No scholarship data available"
          )
        )
      }

      scholarship_data <- app_data()$all_forms$scholarship %>%
        dplyr::filter(record_id == !!record_id)

      if (nrow(scholarship_data) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6; padding: 15px;",
            "No scholarship activities recorded"
          )
        )
      }

      # Group by type and display
      scholarship_types <- list(
        "QI Project" = list(fields = c("schol_qi"), icon = "tasks"),
        "Research" = list(fields = c("schol_res", "schol_res_mentor", "schol_res_status"), icon = "flask"),
        "Presentation" = list(fields = c("schol_pres", "schol_pres_type", "schol_pres_conf"), icon = "presentation"),
        "Publication" = list(fields = c("schol_cit", "schol_pub"), icon = "book"),
        "Committee" = list(fields = c("schol_comm"), icon = "users"),
        "Patient Safety" = list(fields = c("schol_ps", "schol_rca"), icon = "shield-alt")
      )

      # Build output by type
      output_sections <- list()

      for (type_name in names(scholarship_types)) {
        type_info <- scholarship_types[[type_name]]

        # Check if any records have data for this type
        has_data <- FALSE
        for (field in type_info$fields) {
          if (field %in% names(scholarship_data)) {
            field_data <- scholarship_data[[field]]
            if (any(!is.na(field_data) & field_data != "" & field_data != "0")) {
              has_data <- TRUE
              break
            }
          }
        }

        if (has_data) {
          # Get rows with data for this type
          type_rows <- list()

          for (i in 1:nrow(scholarship_data)) {
            row_has_data <- FALSE
            row_content <- list()

            for (field in type_info$fields) {
              if (field %in% names(scholarship_data)) {
                value <- scholarship_data[[field]][i]
                if (!is.na(value) && value != "" && value != "0") {
                  row_has_data <- TRUE

                  # Format field display
                  field_label <- switch(field,
                    "schol_qi" = "Description",
                    "schol_res" = "Description",
                    "schol_res_mentor" = "Mentor",
                    "schol_res_status" = "Status",
                    "schol_pres" = "Presented",
                    "schol_pres_type" = "Type",
                    "schol_pres_conf" = "Conference",
                    "schol_cit" = "Citation",
                    "schol_pub" = "Published",
                    "schol_comm" = "Committee",
                    "schol_ps" = "Patient Safety Review",
                    "schol_rca" = "Root Cause Analysis",
                    field
                  )

                  # Parse yesno fields
                  display_value <- if (field %in% c("schol_pres", "schol_pub", "schol_ps", "schol_rca")) {
                    if (value == "1") "Yes" else if (value == "0") "No" else value
                  } else {
                    value
                  }

                  row_content[[length(row_content) + 1]] <- tags$div(
                    style = "margin-bottom: 5px;",
                    tags$strong(paste0(field_label, ": ")),
                    display_value
                  )
                }
              }
            }

            if (row_has_data) {
              type_rows[[length(type_rows) + 1]] <- tags$div(
                style = "padding: 10px; margin-bottom: 10px; background-color: white; border-radius: 4px; border-left: 3px solid #9b59b6;",
                row_content
              )
            }
          }

          if (length(type_rows) > 0) {
            output_sections[[length(output_sections) + 1]] <- tags$div(
              style = "margin-bottom: 20px;",
              h5(
                icon(type_info$icon),
                " ",
                type_name,
                style = "color: #9b59b6; margin-bottom: 10px;"
              ),
              type_rows
            )
          }
        }
      }

      if (length(output_sections) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6; padding: 15px;",
            "No scholarship activities recorded"
          )
        )
      }

      tagList(output_sections)
    })

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
