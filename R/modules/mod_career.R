# Section 5: Career Planning Module
# Displays resident career planning data and allows coach to enter comments

mod_career_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Previous Period Section
    uiOutput(ns("previous_coach_comments")),

    # Current Period Career Data
    h4("Current Period - Career Planning", style = "color: #34495e; margin-top: 20px;"),

    wellPanel(
      style = "background-color: #fff8e1; border-left: 4px solid #f39c12;",

      fluidRow(
        column(
          width = 6,
          h5("Career Path", style = "color: #f39c12;"),
          uiOutput(ns("career_path"))
        ),
        column(
          width = 6,
          h5("Fellowship Interests", style = "color: #f39c12;"),
          uiOutput(ns("fellowship"))
        )
      ),

      hr(),

      fluidRow(
        column(
          width = 12,
          h5("Track Interest", style = "color: #f39c12;"),
          uiOutput(ns("track"))
        )
      )
    ),

    hr(),

    # Coach Entry for Current Period
    h4("Coach Review - Current Period", style = "color: #34495e; margin-top: 20px;"),

    wellPanel(
      style = "background-color: #ffffff; border-left: 4px solid #27ae60;",

      div(
        style = "margin-bottom: 15px;",
        tags$label(
          "Career Planning Discussion:",
          style = "font-weight: bold; color: #2c3e50;"
        ),
        tags$p(
          "Provide feedback on the resident's career interests, mentorship connections, and development plans.",
          style = "font-size: 12px; color: #7f8c8d; margin-bottom: 5px;"
        ),
        textAreaInput(
          ns("coach_career"),
          label = NULL,
          value = "",
          width = "100%",
          height = "150px",
          placeholder = "Enter your comments on career planning and development..."
        )
      )
    )
  )
}

mod_career_server <- function(id, resident_data, current_period, app_data) {
  moduleServer(id, function(input, output, session) {

    # Helper function to parse checkbox fields
    parse_checkboxes <- function(data, field_prefix, data_dict) {
      # Get all checkbox columns for this field
      checkbox_cols <- grep(paste0("^", field_prefix, "___"), names(data), value = TRUE)

      if (length(checkbox_cols) == 0) {
        return("")
      }

      # Get data dictionary choices
      field_info <- data_dict %>%
        dplyr::filter(field_name == field_prefix)

      if (nrow(field_info) == 0) {
        return("")
      }

      # Parse choices
      choices_str <- field_info$select_choices_or_calculations[1]
      if (is.na(choices_str)) {
        return("")
      }

      choices <- strsplit(choices_str, "\\|")[[1]]
      choices <- trimws(choices)

      parsed <- lapply(choices, function(choice) {
        parts <- strsplit(choice, ",")[[1]]
        if (length(parts) >= 2) {
          code <- trimws(parts[1])
          label <- trimws(paste(parts[-1], collapse = ","))
          return(data.frame(code = code, label = label, stringsAsFactors = FALSE))
        } else {
          return(NULL)
        }
      })

      choices_df <- do.call(rbind, Filter(Negate(is.null), parsed))

      # Find selected options
      selected_labels <- c()
      for (col in checkbox_cols) {
        code <- sub(paste0("^", field_prefix, "___"), "", col)
        val <- data[[col]][1]
        if (!is.na(val) && val == "1") {
          label <- choices_df$label[choices_df$code == code]
          if (length(label) > 0) {
            selected_labels <- c(selected_labels, label[1])
          }
        }
      }

      if (length(selected_labels) == 0) {
        return("")
      }

      return(paste(selected_labels, collapse=", "))
    }

    # Display career path
    output$career_path <- renderUI({
      req(resident_data(), app_data())

      curr_data <- resident_data()$current_period$s_eval

      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(
          p(style = "font-style: italic; color: #95a5a6;",
            "No career planning data available")
        )
      }

      data_dict <- app_data()$data_dict
      career_path <- parse_checkboxes(curr_data, "s_e_career_path", data_dict)

      # Get "other" text if present (check if column exists)
      career_oth <- if ("s_e_career_oth" %in% names(curr_data)) {
        curr_data$s_e_career_oth[1]
      } else {
        NA
      }

      if (career_path == "" && (is.na(career_oth) || is.null(career_oth) || career_oth == "")) {
        return(
          p(style = "font-style: italic; color: #95a5a6;",
            "No career path specified")
        )
      }

      tagList(
        if (career_path != "") {
          p(career_path)
        },
        if (!is.na(career_oth) && !is.null(career_oth) && career_oth != "") {
          p(tags$em("Other: ", career_oth))
        }
      )
    })

    # Display fellowship interests
    output$fellowship <- renderUI({
      req(resident_data(), app_data())

      curr_data <- resident_data()$current_period$s_eval

      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(
          p(style = "font-style: italic; color: #95a5a6;",
            "No fellowship data available")
        )
      }

      data_dict <- app_data()$data_dict
      fellowship <- parse_checkboxes(curr_data, "s_e_fellow", data_dict)

      # Get "other" text if present (check if column exists)
      fellow_oth <- if ("s_e_fellow_oth" %in% names(curr_data)) {
        curr_data$s_e_fellow_oth[1]
      } else {
        NA
      }

      if (fellowship == "" && (is.na(fellow_oth) || is.null(fellow_oth) || fellow_oth == "")) {
        return(
          p(style = "font-style: italic; color: #95a5a6;",
            "No fellowship interests specified")
        )
      }

      tagList(
        if (fellowship != "") {
          tags$ul(
            class = "list-unstyled",
            lapply(strsplit(fellowship, ", ")[[1]], function(f) {
              tags$li(
                style = "padding: 5px 0;",
                icon("stethoscope", class = "text-info"),
                " ", f
              )
            })
          )
        },
        if (!is.na(fellow_oth) && !is.null(fellow_oth) && fellow_oth != "") {
          p(tags$em("Other: ", fellow_oth))
        }
      )
    })

    # Display track interest
    output$track <- renderUI({
      req(resident_data(), app_data())

      curr_data <- resident_data()$current_period$s_eval

      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(
          p(style = "font-style: italic; color: #95a5a6;",
            "No track data available")
        )
      }

      data_dict <- app_data()$data_dict
      track <- parse_checkboxes(curr_data, "s_e_track", data_dict)

      # Get track type if present (check if column exists)
      track_type <- if ("s_e_track_type" %in% names(curr_data)) {
        curr_data$s_e_track_type[1]
      } else {
        NA
      }

      if (track == "" && (is.na(track_type) || is.null(track_type) || track_type == "")) {
        return(
          p(style = "font-style: italic; color: #95a5a6;",
            "No track interest specified")
        )
      }

      tagList(
        if (track != "") {
          p(track)
        },
        if (!is.na(track_type) && !is.null(track_type) && track_type != "") {
          p(tags$strong("Track Type: "), track_type)
        }
      )
    })

    # Display previous period coach comments
    output$previous_coach_comments <- renderUI({
      req(resident_data())

      prev_data <- resident_data()$previous_period$coach_rev

      if (is.null(prev_data) || nrow(prev_data) == 0) {
        return(NULL)
      }

      prev_career <- prev_data$coach_career[1]

      if (is.na(prev_career) || trimws(prev_career) == "") {
        return(NULL)
      }

      tagList(
        h4("Previous Period Coach Comments", style = "color: #34495e; margin-top: 20px;"),

        wellPanel(
          style = "background-color: #e8f4f8; border-left: 4px solid #3498db;",

          h5("Career Planning:", style = "color: #2980b9;"),
          div(
            style = "padding: 10px; background-color: white; border-radius: 4px;",
            HTML(gsub("\n", "<br>", prev_career))
          )
        ),

        hr()
      )
    })

    # Load or clear data when resident changes
    # Track current resident to detect changes
    current_resident_id <- reactiveVal(NULL)

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
          existing_career <- curr_data$coach_career[1]

          # Populate or clear career field
          if (!is.na(existing_career) && existing_career != "") {
            updateTextAreaInput(session, "coach_career", value = existing_career)
            message(sprintf("Loaded existing career data for resident %s", new_resident_id))
          } else {
            updateTextAreaInput(session, "coach_career", value = "")
            message(sprintf("Cleared career form for new resident %s (no existing data)", new_resident_id))
          }
        } else {
          # Clear field if no coach_rev record exists
          updateTextAreaInput(session, "coach_career", value = "")
          message(sprintf("Cleared career form for new resident %s (no coach_rev record)", new_resident_id))
        }
      }
    })

    # Return reactive with entered data
    return(
      reactive({
        list(
          coach_career = input$coach_career,
          is_complete = !is.null(input$coach_career) && nchar(trimws(input$coach_career)) > 0
        )
      })
    )
  })
}
