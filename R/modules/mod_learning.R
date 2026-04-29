# Section 3: Learning & Board Preparation Module
# Displays exam scores, learning topics/styles, and allows coach to enter comments

# Helper function to parse checkbox field choices from data dictionary
parse_choices_safe <- function(choices_str) {
  if (is.na(choices_str) || choices_str == "") {
    return(data.frame(code = character(), label = character(), stringsAsFactors = FALSE))
  }

  # Split by pipe
  choices <- strsplit(choices_str, "\\|")[[1]]
  choices <- trimws(choices)

  # Parse each choice (format: "code, label")
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

  # Combine results
  parsed <- do.call(rbind, Filter(Negate(is.null), parsed))
  return(parsed)
}

mod_learning_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Step 3 status + ITE-derived ABIM board prediction + MKSAP tracker link
    h4("Step 3 & Board Preparation", style = "color: #34495e; margin-top: 10px;"),
    wellPanel(
      style = "background-color: #f8f9fa; border-left: 4px solid #e67e22;",
      gmed::mod_seval_boards_display_ui(ns("boards"))
    ),

    hr(),

    # Learning Topics and Styles Section
    h4("Learning Assessment", style = "color: #34495e; margin-top: 20px;"),

    wellPanel(
      style = "background-color: #f8f9fa; border-left: 4px solid #e67e22;",

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

    hr(),

    # Previous Period Coach Comments
    uiOutput(ns("previous_coach_comments")),

    # Current Period Coach Entry
    h4("Coach Review - Current Period", style = "color: #34495e; margin-top: 20px;"),

    wellPanel(
      style = "background-color: #ffffff; border-left: 4px solid #27ae60;",

      div(
        style = "margin-bottom: 15px;",
        tags$label(
          "Comments on Learning Topics and Styles:",
          style = "font-weight: bold; color: #2c3e50;"
        ),
        tags$p(
          "Provide feedback on the resident's identified learning needs and preferred learning approaches.",
          style = "font-size: 12px; color: #7f8c8d; margin-top: 5px;"
        )
      ),

      textAreaInput(
        ns("coach_topics"),
        label = NULL,
        value = "",
        width = "100%",
        height = "150px",
        placeholder = "Enter your comments about learning topics and styles..."
      ),

      hr(),

      div(
        style = "margin-bottom: 15px;",
        tags$label(
          "Comments on Board Preparation and Exam Performance:",
          style = "font-weight: bold; color: #2c3e50;"
        ),
        tags$p(
          "Provide feedback on the resident's board exam preparation, Step 3 status, and in-training exam performance.",
          style = "font-size: 12px; color: #7f8c8d; margin-top: 5px;"
        )
      ),

      textAreaInput(
        ns("coach_board_prep"),
        label = NULL,
        value = "",
        width = "100%",
        height = "150px",
        placeholder = "Enter your comments about board preparation and exam performance..."
      )
    )
  )
}

mod_learning_server <- function(id, resident_data, current_period, app_data, app_data_rv = NULL) {
  moduleServer(id, function(input, output, session) {

    # Reactive accessor for the REDCap data dictionary; used by the gmed
    # boards display module to translate s_e_mksap_comp into its label.
    data_dict <- reactive({
      if (is.null(app_data_rv)) return(NULL)
      app_data_rv$data_dict
    })

    # ----- Boards / Step 3 / MKSAP (gmed display) -----
    # Pulls Step 3 status from resident_data + s_eval, derives ABIM pass
    # probability from the most recent test_data ITE row, and renders the
    # MKSAP tracker link.
    gmed::mod_seval_boards_display_server(
      "boards",
      current_s_eval = reactive({
        rd <- resident_data(); if (is.null(rd)) NULL else rd$current_period$s_eval
      }),
      resident_info  = reactive({
        rd <- resident_data(); if (is.null(rd)) NULL else rd$resident_info
      }),
      test_data      = reactive({
        ad <- app_data(); rd <- resident_data()
        if (is.null(ad) || is.null(rd)) return(NULL)
        td <- ad$all_forms$test_data
        rid <- rd$resident_info$record_id[1]
        if (is.null(td) || !nrow(td)) NULL else td[td$record_id == rid, , drop = FALSE]
      }),
      pgy = reactive({
        rd <- resident_data(); if (is.null(rd)) return(NA_integer_)
        lvl <- rd$resident_info$Level
        dplyr::case_when(
          lvl == "Intern" ~ 1L,
          lvl == "PGY2"   ~ 2L,
          lvl == "PGY3"   ~ 3L,
          grepl("PGY[0-9]", lvl) ~ as.integer(gsub("[^0-9]", "", lvl)),
          TRUE ~ NA_integer_
        )
      }),
      data_dict = data_dict
    )

    # Display learning topics
    output$learning_topics <- renderUI({
      req(resident_data(), app_data())

      curr_data <- resident_data()$current_period$s_eval

      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No learning assessment available"
          )
        )
      }

      # Get data dictionary choices for s_e_topic_sel field
      data_dict <- app_data()$data_dict
      topic_field_info <- data_dict %>%
        dplyr::filter(field_name == "s_e_topic_sel")

      if (nrow(topic_field_info) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "Topic field configuration not found"
          )
        )
      }

      # Parse choices
      choices_str <- topic_field_info$select_choices_or_calculations[1]
      topic_choices <- parse_choices_safe(choices_str)

      if (nrow(topic_choices) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No topics configured"
          )
        )
      }

      # Get topic checkbox columns and find selected ones
      topic_cols <- grep("^s_e_topic_sel___", names(curr_data), value = TRUE)
      selected_topics <- c()

      for (col in topic_cols) {
        # Extract the code from column name (e.g., "s_e_topic_sel___1" -> "1")
        code <- sub("^s_e_topic_sel___", "", col)

        # Check if this checkbox is selected (value = "1")
        val <- curr_data[[col]][1]
        if (!is.na(val) && val == "1") {
          # Look up the label from topic_choices
          label <- topic_choices$label[topic_choices$code == code]
          if (length(label) > 0) {
            selected_topics <- c(selected_topics, label[1])
          }
        }
      }

      if (length(selected_topics) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No topics identified"
          )
        )
      }

      tags$ul(
        class = "list-unstyled",
        lapply(selected_topics, function(topic) {
          tags$li(
            style = "padding: 5px 0;",
            icon("exclamation-circle", class = "text-warning"),
            " ", topic
          )
        })
      )
    })

    # Display learning styles
    output$learning_styles <- renderUI({
      req(resident_data(), app_data())

      curr_data <- resident_data()$current_period$s_eval

      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No learning preferences available"
          )
        )
      }

      # Get data dictionary choices for s_e_learn_style field
      data_dict <- app_data()$data_dict
      style_field_info <- data_dict %>%
        dplyr::filter(field_name == "s_e_learn_style")

      if (nrow(style_field_info) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "Learning style field configuration not found"
          )
        )
      }

      # Parse choices
      choices_str <- style_field_info$select_choices_or_calculations[1]
      style_choices <- parse_choices_safe(choices_str)

      if (nrow(style_choices) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No learning styles configured"
          )
        )
      }

      # Get style checkbox columns and find selected ones
      style_cols <- grep("^s_e_learn_style___", names(curr_data), value = TRUE)
      selected_styles <- c()

      for (col in style_cols) {
        # Extract the code from column name (e.g., "s_e_learn_style___1" -> "1")
        code <- sub("^s_e_learn_style___", "", col)

        # Check if this checkbox is selected (value = "1")
        val <- curr_data[[col]][1]
        if (!is.na(val) && val == "1") {
          # Look up the label from style_choices
          label <- style_choices$label[style_choices$code == code]
          if (length(label) > 0) {
            selected_styles <- c(selected_styles, label[1])
          }
        }
      }

      if (length(selected_styles) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No learning styles identified"
          )
        )
      }

      tags$ul(
        class = "list-unstyled",
        lapply(selected_styles, function(style) {
          tags$li(
            style = "padding: 5px 0;",
            icon("check-circle", class = "text-success"),
            " ", style
          )
        })
      )
    })


    # Display previous period coach comments
    output$previous_coach_comments <- renderUI({
      req(resident_data())

      prev_data <- resident_data()$previous_period$coach_rev

      if (is.null(prev_data) || nrow(prev_data) == 0) {
        return(NULL)
      }

      prev_topics <- prev_data$coach_ls_and_topic[1]
      prev_board <- prev_data$coach_step_board[1]

      has_prev_data <- (!is.na(prev_topics) && prev_topics != "") ||
                       (!is.na(prev_board) && prev_board != "")

      if (!has_prev_data) {
        return(NULL)
      }

      tagList(
        h4("Previous Period Coach Comments", style = "color: #34495e; margin-top: 20px;"),

        wellPanel(
          style = "background-color: #e8f4f8; border-left: 4px solid #3498db;",

          if (!is.na(prev_topics) && prev_topics != "") {
            tagList(
              h5("Topics and Learning Styles:", style = "color: #2980b9;"),
              div(
                style = "padding: 10px; background-color: white; border-radius: 4px; margin-bottom: 15px;",
                HTML(gsub("\n", "<br>", prev_topics))
              )
            )
          },

          if (!is.na(prev_board) && prev_board != "") {
            tagList(
              h5("Board Preparation:", style = "color: #2980b9;"),
              div(
                style = "padding: 10px; background-color: white; border-radius: 4px;",
                HTML(gsub("\n", "<br>", prev_board))
              )
            )
          }
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
          existing_topics <- curr_data$coach_ls_and_topic[1]
          existing_board <- curr_data$coach_step_board[1]

          # Populate or clear topics field
          if (!is.na(existing_topics) && existing_topics != "") {
            updateTextAreaInput(session, "coach_topics", value = existing_topics)
          } else {
            updateTextAreaInput(session, "coach_topics", value = "")
          }

          # Populate or clear board prep field
          if (!is.na(existing_board) && existing_board != "") {
            updateTextAreaInput(session, "coach_board_prep", value = existing_board)
          } else {
            updateTextAreaInput(session, "coach_board_prep", value = "")
          }

          message(sprintf("Loaded existing learning data for resident %s", new_resident_id))
        } else {
          # Clear both fields if no coach_rev record exists
          updateTextAreaInput(session, "coach_topics", value = "")
          updateTextAreaInput(session, "coach_board_prep", value = "")
          message(sprintf("Cleared learning forms for new resident %s (no coach_rev record)", new_resident_id))
        }
      }
    })

    # Return reactive with entered data
    return(
      reactive({
        list(
          coach_ls_and_topic = input$coach_topics,
          coach_step_board = input$coach_board_prep,
          is_complete = !is.null(input$coach_topics) && nchar(trimws(input$coach_topics)) > 0 &&
                        !is.null(input$coach_board_prep) && nchar(trimws(input$coach_board_prep)) > 0
        )
      })
    )
  })
}
