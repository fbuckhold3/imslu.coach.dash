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
    # Exam Scores Section
    h4("Exam Performance", style = "color: #34495e; margin-top: 10px;"),

    wellPanel(
      style = "background-color: #f8f9fa; border-left: 4px solid #e67e22;",
      uiOutput(ns("exam_scores_table"))
    ),

    hr(),

    # Risk Assessment Section
    h4("Performance Risk Assessment", style = "color: #34495e; margin-top: 20px;"),

    wellPanel(
      style = "background-color: #f8f9fa; border-left: 4px solid #e67e22;",
      div(
        class = "alert alert-info mb-3",
        icon("info-circle", class = "me-2"),
        p(
          class = "mb-0",
          "We are determining a 'risk status' for passing boards based on in-training scores. ",
          "This takes into account a combination of total percentage and/or a drop in scores over a year. ",
          "In meeting with your coach, you will be assigned a learning plan."
        )
      ),
      uiOutput(ns("risk_assessment"))
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

    # Board Preparation Status
    h4("Board Preparation Status", style = "color: #34495e; margin-top: 20px;"),

    wellPanel(
      style = "background-color: #f8f9fa; border-left: 4px solid #e67e22;",
      uiOutput(ns("board_prep_table"))
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

mod_learning_server <- function(id, resident_data, current_period, app_data) {
  moduleServer(id, function(input, output, session) {

    # Display exam scores table
    output$exam_scores_table <- renderUI({
      req(resident_data(), app_data())

      resident_info <- resident_data()$resident_info
      record_id <- resident_info$record_id

      # Get test_data for this resident
      test_data <- app_data()$all_forms$test_data %>%
        dplyr::filter(record_id == !!record_id)

      # Translation map for ITE subspecialty abbreviations
      subspecialty_map <- c(
        "total_ile" = "Total",
        "cards_ile" = "Cardiology",
        "endo_ile" = "Endocrinology",
        "gi_ile" = "Gastroenterology",
        "gim_ile" = "General Internal Medicine",
        "geri_ile" = "Geriatrics",
        "hemonc_ile" = "Hematology/Oncology",
        "id_ile" = "Infectious Disease",
        "nephro_ile" = "Nephrology",
        "neuro_ile" = "Neurology",
        "pulm_ccm_ile" = "Pulmonary/Critical Care",
        "rheum_ile" = "Rheumatology",
        "hvc_ile" = "High Value Care"
      )

      output_sections <- list()

      # ===== USMLE/COMLEX Section =====
      exam_list <- list()

      # Helper function to safely check and add exam score from resident_info
      add_exam_if_present <- function(field_name, exam_name) {
        if (field_name %in% names(resident_info)) {
          value <- resident_info[[field_name]]
          if (!is.na(value) && !is.null(value) && trimws(as.character(value)) != "") {
            exam_list[[length(exam_list) + 1]] <<- c(exam_name, as.character(value))
          }
        }
      }

      # USMLE/COMLEX scores (from resident_data form)
      add_exam_if_present("usmle_step1", "USMLE Step 1")
      add_exam_if_present("usmle_step2", "USMLE Step 2")
      add_exam_if_present("usmle_step3_score", "USMLE Step 3")
      add_exam_if_present("comlex_step1", "COMLEX Level 1")
      add_exam_if_present("comlex_step2", "COMLEX Level 2")
      add_exam_if_present("comlex_step3", "COMLEX Level 3")

      if (length(exam_list) > 0) {
        exam_df <- as.data.frame(do.call(rbind, exam_list), stringsAsFactors = FALSE)
        colnames(exam_df) <- c("Exam", "Score")

        output_sections[[length(output_sections) + 1]] <- tagList(
          h5("USMLE/COMLEX Scores", style = "color: #2c3e50; margin-top: 0;"),
          tags$table(
            class = "table table-striped table-bordered",
            style = "background-color: white; margin-bottom: 20px;",
            tags$thead(
              tags$tr(
                tags$th("Exam"),
                tags$th("Score")
              )
            ),
            tags$tbody(
              lapply(1:nrow(exam_df), function(i) {
                tags$tr(
                  tags$td(exam_df[i, 1]),
                  tags$td(exam_df[i, 2])
                )
              })
            )
          )
        )
      }

      # ===== ITE Section (Pivoted) =====
      if (nrow(test_data) > 0) {
        # Determine which PGY years have data
        available_years <- c()
        for (pgy in c("pgy1", "pgy2", "pgy3")) {
          # Check if any subspecialty field exists for this year
          has_data <- FALSE
          for (subspec_abbr in names(subspecialty_map)) {
            field_name <- paste0(pgy, "_", subspec_abbr)
            if (field_name %in% names(test_data)) {
              value <- test_data[[field_name]][1]
              if (!is.na(value) && !is.null(value) && trimws(as.character(value)) != "") {
                has_data <- TRUE
                break
              }
            }
          }
          if (has_data) {
            available_years <- c(available_years, pgy)
          }
        }

        if (length(available_years) > 0) {
          # Build ITE table with subspecialties as rows, years as columns
          ite_rows <- list()

          for (subspec_abbr in names(subspecialty_map)) {
            row_data <- list(subspecialty = subspecialty_map[subspec_abbr])

            # Get score for each available year
            for (pgy in available_years) {
              field_name <- paste0(pgy, "_", subspec_abbr)
              if (field_name %in% names(test_data)) {
                value <- test_data[[field_name]][1]
                if (!is.na(value) && !is.null(value) && trimws(as.character(value)) != "") {
                  row_data[[pgy]] <- as.character(value)
                } else {
                  row_data[[pgy]] <- "-"
                }
              } else {
                row_data[[pgy]] <- "-"
              }
            }

            # Only include row if at least one score is not "-"
            if (any(unlist(row_data[available_years]) != "-")) {
              ite_rows[[length(ite_rows) + 1]] <- row_data
            }
          }

          if (length(ite_rows) > 0) {
            # Create column headers
            year_headers <- lapply(available_years, function(pgy) {
              year_num <- substr(pgy, 4, 4)
              tags$th(paste0("PGY-", year_num))
            })

            # Create table rows
            table_rows <- lapply(ite_rows, function(row_data) {
              year_cells <- lapply(available_years, function(pgy) {
                tags$td(row_data[[pgy]])
              })

              tags$tr(
                tags$td(row_data$subspecialty),
                year_cells
              )
            })

            output_sections[[length(output_sections) + 1]] <- tagList(
              h5("In-Training Exam (ITE) Percentiles", style = "color: #2c3e50; margin-top: 0;"),
              tags$table(
                class = "table table-striped table-bordered",
                style = "background-color: white;",
                tags$thead(
                  tags$tr(
                    tags$th("Subspecialty"),
                    year_headers
                  )
                ),
                tags$tbody(
                  table_rows
                )
              )
            )
          }
        }
      }

      # Return combined output
      if (length(output_sections) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6; padding: 15px;",
            "No exam scores recorded"
          )
        )
      }

      tagList(output_sections)
    })

    # Display risk assessment
    output$risk_assessment <- renderUI({
      req(resident_data(), app_data())

      resident_info <- resident_data()$resident_info
      record_id <- resident_info$record_id

      # Get test_data for this resident
      test_data <- app_data()$all_forms$test_data %>%
        dplyr::filter(record_id == !!record_id)

      if (is.null(test_data) || nrow(test_data) == 0) {
        return(
          div(
            class = "alert alert-info",
            role = "alert",
            icon("info-circle"),
            " No ITE scores available for risk assessment."
          )
        )
      }

      # Use gmed function to assess risk
      tryCatch({
        risk <- gmed::assess_ite_risk(test_data)

        div(
          class = paste("alert", risk$risk_class),
          role = "alert",
          tags$strong("Risk Level: ", risk$risk_level),
          tags$br(),
          tags$small(risk$details)
        )
      }, error = function(e) {
        div(
          class = "alert alert-info",
          role = "alert",
          icon("info-circle"),
          " Risk assessment not available yet."
        )
      })
    })

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

    # Display board prep status
    output$board_prep_table <- renderUI({
      req(resident_data(), app_data())

      curr_data <- resident_data()$current_period$s_eval

      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6; padding: 15px;",
            "No board preparation data available"
          )
        )
      }

      # Helper function to parse yesno fields (1 = Yes, 0 = No)
      parse_yesno <- function(value) {
        if (is.na(value) || value == "") return("")
        if (value == "1") return("Yes")
        if (value == "0") return("No")
        return(as.character(value))
      }

      # Build board prep data list
      board_list <- list()

      if (!is.na(curr_data$s_e_step3[1]) && curr_data$s_e_step3[1] != "") {
        board_list[[length(board_list) + 1]] <- c("Step 3 Completed", parse_yesno(curr_data$s_e_step3[1]))
      }
      if (!is.na(curr_data$s_e_step3_contact[1]) && curr_data$s_e_step3_contact[1] != "") {
        board_list[[length(board_list) + 1]] <- c("Emailed Score to Program", parse_yesno(curr_data$s_e_step3_contact[1]))
      }
      if (!is.na(curr_data$s_e_step3_date_set[1]) && curr_data$s_e_step3_date_set[1] != "") {
        board_list[[length(board_list) + 1]] <- c("Step 3 Date Set", parse_yesno(curr_data$s_e_step3_date_set[1]))
      }
      if (!is.na(curr_data$s_e_step3_date[1]) && curr_data$s_e_step3_date[1] != "") {
        board_list[[length(board_list) + 1]] <- c("Step 3 Scheduled Date", curr_data$s_e_step3_date[1])
      }
      if (!is.na(curr_data$s_e_board_concern[1]) && curr_data$s_e_board_concern[1] != "") {
        board_list[[length(board_list) + 1]] <- c("Board Concerns", parse_yesno(curr_data$s_e_board_concern[1]))
      }
      if (!is.na(curr_data$s_e_board_help[1]) && curr_data$s_e_board_help[1] != "") {
        board_list[[length(board_list) + 1]] <- c("Discussed with Program", parse_yesno(curr_data$s_e_board_help[1]))
      }

      if (length(board_list) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6; padding: 15px;",
            "No board preparation information provided"
          )
        )
      }

      # Convert to data frame
      board_df <- as.data.frame(do.call(rbind, board_list), stringsAsFactors = FALSE)
      colnames(board_df) <- c("Category", "Value")

      # Render as HTML table
      tags$table(
        class = "table table-striped table-bordered",
        style = "background-color: white;",
        tags$thead(
          tags$tr(
            tags$th("Category"),
            tags$th("Status")
          )
        ),
        tags$tbody(
          lapply(1:nrow(board_df), function(i) {
            tags$tr(
              tags$td(board_df[i, 1]),
              tags$td(board_df[i, 2])
            )
          })
        )
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

    # Pre-populate if data already exists for current period
    observe({
      req(resident_data())

      curr_data <- resident_data()$current_period$coach_rev

      if (!is.null(curr_data) && nrow(curr_data) > 0) {
        existing_topics <- curr_data$coach_ls_and_topic[1]
        existing_board <- curr_data$coach_step_board[1]

        if (!is.na(existing_topics) && existing_topics != "") {
          updateTextAreaInput(
            session,
            "coach_topics",
            value = existing_topics
          )
        }

        if (!is.na(existing_board) && existing_board != "") {
          updateTextAreaInput(
            session,
            "coach_board_prep",
            value = existing_board
          )
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
