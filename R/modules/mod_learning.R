# Section 3: Learning & Board Preparation Module
# Displays exam scores, learning topics/styles, and allows coach to enter comments

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
      req(resident_data())

      curr_data <- resident_data()$current_period$s_eval
      resident_info <- resident_data()$resident_info

      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6; padding: 15px;",
            "No exam data available for current period"
          )
        )
      }

      # Build exam data list
      exam_list <- list()

      # USMLE/COMLEX scores
      if (!is.na(resident_info$usmle1) && resident_info$usmle1 != "") {
        exam_list[[length(exam_list) + 1]] <- c("USMLE Step 1", "", as.character(resident_info$usmle1))
      }
      if (!is.na(resident_info$usmle2) && resident_info$usmle2 != "") {
        exam_list[[length(exam_list) + 1]] <- c("USMLE Step 2", "", as.character(resident_info$usmle2))
      }
      if (!is.na(resident_info$usmle3) && resident_info$usmle3 != "") {
        exam_list[[length(exam_list) + 1]] <- c("USMLE Step 3", "", as.character(resident_info$usmle3))
      }
      if (!is.na(resident_info$comlex1) && resident_info$comlex1 != "") {
        exam_list[[length(exam_list) + 1]] <- c("COMLEX Level 1", "", as.character(resident_info$comlex1))
      }
      if (!is.na(resident_info$comlex2) && resident_info$comlex2 != "") {
        exam_list[[length(exam_list) + 1]] <- c("COMLEX Level 2", "", as.character(resident_info$comlex2))
      }

      # ITE scores with year
      if (!is.na(resident_info$ite_int) && resident_info$ite_int != "") {
        exam_list[[length(exam_list) + 1]] <- c("ITE", "Intern", as.character(resident_info$ite_int))
      }
      if (!is.na(resident_info$ite2) && resident_info$ite2 != "") {
        exam_list[[length(exam_list) + 1]] <- c("ITE", "PGY-2", as.character(resident_info$ite2))
      }
      if (!is.na(resident_info$ite3) && resident_info$ite3 != "") {
        exam_list[[length(exam_list) + 1]] <- c("ITE", "PGY-3", as.character(resident_info$ite3))
      }

      if (length(exam_list) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6; padding: 15px;",
            "No exam scores recorded"
          )
        )
      }

      # Convert to data frame
      exam_df <- as.data.frame(do.call(rbind, exam_list), stringsAsFactors = FALSE)
      colnames(exam_df) <- c("Exam", "Year", "Score/Percentile")

      # Render as HTML table
      tagList(
        tags$table(
          class = "table table-striped table-bordered",
          style = "background-color: white;",
          tags$thead(
            tags$tr(
              tags$th("Exam"),
              tags$th("Year"),
              tags$th("Score/Percentile")
            )
          ),
          tags$tbody(
            lapply(1:nrow(exam_df), function(i) {
              tags$tr(
                tags$td(exam_df[i, 1]),
                tags$td(exam_df[i, 2]),
                tags$td(exam_df[i, 3])
              )
            })
          )
        ),
        tags$p(
          style = "font-size: 12px; color: #7f8c8d; margin-top: 10px;",
          "Note: ITE scores are percentiles by training year"
        )
      )
    })

    # Display learning topics
    output$learning_topics <- renderUI({
      req(resident_data())

      curr_data <- resident_data()$current_period$s_eval

      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No learning assessment available"
          )
        )
      }

      # Get topic columns
      topic_cols <- grep("^s_e_topic_sel___", names(curr_data), value = TRUE)
      selected_topics <- c()

      for (col in topic_cols) {
        val <- curr_data[[col]][1]
        if (!is.na(val) && val != "") {
          selected_topics <- c(selected_topics, val)
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
      req(resident_data())

      curr_data <- resident_data()$current_period$s_eval

      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6;",
            "No learning preferences available"
          )
        )
      }

      # Get style columns
      style_cols <- grep("^s_e_learn_style___", names(curr_data), value = TRUE)
      selected_styles <- c()

      for (col in style_cols) {
        val <- curr_data[[col]][1]
        if (!is.na(val) && val != "") {
          selected_styles <- c(selected_styles, val)
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
      req(resident_data())

      curr_data <- resident_data()$current_period$s_eval

      if (is.null(curr_data) || nrow(curr_data) == 0) {
        return(
          div(
            style = "font-style: italic; color: #95a5a6; padding: 15px;",
            "No board preparation data available"
          )
        )
      }

      # Build board prep data list
      board_list <- list()

      if (!is.na(curr_data$s_e_step3[1]) && curr_data$s_e_step3[1] != "") {
        board_list[[length(board_list) + 1]] <- c("Step 3 Status", curr_data$s_e_step3[1])
      }
      if (!is.na(curr_data$s_e_step3_date[1]) && curr_data$s_e_step3_date[1] != "") {
        board_list[[length(board_list) + 1]] <- c("Step 3 Date", curr_data$s_e_step3_date[1])
      }
      if (!is.na(curr_data$s_e_board_concern[1]) && curr_data$s_e_board_concern[1] != "") {
        board_list[[length(board_list) + 1]] <- c("Board Concerns", curr_data$s_e_board_concern[1])
      }
      if (!is.na(curr_data$s_e_mksap_comp[1]) && curr_data$s_e_mksap_comp[1] != "") {
        board_list[[length(board_list) + 1]] <- c("MKSAP Complete", curr_data$s_e_mksap_comp[1])
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
