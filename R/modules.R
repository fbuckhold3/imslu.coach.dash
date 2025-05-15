# UI Component for Card 2: Resident Information

card2UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        card(
          card_header("Resident Information"),
          card_body(
            uiOutput(ns("resident_questions"))
          )
        )
      )
    )
  )
}

# Server logic for Card 2
card2Server <- function(id, resident_name, is_intro_period) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Generate dynamic questions based on whether it's an intro period
    output$resident_questions <- renderUI({
      req(resident_name())
      
      if (is_intro_period()) {
        # Introduction period questions
        tagList(
          h4(paste("Let's learn more about", resident_name())),
          textAreaInput(
            ns("background"), 
            label = paste0("Where is ", resident_name(), " from, what are they excited about, and what do they do outside of the program?"),
            height = "150px",
            width = "100%"
          )
        )
      } else {
        # Regular period questions (shown for all periods)
        tagList(
          h4(paste("Check-in with", resident_name())),
          textAreaInput(
            ns("dealing_with_residency"), 
            label = paste0("How is ", resident_name(), " dealing with residency? Any concerns or issues? Are they supported?"),
            height = "150px",
            width = "100%"
          ),
          textAreaInput(
            ns("wellbeing"), 
            label = paste0("How is ", resident_name(), "'s wellbeing? Any concerns or considerations?"),
            height = "150px",
            width = "100%"
          )
        )
      }
    })
    
    # Return entered values for use in parent module/app
    return(
      reactive({
        if (is_intro_period()) {
          list(
            background = input$background
          )
        } else {
          list(
            dealing_with_residency = input$dealing_with_residency,
            wellbeing = input$wellbeing
          )
        }
      })
    )
  })
}


# UI Component for Milestone Goals Tab
milestone_goals_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Card for Previous Milestone Goals
    card(
      card_header("Previous Milestone Goals"),
      card_body(
        uiOutput(ns("prior_milestone_goals"))
      )
    ),
    
    # Card for New Milestone Goals
    card(
      card_header("New Milestone Goals"),
      card_body(
        # 1. Patient Care / Medical Knowledge Goal
        div(
          class = "mb-4",
          h4("1. Patient Care / Medical Knowledge Milestone Goal"),
          div(
            class = "row",
            div(
              class = "col-md-12",
              selectInput(
                ns("pcmk_milestone_goal"),
                "Select a milestone to focus on:",
                choices = c("Select a milestone" = ""),
                width = "100%"
              )
            )
          ),
          div(
            class = "row mt-2",
            div(
              class = "col-md-12",
              textAreaInput(
                ns("how_pcmk"),
                "How will you work on this milestone? (Be specific about resources and approach)",
                rows = 3,
                width = "100%",
                placeholder = "Describe specific actions, resources, and timeline..."
              )
            )
          )
        ),
        
        # 2. SBP / PBLI Goal
        div(
          class = "mb-4",
          h4("2. Systems-Based Practice / Practice-Based Learning and Improvement Milestone Goal"),
          div(
            class = "row",
            div(
              class = "col-md-12",
              selectInput(
                ns("sbppbl_milestone_goal"),
                "Select a milestone to focus on:",
                choices = c("Select a milestone" = ""),
                width = "100%"
              )
            )
          ),
          div(
            class = "row mt-2",
            div(
              class = "col-md-12",
              textAreaInput(
                ns("how_sbppbl"),
                "How will you work on this milestone? (Be specific about resources and approach)",
                rows = 3,
                width = "100%",
                placeholder = "Describe specific actions, resources, and timeline..."
              )
            )
          )
        ),
        
        # 3. Prof / ICS Goal
        div(
          class = "mb-4",
          h4("3. Professionalism / Interpersonal Communication Skills Milestone Goal"),
          div(
            class = "row",
            div(
              class = "col-md-12",
              selectInput(
                ns("profics_milestone_goal"),
                "Select a milestone to focus on:",
                choices = c("Select a milestone" = ""),
                width = "100%"
              )
            )
          ),
          div(
            class = "row mt-2",
            div(
              class = "col-md-12",
              textAreaInput(
                ns("how_profics"),
                "How will you work on this milestone? (Be specific about resources and approach)",
                rows = 3,
                width = "100%",
                placeholder = "Describe specific actions, resources, and timeline..."
              )
            )
          )
        ),
        
        # Overall comments section
        div(
          class = "mt-4",
          h4("Overall Comments on Milestone-Based Goals"),
          textAreaInput(
            ns("milestone_goals_comments"),
            NULL,
            rows = 4,
            width = "100%",
            placeholder = "Provide comments on the selected milestone goals, appropriateness, and additional guidance..."
          )
        )
      )
    )
  )
}

# Server Function for Milestone Goals Module
milestone_goals_server <- function(id, values, app_data, processed_resident_data) {
  moduleServer(id, function(input, output, session) {
    # Function to extract the first non-NA milestone value
    get_first_non_na_milestone <- function(resident_data, resident_name, prefix_list) {
      # Filter for this resident
      resident_rows <- resident_data %>% 
        filter(name == resident_name)
      
      if (nrow(resident_rows) == 0) {
        return(NULL)
      }
      
      # For each prefix, look for columns matching that prefix
      for (prefix in prefix_list) {
        matching_cols <- grep(paste0("^", prefix, "[0-9]+_r[0-9]+$"), names(resident_rows), value = TRUE)
        
        # For each matching column, check if there are any non-NA values
        for (col in matching_cols) {
          for (i in 1:nrow(resident_rows)) {
            value <- resident_rows[[col]][i]
            if (!is.na(value) && value != "") {
              return(list(column = col, value = value))
            }
          }
        }
      }
      
      return(NULL)
    }
    
    # Load milestone descriptions for dropdown options
    milestone_descriptions <- reactive({
      req(values$selected_resident)
      req(app_data())
      
      # Get the RDM dictionary
      rdm_dict <- app_data()$rdm_dict
      
      # Function to extract milestone descriptions from dictionary
      extract_milestone_descriptions <- function(prefix, dict) {
        fields <- grep(paste0("^", prefix, "[0-9]+_r[0-9]+$"), dict$field_name, value = TRUE)
        descriptions <- character(0)
        
        # Remove duplicates by extracting the base milestone ID
        base_fields <- unique(gsub("_r[0-9]+$", "", fields))
        
        for (base_field in base_fields) {
          # Get all fields for this base milestone
          milestone_fields <- grep(paste0("^", base_field, "_r[0-9]+$"), fields, value = TRUE)
          
          # Use the first field to get the description
          field <- milestone_fields[1]
          field_row <- dict[dict$field_name == field, ]
          
          if (nrow(field_row) > 0 && "field_label" %in% names(field_row)) {
            description <- field_row$field_label[1]
            # Store with both ID and description
            milestone_id <- gsub("_r[0-9]+$", "", field)
            label <- paste0(milestone_id, ": ", description)
            descriptions[label] <- milestone_id
          }
        }
        
        return(descriptions)
      }
      
      # Extract descriptions for each competency
      milestone_desc <- list()
      
      # PC/MK Milestones
      milestone_desc$pcmk <- c(
        extract_milestone_descriptions("pc", rdm_dict),
        extract_milestone_descriptions("mk", rdm_dict)
      )
      
      # SBP/PBLI Milestones
      milestone_desc$sbppbl <- c(
        extract_milestone_descriptions("sbp", rdm_dict),
        extract_milestone_descriptions("pbl", rdm_dict)
      )
      
      # Prof/ICS Milestones
      milestone_desc$profics <- c(
        extract_milestone_descriptions("prof", rdm_dict),
        extract_milestone_descriptions("ics", rdm_dict)
      )
      
      return(milestone_desc)
    })
    
    # Update the dropdown options with milestone descriptions
    observe({
      req(milestone_descriptions())
      
      descs <- milestone_descriptions()
      
      if (length(descs$pcmk) > 0) {
        updateSelectInput(session, "pcmk_milestone_goal",
                          choices = c("Select a milestone" = "", descs$pcmk))
      }
      
      if (length(descs$sbppbl) > 0) {
        updateSelectInput(session, "sbppbl_milestone_goal",
                          choices = c("Select a milestone" = "", descs$sbppbl))
      }
      
      if (length(descs$profics) > 0) {
        updateSelectInput(session, "profics_milestone_goal",
                          choices = c("Select a milestone" = "", descs$profics))
      }
    })
    
    # Display previous milestone goals
    output$prior_milestone_goals <- renderUI({
      req(values$selected_resident)
      req(values$redcap_prev_period)  # Use the centrally mapped previous period
      
      # Debug
      message("Looking for previous ILP data for milestone goals - resident: ", 
              values$selected_resident$name, ", period: ", values$redcap_prev_period)
      
      if (is.na(values$redcap_prev_period)) {
        return(div(class = "alert alert-info", 
                   "No previous milestone goals available for this resident's current level and period."))
      }
      
      # Get app data
      data <- app_data()
      
      # Create ILP table using the centrally mapped previous period
      ilp_table <- tryCatch({
        create_ilp_data_table(
          resident_name = values$selected_resident$name,
          period = values$redcap_prev_period,
          resident_data = processed_resident_data(),
          rdm_dict = data$rdm_dict
        )
      }, error = function(e) {
        message("Error creating ILP table for milestone goals: ", e$message)
        return(NULL)
      })
      
      if (is.null(ilp_table) || nrow(ilp_table) == 0) {
        return(div(class = "alert alert-info", 
                   paste("No prior milestone goals available for", values$selected_resident$name, 
                         "in the", values$redcap_prev_period, "period.")))
      }
      
      # Clean up column names
      colnames(ilp_table) <- c(
        "Competency", 
        "Goal Met", 
        "Comments", 
        "Goal Description", 
        "Action Plan", 
        "Milestone Descriptions"
      )
      
      # Clean up milestone descriptions
      ilp_table$`Milestone Descriptions` <- gsub("\\n\\nNA", "", ilp_table$`Milestone Descriptions`)
      
      # Return the formatted table
      div(
        h4(paste("Previous ILP from", values$redcap_prev_period, "Period")),
        div(
          style = "overflow-x: auto;",
          DT::renderDataTable({
            DT::datatable(
              ilp_table,
              options = list(
                pageLength = 3,
                scrollX = TRUE,
                dom = 't',
                ordering = FALSE,
                columnDefs = list(
                  list(className = 'dt-left', targets = "_all"),
                  list(width = '18%', targets = 0),
                  list(width = '8%', targets = 1),
                  list(width = '15%', targets = 2),
                  list(width = '20%', targets = 3),
                  list(width = '15%', targets = 4),
                  list(width = '24%', targets = 5)
                )
              ),
              rownames = FALSE,
              escape = FALSE,
              class = "display compact stripe"
            ) %>%
              DT::formatStyle(
                'Competency',
                fontWeight = 'bold',
                backgroundColor = '#f0f7fa'
              ) %>%
              DT::formatStyle(
                'Goal Met',
                backgroundColor = styleEqual(
                  c("Yes", "No"), 
                  c('#d4edda', '#f8d7da')
                ),
                color = styleEqual(
                  c("Yes", "No"), 
                  c('#155724', '#721c24')
                ),
                fontWeight = 'bold',
                textAlign = 'center'
              ) %>%
              DT::formatStyle(
                'Goal Description',
                fontWeight = 'bold',
                backgroundColor = '#f8f9fa'
              ) %>%
              DT::formatStyle(
                'Milestone Descriptions',
                whiteSpace = 'pre-line',
                fontSize = '90%',
                backgroundColor = '#fafafa'
              )
          })
        )
      )
    })
    
    # Find non-NA milestone values for each competency group
    observe({
      req(values$selected_resident)
      req(processed_resident_data())
      
      # Get resident data
      resident_data <- processed_resident_data()
      
      # Find values for each competency group
      pcmk_result <- get_first_non_na_milestone(
        resident_data,
        values$selected_resident$name,
        c("pc", "mk")
      )
      
      sbppbl_result <- get_first_non_na_milestone(
        resident_data,
        values$selected_resident$name,
        c("sbp", "pbl")
      )
      
      profics_result <- get_first_non_na_milestone(
        resident_data,
        values$selected_resident$name,
        c("prof", "ics")
      )
      
      # Update the dropdowns if values are found
      milestones <- milestone_descriptions()
      
      if (!is.null(pcmk_result) && !is.null(milestones$pcmk)) {
        base_col <- gsub("_r[0-9]+$", "", pcmk_result$column)
        for (opt_name in names(milestones$pcmk)) {
          if (milestones$pcmk[opt_name] == base_col) {
            updateSelectInput(session, "pcmk_milestone_goal", selected = opt_name)
            break
          }
        }
      }
      
      if (!is.null(sbppbl_result) && !is.null(milestones$sbppbl)) {
        base_col <- gsub("_r[0-9]+$", "", sbppbl_result$column)
        for (opt_name in names(milestones$sbppbl)) {
          if (milestones$sbppbl[opt_name] == base_col) {
            updateSelectInput(session, "sbppbl_milestone_goal", selected = opt_name)
            break
          }
        }
      }
      
      if (!is.null(profics_result) && !is.null(milestones$profics)) {
        base_col <- gsub("_r[0-9]+$", "", profics_result$column)
        for (opt_name in names(milestones$profics)) {
          if (milestones$profics[opt_name] == base_col) {
            updateSelectInput(session, "profics_milestone_goal", selected = opt_name)
            break
          }
        }
      }
    })
    
    # Fill previous action plan values if available
    observe({
      req(values$selected_resident)
      
      # Get resident data
      data <- processed_resident_data()
      
      # Filter for this resident
      resident_rows <- data %>% 
        filter(name == values$selected_resident$name)
      
      if (nrow(resident_rows) > 0) {
        # Try to find the most recent action plan values
        for (field in c("how_pcmk", "how_sbppbl", "how_profics")) {
          if (field %in% names(resident_rows)) {
            # Find first non-NA value
            for (i in 1:nrow(resident_rows)) {
              value <- resident_rows[[field]][i]
              if (!is.na(value) && value != "") {
                # Update the corresponding input
                updateTextAreaInput(session, field, value = value)
                break
              }
            }
          }
        }
      }
    })
    
    # Return the input values
    return(reactive({
      list(
        pcmk_milestone_goal = input$pcmk_milestone_goal,
        how_pcmk = input$how_pcmk,
        sbppbl_milestone_goal = input$sbppbl_milestone_goal,
        how_sbppbl = input$how_sbppbl,
        profics_milestone_goal = input$profics_milestone_goal,
        how_profics = input$how_profics,
        milestone_goals_comments = input$milestone_goals_comments
      )
    }))
  })
}