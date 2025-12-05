# ==============================================================================
# RESIDENT SELECTION TABLE MODULE
# R/modules/mod_resident_table.R
# ==============================================================================
#
# Interactive table for selecting residents to review
# Shows completion status for self-eval, coach review, and second review
# Allows period filtering and resident selection
#
# FIXES APPLIED:
# 1. Replaced cur_data() with pick(everything()) for dplyr 1.1.0+
# 2. Fixed Level display to show actual PGY level instead of "Unknown"
#
# ==============================================================================

#' Resident Table Module UI
#'
#' @param id Module namespace ID
#' @return UI elements for resident selection table
#' @export
mod_resident_table_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .resident-table-container {
        background: white;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .resident-table-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 20px;
      }
      .resident-table-filters {
        display: flex;
        gap: 15px;
        align-items: flex-end;
      }
      .completion-icon {
        font-size: 1.3em;
        cursor: help;
      }
      .completion-complete {
        color: #28a745;
      }
      .completion-incomplete {
        color: #dc3545;
      }
      .resident-row {
        cursor: pointer;
      }
      .resident-row:hover {
        background-color: #f8f9fa !important;
      }
      .resident-row.selected {
        background-color: #e3f2fd !important;
      }
      .table-legend {
        display: flex;
        gap: 20px;
        margin-top: 15px;
        padding: 10px;
        background: #f8f9fa;
        border-radius: 4px;
        font-size: 14px;
      }
      .legend-item {
        display: flex;
        align-items: center;
        gap: 5px;
      }
      .ad-hoc-section {
        margin-top: 20px;
        padding-top: 20px;
        border-top: 2px solid #e9ecef;
      }
    ")),
    
    div(
      class = "resident-table-container",
      
      div(
        class = "resident-table-header",
        div(
          h4(icon("users"), " Your Residents")
        ),
        div(
          actionButton(
            ns("back_to_coach_select"),
            "← Back to Coach Selection",
            class = "btn-secondary",
            icon = icon("user-tie")
          )
        )
      ),
      
      # Main resident table
      DT::DTOutput(ns("resident_table")),
      
      # Legend
      div(
        class = "table-legend",
        div(
          class = "legend-item",
          span(class = "completion-icon completion-complete", "\u2713"),
          span("Completed")
        ),
        div(
          class = "legend-item",
          span(class = "completion-icon completion-incomplete", "\u25CF"),
          span("Not Completed")
        ),
        div(
          class = "legend-item",
          icon("info-circle"),
          span("Click any row to open review")
        )
      ),
      
      # Ad hoc review section
      div(
        class = "ad-hoc-section",
        h5(icon("calendar-plus"), " Ad Hoc Reviews"),
        p(
          style = "color: #666; font-size: 14px;",
          "For unscheduled reviews outside regular periods"
        ),
        actionButton(
          ns("ad_hoc_btn"),
          "Create Ad Hoc Review",
          icon = icon("plus"),
          class = "btn-secondary",
          disabled = "disabled"  # Will enable in future phase
        ),
        span(
          style = "margin-left: 10px; color: #999; font-size: 13px;",
          "(Coming soon)"
        )
      )
    )
  )
}

#' Resident Table Module Server
#'
#' @param id Module namespace ID
#' @param coach_data Reactive containing coach and filtered residents
#' @param app_data Reactive containing complete RDM data structure
#' @return Reactive list with selected resident and period information
#' @export
mod_resident_table_server <- function(id, coach_data, app_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Selected resident state
    selected_resident <- reactiveVal(NULL)
    selected_period <- reactiveVal(NULL)
    
    # Build table data with completion status
    table_data <- reactive({
  req(app_data())  # REMOVED selected_period() requirement

  # Safely get coach data
  coach_info <- tryCatch({
    coach_data()
  }, error = function(e) {
    return(NULL)
  })

  # Require valid coach data with a selected coach
  req(coach_info)
  req(!is.null(coach_info$coach_name))
  req(nzchar(coach_info$coach_name))

  tryCatch({
    residents <- coach_info$residents
    coach_name <- coach_info$coach_name
    # REMOVED: period_num <- get_period_number(selected_period())
    
    # For each resident, check completion status using THEIR detected period
    table_df <- residents %>%
      rowwise() %>%
      mutate(
        # Determine review role
        review_role = get_review_role(pick(everything()), coach_name),
        
        # Use the PRE-DETECTED period for this resident
        period_num = current_period_num,
        
        # Check completion status for THIS resident's period
        seval_complete = check_seval_complete(
          app_data()$all_forms,
          record_id,
          period_num
        ),
        coach_complete = check_coach_review_complete(
          app_data()$all_forms,
          record_id,
          period_num
        ),
        second_complete = check_second_review_complete(
          app_data()$all_forms,
          record_id,
          period_num
        ),
        
        # Format for display
        display_name = full_name,
        display_level = Level,
        display_period = current_period  # Show detected period
      ) %>%
      ungroup() %>%
      select(
        record_id,
        display_name,
        display_level,
        display_period,
        review_role,
        seval_complete,
        coach_complete,
        second_complete
      ) %>%
      arrange(display_name)

    return(table_df)

  }, error = function(e) {
    message("ERROR in table_data: ", e$message)
    return(NULL)
  })
})


    # Render interactive DataTable
    output$resident_table <- DT::renderDT({
      req(table_data())
      df <- table_data()

      # Check if NULL (error in building)
      if (is.null(df)) {
        return(NULL)
      }
      
      tryCatch({
      df <- df %>%
        mutate(
          `Self-Eval` = ifelse(
            seval_complete,
            '<span class="completion-icon completion-complete" title="Completed">&#x2713;</span>',
            '<span class="completion-icon completion-incomplete" title="Not Completed">&#x25CF;</span>'
          ),
          `Coach Review` = ifelse(
            coach_complete,
            '<span class="completion-icon completion-complete" title="Completed">&#x2713;</span>',
            '<span class="completion-icon completion-incomplete" title="Not Completed">&#x25CF;</span>'
          ),
          `Second Review` = ifelse(
            second_complete,
            '<span class="completion-icon completion-complete" title="Completed">&#x2713;</span>',
            '<span class="completion-icon completion-incomplete" title="Not Completed">&#x25CF;</span>'
          ),
          Role = case_when(
            review_role == "primary" ~ '<span class="badge badge-primary">Primary</span>',
            review_role == "secondary" ~ '<span class="badge badge-secondary">Secondary</span>',
            TRUE ~ ''
          )
        )
      
      # Select columns for display
      display_df <- df %>%
        select(
          Name = display_name,
          Level = display_level,
          Role,
          `Self-Eval`,
          `Coach Review`,
          `Second Review`
        )
      
      # Create DataTable
      DT::datatable(
        display_df,
        selection = 'single',
        escape = FALSE,  # Allow HTML in cells
        rownames = FALSE,
        options = list(
          pageLength = 25,
          dom = 'ftip',  # Filter, table, info, pagination
          language = list(
            search = "Search residents:",
            info = "Showing _START_ to _END_ of _TOTAL_ residents",
            infoEmpty = "No residents found",
            infoFiltered = "(filtered from _MAX_ total)",
            emptyTable = "No residents assigned for this period"
          ),
          columnDefs = list(
            list(
              targets = c(2, 3, 4, 5),  # Role and completion columns
              className = 'dt-center'
            )
          ),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().container()).find('table').addClass('table-hover');",
            "}"
          )
        ),
        class = 'table table-striped table-bordered'
      )
      
      }, error = function(e) {
        message("ERROR rendering table: ", e$message)
        print(e)
        return(NULL)
      })
    })
    
    # Handle row selection
    # Handle row selection
observeEvent(input$resident_table_rows_selected, {
  req(table_data())
  selected_row <- input$resident_table_rows_selected
  
  if (length(selected_row) > 0) {
    df <- table_data()
    selected_resident_id <- df$record_id[selected_row]
    
    # Get full resident info
    resident_info <- app_data()$residents %>%
      filter(record_id == selected_resident_id) %>%
      slice(1)
    
    # Use PRE-DETECTED period from resident_info
    resident_period <- resident_info$current_period[1]
    
    # Update selected period
    selected_period(resident_period)
    
    # Set selected resident
    selected_resident(list(
      record_id = selected_resident_id,
      name = df$display_name[selected_row],
      level = df$display_level[selected_row],
      role = df$review_role[selected_row],
      period = resident_period,
      period_number = get_period_number(resident_period),
      resident_info = resident_info
    ))
    
    message(sprintf(
      "[%s] Resident selected: %s (ID: %s, Period: %s [auto-detected])",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      df$display_name[selected_row],
      selected_resident_id,
      resident_period
    ))
    
    # Show confirmation
    showNotification(
      sprintf(
        "Opening review for %s - %s (auto-detected)",
        df$display_name[selected_row],
        resident_period
      ),
      type = "message",
      duration = 3
    )
  }
})
    
    # Deselect resident (called externally when returning to table)
    deselect_resident <- function() {
      selected_resident(NULL)
      DT::selectRows(
        DT::dataTableProxy("resident_table", session),
        NULL
      )
    }
    
    # Back to coach selection button
    back_to_coach_clicked <- reactive({
      input$back_to_coach_select
    })

    observeEvent(input$back_to_coach_select, {
      message(sprintf(
        "[%s] Back to coach selection button clicked",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ))

      showNotification(
        "Returning to coach selection",
        type = "message",
        duration = 2
      )
    })

    # Ad hoc review button (placeholder for future)
    observeEvent(input$ad_hoc_btn, {
      showNotification(
        "Ad hoc reviews will be available in a future update.",
        type = "message",
        duration = 3
      )
    })

    # Return reactive values and functions
    return(
      list(
        selected_resident = selected_resident,
        selected_period = selected_period,
        current_period = reactive({ get_period_number(selected_period()) }),  # Add this for compatibility
        clear_selection = deselect_resident,  # Renamed for clarity
        back_to_coach_clicked = back_to_coach_clicked  # Add back button reactive
      )
    )
  })
}

#' Get Resident Summary Text
#'
#' Helper to create summary text for selected resident
#' 
#' @param resident_data List from selected_resident reactive
#' @return Character string with formatted summary
#' @export
format_resident_summary <- function(resident_data) {
  sprintf(
    "%s (%s) - %s - %s Review",
    resident_data$name,
    resident_data$level,
    resident_data$period,
    tools::toTitleCase(resident_data$role)
  )
}