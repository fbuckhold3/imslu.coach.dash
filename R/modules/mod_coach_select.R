# ==============================================================================
# COACH SELECTION MODULE - FINAL FIX
# R/modules/mod_coach_select.R
# ==============================================================================
#
# FINAL FIX: Replaced apply() with safer for loop in renderUI
#
# ==============================================================================

#' Coach Selection Module UI
#'
#' @param id Module namespace ID
#' @return UI elements for coach selection
#' @export
mod_coach_select_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(HTML("
      .coach-select-container {
        background: white;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin-bottom: 20px;
      }
      .coach-select-header {
        display: flex;
        align-items: center;
        margin-bottom: 15px;
      }
      .coach-select-header h4 {
        margin: 0;
        color: #0072B2;
        flex-grow: 1;
      }
      .coach-select-info {
        background: #f8f9fa;
        padding: 10px;
        border-radius: 4px;
        margin-top: 10px;
        font-size: 14px;
        color: #666;
      }
      .coach-stats {
        display: flex;
        gap: 20px;
        margin-top: 10px;
      }
      .coach-stat {
        display: flex;
        align-items: center;
        gap: 5px;
      }
      .coach-stat-label {
        font-weight: 600;
        color: #333;
      }
    ")),
    
    div(
      class = "coach-select-container",
      
      div(
        class = "coach-select-header",
        h4(icon("user-tie"), " Select Your Name"),
        uiOutput(ns("refresh_btn"))
      ),
      
      selectInput(
        ns("coach_name"),
        label = NULL,
        choices = c("Select coach..." = ""),
        width = "100%"
      ),
      
      uiOutput(ns("coach_info"))
    )
  )
}

#' Coach Selection Module Server
#'
#' @param id Module namespace ID
#' @param app_data Reactive containing complete RDM data structure
#' @return Reactive list with selected_coach and filtered_residents
#' @export
mod_coach_select_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize coach selection state
    selected_coach <- reactiveVal(NULL)
    
    # Update coach choices when data loads
    observe({
      req(app_data())

      message("DEBUG: Updating coach choices...")

      # Get unique coaches from resident data
      tryCatch({
        coaches <- app_data()$residents %>%
          filter(!is.na(coach) & coach != "") %>%
          pull(coach) %>%
          unique() %>%
          sort()

        message("DEBUG: Found ", length(coaches), " coaches")

        # Add "Select coach..." placeholder
        coach_choices <- c("Select coach..." = "", setNames(coaches, coaches))

        # Update dropdown
        updateSelectInput(
          session,
          "coach_name",
          choices = coach_choices,
          selected = selected_coach()
        )

        message("DEBUG: Coach dropdown updated successfully")
      }, error = function(e) {
        message("ERROR in coach choice update: ", e$message)
        print(e)
      })
    })
    
    # Update selected coach when dropdown changes
    observeEvent(input$coach_name, {
      if (nzchar(input$coach_name)) {
        selected_coach(input$coach_name)
        
        message(sprintf(
          "[%s] Coach selected: %s",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          input$coach_name
        ))
      } else {
        selected_coach(NULL)
      }
    })
    
    # Get filtered residents for selected coach
    coach_residents <- reactive({
      req(app_data(), selected_coach())
      
      # Use helper function from globals.R
      residents <- get_coach_residents(
        app_data()$residents,
        selected_coach()
      )
      
      message(sprintf(
        "Coach '%s' has %d residents assigned",
        selected_coach(),
        nrow(residents)
      ))
      
      return(residents)
    })
    
    # Calculate coach statistics - SAFE VERSION
    coach_stats <- reactive({
      req(coach_residents())
      
      residents <- coach_residents()
      
      # Count by role (primary vs secondary)
      primary_count <- sum(residents$coach == selected_coach(), na.rm = TRUE)
      secondary_count <- sum(
        !is.na(residents$second_rev) & residents$second_rev == selected_coach(),
        na.rm = TRUE
      )
      
      # Count by level - SAFE with base R table()
      level_counts <- tryCatch({
        # Use whichever level field exists
        if ("Level" %in% names(residents) && any(!is.na(residents$Level))) {
          tbl <- table(residents$Level, useNA = "no")
          data.frame(
            level = names(tbl),
            n = as.integer(tbl),
            stringsAsFactors = FALSE
          )
        } else if ("current_level" %in% names(residents) && any(!is.na(residents$current_level))) {
          tbl <- table(residents$current_level, useNA = "no")
          data.frame(
            level = names(tbl),
            n = as.integer(tbl),
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            level = "Unknown",
            n = nrow(residents),
            stringsAsFactors = FALSE
          )
        }
      }, error = function(e) {
        message("ERROR counting by level: ", e$message)
        data.frame(
          level = "Unknown",
          n = nrow(residents),
          stringsAsFactors = FALSE
        )
      })
      
      list(
        total = nrow(residents),
        primary = primary_count,
        secondary = secondary_count,
        by_level = level_counts
      )
    })
    
    # Render coach information panel - SAFE VERSION
    output$coach_info <- renderUI({
      message("DEBUG: coach_info renderUI called")

      req(selected_coach(), coach_stats())

      message("DEBUG: coach_info - getting stats...")
      stats <- coach_stats()
      message("DEBUG: coach_info - stats retrieved")
      
      # Build level summary SAFELY - avoid apply()
      level_summary <- tryCatch({
        if (nrow(stats$by_level) > 0) {
          # Use simple for loop instead of apply()
          level_parts <- character(nrow(stats$by_level))
          for (i in 1:nrow(stats$by_level)) {
            level_parts[i] <- paste0(
              stats$by_level$level[i], 
              " (", 
              stats$by_level$n[i], 
              ")"
            )
          }
          paste(level_parts, collapse = " • ")
        } else {
          "No level data"
        }
      }, error = function(e) {
        message("ERROR building level summary: ", e$message)
        "Error displaying levels"
      })
      
      tagList(
        div(
          class = "coach-select-info",
          div(
            class = "coach-stats",
            div(
              class = "coach-stat",
              span(class = "coach-stat-label", "Total Residents:"),
              span(stats$total)
            ),
            div(
              class = "coach-stat",
              span(class = "coach-stat-label", "Primary:"),
              span(stats$primary)
            ),
            div(
              class = "coach-stat",
              span(class = "coach-stat-label", "Secondary:"),
              span(stats$secondary)
            )
          ),
          div(
            style = "margin-top: 10px;",
            strong("By Level: "),
            level_summary
          )
        )
      )
    })
    
    # Refresh button (optional - for manual data reload)
    output$refresh_btn <- renderUI({
      req(selected_coach())
      
      actionButton(
        ns("refresh_data"),
        label = NULL,
        icon = icon("sync-alt"),
        class = "btn-sm btn-outline-secondary",
        title = "Refresh resident data",
        style = "padding: 5px 10px;"
      )
    })
    
    # Handle refresh button
    observeEvent(input$refresh_data, {
      showNotification(
        "Refreshing data...",
        type = "message",
        duration = 2
      )
      
      # Trigger data reload in main app
      # (This would be handled by main server, not here)
      message(sprintf(
        "[%s] Data refresh requested by %s",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        selected_coach()
      ))
    })
    
    # Return reactive values
    return(
      reactive({
        message("DEBUG: coach_data return reactive called")

        # Only return valid data if coach is selected
        coach <- selected_coach()
        message("DEBUG: selected_coach = ", if(is.null(coach)) "NULL" else coach)

        if (is.null(coach) || !nzchar(coach)) {
          message("DEBUG: No coach selected, returning empty structure")
          return(list(
            coach_name = NULL,
            residents = data.frame(),
            stats = list(total = 0, primary = 0, secondary = 0, by_level = data.frame())
          ))
        }

        message("DEBUG: Getting residents for coach: ", coach)

        # Safely get residents and stats - handle both errors AND NULL returns from req()
        residents <- tryCatch({
          message("DEBUG: Calling coach_residents()...")
          res <- coach_residents()
          message("DEBUG: coach_residents() returned ", if(is.null(res)) "NULL" else nrow(res), " rows")
          if (is.null(res)) data.frame() else res
        }, error = function(e) {
          message("ERROR getting coach residents: ", e$message)
          print(e)
          data.frame()
        })

        message("DEBUG: Getting stats...")
        stats <- tryCatch({
          message("DEBUG: Calling coach_stats()...")
          st <- coach_stats()
          message("DEBUG: coach_stats() returned")
          if (is.null(st)) {
            list(total = 0, primary = 0, secondary = 0, by_level = data.frame())
          } else {
            st
          }
        }, error = function(e) {
          message("ERROR getting coach stats: ", e$message)
          print(e)
          list(total = 0, primary = 0, secondary = 0, by_level = data.frame())
        })

        message("DEBUG: Returning coach_data with coach_name = ", coach)
        list(
          coach_name = coach,
          residents = residents,
          stats = stats
        )
      })
    )
  })
}

#' Get Coach Email
#'
#' Helper function to get coach email from resident data
#' 
#' @param residents_df Resident data frame
#' @param coach_name Coach name
#' @return Email address or NULL
#' @export
get_coach_email <- function(residents_df, coach_name) {
  email <- residents_df %>%
    filter(coach == coach_name) %>%
    pull(coach_email) %>%
    first()
  
  if (length(email) == 0 || is.na(email)) {
    return(NULL)
  }
  
  return(email)
}