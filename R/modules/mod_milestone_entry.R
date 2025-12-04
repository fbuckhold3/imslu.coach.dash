#' Milestone Entry Wrapper Module UI (Coach Version)
#'
#' @param id Module namespace ID
#' @export
mod_milestone_entry_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h4("Coach Milestone Ratings"),

    # Collapsible section for previous milestones
    bslib::accordion(
      id = ns("milestone_accordion"),
      open = TRUE,  # Start expanded to show visualizations
      bslib::accordion_panel(
        "View Previous Milestone Assessments",
        shiny::p("Review resident's previous self-assessment and ACGME ratings for comparison."),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::h5("Previous Self-Assessment"),
            plotly::plotlyOutput(ns("prev_self_plot"), height = "400px")
          ),
          shiny::column(
            6,
            shiny::h5("Previous ACGME Assessment"),
            plotly::plotlyOutput(ns("prev_acgme_plot"), height = "400px")
          )
        )
      )
    ),

    shiny::hr(),

    # Entry section - the image-based milestone rating interface
    shiny::h4("Enter Coach Milestone Ratings"),
    shiny::p(
      "Use the interface below to rate the resident's milestone levels. ",
      "Click through each milestone image and select your rating (1-9)."
    ),

    # Hide the submit button from gmed module using CSS and JavaScript
    shiny::tags$style(shiny::HTML(paste0("
      /* Hide submit buttons in milestone rating module */
      #", ns("rating"), "-submit_button { display: none !important; }
      #", ns("rating"), " button[id*='submit'] { display: none !important; }
      #", ns("rating"), " .btn-success { display: none !important; }
      #", ns("rating"), " [type='submit'] { display: none !important; }
      button[id*='milestone'][id*='submit'] { display: none !important; }
      button:contains('Submit') { display: none !important; }
    "))),

    # Additional JavaScript to hide submit buttons after they load
    shiny::tags$script(shiny::HTML(paste0("
      $(document).ready(function() {
        // Wait a bit for the module to load, then hide submit buttons
        setTimeout(function() {
          $('#", ns("rating"), "').find('button').each(function() {
            var btnText = $(this).text().toLowerCase();
            if (btnText.includes('submit') || btnText.includes('done')) {
              $(this).hide();
              console.log('Hidden button:', btnText);
            }
          });

          // Also hide by class
          $('#", ns("rating"), " .btn-success').hide();
          $('#", ns("rating"), " button[type=\"submit\"]').hide();
        }, 500);

        // Check again after 2 seconds (in case of dynamic loading)
        setTimeout(function() {
          $('#", ns("rating"), "').find('button').each(function() {
            var btnText = $(this).text().toLowerCase();
            if (btnText.includes('submit') || btnText.includes('done')) {
              $(this).hide();
            }
          });
        }, 2000);
      });
    "))),

    gmed::mod_miles_rating_ui(ns("rating"))
  )
}

#' Helper: Convert period name to number
#' @param period_name Period name (e.g., "Mid Intern", "End PGY2")
#' @return Period number (1-7)
get_period_number_from_name <- function(period_name) {
  period_mapping <- c(
    "Mid Intern" = 1,
    "End Intern" = 2,
    "Mid PGY2" = 3,
    "End PGY2" = 4,
    "Mid PGY3" = 5,
    "End PGY3" = 6,
    "Graduating" = 6,
    "Entering Residency" = 7
  )

  as.numeric(period_mapping[period_name])
}

#' Milestone Entry Wrapper Module Server (Coach Version)
#'
#' @param id Module namespace ID
#' @param rdm_data Reactive containing all RDM data
#' @param record_id Reactive value for resident record ID
#' @param current_period Reactive value for current evaluation period (name)
#' @param data_dict Reactive containing data dictionary
#' @export
mod_milestone_entry_server <- function(id, rdm_data, record_id, current_period, data_dict) {
  shiny::moduleServer(id, function(input, output, session) {

    # Process milestone workflow when data loads
    milestone_results <- shiny::reactive({
      req(rdm_data())

      # Use the milestone_workflow that's already in rdm_data if it exists
      if (!is.null(rdm_data()$milestone_workflow)) {
        return(rdm_data()$milestone_workflow)
      }

      # Otherwise create it
      workflow <- tryCatch({
        gmed::create_milestone_workflow_from_dict(
          all_forms = rdm_data()$all_forms,
          data_dict = rdm_data()$data_dict,
          resident_data = rdm_data()$residents,
          verbose = FALSE
        )
      }, error = function(e) {
        message("Error creating milestone workflow: ", e$message)
        NULL
      })

      workflow
    })

    # Get previous period text
    previous_period <- shiny::reactive({
      req(current_period())

      # Get period number from period name
      period_num <- get_period_number_from_name(current_period())

      if (is.na(period_num)) return(NA)

      # Get previous period text
      period_mapping <- data.frame(
        num = 1:7,
        text = c("Mid Intern", "End Intern", "Mid PGY2", "End PGY2",
                 "Mid PGY3", "Graduating", "Entering Residency"),
        stringsAsFactors = FALSE
      )

      # Special case: period 1 (Mid Intern) looks back to period 7 (Entering Residency)
      if (period_num == 1) {
        prev_num <- 7
      } else {
        prev_num <- period_num - 1
      }

      prev_text <- period_mapping$text[period_mapping$num == prev_num]

      if (length(prev_text) == 0) return(NA)
      prev_text
    })

    # Get self milestone data
    self_milestone_data <- shiny::reactive({
      req(milestone_results())

      gmed::get_milestone_data(
        workflow_results = milestone_results(),
        milestone_type = "self",
        milestone_system = "rep"
      )
    })

    # Get ACGME milestone data
    acgme_milestone_data <- shiny::reactive({
      req(milestone_results())

      gmed::get_milestone_data(
        workflow_results = milestone_results(),
        milestone_type = "program",
        milestone_system = "acgme"
      )
    })

    # Render previous self plot (PLOTLY)
    output$prev_self_plot <- plotly::renderPlotly({
      req(self_milestone_data(), record_id())

      prev_period <- previous_period()

      if (is.na(prev_period)) {
        return(plotly::plotly_empty() %>%
                 plotly::add_annotations(
                   text = "No previous assessment period",
                   x = 0.5, y = 0.5, showarrow = FALSE,
                   font = list(size = 16, color = "gray")
                 ))
      }

      tryCatch({
        dashboard <- gmed::create_milestone_overview_dashboard(
          milestone_results = milestone_results(),
          resident_id = record_id(),
          period_text = prev_period,
          milestone_type = "self",
          milestone_system = "rep",
          resident_data = rdm_data()$residents
        )

        return(dashboard$spider_plot)

      }, error = function(e) {
        return(plotly::plotly_empty() %>%
                 plotly::add_annotations(
                   text = "No data available",
                   x = 0.5, y = 0.5, showarrow = FALSE,
                   font = list(size = 14, color = "orange")
                 ))
      })
    })

    # Render previous ACGME plot (PLOTLY)
    output$prev_acgme_plot <- plotly::renderPlotly({
      req(acgme_milestone_data(), record_id())

      prev_period <- previous_period()

      if (is.na(prev_period)) {
        return(plotly::plotly_empty() %>%
                 plotly::add_annotations(
                   text = "No previous assessment period",
                   x = 0.5, y = 0.5, showarrow = FALSE,
                   font = list(size = 16, color = "gray")
                 ))
      }

      tryCatch({
        dashboard <- gmed::create_milestone_overview_dashboard(
          milestone_results = milestone_results(),
          resident_id = record_id(),
          period_text = prev_period,
          milestone_type = "program",      # Program = CCC assessment
          milestone_system = "acgme",       # ACGME system
          resident_data = rdm_data()$residents
        )

        return(dashboard$spider_plot)

      }, error = function(e) {
        return(plotly::plotly_empty() %>%
                 plotly::add_annotations(
                   text = "No data available",
                   x = 0.5, y = 0.5, showarrow = FALSE,
                   font = list(size = 14, color = "orange")
                 ))
      })
    })

    # Initialize milestone rating module
    # Ensure period is never NA (gmed module can't handle NA in if statements)
    safe_period <- reactive({
      period_val <- current_period()
      if (is.null(period_val) || is.na(period_val) || period_val == "") {
        return("")
      }
      return(period_val)
    })

    milestone_scores <- gmed::mod_miles_rating_server("rating", period = safe_period)

    # Return reactive with milestone data (no auto-submission - parent handles it)
    return(
      reactive({
        list(
          scores = milestone_scores$scores(),
          descriptions = milestone_scores$desc(),
          done = milestone_scores$done(),
          is_complete = length(milestone_scores$scores()) > 0,
          milestone_results = milestone_results()
        )
      })
    )
  })
}
