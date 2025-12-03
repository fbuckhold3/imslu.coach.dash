#' Milestone Entry Wrapper Module UI
#'
#' @param id Module namespace ID
#' @export
mod_milestone_entry_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h4("Milestone Ratings"),
    shiny::p("Rate the resident's milestone achievement for each competency."),

    # Use gmed's milestone rating interface
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

#' Milestone Entry Wrapper Module Server
#'
#' @param id Module namespace ID
#' @param rdm_data Reactive containing all RDM data
#' @param record_id Reactive value for resident record ID
#' @param current_period Reactive value for current evaluation period (name)
#' @param data_dict Reactive containing data dictionary
#' @export
mod_milestone_entry_server <- function(id, rdm_data, record_id, current_period, data_dict) {
  shiny::moduleServer(id, function(input, output, session) {

    # Initialize milestone rating module
    milestone_scores <- gmed::mod_miles_rating_server("rating", period = current_period)

    # Return reactive with milestone data
    return(
      reactive({
        list(
          scores = milestone_scores$scores(),
          descriptions = milestone_scores$desc(),
          done = milestone_scores$done(),
          is_complete = length(milestone_scores$scores()) > 0
        )
      })
    )
  })
}
