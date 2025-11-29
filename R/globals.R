# ==============================================================================
# IMSLU COACHING DASHBOARD - GLOBAL CONFIGURATION
# R/globals.R
# ==============================================================================
#
# This file sets up:
# - Package loading
# - Environment configuration (SSL, options)
# - REDCap tokens and API configuration
# - Data loading function
# - Period definitions
# - Helper functions for filtering and data access
#
# ==============================================================================

# ==============================================================================
# SHINY CONFIGURATION
# ==============================================================================
options(shiny.launch.browser = TRUE)
options(spinner.color = "#0072B2")  # SLU blue

# ==============================================================================
# LOAD REQUIRED PACKAGES
# ==============================================================================
library(shiny)
library(shinydashboard)
library(bslib)  # For accordion and modern components
library(shinyjs)
library(DT)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(plotly)
library(ggplot2)
library(httr)

# Load gmed package (from GitHub: fbuckhold3/gmed)
if (!require(gmed)) {
  stop("gmed package not installed. Install with: remotes::install_github('fbuckhold3/gmed')")
}


# ==============================================================================
# SSL CONFIGURATION FOR REDCAP
# ==============================================================================
# Disable SSL verification for institutional REDCap (certificate issues)
httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))

# ==============================================================================
# API CONFIGURATION
# ==============================================================================
# Load tokens from .Renviron file
# Create .Renviron in project root with:
#   RDM_TOKEN=your_rdm_token_here
#   ACCESS_CODE=your_access_code_here

# REDCap API configuration
REDCAP_CONFIG <- list(
  url = "https://redcapsurvey.slu.edu/api/",
  rdm_token = Sys.getenv("RDM_TOKEN"),
  timeout = 300  # 5 minutes for large data pulls
)

# Validate required tokens
if (!nzchar(REDCAP_CONFIG$rdm_token)) {
  stop("RDM_TOKEN not found. Please set in .Renviron or config.yml")
}

# Authentication configuration
AUTH_CONFIG <- list(
  access_code = Sys.getenv("ACCESS_CODE", unset = ""),
  timeout_minutes = 120  # Auto-logout after 2 hours of inactivity
)

# ==============================================================================
# PERIOD DEFINITIONS
# ==============================================================================
# Based on RDM 2.0 structure and self-assessment app pattern

# Period names (in chronological order)
PERIOD_NAMES <- c(
  "Entering Residency",  # Period 0 (July orientation)
  "Mid Intern",          # Period 1 (December)
  "End Intern",          # Period 2 (June)
  "Mid PGY2",            # Period 3 (December)
  "End PGY2",            # Period 4 (June)
  "Mid PGY3",            # Period 5 (December)
  "Graduating"           # Period 6 (April/May)
)

# Period number mapping (for REDCap instances)
PERIOD_TO_NUMBER <- setNames(0:6, PERIOD_NAMES)

# Reverse mapping
NUMBER_TO_PERIOD <- setNames(PERIOD_NAMES, 0:6)

# Previous period lookup
PREVIOUS_PERIOD <- c(
  "Entering Residency" = NA,
  "Mid Intern" = "Entering Residency",
  "End Intern" = "Mid Intern",
  "Mid PGY2" = "End Intern",
  "End PGY2" = "Mid PGY2",
  "Mid PGY3" = "End PGY2",
  "Graduating" = "Mid PGY3"
)

# ==============================================================================
# PERIOD DATE CALCULATION
# ==============================================================================

#' Get Current Active Period
#' 
#' Determines which coaching period we're currently in based on the date
#' Academic year runs July 1 - June 30
#' 
#' @param current_date Date to check (defaults to today)
#' @return Character string of current period name
#' @export
get_current_period <- function(current_date = Sys.Date()) {
  # Determine academic year boundaries
  if (format(current_date, "%m-%d") >= "07-01") {
    academic_year_start <- as.Date(paste0(format(current_date, "%Y"), "-07-01"))
    academic_year_end <- as.Date(paste0(as.numeric(format(current_date, "%Y")) + 1, "-06-30"))
  } else {
    academic_year_start <- as.Date(paste0(as.numeric(format(current_date, "%Y")) - 1, "-07-01"))
    academic_year_end <- as.Date(paste0(format(current_date, "%Y"), "-06-30"))
  }
  
  # Define period boundaries
  # Entering Residency: July 1 - mid-September (first ~75 days)
  entering_end <- academic_year_start + 75
  
  # Mid Intern: mid-September - January 31
  mid_intern_end <- as.Date(paste0(format(academic_year_start, "%Y"), "-12-31"))
  
  # End Intern: February 1 - June 30 (year 1)
  # Mid PGY2: July 1 - December 31 (year 2)
  # End PGY2: January 1 - June 30 (year 2)
  # Mid PGY3: July 1 - December 31 (year 3)
  # Graduating: January 1 - June 30 (year 3)
  
  # Since coaching reviews are done by PGY level, return the general period
  # The level-specific filtering will happen in the data loading
  
  if (current_date >= academic_year_start && current_date <= entering_end) {
    return("Entering Residency")
  } else if (current_date > entering_end && current_date <= mid_intern_end) {
    # For interns: Mid Intern; for others: mid-year review
    return("Mid Year")  # Will be refined by level
  } else {
    # January - June: end of year reviews
    return("End Year")  # Will be refined by level
  }
}

#' Get Period Number from Name
#' 
#' @param period_name Character string of period name
#' @return Integer period number (0-6) or NA if invalid
#' @export
get_period_number <- function(period_name) {
  if (is.na(period_name) || is.null(period_name)) return(NA_integer_)
  PERIOD_TO_NUMBER[period_name]
}

#' Get Period Name from Number
#' 
#' @param period_number Integer period number (0-6)
#' @return Character string of period name or NA if invalid
#' @export
get_period_name <- function(period_number) {
  if (is.na(period_number) || is.null(period_number)) return(NA_character_)
  NUMBER_TO_PERIOD[as.character(period_number)]
}

#' Get Previous Period Name
#' 
#' @param current_period Character string of current period name
#' @return Character string of previous period name, or NA if none
#' @export
get_previous_period <- function(current_period) {
  if (is.na(current_period) || is.null(current_period)) return(NA_character_)
  prev <- PREVIOUS_PERIOD[current_period]
  if (is.null(prev)) return(NA_character_)
  prev
}

# ==============================================================================
# DATA LOADING FUNCTION
# ==============================================================================

#' Load RDM Data for Coaching Dashboard
#' 
#' Loads all necessary data from REDCap using gmed::load_rdm_complete()
#' Includes filtering for non-archived residents and coach assignments
#' 
#' @param redcap_url REDCap API URL
#' @param rdm_token RDM REDCap token
#' @param include_archived Logical, whether to include archived residents
#' @return List with components: residents, assessment_data, all_forms, 
#'         historical_medians, milestone_data, data_dict
#' @export
load_coaching_data <- function(
  redcap_url = REDCAP_CONFIG$url,
  rdm_token = REDCAP_CONFIG$rdm_token,
  include_archived = FALSE
) {
  
  message(sprintf(
    "[%s] Loading RDM data for coaching dashboard...",
    format(Sys.time(), "%H:%M:%S")
  ))
  message("  -> Step 1/4: Connecting to REDCap API...")
  
  # Use gmed's data loading function
  # NOTE: Using "label" for readable names. Checkbox fields will need special parsing.
  # For checkbox fields, we'll need to query raw values separately when needed.
  rdm_data <- gmed::load_rdm_complete(
    redcap_url = redcap_url,
    rdm_token = rdm_token,
    raw_or_label = "label"  # Use labels for readable display
  )
  
  message("  -> Step 2/4: Processing resident data...")
  
  # Filter archived residents if requested
  if (!include_archived) {
    archived_records <- rdm_data$residents %>%
      filter(!is.na(res_archive) & res_archive == "1") %>%
      pull(record_id)
    
    if (length(archived_records) > 0) {
      message("Filtering out ", length(archived_records), " archived residents")
      
      # Filter from all data frames
      rdm_data$residents <- rdm_data$residents %>%
        filter(!(record_id %in% archived_records))
      
      rdm_data$assessment_data <- rdm_data$assessment_data %>%
        filter(!(record_id %in% archived_records))
      
      # Filter from all_forms
      for (form_name in names(rdm_data$all_forms)) {
        if ("record_id" %in% names(rdm_data$all_forms[[form_name]])) {
          rdm_data$all_forms[[form_name]] <- rdm_data$all_forms[[form_name]] %>%
            filter(!(record_id %in% archived_records))
        }
      }
      
      if (!is.null(rdm_data$milestone_data)) {
        rdm_data$milestone_data <- rdm_data$milestone_data %>%
          filter(!(record_id %in% archived_records))
      }
    }
  }
  
  message("  -> Step 3/4: Adding convenience fields...")
  
  # Add convenience fields for coaching
  rdm_data$residents <- rdm_data$residents %>%
    mutate(
      # Full name for display
      full_name = if_else(
        !is.na(first_name) & !is.na(last_name),
        paste(first_name, last_name),
        name
      ),
      # Current level (should already be in data from gmed)
      current_level = case_when(
        grepl("PGY-?1|Intern", Level, ignore.case = TRUE) ~ "Intern",
        grepl("PGY-?2", Level, ignore.case = TRUE) ~ "PGY2",
        grepl("PGY-?3", Level, ignore.case = TRUE) ~ "PGY3",
        TRUE ~ Level
      )
    )
  
  message("  -> Step 4/4: Finalizing...")
  message(sprintf(
    "[%s] Data loading complete! %d active residents found.",
    format(Sys.time(), "%H:%M:%S"),
    nrow(rdm_data$residents)
  ))
  
  return(rdm_data)
}

# ==============================================================================
# COACH FILTERING FUNCTIONS
# ==============================================================================

#' Get Residents for Coach
#' 
#' Filters resident list to those assigned to a specific coach
#' (either as primary coach or secondary reviewer)
#' 
#' @param residents_df Resident data frame from rdm_data$residents
#' @param coach_name Name of coach (matches coach or second_rev field)
#' @return Filtered resident data frame
#' @export
get_coach_residents <- function(residents_df, coach_name) {
  residents_df %>%
    filter(
      coach == coach_name | second_rev == coach_name
    )
}

#' Check Review Role for Resident
#' 
#' Determines if coach is primary or secondary reviewer
#' 
#' @param resident_row Single row from residents data frame
#' @param coach_name Name of coach
#' @return Character: "primary", "secondary", or "none"
#' @export
get_review_role <- function(resident_row, coach_name) {
  if (resident_row$coach == coach_name) {
    return("primary")
  } else if (!is.na(resident_row$second_rev) && resident_row$second_rev == coach_name) {
    return("secondary")
  } else {
    return("none")
  }
}

# ==============================================================================
# COMPLETION STATUS FUNCTIONS
# ==============================================================================

#' Check S_Eval Completion Status
#' 
#' Checks if resident has completed self-evaluation for a period
#' 
#' @param all_forms List of all form data
#' @param record_id Resident record ID
#' @param period_number Period number (0-6) or instance number
#' @return Logical TRUE if complete
#' @export
check_seval_complete <- function(all_forms, record_id, period_number) {
  if (is.null(all_forms$s_eval)) return(FALSE)
  
  s_eval_data <- all_forms$s_eval %>%
    filter(
      record_id == !!record_id,
      redcap_repeat_instance == !!period_number
    )
  
  if (nrow(s_eval_data) == 0) return(FALSE)
  
  # Check completion status field or presence of key data
  if ("s_eval_complete" %in% names(s_eval_data)) {
    return(s_eval_data$s_eval_complete[1] == "2")
  }
  
  # Fallback: check if key fields have data
  key_fields <- c("s_e_well", "s_e_plus", "s_e_delta")
  has_data <- any(!is.na(s_eval_data[key_fields]) & s_eval_data[key_fields] != "")
  
  return(has_data)
}

#' Check Coach Review Completion Status
#' 
#' Checks if coach review has been completed for a period
#' 
#' @param all_forms List of all form data
#' @param record_id Resident record ID
#' @param period_number Period number (0-6) or instance number
#' @return Logical TRUE if complete
#' @export
check_coach_review_complete <- function(all_forms, record_id, period_number) {
  if (is.null(all_forms$coach_rev)) return(FALSE)
  
  coach_data <- all_forms$coach_rev %>%
    filter(
      record_id == !!record_id,
      redcap_repeat_instance == !!period_number
    )
  
  if (nrow(coach_data) == 0) return(FALSE)
  
  # Check completion field
  if ("coach_rev_complete" %in% names(coach_data)) {
    return(coach_data$coach_rev_complete[1] == "2")
  }
  
  # Fallback: check if ILP final is filled (last required field)
  return(!is.na(coach_data$coach_ilp_final[1]) && 
         nzchar(coach_data$coach_ilp_final[1]))
}

#' Check Second Review Completion Status
#' 
#' Checks if second review has been completed for a period
#' 
#' @param all_forms List of all form data
#' @param record_id Resident record ID
#' @param period_number Period number (0-6) or instance number
#' @return Logical TRUE if complete
#' @export
check_second_review_complete <- function(all_forms, record_id, period_number) {
  if (is.null(all_forms$second_review)) return(FALSE)
  
  second_data <- all_forms$second_review %>%
    filter(
      record_id == !!record_id,
      redcap_repeat_instance == !!period_number
    )
  
  if (nrow(second_data) == 0) return(FALSE)
  
  # Check completion field
  if ("second_review_complete" %in% names(second_data)) {
    return(second_data$second_review_complete[1] == "2")
  }
  
  # Fallback: check if comments are present
  return(!is.na(second_data$second_comments[1]) && 
         nzchar(second_data$second_comments[1]))
}

get_resident_period_data <- function(rdm_data, record_id, current_period, include_previous = TRUE) {
  
  # Automatically unwrap ALL reactive values
  data <- rdm_data
  while (is.function(data)) {
    data <- data()
  }
  
  # Unwrap record_id if it's reactive
  rid <- record_id
  while (is.function(rid)) {
    rid <- rid()
  }
  
  # Unwrap current_period if it's reactive
  period <- current_period
  while (is.function(period)) {
    period <- period()
  }
  
  # Convert period name to number if needed
  if (is.character(period)) {
    period <- get_period_number(period)
  }
  
  # Get resident info
  resident_info <- data$residents %>%
    filter(record_id == !!rid) %>%
    slice(1)
  
  # Get current period data from all_forms
  current_s_eval <- if (!is.null(data$all_forms$s_eval)) {
    data$all_forms$s_eval %>%
      filter(record_id == !!rid, redcap_repeat_instrument == "s_eval") %>%
      filter(!is.na(s_e_period), s_e_period == !!period)
  } else {
    data.frame()
  }
  
  current_coach_rev <- if (!is.null(data$all_forms$coach_rev)) {
    data$all_forms$coach_rev %>%
      filter(record_id == !!rid, redcap_repeat_instrument == "coach_rev") %>%
      filter(!is.na(coach_period), coach_period == !!period)
  } else {
    data.frame()
  }
  
  current_ilp <- if (!is.null(data$all_forms$ilp)) {
    data$all_forms$ilp %>%
      filter(record_id == !!rid, redcap_repeat_instrument == "ilp") %>%
      filter(!is.na(ilp_period), ilp_period == !!period)
  } else {
    data.frame()
  }
  
  current_milestone <- if (!is.null(data$all_forms$milestone_entry)) {
    data$all_forms$milestone_entry %>%
      filter(record_id == !!rid, redcap_repeat_instrument == "milestone_entry") %>%
      filter(!is.na(prog_mile_period), prog_mile_period == !!period)
  } else {
    data.frame()
  }
  
  result <- list(
    resident_info = resident_info,
    current_period = list(
      s_eval = current_s_eval,
      coach_rev = current_coach_rev,
      ilp = current_ilp,
      milestone_entry = current_milestone
    )
  )
  
  # Add previous period if requested
  if (include_previous && period > 0) {
    prev_period <- period - 1
    
    prev_s_eval <- if (!is.null(data$all_forms$s_eval)) {
      data$all_forms$s_eval %>%
        filter(record_id == !!rid, redcap_repeat_instrument == "s_eval") %>%
        filter(!is.na(s_e_period), s_e_period == !!prev_period)
    } else {
      data.frame()
    }
    
    prev_coach_rev <- if (!is.null(data$all_forms$coach_rev)) {
      data$all_forms$coach_rev %>%
        filter(record_id == !!rid, redcap_repeat_instrument == "coach_rev") %>%
        filter(!is.na(coach_period), coach_period == !!prev_period)
    } else {
      data.frame()
    }
    
    result$previous_period <- list(
      s_eval = prev_s_eval,
      coach_rev = prev_coach_rev
    )
  } else {
    result$previous_period <- NULL
  }
  
  return(result)
}

# ==============================================================================
# UI HELPER FUNCTIONS
# ==============================================================================

#' Create Completion Indicator Icon
#' 
#' Returns HTML for completion status indicator
#' 
#' @param is_complete Logical
#' @return HTML tag for icon
#' @export
completion_icon <- function(is_complete) {
  if (is_complete) {
    tags$span(
      style = "color: #28a745; font-size: 1.2em;",
      title = "Completed",
      "\u2713"  # Check mark
    )
  } else {
    tags$span(
      style = "color: #dc3545; font-size: 1.2em;",
      title = "Not completed",
      "\u25CF"  # Bullet point
    )
  }
}

# ==============================================================================
# REDCAP SUBMISSION HELPERS
# ==============================================================================

#' Get REDCap Instance Number for Review
#' 
#' Wrapper around gmed function with coaching-specific logic
#' 
#' @param level Resident level (Intern, PGY2, PGY3)
#' @param period_name Period name
#' @param review_type Type of review ("scheduled" or "ad_hoc")
#' @return Integer instance number
#' @export
get_review_instance <- function(level, period_name, review_type = "scheduled") {
  # For scheduled reviews, instance = period number
  if (review_type == "scheduled") {
    return(get_period_number(period_name))
  }
  
  # For ad hoc reviews, use gmed function (if available)
  if (exists("get_redcap_instance", where = "package:gmed")) {
    return(gmed::get_redcap_instance(level, period_name, review_type))
  }
  
  # Fallback: use period number
  return(get_period_number(period_name))
}

# ==============================================================================
# SOURCE MODULE FILES
# ==============================================================================
# Will be added as modules are created

source("R/modules/mod_login.R")
source("R/modules/mod_coach_select.R")
source("R/modules/mod_resident_table.R")
source("R/modules/mod_wellness.R")
source("R/modules/mod_review_interface.R")

# ==============================================================================
# STARTUP MESSAGE
# ==============================================================================
message("=====================================================")
message("IMSLU Coaching Dashboard - RDM 2.0")
message("=====================================================")
message("REDCap URL: ", REDCAP_CONFIG$url)
message("Token configured: ", nzchar(REDCAP_CONFIG$rdm_token))
message("Current period: ", get_current_period())
message("=====================================================")