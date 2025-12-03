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
# PERIOD DETECTION HELPER
# ==============================================================================

#' Get Most Recent Period with Data for a Resident (Simple Version)
#' 
#' Checks s_eval form for most recent period with data
#' 
#' @param resident_info Single row of resident data as list
#' @param app_data Full rdm_data object
#' @param verbose Logical, print debug info
#' @return Character period name or NULL
get_most_recent_period_simple <- function(resident_info, app_data, verbose = FALSE) {
  
  rid <- resident_info$record_id
  
  if (verbose) {
    cat(sprintf("\n[Period Detection] Checking resident %s\n", rid))
  }
  
  # Get all s_eval records for this resident
  if (is.null(app_data$all_forms$s_eval)) {
    if (verbose) cat("  No s_eval data available\n")
    return(NULL)
  }
  
  s_eval_data <- app_data$all_forms$s_eval %>%
    filter(record_id == rid, !is.na(s_e_period))
  
  if (nrow(s_eval_data) == 0) {
    if (verbose) cat(sprintf("  No s_eval records found for resident %s\n", rid))
    return(NULL)
  }
  
  # Get the most recent period by sorting periods
  periods <- unique(s_eval_data$s_e_period)
  
  if (verbose) {
    cat(sprintf("  Found periods: %s\n", paste(periods, collapse=", ")))
  }
  
  period_numbers <- sapply(periods, get_period_number)
  
  if (all(is.na(period_numbers))) {
    if (verbose) cat("  Could not convert periods to numbers\n")
    return(NULL)
  }
  
  # Return the period with highest number
  max_period_num <- max(period_numbers, na.rm = TRUE)
  max_period_name <- get_period_name(max_period_num)
  
  if (verbose) {
    cat(sprintf("  Most recent period: %s (number %d)\n", max_period_name, max_period_num))
  }
  
  return(max_period_name)
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
  # CRITICAL: Must use "raw" format to preserve numerical data and checkbox codes
  # Translation layers will be created in UI modules for display purposes
  rdm_data <- gmed::load_rdm_complete(
    redcap_url = redcap_url,
    rdm_token = rdm_token,
    raw_or_label = "raw"  # Use raw format (required for numeric fields and checkboxes)
  )
  
  message("  -> Step 2/4: Processing resident data...")

# Translate period fields from codes to labels
# Period fields in forms need to match the period names we use for filtering
if (!is.null(rdm_data$data_dict)) {
  message("  Translating period field codes to labels...")

  # Get period field choices from data dictionary
  period_field_candidates <- c("s_e_period", "coach_period", "second_period",
                               "year_resident", "prog_mile_period", "prog_mile_period_self",
                               "acgme_mile_period", "ccc_session")

  for (field in period_field_candidates) {
    period_choices <- rdm_data$data_dict %>%
      filter(field_name == !!field) %>%
      pull(select_choices_or_calculations)

    if (length(period_choices) > 0 && !is.na(period_choices[1])) {
      # Build translation map
      choice_pairs <- strsplit(period_choices[1], "\\|")[[1]]
      period_map <- list()
      for (pair in choice_pairs) {
        parts <- strsplit(trimws(pair), ",", fixed = TRUE)[[1]]
        if (length(parts) >= 2) {
          code <- trimws(parts[1])
          label <- trimws(paste(parts[-1], collapse = ","))
          period_map[[code]] <- label
        }
      }
      period_map <- unlist(period_map)

      # Translate in all forms that have this field
      for (form_name in names(rdm_data$all_forms)) {
        if (field %in% names(rdm_data$all_forms[[form_name]])) {
          rdm_data$all_forms[[form_name]] <- rdm_data$all_forms[[form_name]] %>%
            mutate(
              !!field := if_else(!is.na(.data[[field]]) & .data[[field]] %in% names(period_map),
                                period_map[.data[[field]]], .data[[field]])
            )
        }
      }
      message(sprintf("    Translated %s field", field))
    }
  }
  message("  Period field translation complete")
}
  
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

  
  # ==============================================================================
# SIMPLEST FIX for R/globals.R lines 254-270
# Just let gmed's Level field be used as-is
# ==============================================================================

message("  -> Step 3/4: Adding convenience fields...")

# Calculate Level using gmed's logic (which works correctly)
current_date <- Sys.Date()

message("  Calculating resident levels using gmed logic...")
message("  Current date: ", current_date)

# Check for required fields
if ("type" %in% names(rdm_data$residents) && "grad_yr" %in% names(rdm_data$residents)) {

  # Translate type and grad_yr from raw codes to actual values using data_dict
  if (!is.null(rdm_data$data_dict)) {
    message("  Translating type and grad_yr codes...")

    # Get type field choices
    type_choices <- rdm_data$data_dict %>%
      filter(field_name == "type") %>%
      pull(select_choices_or_calculations)

    if (length(type_choices) > 0 && !is.na(type_choices[1])) {
      choice_pairs <- strsplit(type_choices[1], "\\|")[[1]]
      type_map <- list()
      for (pair in choice_pairs) {
        parts <- strsplit(trimws(pair), ",", fixed = TRUE)[[1]]
        if (length(parts) >= 2) {
          code <- trimws(parts[1])
          label <- trimws(paste(parts[-1], collapse = ","))
          type_map[[code]] <- label
        }
      }
      type_map <- unlist(type_map)

      rdm_data$residents <- rdm_data$residents %>%
        mutate(
          type = if_else(!is.na(type) & type %in% names(type_map),
                        type_map[type], type)
        )
      message("    Type field translated")
    }

    # Get grad_yr field choices
    grad_yr_choices <- rdm_data$data_dict %>%
      filter(field_name == "grad_yr") %>%
      pull(select_choices_or_calculations)

    if (length(grad_yr_choices) > 0 && !is.na(grad_yr_choices[1])) {
      choice_pairs <- strsplit(grad_yr_choices[1], "\\|")[[1]]
      grad_yr_map <- list()
      for (pair in choice_pairs) {
        parts <- strsplit(trimws(pair), ",", fixed = TRUE)[[1]]
        if (length(parts) >= 2) {
          code <- trimws(parts[1])
          year <- trimws(paste(parts[-1], collapse = ","))
          grad_yr_map[[code]] <- year
        }
      }
      grad_yr_map <- unlist(grad_yr_map)

      rdm_data$residents <- rdm_data$residents %>%
        mutate(
          grad_yr = if_else(!is.na(grad_yr) & grad_yr %in% names(grad_yr_map),
                           grad_yr_map[grad_yr], grad_yr)
        )
      message("    Grad_yr field translated")
    }
  }

  rdm_data$residents <- rdm_data$residents %>%
    mutate(
      # Calculate academic year for current date (July 1 start)
      academic_year = ifelse(
        format(current_date, "%m-%d") >= "07-01",
        as.numeric(format(current_date, "%Y")),
        as.numeric(format(current_date, "%Y")) - 1
      ),

      # Convert grad_yr to numeric
      grad_yr_numeric = suppressWarnings(as.numeric(grad_yr)),

      # Calculate level using gmed logic
      Level = case_when(
        # Missing data
        is.na(type) | is.na(grad_yr_numeric) ~ "Unknown",

        # Preliminary residents are always Intern
        tolower(type) == "preliminary" ~ "Intern",

        # Categorical residents - calculate based on years until graduation
        tolower(type) == "categorical" ~ {
          years_to_grad <- grad_yr_numeric - academic_year

          case_when(
            years_to_grad == 3 ~ "Intern",      # 3 years until graduation = PGY1
            years_to_grad == 2 ~ "PGY2",        # 2 years until graduation = PGY2
            years_to_grad == 1 ~ "PGY3",        # 1 year until graduation = PGY3
            years_to_grad <= 0 ~ "Graduated",   # Past graduation
            years_to_grad > 3 ~ "Pre-Intern",   # Before starting residency
            TRUE ~ "Unknown"
          )
        },

        # Rotators
        tolower(type) == "rotator" ~ "Rotator",

        TRUE ~ "Unknown"
      ),

      # Full name for display
      full_name = if_else(
        !is.na(first_name) & !is.na(last_name),
        paste(first_name, last_name),
        name
      )
    ) %>%
    select(-academic_year, -grad_yr_numeric)  # Remove temporary columns

  message("  Level calculation complete using type and grad_yr")

} else {
  warning("Required fields (type, grad_yr) not found - cannot calculate Level")
  message("  WARNING: Missing type or grad_yr field, setting all to Unknown")

  rdm_data$residents <- rdm_data$residents %>%
    mutate(
      Level = "Unknown",
      full_name = if_else(
        !is.na(first_name) & !is.na(last_name),
        paste(first_name, last_name),
        name
      )
    )
}

# Translate coach codes to names using data_dict
if (!is.null(rdm_data$data_dict)) {
  message("  Translating coach codes to names...")

  # Get coach field choices from data_dict (using API column names)
  coach_choices <- rdm_data$data_dict %>%
    filter(field_name == "coach") %>%
    pull(select_choices_or_calculations)

  if (length(coach_choices) > 0 && !is.na(coach_choices[1])) {
    # Parse choices (format: "1, Name 1 | 2, Name 2 | ...")
    choice_pairs <- strsplit(coach_choices[1], "\\|")[[1]]

    # Build map: code -> name (extract just the name without number)
    coach_map <- list()
    for (pair in choice_pairs) {
      parts <- strsplit(trimws(pair), ",", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        code <- trimws(parts[1])
        name <- trimws(paste(parts[-1], collapse = ","))
        coach_map[[code]] <- name
      }
    }
    coach_map <- unlist(coach_map)

    # Translate coach and second_rev fields
    rdm_data$residents <- rdm_data$residents %>%
      mutate(
        coach = if_else(!is.na(coach) & coach %in% names(coach_map),
                       coach_map[coach], coach),
        second_rev = if_else(!is.na(second_rev) & second_rev %in% names(coach_map),
                            coach_map[second_rev], second_rev)
      )

    message("  Coach translation complete")
  } else {
    message("  WARNING: Could not find coach field choices in data dictionary")
  }
}

# === AUTO-DETECT PERIOD FOR EACH RESIDENT ===
message("  -> Detecting current period for each resident...")

rdm_data$residents <- rdm_data$residents %>%
  rowwise() %>%
  mutate(
    current_period = {
      # Auto-detect most recent period with data
      resident_info_list <- as.list(pick(everything()))

      tryCatch({
        detected <- get_most_recent_period_simple(
          resident_info = resident_info_list,
          app_data = rdm_data,
          verbose = FALSE  # Set to TRUE for debugging
        )

        if (!is.null(detected) && !is.na(detected)) {
          detected
        } else {
          "Mid PGY3"  # Fallback
        }
      }, error = function(e) {
        "Mid PGY3"  # Fallback on error
      })
    },
    current_period_num = get_period_number(current_period)
  ) %>%
  ungroup()

message(sprintf(
  "  Period detection complete. Sample: %s",
  paste(head(rdm_data$residents$current_period, 3), collapse=", ")
))

# Debug: Show Level calculation results
message("  Level calculation debug (first 5 residents):")
debug_sample <- rdm_data$residents %>%
  select(name, any_of(c("type", "grad_yr")), Level) %>%
  head(5)

for (i in 1:min(nrow(debug_sample), 5)) {
  type_val <- if("type" %in% names(debug_sample)) debug_sample$type[i] else "N/A"
  grad_val <- if("grad_yr" %in% names(debug_sample)) debug_sample$grad_yr[i] else "N/A"
  message(sprintf("    %s: type=%s, grad_yr=%s → Level=%s",
                 debug_sample$name[i],
                 type_val,
                 grad_val,
                 debug_sample$Level[i]))
}
  
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

#' Get Form Data for Resident and Period
#' 
#' Handles different period field names and logic for each form
#' 
#' @param all_forms List of all form data
#' @param form_name Name of the form
#' @param record_id Resident record ID
#' @param period_name Period name (e.g., "Mid PGY3")
#' @return Filtered data frame
get_form_data_for_period <- function(all_forms, form_name, record_id, period_name, debug = FALSE) {

  # Return empty if form doesn't exist
  if (is.null(all_forms[[form_name]])) {
    if (debug) message("DEBUG: Form ", form_name, " not found in all_forms")
    return(data.frame())
  }

  # Get base data for this resident
  form_data <- all_forms[[form_name]] %>%
    filter(record_id == !!record_id)

  if (debug) {
    message(sprintf("DEBUG: get_form_data_for_period(form=%s, record_id=%s, period=%s)",
                   form_name, record_id, period_name))
    message("  Total rows for resident: ", nrow(form_data))
    if (nrow(form_data) > 0) {
      if ("redcap_repeat_instrument" %in% names(form_data)) {
        message("  Instruments: ", paste(unique(form_data$redcap_repeat_instrument), collapse=", "))
      }
      if (form_name == "s_eval" && "s_e_period" %in% names(form_data)) {
        message("  Periods in data: ", paste(unique(form_data$s_e_period), collapse=", "))
      }
    }
  }
  
  # Form-specific period filtering
  filtered_data <- switch(
    form_name,
    
    # S Eval uses s_e_period
    # NOTE: Using raw instrument name "s_eval" not label "S Eval"
    "s_eval" = {
      if ("s_e_period" %in% names(form_data)) {
        form_data %>%
          filter(redcap_repeat_instrument == "s_eval") %>%
          filter(!is.na(s_e_period), s_e_period == !!period_name)
      } else {
        form_data %>%
          filter(redcap_repeat_instrument == "s_eval")
      }
    },

    # Coach Rev uses coach_period
    # NOTE: Using raw instrument name "coach_rev" not label "Coach Rev"
    "coach_rev" = {
      if ("coach_period" %in% names(form_data)) {
        form_data %>%
          filter(redcap_repeat_instrument == "coach_rev") %>%
          filter(!is.na(coach_period), coach_period == !!period_name)
      } else {
        form_data %>%
          filter(redcap_repeat_instrument == "coach_rev")
      }
    },

    # Second Review uses second_period
    # NOTE: Using raw instrument name "second_review" not label "Second Review"
    "second_review" = {
      if ("second_period" %in% names(form_data)) {
        form_data %>%
          filter(redcap_repeat_instrument == "second_review") %>%
          filter(!is.na(second_period), second_period == !!period_name)
      } else {
        form_data %>%
          filter(redcap_repeat_instrument == "second_review")
      }
    },

    # ILP uses year_resident
    # NOTE: Using raw instrument name "ilp" not label "ILP"
    "ilp" = {
      if ("year_resident" %in% names(form_data)) {
        form_data %>%
          filter(redcap_repeat_instrument == "ilp") %>%
          filter(!is.na(year_resident), year_resident == !!period_name)
      } else {
        form_data %>%
          filter(redcap_repeat_instrument == "ilp")
      }
    },

    # Milestone Entry uses prog_mile_period
    # NOTE: Using raw instrument name "milestone_entry" not label "Milestone Entry"
    "milestone_entry" = {
      if ("prog_mile_period" %in% names(form_data)) {
        form_data %>%
          filter(redcap_repeat_instrument == "milestone_entry") %>%
          filter(!is.na(prog_mile_period), prog_mile_period == !!period_name)
      } else {
        form_data %>%
          filter(redcap_repeat_instrument == "milestone_entry")
      }
    },

    # Milestone Self-Evaluation uses prog_mile_period_self
    # NOTE: Using raw instrument name "milestone_selfevaluation_c33c" not label
    "milestone_selfevaluation_c33c" = {
      if ("prog_mile_period_self" %in% names(form_data)) {
        form_data %>%
          filter(redcap_repeat_instrument == "milestone_selfevaluation_c33c") %>%
          filter(!is.na(prog_mile_period_self), prog_mile_period_self == !!period_name)
      } else {
        form_data %>%
          filter(redcap_repeat_instrument == "milestone_selfevaluation_c33c")
      }
    },
    
    # ACGME Miles uses acgme_mile_period
    # NOTE: Using raw instrument name "acgme_miles" not label "ACGME Miles"
    "acgme_miles" = {
      if ("acgme_mile_period" %in% names(form_data)) {
        form_data %>%
          filter(redcap_repeat_instrument == "acgme_miles") %>%
          filter(!is.na(acgme_mile_period), acgme_mile_period == !!period_name)
      } else {
        form_data %>%
          filter(redcap_repeat_instrument == "acgme_miles")
      }
    },

    # CCC Review uses ccc_session
    # NOTE: Using raw instrument name "ccc_review" not label "CCC Review"
    "ccc_review" = {
      if ("ccc_session" %in% names(form_data)) {
        form_data %>%
          filter(redcap_repeat_instrument == "ccc_review") %>%
          filter(!is.na(ccc_session), ccc_session == !!period_name)
      } else {
        form_data %>%
          filter(redcap_repeat_instrument == "ccc_review")
      }
    },
    
    # Default: no period filtering, just return resident's data
    {
      form_data
    }
  )
  
  return(filtered_data)
}

get_resident_period_data <- function(rdm_data, record_id, current_period, include_previous = TRUE) {
  
  # Unwrap reactive values
  data <- rdm_data
  while (is.function(data)) data <- data()
  
  rid <- record_id
  while (is.function(rid)) rid <- rid()
  
  period <- current_period
  while (is.function(period)) period <- period()
  
  # Convert to period name
  period_name <- if (is.numeric(period)) {
    get_period_name(period)
  } else {
    period
  }
  
  # Get resident info
  resident_info <- data$residents %>%
    filter(record_id == !!rid) %>%
    slice(1)
  
  # Get current period data using helper
  result <- list(
    resident_info = resident_info,
    current_period = list(
      s_eval = get_form_data_for_period(data$all_forms, "s_eval", rid, period_name),
      coach_rev = get_form_data_for_period(data$all_forms, "coach_rev", rid, period_name),
      ilp = get_form_data_for_period(data$all_forms, "ilp", rid, period_name),
      milestone_entry = get_form_data_for_period(data$all_forms, "milestone_entry", rid, period_name),
      milestone_selfevaluation = get_form_data_for_period(data$all_forms, "milestone_selfevaluation_c33c", rid, period_name),
      second_review = get_form_data_for_period(data$all_forms, "second_review", rid, period_name)
    )
  )
  
  # Add previous period if requested
  if (include_previous && !is.na(period_name)) {
    prev_period_name <- get_previous_period(period_name)
    
    if (!is.na(prev_period_name)) {
      result$previous_period <- list(
        s_eval = get_form_data_for_period(data$all_forms, "s_eval", rid, prev_period_name),
        coach_rev = get_form_data_for_period(data$all_forms, "coach_rev", rid, prev_period_name),
        ilp = get_form_data_for_period(data$all_forms, "ilp", rid, prev_period_name),
        acgme_miles = get_form_data_for_period(data$all_forms, "acgme_miles", rid, prev_period_name),
        ccc_review = get_form_data_for_period(data$all_forms, "ccc_review", rid, prev_period_name)
      )
    }
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
source("R/modules/mod_evaluations.R")
source("R/modules/mod_learning.R")
source("R/modules/mod_scholarship.R")
source("R/modules/mod_career.R")

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