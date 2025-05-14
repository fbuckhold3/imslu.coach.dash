calculate_resident_level <- function(coach_data) {
  # Debug: Print class and structure of input
  message("Inside calculate_resident_level")
  message("Input class: ", paste(class(coach_data), collapse = ", "))
  
  if (!inherits(coach_data, "data.frame")) {
    warning("Input to calculate_resident_level must be a data frame or tibble. Returning as-is.")
    return(coach_data)
  }
  
  # Check if required columns exist
  has_type <- "type" %in% names(coach_data)
  has_grad_yr <- "grad_yr" %in% names(coach_data)
  
  if (!has_type || !has_grad_yr) {
    warning("Missing required columns (type or grad_yr) for level calculation. Using fallback method.")
    
    # Try to determine level from other columns
    if ("Level" %in% names(coach_data)) {
      message("Level column already exists, no calculation needed")
      return(coach_data)
    }
    
    # Add Level column with fallback logic
    coach_data$Level <- NA_character_
    
    # If year column exists, try to determine level from it
    if ("year" %in% names(coach_data)) {
      coach_data <- coach_data %>%
        mutate(
          Level = case_when(
            year == "PGY-1" ~ "Intern",
            year == "PGY-2" ~ "PGY2",
            year == "PGY-3" ~ "PGY3",
            TRUE ~ NA_character_
          )
        )
      message("Determined Level from 'year' column for some residents")
    }
    
    # For remaining rows without Level, assign "Intern" as default
    coach_data <- coach_data %>%
      mutate(
        Level = ifelse(is.na(Level), "Intern", Level)
      )
    
    message("Assigned default Level (Intern) for residents with missing data")
    return(coach_data)
  }
  
  # Standard calculation with type and grad_yr
  # Get today's date and current academic year
  current_date <- Sys.Date()
  current_academic_year <- ifelse(format(current_date, "%m-%d") >= "07-01", 
                                  as.numeric(format(current_date, "%Y")), 
                                  as.numeric(format(current_date, "%Y")) - 1)
  
  # Ensure grad_yr is numeric
  coach_data <- coach_data %>%
    mutate(
      grad_yr = suppressWarnings(as.numeric(grad_yr))
    )
  
  # Calculate Level based on type and grad_yr
  coach_data <- coach_data %>%
    mutate(
      Level = case_when(
        type == "Preliminary" ~ "Intern",  # Prelim residents are always Interns
        type == "Categorical" & grad_yr == current_academic_year + 3 ~ "Intern",  # PGY1 (Intern) 
        type == "Categorical" & grad_yr == current_academic_year + 2 ~ "PGY2",    # PGY2
        type == "Categorical" & grad_yr == current_academic_year + 1 ~ "PGY3",    # PGY3
        !is.na(type) & !is.na(grad_yr) ~ "Intern",  # Default to Intern if type and grad_yr are present but don't match criteria
        TRUE ~ NA_character_  # Keep NA if both type and grad_yr are missing
      )
    )
  
  # Now handle remaining NAs - default to Intern
  coach_data <- coach_data %>%
    mutate(
      Level = ifelse(is.na(Level), "Intern", Level)
    )
  
  message("Level calculation complete. Sample levels: ", 
          paste(head(table(coach_data$Level)), collapse = ", "))
  
  return(coach_data)
}


# ----------- PERIOD SELECTION
# App to automatically select period (may add to imres package, if done then delete this)

get_current_period <- function(current_date = Sys.Date()) {
  if (format(current_date, "%m-%d") >= "07-01") {
    academic_year_start <- as.Date(paste0(format(current_date, "%Y"), "-07-01"))
    academic_year_end <- as.Date(paste0(as.numeric(format(current_date, "%Y")) + 1, "-06-30"))
  } else {
    academic_year_start <- as.Date(paste0(as.numeric(format(current_date, "%Y")) - 1, "-07-01"))
    academic_year_end <- as.Date(paste0(format(current_date, "%Y"), "-06-30"))
  }
  
  intern_intro_end <- academic_year_start + 76
  mid_review_end <- as.Date(paste0(format(academic_year_start + 365, "%Y"), "-01-31"))
  
  if (current_date >= academic_year_start & current_date <= intern_intro_end) {
    return("Intern Intro")
  } else if (current_date >= intern_intro_end + 1 & current_date <= mid_review_end) {
    return("Mid Review")
  } else if (current_date > mid_review_end & current_date <= academic_year_end) {
    return("End Review")
  } else {
    return(NULL)
  }
}

#' Map Period and Level to Various Formats
#'
#' Maps the app's period and resident level to different formats used in the app
#'
#' @param level The resident level (Intern, PGY2, PGY3)
#' @param period The app period (Intern Intro, Mid Review, End Review)
#' @param return_type What format to return ("instance", "milestone", "readable")
#'
#' @return The mapped value in the requested format
#' @export
map_period_format <- function(level, period, return_type = "instance") {
  # Handle NA or missing values
  if (is.null(level) || is.na(level)) {
    level <- "Intern"  # Default to Intern if level is missing
    warning("Using default level (Intern) in map_period_format")
  }
  
  if (is.null(period) || is.na(period)) {
    if (return_type == "instance") return(8)  # Default to Interim
    if (return_type == "milestone" || return_type == "readable") return(NA)
  }
  
  # Define mappings
  mappings <- list(
    "Intern" = list(
      "Intern Intro" = list(
        instance = 7,
        milestone = "Intern Intro",
        readable = "Intern Intro"
      ),
      "Mid Review" = list(
        instance = 1,
        milestone = "Mid Intern",
        readable = "Mid Intern"
      ),
      "End Review" = list(
        instance = 2,
        milestone = "End Intern",
        readable = "End Intern"
      )
    ),
    "PGY2" = list(
      "Intern Intro" = list(  # This combination doesn't make logical sense, but included for completeness
        instance = 8,
        milestone = NA,
        readable = NA
      ),
      "Mid Review" = list(
        instance = 3,
        milestone = "Mid PGY2",
        readable = "Mid PGY2"
      ),
      "End Review" = list(
        instance = 4,
        milestone = "End PGY2",
        readable = "End PGY2"
      )
    ),
    "PGY3" = list(
      "Intern Intro" = list(  # This combination doesn't make logical sense, but included for completeness
        instance = 8,
        milestone = NA,
        readable = NA
      ),
      "Mid Review" = list(
        instance = 5,
        milestone = "Mid PGY3",
        readable = "Mid PGY3"
      ),
      "End Review" = list(
        instance = 6,
        milestone = "Graduation",
        readable = "Graduation"
      )
    )
  )
  
  # Look up the mapping
  if (level %in% names(mappings) && period %in% names(mappings[[level]])) {
    return(mappings[[level]][[period]][[return_type]])
  } else {
    # Default case
    warning(paste("No mapping found for level:", level, "and period:", period))
    if (return_type == "instance") return(8)  # Default to Interim
    return(NA)
  }
}

# Function to get the previous period based on current period and resident level
get_previous_period <- function(current_period, resident_level) {
  # First convert to a standard format - the "readable" format
  readable_period <- NULL
  
  # Handle different formats (app, ccc, or already readable)
  if (current_period %in% c("Intern Intro", "Mid Review", "End Review")) {
    # App format - convert to readable based on level
    if (current_period == "Intern Intro") {
      readable_period <- "Intern Intro"
    } else if (current_period == "Mid Review") {
      if (resident_level == "Intern") readable_period <- "Mid Intern"
      else if (resident_level == "PGY2") readable_period <- "Mid PGY2"
      else if (resident_level == "PGY3") readable_period <- "Mid PGY3"
    } else if (current_period == "End Review") {
      if (resident_level == "Intern") readable_period <- "End Intern"
      else if (resident_level == "PGY2") readable_period <- "End PGY2"
      else if (resident_level == "PGY3" || resident_level == "Graduating") readable_period <- "Graduation"
    }
  } else if (current_period %in% c("1", "2", "3", "4", "5", "6", "7")) {
    # CCC format - use mapping
    ccc_to_readable <- c(
      "1" = "Mid Intern",
      "2" = "End Intern",
      "3" = "Mid PGY2",
      "4" = "End PGY2",
      "5" = "Mid PGY3",
      "6" = "Graduation",
      "7" = "Intern Intro"
    )
    readable_period <- ccc_to_readable[current_period]
  } else {
    # Assume it's already in readable format
    readable_period <- current_period
  }
  
  # If we couldn't map to a readable period, return NA
  if (is.null(readable_period) || is.na(readable_period)) {
    message("Couldn't map period '", current_period, "' to a standard format")
    return(NA)
  }
  
  # Define the sequence of periods
  period_sequence <- c(
    "Intern Intro",
    "Mid Intern",
    "End Intern",
    "Mid PGY2",
    "End PGY2",
    "Mid PGY3",
    "Graduation"
  )
  
  # Find the current period's index in the sequence
  current_index <- match(readable_period, period_sequence)
  
  # If period not found or it's the first one (Intern Intro), return NA
  if (is.na(current_index) || current_index == 1) {
    message("No previous period found for '", readable_period, "' (first period or not found)")
    return(NA)
  }
  
  # Return the previous period in the sequence
  prev_period <- period_sequence[current_index - 1]
  message("Previous period for '", readable_period, "' (", resident_level, ") is '", prev_period, "'")
  return(prev_period)
}

#' Check if Period Should Have Program Data
#'
#' Determines if a period should have program milestone assessments
#'
#' @param period The review period to check
#'
#' @return Boolean indicating whether the period should have program data
#' @export
has_program_data <- function(period) {
  # Adjust based on your program's workflow
  return(period != "Intern Intro")
}

find_record_id <- function(data, resident_name) {
  # First try resident_data
  if (!is.null(data$resident_data)) {
    matches <- data$resident_data %>%
      filter(name == resident_name, !is.na(record_id)) %>%
      select(record_id, name) %>%
      distinct()
    
    if (nrow(matches) > 0) {
      return(matches$record_id[1])
    }
  }
  
  # If not found, search all components
  for (component_name in names(data)) {
    if (!is.data.frame(data[[component_name]])) next
    
    component <- data[[component_name]]
    if ("name" %in% names(component) && "record_id" %in% names(component)) {
      matches <- component %>%
        filter(name == resident_name, !is.na(record_id))
      
      if (nrow(matches) > 0) {
        return(matches$record_id[1])
      }
    }
  }
  
  # Last resort: generate a hash ID from the name
  name_hash <- sum(utf8ToInt(resident_name)) %% 10000 + 1000
  return(as.character(name_hash))
}

# Rename the second map_period_format function to avoid conflict
map_period_between_formats <- function(period, from_format = "app", to_format = "ccc") {
  # Mapping tables for different formats
  app_to_ccc <- c(
    "Intern Intro" = "7",    # Intern Intro maps to 7 (Intern Intro)
    "Mid Review" = c(        # Mid Review maps differently based on level
      "Intern" = "1",        # Mid Intern
      "PGY2" = "3",          # Mid PGY2
      "PGY3" = "5"           # Mid PGY3
    ),
    "End Review" = c(        # End Review maps differently based on level
      "Intern" = "2",        # End Intern
      "PGY2" = "4",          # End PGY2
      "PGY3" = "6"           # Graduation
    )
  )
  
  # CCC session values to human-readable names
  ccc_to_readable <- c(
    "1" = "Mid Intern",
    "2" = "End Intern",
    "3" = "Mid PGY2",
    "4" = "End PGY2",
    "5" = "Mid PGY3",
    "6" = "Graduation",
    "7" = "Intern Intro"
  )
  
  readable_to_ccc <- setNames(names(ccc_to_readable), ccc_to_readable)
  
  # App periods to human-readable (depends on resident level too)
  app_to_readable <- function(period, level) {
    if (period == "Intern Intro") {
      return("Intern Intro")
    } else if (period == "Mid Review") {
      if (level == "Intern") return("Mid Intern")
      if (level == "PGY2") return("Mid PGY2")
      if (level == "PGY3") return("Mid PGY3")
      return(NA)
    } else if (period == "End Review") {
      if (level == "Intern") return("End Intern")
      if (level == "PGY2") return("End PGY2")
      if (level == "PGY3") return("Graduation")
      return(NA)
    }
    return(NA)
  }
  
  # Convert based on the from and to formats
  if (from_format == "app" && to_format == "ccc") {
    # This requires the resident level, which should be passed in the period parameter
    # as a list with period and level
    if (is.list(period)) {
      app_period <- period$period
      level <- period$level
      readable_period <- app_to_readable(app_period, level)
      return(readable_to_ccc[readable_period])
    } else {
      warning("Converting from app to ccc format requires both period and level")
      return(NA)
    }
  } else if (from_format == "ccc" && to_format == "readable") {
    return(ccc_to_readable[as.character(period)])
  } else if (from_format == "readable" && to_format == "ccc") {
    return(readable_to_ccc[period])
  } else {
    warning("Unsupported conversion: ", from_format, " to ", to_format)
    return(NA)
  }
}

#' Get CCC Session Name
#'
#' Converts a CCC session value to a readable name
#'
#' @param session The CCC session value (1-7)
#'
#' @return A readable name for the session
#' @export
get_ccc_session_name <- function(session) {
  session_names <- c(
    "1" = "Mid Intern",
    "2" = "End Intern",
    "3" = "Mid PGY2",
    "4" = "End PGY2",
    "5" = "Mid PGY3",
    "6" = "Graduation",
    "7" = "Intern Intro"
  )
  
  return(session_names[as.character(session)])
}

# Function to map app period to milestone period format
map_to_milestone_period <- function(level, period) {
  # Debug
  message(paste("Mapping period:", period, "for level:", level))
  
  # Direct mapping for standard period names
  if (period %in% c("Intern Intro", "Mid Intern", "End Intern", 
                    "Mid PGY2", "End PGY2", "Mid PGY3", "Graduation")) {
    message(paste("Using direct mapping for standard period:", period))
    return(period)
  }
  
  # Handle app format periods
  if (period == "Mid Review") {
    if (level == "Intern") return("Mid Intern")
    if (level == "PGY2") return("Mid PGY2")
    if (level == "PGY3") return("Mid PGY3")
  } 
  else if (period == "End Review") {
    if (level == "Intern") return("End Intern")
    if (level == "PGY2") return("End PGY2")
    if (level == "PGY3") return("Graduation")
  }
  else if (period == "Intro") {
    return("Intern Intro")
  }
  
  # For numeric codes
  period_codes <- c(
    "1" = "Mid Intern",
    "2" = "End Intern",
    "3" = "Mid PGY2",
    "4" = "End PGY2",
    "5" = "Mid PGY3",
    "6" = "Graduation",
    "7" = "Intern Intro"
  )
  
  if (period %in% names(period_codes)) {
    message(paste("Mapping numeric code", period, "to", period_codes[period]))
    return(period_codes[period])
  }
  
  # If we can't map, return NA
  message(paste("WARNING: Could not map period", period, "for level", level))
  return(NA)
}

extract_non_na_values <- function(data, resident_identifier, column_names, period = NULL) {
  # Determine which identifier column to use (record_id or name)
  id_column <- NULL
  
  if ("name" %in% names(data)) {
    id_column <- "name"
  } else if ("record_id" %in% names(data)) {
    id_column <- "record_id"
  } else {
    stop("Neither 'name' nor 'record_id' columns found in the data")
  }
  
  # Filter data for the specific resident by identifier
  resident_data <- data[data[[id_column]] == resident_identifier, ]
  
  # Further filter by period if provided
  if (!is.null(period)) {
    # Check which period column to use
    period_column <- NULL
    
    if ("year_resident.ilp_date" %in% names(resident_data)) {
      period_column <- "year_resident.ilp_date"
    } else if ("year_resident" %in% names(resident_data)) {
      period_column <- "year_resident"
    } else if ("ilp_date" %in% names(resident_data)) {
      period_column <- "ilp_date"
    } else {
      warning("No suitable period column found. Skipping period filtering.")
    }
    
    if (!is.null(period_column)) {
      resident_data <- resident_data[resident_data[[period_column]] == period, ]
    }
  }
  
  # Check if we have any data after filtering
  if (nrow(resident_data) == 0) {
    warning("No data found for the specified resident and period")
    return(NULL)
  }
  
  # Ensure all columns exist in the dataframe
  valid_columns <- column_names[column_names %in% names(resident_data)]
  
  if (length(valid_columns) == 0) {
    warning("None of the provided column names exist in the filtered data")
    return(NULL)
  }
  
  # Select only the specified columns
  result_df <- resident_data[, c(id_column, valid_columns), drop = FALSE]
  
  # Keep only rows that have at least one non-NA value in the specified columns
  have_non_na <- apply(result_df[, valid_columns, drop = FALSE], 1, function(row) any(!is.na(row)))
  result_df <- result_df[have_non_na, , drop = FALSE]
  
  # If no rows remain, return NULL
  if (nrow(result_df) == 0) {
    warning("No non-NA values found for the specified columns")
    return(NULL)
  }
  
  # Remove columns that are all NA
  cols_with_values <- apply(result_df, 2, function(col) any(!is.na(col)))
  result_df <- result_df[, cols_with_values, drop = FALSE]
  
  return(result_df)
}


get_fields_by_form <- function(data_dict, form_name) {
  # Filter the data dictionary for the specific form
  form_fields <- data_dict[data_dict$form_name == form_name, ]
  
  # Extract the field names from the "field_name" column
  field_names <- form_fields$field_name
  
  return(field_names)
}

# Function to create formatted output for displaying data with proper field labels
create_formatted_display <- function(data, data_dict, section_title, 
                                     exclude_fields = c("record_id", "name", "period", "date", 
                                                        "redcap_repeat_instrument", "redcap_repeat_instance", 
                                                        "complete", "status")) {
  # Check if we have data
  if (is.null(data) || nrow(data) == 0 || ncol(data) <= length(exclude_fields)) {
    return(paste0("# ", section_title, "\n\nNo data available."))
  }
  
  # Initialize output text
  output_text <- paste0("# ", section_title, "\n\n")
  
  # Get all columns except excluded ones
  all_cols <- setdiff(names(data), exclude_fields)
  
  # For each column, add formatted content
  for (col in all_cols) {
    # Skip if value is NA or empty
    value <- data[[col]][1]
    if (is.na(value) || value == "") {
      next
    }
    
    # Find the field label in the data dictionary
    field_label <- get_field_label(data_dict, col)
    
    # Format and add to output
    output_text <- paste0(output_text, "## ", field_label, "\n\n", value, "\n\n")
  }
  
  return(output_text)
}

# Helper function to get field label from data dictionary
get_field_label <- function(data_dict, field_name) {
  # Match on variable/field name column
  field_name_col <- if ("Variable / Field Name" %in% names(data_dict)) {
    "Variable / Field Name"
  } else {
    "field_name"
  }
  
  # Find the matching row
  match_row <- data_dict[data_dict[[field_name_col]] == field_name, ]
  
  # Get the field label
  label_col <- if ("Field Label" %in% names(data_dict)) {
    "Field Label"
  } else {
    "field_label"
  }
  
  if (nrow(match_row) > 0 && !is.na(match_row[[label_col]][1]) && match_row[[label_col]][1] != "") {
    return(match_row[[label_col]][1])
  } else {
    # Format the field name if no label found
    formatted_name <- gsub("_", " ", field_name)
    return(paste0(toupper(substr(formatted_name, 1, 1)), 
                  substr(formatted_name, 2, nchar(formatted_name))))
  }
}

# Function to format checkbox values with labels from the data dictionary
format_checkbox_values <- function(value, field_name, data_dict) {
  if (is.na(value) || value == "") {
    return("None selected")
  }
  
  # Get choices from data dictionary
  choices <- get_field_choices(data_dict, field_name)
  
  if (is.null(choices) || length(choices) == 0) {
    return(value)  # Return original value if no choices found
  }
  
  # Split the values string into individual values
  values <- strsplit(value, "\\s*\\|\\s*")[[1]]
  
  # Extract the actual labels from the mapping
  labels <- sapply(values, function(val) {
    for (choice in choices) {
      parts <- strsplit(choice, "\\s*,\\s*")[[1]]
      if (length(parts) >= 2) {
        code <- trimws(parts[1])
        if (trimws(val) == code) {
          return(paste(parts[-1], collapse = ", "))
        }
      }
    }
    return(val)  # Return the original value if not found
  })
  
  # Format as HTML bullet list
  result <- "<ul>\n"
  for (label in labels) {
    result <- paste0(result, "  <li>", label, "</li>\n")
  }
  result <- paste0(result, "</ul>")
  
  return(result)
}

# Helper function to get choices from data dictionary
get_field_choices <- function(data_dict, field_name) {
  field_name_col <- if ("Variable / Field Name" %in% names(data_dict)) {
    "Variable / Field Name"
  } else {
    "field_name"
  }
  
  choices_col <- if ("Choices, Calculations, OR Slider Labels" %in% names(data_dict)) {
    "Choices, Calculations, OR Slider Labels"
  } else {
    "select_choices_or_calculations"
  }
  
  match_row <- data_dict[data_dict[[field_name_col]] == field_name, ]
  
  if (nrow(match_row) > 0 && !is.na(match_row[[choices_col]][1]) && match_row[[choices_col]][1] != "") {
    choices <- strsplit(match_row[[choices_col]][1], "\\s*\\|\\s*")[[1]]
    return(choices)
  } else {
    return(NULL)
  }
}

# Function to determine if a field is a checkbox type
is_checkbox_field <- function(data_dict, field_name) {
  field_name_col <- if ("Variable / Field Name" %in% names(data_dict)) {
    "Variable / Field Name"
  } else {
    "field_name"
  }
  
  field_type_col <- if ("Field Type" %in% names(data_dict)) {
    "Field Type"
  } else {
    "field_type"
  }
  
  match_row <- data_dict[data_dict[[field_name_col]] == field_name, ]
  
  if (nrow(match_row) > 0 && !is.na(match_row[[field_type_col]][1])) {
    return(match_row[[field_type_col]][1] == "checkbox")
  } else {
    return(FALSE)
  }
}

# Enhanced function to create formatted display with checkbox handling
create_enhanced_display <- function(data, data_dict, section_title, 
                                    exclude_fields = c("record_id", "name", "period", "date", 
                                                       "redcap_repeat_instrument", "redcap_repeat_instance", 
                                                       "complete", "status"),
                                    checkbox_pattern = NULL) {
  # Check if we have data
  if (is.null(data) || nrow(data) == 0 || ncol(data) <= length(exclude_fields)) {
    return(paste0("# ", section_title, "\n\nNo data available."))
  }
  
  # Initialize output text
  output_text <- paste0("# ", section_title, "\n\n")
  
  # Get all columns except excluded ones
  all_cols <- setdiff(names(data), exclude_fields)
  
  # For each column, add formatted content
  for (col in all_cols) {
    # Skip if value is NA or empty
    value <- data[[col]][1]
    if (is.na(value) || value == "") {
      next
    }
    
    # Find the field label in the data dictionary
    field_label <- get_field_label(data_dict, col)
    
    # Check if this is a checkbox field or matches checkbox pattern
    is_checkbox <- is_checkbox_field(data_dict, col)
    if (!is_checkbox && !is.null(checkbox_pattern)) {
      is_checkbox <- grepl(checkbox_pattern, col)
    }
    
    # Format value if checkbox
    if (is_checkbox) {
      formatted_value <- format_checkbox_values(value, col, data_dict)
    } else {
      formatted_value <- value
    }
    
    # Format and add to output
    output_text <- paste0(output_text, "## ", field_label, "\n\n", formatted_value, "\n\n")
  }
  
  return(output_text)
}

# Function to group fields by section header
group_fields_by_section <- function(data, data_dict, section_title,
                                    exclude_fields = c("record_id", "name", "period", "date", 
                                                       "redcap_repeat_instrument", "redcap_repeat_instance", 
                                                       "complete", "status")) {
  # Check if we have data
  if (is.null(data) || nrow(data) == 0 || ncol(data) <= length(exclude_fields)) {
    return(paste0("# ", section_title, "\n\nNo data available."))
  }
  
  # Initialize output text
  output_text <- paste0("# ", section_title, "\n\n")
  
  # Get all columns except excluded ones
  all_cols <- setdiff(names(data), exclude_fields)
  
  # Extract section headers for each field
  field_name_col <- if ("Variable / Field Name" %in% names(data_dict)) {
    "Variable / Field Name"
  } else {
    "field_name"
  }
  
  section_header_col <- if ("Section Header" %in% names(data_dict)) {
    "Section Header"
  } else {
    "section_header"
  }
  
  # Create a mapping of fields to sections
  sections <- list()
  for (col in all_cols) {
    # Skip if value is NA or empty
    value <- data[[col]][1]
    if (is.na(value) || value == "") {
      next
    }
    
    # Find section header
    match_row <- data_dict[data_dict[[field_name_col]] == col, ]
    section <- if (nrow(match_row) > 0 && !is.na(match_row[[section_header_col]][1]) && 
                   match_row[[section_header_col]][1] != "") {
      match_row[[section_header_col]][1]
    } else {
      "Other" # Default section
    }
    
    # Add field to appropriate section
    if (!section %in% names(sections)) {
      sections[[section]] <- list()
    }
    sections[[section]] <- c(sections[[section]], col)
  }
  
  # Process each section
  for (section_name in names(sections)) {
    if (section_name != "Other") {
      output_text <- paste0(output_text, "## ", section_name, "\n\n")
    }
    
    # Process fields in this section
    for (col in sections[[section_name]]) {
      value <- data[[col]][1]
      field_label <- get_field_label(data_dict, col)
      
      # Check if checkbox
      is_checkbox <- is_checkbox_field(data_dict, col)
      
      # Format value if checkbox
      if (is_checkbox) {
        formatted_value <- format_checkbox_values(value, col, data_dict)
      } else {
        formatted_value <- value
      }
      
      # Add field to output
      output_text <- paste0(output_text, "### ", field_label, "\n\n", formatted_value, "\n\n")
    }
  }
  
  return(output_text)
}


# Updated function to create ILP data table for prior review tab
create_ilp_data_table <- function(resident_name, period, resident_data, rdm_dict) {
  message(paste("Creating ILP data table for", resident_name, "period:", period))
  
  # Get ILP columns from data dictionary - adjusted for your rdm_dict structure
  ilp_cols <- rdm_dict$field_name[rdm_dict$form_name == "ilp"]
  
  # Extract the resident's ILP data
  ilp_dat <- extract_non_na_values(resident_data, resident_name, ilp_cols, period)
  
  if (is.null(ilp_dat) || nrow(ilp_dat) == 0) {
    message("No ILP data found for", resident_name, "in period", period)
    return(NULL)
  }
  
  # Function to safely get a value from ilp_dat
  get_val <- function(column) {
    if (column %in% names(ilp_dat)) {
      return(as.character(ilp_dat[[column]][1]))
    } else {
      return(NA_character_)
    }
  }
  
  # Function to get review question value with fallback
  # Improved function to get review question value with grepl
  get_review_q <- function(prefix) {
    # Use grepl to find columns matching pattern
    review_cols <- grep(paste0("^review_q[0-9]*_", prefix, "$"), names(ilp_dat), value = TRUE)
    
    # Check each matching column
    for (col in review_cols) {
      val <- get_val(col)
      if (!is.na(val) && val != "") {
        return(val)
      }
    }
    
    # Return NA if no values found
    return(NA_character_)
  }
  
  # Function to extract milestone descriptions
  get_milestone_desc <- function(prefix) {
    milestone_cols <- grep(paste0("^", prefix, "[0-9]+_r1$"), names(ilp_dat), value = TRUE)
    if (length(milestone_cols) > 0) {
      # Get all milestone descriptions and combine them
      descriptions <- character(0)
      for (col in milestone_cols) {
        val <- get_val(col)
        if (!is.na(val) && val != "") {
          milestone_num <- gsub("_r1$", "", gsub(paste0("^", prefix), "", col))
          descriptions <- c(descriptions, paste0(prefix, milestone_num, ": ", val))
        }
      }
      return(paste(descriptions, collapse = "\n"))
    }
    return(NA_character_)
  }
  
  # Function to get goal description with level
  get_goal_desc <- function(prefix) {
    goal_val <- get_val(paste0("goal_", prefix))
    level_val <- get_val(paste0("goal_level_", prefix))
    
    if (!is.na(goal_val) && goal_val != "") {
      if (!is.na(level_val) && level_val != "") {
        return(paste0(goal_val, " (Level ", level_val, ")"))
      } else {
        return(goal_val)
      }
    } else {
      return(NA_character_)
    }
  }
  
  # Build data table with the three competency pairs
  ilp_table <- data.frame(
    Competency = c(
      "Patient Care / Medical Knowledge",
      "Systems-Based Practice / Practice-Based Learning",
      "Professionalism / Interpersonal Communication Skills"
    ),
    `Prior Goal Met?` = c(
      get_val("prior_goal_pcmk"),
      get_val("prior_goal_sbppbl"),
      get_val("prior_goal_profics")
    ),
    `Review Comments` = c(
      get_review_q("pcmk"),
      get_review_q("sbppbl"),
      get_review_q("profics")
    ),
    `Goal Description` = c(
      get_goal_desc("pcmk"),
      get_goal_desc("sbppbl"),
      get_goal_desc("profics")
    ),
    `Action Plan` = c(
      get_val("how_pcmk"),
      get_val("how_sbppbl"),
      get_val("how_profics")
    ),
    `Milestone Descriptions` = c(
      paste(get_milestone_desc("pc"), get_milestone_desc("mk"), sep = "\n\n"),
      paste(get_milestone_desc("sbp"), get_milestone_desc("pbl"), sep = "\n\n"),
      paste(get_milestone_desc("prof"), get_milestone_desc("ics"), sep = "\n\n")
    ),
    stringsAsFactors = FALSE
  )
  
  # Clean up any NA values for display
  ilp_table[is.na(ilp_table)] <- ""
  
  return(ilp_table)
}

# Function to get and format prior CCC notes
create_ccc_notes_table <- function(resident_name, current_period, resident_level, resident_data, rdm_dict) {
  message(paste("Getting prior CCC notes for:", resident_name, "current period:", current_period))
  
  # Get CCC columns from data dictionary
  ccc_cols <- get_fields_by_form(rdm_dict, 'ccc_review')
  
  # Extract all CCC data for this resident
  ccc_dat <- extract_non_na_values(resident_data, resident_name, ccc_cols)
  
  if (is.null(ccc_dat) || nrow(ccc_dat) == 0) {
    message("No CCC data found for", resident_name)
    return(NULL)
  }
  
  # Convert current period to readable format if needed
  readable_current_period <- NULL
  if (current_period %in% c("Intern Intro", "Mid Review", "End Review")) {
    if (current_period == "Intern Intro") {
      readable_current_period <- "Intern Intro"
    } else if (current_period == "Mid Review") {
      if (resident_level == "Intern") readable_current_period <- "Mid Intern"
      else if (resident_level == "PGY2") readable_current_period <- "Mid PGY2"
      else if (resident_level == "PGY3") readable_current_period <- "Mid PGY3"
    } else if (current_period == "End Review") {
      if (resident_level == "Intern") readable_current_period <- "End Intern"
      else if (resident_level == "PGY2") readable_current_period <- "End PGY2"
      else if (resident_level == "PGY3") readable_current_period <- "Graduation"
    }
  } else {
    # Assume it's already in readable format
    readable_current_period <- current_period
  }
  
  # Filter out rows that match the current period
  ccc_dat <- ccc_dat[ccc_dat$ccc_session != readable_current_period, ]
  
  # Sort by date in descending order (most recent first)
  if ("ccc_date" %in% names(ccc_dat) && !all(is.na(ccc_dat$ccc_date))) {
    # Convert date strings to Date objects
    ccc_dat$ccc_date <- as.Date(ccc_dat$ccc_date)
    # Sort by date descending
    ccc_dat <- ccc_dat[order(ccc_dat$ccc_date, decreasing = TRUE), ]
  }
  
  # Select relevant columns for display
  cols_to_display <- intersect(
    c("ccc_date", "ccc_rev_type", "ccc_session", "ccc_interim", 
      "ccc_concern", "ccc_ilp", "ccc_mile", "ccc_mile_notes", 
      "ccc_issues_follow_up", "ccc_comments"),
    names(ccc_dat)
  )
  
  # If no columns to display, return NULL
  if (length(cols_to_display) == 0) {
    message("No relevant CCC columns found")
    return(NULL)
  }
  
  # Subset the data frame to only include the display columns
  display_data <- ccc_dat[, cols_to_display, drop = FALSE]
  
  # Rename columns for better display
  col_labels <- c(
    "ccc_date" = "Date",
    "ccc_rev_type" = "Review Type",
    "ccc_session" = "Session",
    "ccc_interim" = "Interim Notes",
    "ccc_concern" = "Concerns",
    "ccc_ilp" = "ILP Notes",
    "ccc_mile" = "Milestones Complete",
    "ccc_mile_notes" = "Milestone Notes",
    "ccc_issues_follow_up" = "Follow Up",
    "ccc_comments" = "Comments"
  )
  
  # Rename columns that exist in the data
  new_names <- sapply(names(display_data), function(name) {
    if (name %in% names(col_labels)) col_labels[name] else name
  })
  names(display_data) <- new_names
  
  message("Found", nrow(display_data), "prior CCC records")
  return(display_data)
}
