# ============================================================================
# GLOBAL.R - IMSLU COACHING APP (RDM 2.0)
# Cleaned for Posit Connect deployment
# ============================================================================

# Load required packages
library(shiny)
library(shinyjs)
library(bslib)
library(DT)
library(dplyr)
library(purrr)
library(httr)
library(jsonlite)
library(config)
library(plotly)

# Load gmed package
library(gmed)

# ============================================================================
# SOURCE HELPER FILES
# ============================================================================

# Source helper functions with error handling
tryCatch({
  source("R/helpers.R", local = TRUE)
  message("✅ Helper functions loaded from R/helpers.R")
}, error = function(e) {
  message("❌ Error loading helpers.R: ", e$message)
  # Create minimal fallback functions for Posit Connect
  map_app_period_to_coach_period <<- function(period, level) "7"
  validate_access_code <<- function(code) code == "coach"
  `%||%` <<- function(a, b) if (is.null(a) || length(a) == 0) b else a
})

# Source intern intro module
tryCatch({
  source("R/intern_intro_module.R", local = TRUE)
  message("✅ Intern intro module loaded")
}, error = function(e) {
  message("❌ Error loading intern_intro_module.R: ", e$message)
  # Create fallback function for intern intro
  create_intern_intro_interface <<- function(resident_name, app_data) {
    div(class = "alert alert-warning", "Intern intro module not available")
  }
})

# Source additional modules if they exist
tryCatch({
  source("R/modules.R", local = TRUE)
  message("✅ Additional modules loaded")
}, error = function(e) {
  message("ℹ️ R/modules.R not found or has issues: ", e$message)
})

# ============================================================================
# CONFIGURATION AND AUTHENTICATION
# ============================================================================

# Load configuration (Posit Connect friendly)
app_config <- tryCatch({
  # Try config file first (for local development)
  if (file.exists("config.yml")) {
    config::get()
  } else {
    # Fallback to environment variables (for Posit Connect)
    list(
      rdm_token = Sys.getenv("RDM_TOKEN"),
      access_code = Sys.getenv("ACCESS_CODE")
    )
  }
}, error = function(e) {
  message("❌ Config loading failed: ", e$message)
  # Final fallback
  list(
    rdm_token = Sys.getenv("RDM_TOKEN"),
    access_code = Sys.getenv("ACCESS_CODE", "coach")  # Default access code
  )
})

# REDCap configuration
REDCAP_URL <- "https://redcapsurvey.slu.edu/api/"

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

# Safe null coalescing operator (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
}

# Enhanced access code validation for production
validate_access_code <- function(code) {
  if (!exists("app_config") || is.null(code) || code == "") {
    return(FALSE)
  }
  
  # Get valid codes from different sources
  valid_codes <- c(
    Sys.getenv("ACCESS_CODE"),
    app_config$access_code,
    "coach",     # Default for development
    "demo"       # Demo access
  )
  
  # Remove empty/NA codes
  valid_codes <- valid_codes[!is.na(valid_codes) & valid_codes != ""]
  
  # For production, be more restrictive
  if (Sys.getenv("R_CONFIG_ACTIVE") == "production") {
    # Only allow environment variable in production
    valid_codes <- valid_codes[1]  # Only the first one (from environment)
  }
  
  return(code %in% valid_codes)
}

# Test REDCap connection with better error handling
test_redcap_connection <- function(url, token) {
  if (is.null(token) || token == "") {
    message("❌ No REDCap token provided")
    return(FALSE)
  }
  
  tryCatch({
    response <- httr::POST(
      url,
      body = list(
        token = token,
        content = "project"
      ),
      encode = "form",
      httr::timeout(15)  # Increased timeout for Posit Connect
    )
    
    status <- httr::status_code(response)
    success <- status == 200
    
    if (success) {
      message("✅ REDCap connection successful")
    } else {
      message("❌ REDCap connection failed - Status: ", status)
    }
    
    return(success)
  }, error = function(e) {
    message("❌ REDCap connection error: ", e$message)
    return(FALSE)
  })
}

# ============================================================================
# DATA LOADING FUNCTIONS
# ============================================================================

# Main data loading function (optimized for Posit Connect)
load_app_data <- function() {
  message("=== LOADING APPLICATION DATA ===")
  
  # Validate token
  if (is.null(app_config$rdm_token) || app_config$rdm_token == "") {
    stop("❌ RDM_TOKEN not found. Please set the RDM_TOKEN environment variable.")
  }
  
  # Test REDCap connection
  if (!test_redcap_connection(REDCAP_URL, app_config$rdm_token)) {
    stop("❌ Cannot connect to REDCap. Check your RDM_TOKEN and network connection.")
  }
  
  # Load data dictionary
  message("Loading REDCap data dictionary...")
  rdm_dict <- tryCatch({
    if (exists("get_evaluation_dictionary", where = "package:gmed")) {
      message("Using gmed::get_evaluation_dictionary")
      gmed::get_evaluation_dictionary(app_config$rdm_token, REDCAP_URL)
    } else {
      message("Using direct API call for data dictionary")
      response <- httr::POST(
        REDCAP_URL,
        body = list(
          token = app_config$rdm_token,
          content = "metadata",
          format = "json",
          returnFormat = "json"
        ),
        encode = "form",
        httr::timeout(45)  # Longer timeout for production
      )
      
      if (httr::status_code(response) == 200) {
        content <- httr::content(response, "text", encoding = "UTF-8")
        dict <- jsonlite::fromJSON(content, flatten = TRUE)
        message("✅ Data dictionary loaded: ", nrow(dict), " fields")
        dict
      } else {
        stop("❌ Data dictionary API call failed with status: ", httr::status_code(response))
      }
    }
  }, error = function(e) {
    message("❌ Data dictionary loading failed: ", e$message)
    stop("Cannot load data dictionary: ", e$message)
  })
  
  # Load all RDM data
  message("Loading all RDM 2.0 data...")
  all_rdm_data <- tryCatch({
    if (exists("pull_all_redcap_data", where = "package:gmed")) {
      message("Using gmed::pull_all_redcap_data")
      gmed::pull_all_redcap_data(app_config$rdm_token, REDCAP_URL)
    } else {
      message("Using direct REDCap API call")
      response <- httr::POST(
        REDCAP_URL,
        body = list(
          token = app_config$rdm_token,
          content = "record",
          action = "export", 
          format = "json",
          type = "flat",
          returnFormat = "json"
        ),
        encode = "form",
        httr::timeout(90)  # Extended timeout for large datasets
      )
      
      if (httr::status_code(response) == 200) {
        content <- httr::content(response, "text", encoding = "UTF-8")
        data <- jsonlite::fromJSON(content)
        message("✅ RDM data loaded: ", nrow(data), " total rows")
        data
      } else {
        stop("❌ RDM data API call failed with status: ", httr::status_code(response))
      }
    }
  }, error = function(e) {
    message("❌ RDM data loading failed: ", e$message)
    stop("Cannot load RDM data: ", e$message)
  })
  
  # Process resident data
  message("=== PROCESSING RESIDENT DATA ===")
  
  # Get base resident data (main form only, no repeat instruments)
  if ("redcap_repeat_instrument" %in% names(all_rdm_data)) {
    resident_data <- all_rdm_data %>%
      filter(is.na(redcap_repeat_instrument) | redcap_repeat_instrument == "")
    message("Filtered to ", nrow(resident_data), " base resident records")
  } else {
    resident_data <- all_rdm_data
    message("No repeat instrument column found, using all ", nrow(resident_data), " records")
  }
  
  # Filter out archived residents
  if ("res_archive" %in% names(resident_data)) {
    before_count <- nrow(resident_data)
    resident_data <- resident_data %>%
      filter(is.na(res_archive) | (!res_archive %in% c("Yes", "1")))
    message("Archive filter: ", before_count, " -> ", nrow(resident_data), " active residents")
  }
  
  # Calculate resident levels
  message("Calculating resident levels...")
  resident_data <- tryCatch({
    gmed::calculate_resident_level(resident_data)
  }, error = function(e) {
    message("⚠️ gmed level calculation failed, using fallback: ", e$message)
    
    # Manual fallback calculation
    current_date <- Sys.Date()
    current_academic_year <- ifelse(
      format(current_date, "%m-%d") >= "07-01",
      as.numeric(format(current_date, "%Y")),
      as.numeric(format(current_date, "%Y")) - 1
    )
    
    resident_data %>%
      mutate(
        grad_yr_numeric = suppressWarnings(as.numeric(grad_yr)),
        Level = case_when(
          type == "Preliminary" ~ "Intern",
          type == "Categorical" & grad_yr_numeric == current_academic_year + 3 ~ "Intern",
          type == "Categorical" & grad_yr_numeric == current_academic_year + 2 ~ "PGY2",
          type == "Categorical" & grad_yr_numeric == current_academic_year + 1 ~ "PGY3",
          TRUE ~ "Unknown"
        )
      ) %>%
      select(-grad_yr_numeric)
  })
  
  # Log level distribution
  if ("Level" %in% names(resident_data)) {
    level_counts <- table(resident_data$Level, useNA = "always")
    message("Level distribution: ", paste(names(level_counts), "=", level_counts, collapse = ", "))
  }
  
  # Organize other data components
  message("Organizing additional data components...")
  # Fix for your global.R - replace the organize_redcap_data section
  
  # Updated data organization section in load_app_data function
  organized_data <- tryCatch({
    if (exists("organize_redcap_data", where = "package:gmed")) {
      # Try using gmed function first
      result <- gmed::organize_redcap_data(all_rdm_data)
      # Override with our filtered resident data
      result$resident_data <- resident_data
      
      # CRITICAL FIX: Manually extract s_eval if it's missing
      if (is.null(result$s_eval) || nrow(result$s_eval) == 0) {
        message("⚠️ s_eval missing from gmed organization, extracting manually...")
        result$s_eval <- all_rdm_data %>%
          filter(!is.na(redcap_repeat_instrument) & redcap_repeat_instrument == "s_eval")
        message("✅ Manually extracted ", nrow(result$s_eval), " s_eval records")
      }
      
      result
    } else {
      # Manual organization
      message("Using manual data organization...")
      
      list(
        resident_data = resident_data,
        s_eval = all_rdm_data %>%
          filter(!is.na(redcap_repeat_instrument) & redcap_repeat_instrument == "S Eval"),
        milestone_program = all_rdm_data %>%
          filter(!is.na(redcap_repeat_instrument) & redcap_repeat_instrument == "milestone_entry"),
        milestone_self = all_rdm_data %>%
          filter(!is.na(redcap_repeat_instrument) & redcap_repeat_instrument == "milestone_selfevaluation_c33c"),
        assessment_data = all_rdm_data %>%
          filter(!is.na(redcap_repeat_instrument) & redcap_repeat_instrument == "assessment"),
        raw_data = all_rdm_data
      )
    }
  }, error = function(e) {
    message("⚠️ organize_redcap_data issues: ", e$message)
    
    # Fallback manual organization
    message("Using fallback manual organization...")
    
    list(
      resident_data = resident_data,
      s_eval = all_rdm_data %>%
        filter(!is.na(redcap_repeat_instrument) & redcap_repeat_instrument == "s_eval"),
      milestone_program = all_rdm_data %>%
        filter(!is.na(redcap_repeat_instrument) & redcap_repeat_instrument == "milestone_entry"),
      milestone_self = all_rdm_data %>%
        filter(!is.na(redcap_repeat_instrument) & redcap_repeat_instrument == "milestone_selfevaluation_c33c"),
      assessment_data = all_rdm_data %>%
        filter(!is.na(redcap_repeat_instrument) & redcap_repeat_instrument == "assessment"),
      raw_data = all_rdm_data
    )
  })
  
  # Add debugging for s_eval after organization
  message("=== S_EVAL DEBUG ===")
  if (!is.null(organized_data$s_eval)) {
    message("✅ s_eval found with ", nrow(organized_data$s_eval), " records")
    
    # Check for Claire Boehm specifically
    claire_data <- organized_data$s_eval %>%
      filter(name == "Claire Boehm")
    
    if (nrow(claire_data) > 0) {
      message("✅ Found s_eval data for Claire Boehm")
      
      # Check specific fields
      goal_fields <- c("s_e_ume_goal1", "s_e_ume_goal2", "s_e_ume_goal3")
      for (field in goal_fields) {
        if (field %in% names(claire_data)) {
          value <- claire_data[[field]][1]
          message("  ", field, ": ", ifelse(is.na(value), "NA", as.character(value)))
        }
      }
    } else {
      message("❌ No s_eval data found for Claire Boehm")
    }
  } else {
    message("❌ s_eval is NULL in organized data")
  }
  message("===================")
  
  message("✅ Data loading completed successfully")
  message("✅ Loaded ", nrow(organized_data$resident_data), " resident records")
  
  return(list(
    url = REDCAP_URL,
    rdm_token = app_config$rdm_token,
    resident_data = organized_data$resident_data,
    s_eval = organized_data$s_eval,
    rdm_dict = rdm_dict,
    milestone_program = organized_data$milestone_program,
    milestone_self = organized_data$milestone_self,
    assessment_data = organized_data$assessment_data,
    raw_data = organized_data$raw_data
  ))
}

# ============================================================================
# REACTIVE DATA (with error handling)
# ============================================================================

app_data <- reactive({
  tryCatch({
    data <- load_app_data()
    
    # Log successful loading
    if (!is.null(data$resident_data)) {
      message("✅ App data reactive: ", nrow(data$resident_data), " residents loaded")
    }
    
    return(data)
  }, error = function(e) {
    message("❌ App data loading failed in reactive: ", e$message)
    
    # Return minimal structure to prevent app crash
    showNotification(
      paste("Data loading failed:", e$message),
      type = "error",
      duration = NULL
    )
    
    return(list(
      error = e$message,
      resident_data = data.frame(),
      rdm_dict = data.frame(),
      url = REDCAP_URL,
      rdm_token = app_config$rdm_token
    ))
  })
})

# ============================================================================
# STARTUP VALIDATION
# ============================================================================

# Validate environment on startup
message("=== COACHING APP STARTUP ===")
message("Environment: ", Sys.getenv("R_CONFIG_ACTIVE", "default"))
message("Current period: ", tryCatch(gmed::get_current_period(), error = function(e) "Error"))
message("RDM Token available: ", !is.null(app_config$rdm_token) && nchar(app_config$rdm_token) > 10)
message("GMED package loaded: ", "gmed" %in% loadedNamespaces())

# Test key gmed functions
if ("gmed" %in% loadedNamespaces()) {
  required_functions <- c("get_current_period", "calculate_resident_level", "map_to_milestone_period")
  for (func in required_functions) {
    exists_check <- exists(func, where = "package:gmed")
    message(func, " available: ", exists_check)
  }
}

# Only test connection interactively (not on Posit Connect startup)
if (interactive() && !is.null(app_config$rdm_token) && app_config$rdm_token != "") {
  message("Testing REDCap connection...")
  connection_test <- test_redcap_connection(REDCAP_URL, app_config$rdm_token)
  message("Connection test result: ", connection_test)
}

message("=== Global.R initialization complete ===")