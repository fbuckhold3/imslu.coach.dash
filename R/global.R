# ---------- LIBRARY IMPORTS ----------
library(shiny)
library(shinyjs)
library(redcapAPI)
library(REDCapR)
library(ggplot2)
library(DT)
library(dplyr)
library(config)
library(imres)  # Make sure this package is installed
library(bslib)
library(httr)
library(gganimate)
library(stringr)
library(xml2)
library(fontawesome)
library(tidyr)
library(reactable)
library(htmltools)
library(data.table)
library(purrr)
library(ggradar)

# ---------- SOURCE HELPER FUNCTIONS ----------
# Uncomment and adjust paths as needed
source("R/helpers.R")
# source("R/goal_mod.R")  # Uncomment if needed

# ---------- INITIALIZE APP CONFIG ----------
initialize_app_config <- function() {
  # Set up REDCap API URL
  url <- "https://redcapsurvey.slu.edu/api/"
  
  # Debug information about environment variables
  message("Available environment variables (first 10):")
  env_vars <- names(Sys.getenv())
  print(head(env_vars, 10))
  message("EVAL_TOKEN exists:", "EVAL_TOKEN" %in% names(Sys.getenv()))
  message("RDM_TOKEN exists:", "RDM_TOKEN" %in% names(Sys.getenv()))
  message("FAC_TOKEN exists:", "FAC_TOKEN" %in% names(Sys.getenv()))
  
  # Identify whether we are in a hosted environment
  is_hosted <- Sys.getenv("EVAL_TOKEN") != ""
  
  # Load tokens from environment variables or config file
  if (is_hosted) {
    eval_token <- Sys.getenv("EVAL_TOKEN")
    rdm_token <- Sys.getenv("RDM_TOKEN")
    fac_token <- Sys.getenv("FAC_TOKEN")
    
    # Check if tokens are empty strings even though they exist
    if (nchar(eval_token) == 0 || nchar(rdm_token) == 0 || nchar(fac_token) == 0) {
      message("WARNING: One or more required tokens are empty in environment!")
      message("Using config file as fallback.")
      # Load from config file as fallback
      conf <- tryCatch({
        config::get(file = "config.yml")
      }, error = function(e) {
        message("Error loading config file: ", e$message)
        list(
          eval_token = "",
          rdm_token = "",
          fac_token = ""
        )
      })
      
      # Use config values if environment variables are empty
      if (nchar(eval_token) == 0) eval_token <- conf$eval_token
      if (nchar(rdm_token) == 0) rdm_token <- conf$rdm_token
      if (nchar(fac_token) == 0) fac_token <- conf$fac_token
    }
    
    # Disable SSL verification in the hosted environment (NOT recommended for production)
    httr::set_config(httr::config(ssl_verifypeer = FALSE))
  } else {
    # Use config file for local development
    conf <- tryCatch({
      config::get(file = "config.yml")
    }, error = function(e) {
      message("Error loading config file: ", e$message)
      list(
        eval_token = "",
        rdm_token = "",
        fac_token = ""
      )
    })
    eval_token <- conf$eval_token
    rdm_token <- conf$rdm_token
    fac_token <- conf$fac_token
  }
  
  # Print token values (length only for security)
  message("EVAL_TOKEN length:", nchar(eval_token))
  message("RDM_TOKEN length:", nchar(rdm_token))
  message("FAC_TOKEN length:", nchar(fac_token))
  
  # Return the environment with the tokens and URL
  list(
    url = url,
    eval_token = eval_token,
    rdm_token = rdm_token,
    fac_token = fac_token
  )
}

# ---------- LOAD IMRES DATA ----------
load_imres_data <- function(config) {
  message("=== STARTING load_imres_data FUNCTION ===")
  
  # --- RDM Dictionary ---
  rdm_dict <- tryCatch({
    message("Attempting to get rdm_dict data dictionary...")
    result <- get_data_dict(config$rdm_token, config$url)
    message("Successfully retrieved rdm_dict with ", nrow(result), " rows")
    result
  }, error = function(e) {
    message("Error getting rdm_dict: ", e$message)
    NULL
  })
  
  # --- Assessment Dictionary ---
  ass_dict <- tryCatch({
    message("Attempting to get ass_dict data dictionary...")
    result <- get_data_dict(config$eval_token, config$url)
    message("Successfully retrieved ass_dict with ", nrow(result), " rows")
    result
  }, error = function(e) {
    message("Error getting ass_dict: ", e$message)
    NULL
  })
  
  # --- Pull all forms data ---
  message("About to call forms_api_pull with these arguments:")
  message("- rdm_token length: ", nchar(config$rdm_token))
  message("- url: ", config$url)
  message("- forms: resident_data, faculty_evaluation, ilp, s_eval, scholarship")
  
  rdm_dat <- tryCatch({
    message("Pulling forms data...")
    result <- forms_api_pull(config$rdm_token, config$url, 'resident_data', 'faculty_evaluation', 'ilp', 's_eval', 'scholarship')
    
    message("Forms data pulled. Structure of rdm_dat:")
    message("rdm_dat is of class: ", paste(class(result), collapse=", "))
    message("rdm_dat contains these keys: ", paste(names(result), collapse=", "))
    
    # If rdm_dat is a data frame, check if it has the redcap_repeat_instrument column
    if (is.data.frame(result)) {
      message("rdm_dat is a data frame with ", nrow(result), " rows and ", ncol(result), " columns")
      if ("redcap_repeat_instrument" %in% names(result)) {
        message("redcap_repeat_instrument values: ", paste(unique(result$redcap_repeat_instrument), collapse=", "))
      } else {
        message("WARNING: redcap_repeat_instrument column not found in rdm_dat")
      }
    }
    
    result
  }, error = function(e) {
    message("Error pulling forms data: ", e$message)
    NULL
  })
  
  # --- Extract ILP data ---
  ilp_data <- tryCatch({
    message("Starting ILP data extraction")
    if (is.null(rdm_dat)) {
      message("rdm_dat is NULL, cannot extract ILP data")
      NULL
    } else {
      message("rdm_dat contains these keys: ", paste(names(rdm_dat), collapse=", "))
      
      # Try multiple approaches to extract ILP data
      if ("ilp" %in% names(rdm_dat)) {
        message("Found ilp data (lowercase) in rdm_dat")
        ilp_data <- rdm_dat$ilp
      } else if ("ILP" %in% names(rdm_dat)) {
        message("Found ILP data (capitalized) in rdm_dat")
        ilp_data <- rdm_dat$ILP
      } else if (is.data.frame(rdm_dat) && "redcap_repeat_instrument" %in% names(rdm_dat)) {
        # Check for ILP in repeating instruments, case-insensitive
        message("Looking for ilp data in repeating instruments")
        
        # Print all unique values in redcap_repeat_instrument
        message("Unique redcap_repeat_instrument values: ", 
                paste(unique(rdm_dat$redcap_repeat_instrument), collapse=", "))
        
        ilp_data <- rdm_dat %>%
          filter(tolower(redcap_repeat_instrument) == "ilp")
        
        if (nrow(ilp_data) > 0) {
          message("Extracted ", nrow(ilp_data), " rows of ILP data from main dataframe")
        } else {
          message("No rows found with redcap_repeat_instrument='ilp' (case insensitive)")
          
          # Try one more approach - check if there's a column that has 'ilp' in its name
          ilp_columns <- names(rdm_dat)[grepl("ilp", tolower(names(rdm_dat)))]
          if (length(ilp_columns) > 0) {
            message("Found columns with 'ilp' in their name: ", paste(ilp_columns, collapse=", "))
          }
          
          NULL
        }
      } else {
        message("No ILP data found using any extraction method")
        NULL
      }
      
      # Debug the extracted data
      if (exists("ilp_data") && !is.null(ilp_data)) {
        message("ILP data class: ", paste(class(ilp_data), collapse=", "))
        if (is.data.frame(ilp_data)) {
          message("ILP data has ", nrow(ilp_data), " rows and ", ncol(ilp_data), " columns")
          message("ILP data column names: ", paste(names(ilp_data), collapse=", "))
        }
        ilp_data
      } else {
        message("ilp_data is NULL after extraction attempts")
        NULL
      }
    }
  }, error = function(e) {
    message("Error extracting ILP data: ", e$message)
    NULL
  })
  
  # --- Extract s_eval data ---
  s_eval_data <- tryCatch({
    message("Starting s_eval data extraction")
    if (is.null(rdm_dat)) {
      message("rdm_dat is NULL, cannot extract s_eval data")
      NULL
    } else {
      message("rdm_dat contains these keys: ", paste(names(rdm_dat), collapse=", "))
      
      # Try multiple approaches to extract s_eval data
      if ("s_eval" %in% names(rdm_dat)) {
        message("Found s_eval data (lowercase) in rdm_dat")
        s_eval_data <- rdm_dat$s_eval
      } else if ("S_eval" %in% names(rdm_dat)) {
        message("Found S_eval data (capitalized) in rdm_dat")
        s_eval_data <- rdm_dat$S_eval
      } else if (is.data.frame(rdm_dat) && "redcap_repeat_instrument" %in% names(rdm_dat)) {
        # Check for s_eval in repeating instruments, case-insensitive
        message("Looking for s_eval data in repeating instruments")
        
        s_eval_data <- rdm_dat %>%
          filter(tolower(redcap_repeat_instrument) == "s_eval")
        
        if (nrow(s_eval_data) > 0) {
          message("Extracted ", nrow(s_eval_data), " rows of s_eval data from main dataframe")
        } else {
          message("No rows found with redcap_repeat_instrument='s_eval' (case insensitive)")
          NULL
        }
      } else {
        message("No s_eval data found using any extraction method")
        NULL
      }
      
      # Debug the extracted data
      if (exists("s_eval_data") && !is.null(s_eval_data)) {
        message("s_eval data class: ", paste(class(s_eval_data), collapse=", "))
        if (is.data.frame(s_eval_data)) {
          message("s_eval data has ", nrow(s_eval_data), " rows and ", ncol(s_eval_data), " columns")
        }
        s_eval_data
      } else {
        message("s_eval_data is NULL after extraction attempts")
        NULL
      }
    }
  }, error = function(e) {
    message("Error extracting s_eval data: ", e$message)
    NULL
  })
  
  # --- Extract scholarship data ---
  schol_data <- tryCatch({
    message("Starting scholarship data extraction")
    if (is.null(rdm_dat)) {
      message("rdm_dat is NULL, cannot extract scholarship data")
      NULL
    } else {
      message("rdm_dat is not NULL")
      message("rdm_dat contains these keys: ", paste(names(rdm_dat), collapse=", "))
      
      # Try multiple approaches to extract scholarship data
      if ("scholarship" %in% names(rdm_dat)) {
        message("Found scholarship data (lowercase) in rdm_dat")
        schol_data <- rdm_dat$scholarship
      } else if ("Scholarship" %in% names(rdm_dat)) {
        message("Found Scholarship data (capitalized) in rdm_dat")
        schol_data <- rdm_dat$Scholarship
      } else if (is.data.frame(rdm_dat) && "redcap_repeat_instrument" %in% names(rdm_dat)) {
        # Check for scholarship in repeating instruments, case-insensitive
        message("Looking for scholarship data in repeating instruments")
        
        schol_data <- rdm_dat %>%
          filter(tolower(redcap_repeat_instrument) == "scholarship")
        
        if (nrow(schol_data) > 0) {
          message("Extracted ", nrow(schol_data), " rows of scholarship data from main dataframe")
        } else {
          message("No scholarship data found in main dataframe")
          NULL
        }
      } else {
        message("No scholarship data found using any extraction method")
        NULL
      }
      
      # Debug the extracted data
      if (exists("schol_data") && !is.null(schol_data)) {
        message("scholarship data class: ", paste(class(schol_data), collapse=", "))
        if (is.data.frame(schol_data)) {
          message("scholarship data has ", nrow(schol_data), " rows and ", ncol(schol_data), " columns")
        }
        schol_data
      } else {
        message("schol_data is NULL after extraction attempts")
        NULL
      }
    }
  }, error = function(e) {
    message("Error extracting scholarship data: ", e$message)
    NULL
  })
  
  # --- Get resident data ---
  resident_data <- tryCatch({
    message("Attempting to pull assessment data...")
    ass_dat <- full_api_pull(config$eval_token, config$url)
    message("Successfully pulled assessment data")
    
    message("Wrangling assessment data...")
    ass_dat <- wrangle_assessment_data(ass_dat)
    message("Assessment data wrangled")
    
    message("Creating resident data...")
    result <- create_res_data(ass_dat, rdm_dat)
    message("Resident data created with ", nrow(result), " rows")
    
    result
  }, error = function(e) {
    message("Error in resident data API pull: ", e$message)
    NULL
  })
  
  # Debug check for resident_data
  if (exists("resident_data")) {
    message("resident_data exists and is ", if(is.null(resident_data)) "NULL" else "not NULL")
  } else {
    message("WARNING: resident_data variable doesn't exist!")
    # Initialize it to prevent errors
    resident_data <- NULL
  }
  
  # --- Get milestone data ---
  miles <- tryCatch({
    message("Getting all milestones...")
    result <- get_all_milestones(config$rdm_token, config$url)
    message("All milestones retrieved")
    
    message("Filling missing resident data in milestones...")
    result <- fill_missing_resident_data(result)
    message("Resident data filled in milestones")
    
    result
  }, error = function(e) {
    message("Error loading milestones: ", e$message)
    NULL
  })
  
  # --- Process milestones ---
  p_miles <- NULL
  s_miles <- NULL
  if (!is.null(miles)) {
    p_miles <- tryCatch({
      message("Processing program milestones...")
      result <- process_milestones(miles, type = "program")
      message("Program milestones processed")
      result
    }, error = function(e) {
      message("Error processing program milestones: ", e$message)
      NULL
    })
    
    s_miles <- tryCatch({
      message("Processing self milestones...")
      result <- process_milestones(miles, type = "self")
      message("Self milestones processed")
      result
    }, error = function(e) {
      message("Error processing self milestones: ", e$message)
      NULL
    })
  }
  
  # --- Create result list ---
  message("Preparing return value with all data components")
  
  # Make sure all variables exist before creating the result list
  # If any variable doesn't exist, initialize it to NULL
  for (var_name in c("rdm_dict", "ass_dict", "resident_data", "miles", 
                     "ilp_data", "s_eval_data", "schol_data", "p_miles", "s_miles")) {
    if (!exists(var_name)) {
      message(paste("WARNING:", var_name, "variable doesn't exist, initializing to NULL"))
      assign(var_name, NULL)
    }
  }
  
  result_list <- list(
    rdm_dict = rdm_dict,
    ass_dict = ass_dict,
    resident_data = resident_data,
    miles = miles,
    ilp = ilp_data,  # Make sure to include ilp data here
    s_eval = s_eval_data,
    schol_data = schol_data,
    p_miles = p_miles,
    s_miles = s_miles,
    url = config$url,
    eval_token = config$eval_token,
    rdm_token = config$rdm_token,
    fac_token = config$fac_token
  )
  
  # Final debugging check of the return value
  message("Return list contains these keys: ", paste(names(result_list), collapse=", "))
  message("ILP data in return list is NULL? ", is.null(result_list$ilp))
  
  message("=== FINISHED load_imres_data FUNCTION ===")
  return(result_list)
}

# ---------- GLOBAL APP DATA MANAGEMENT ----------
# Define a variable to hold app data
app_data_store <- NULL

# Function to ensure data is loaded
ensure_data_loaded <- function() {
  if (is.null(app_data_store)) {
    # Only initialize data when needed
    message("Starting data load process...")
    config <- initialize_app_config()
    message("Config initialized")
    app_data_store <<- load_imres_data(config)
    message("Data loaded")
    
    # Add final check after data is loaded
    message("FINAL CHECK: app_data_store contains these keys:", paste(names(app_data_store), collapse=", "))
    message("FINAL CHECK: ILP data is NULL?", is.null(app_data_store$ilp))
  }
  return(app_data_store)
}