# ---------- LIBRARY IMPORTS ----------
library(shiny)
library(shinyjs)
library(redcapAPI)
library(REDCapR)
library(ggplot2)
library(DT)
library(dplyr)
library(config)
library(imres)
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

# Add this to your global.R file after initialize_app_config
load_imres_data <- function(config) {
  
  rdm_dict <- tryCatch({
    cat("Attempting to get rdm_dict data dictionary...\n")
    result <- get_data_dict(config$rdm_token, config$url)
    cat("Successfully retrieved rdm_dict with", nrow(result), "rows\n")
    result
  }, error = function(e) {
    cat("Error getting rdm_dict:", e$message, "\n")
    NULL
  })
  
  ass_dict <- tryCatch({
    cat("Attempting to get ass_dict data dictionary...\n")
    result <- get_data_dict(config$eval_token, config$url)
    cat("Successfully retrieved ass_dict with", nrow(result), "rows\n")
    result
  }, error = function(e) {
    cat("Error getting ass_dict:", e$message, "\n")
    NULL
  })
  
  # Pull forms data first to get rdm_dat
  rdm_dat <- tryCatch({
    cat("Pulling forms data...\n")
    result <- forms_api_pull(config$rdm_token, config$url, 'resident_data', 'faculty_evaluation', 'ilp', 's_eval', 'scholarship')
    cat("Forms data pulled\n")
    result
  }, error = function(e) {
    cat("Error pulling forms data:", e$message, "\n")
    NULL
  })
  
  # Extract ILP data from rdm_dat
  ilp_data <- tryCatch({
    cat("DEBUG: Starting ILP data extraction\n")
    if (is.null(rdm_dat)) {
      cat("DEBUG: rdm_dat is NULL, cannot extract ILP data\n")
      NULL
    } else {
      cat("DEBUG: rdm_dat contains these keys:", paste(names(rdm_dat), collapse=", "), "\n")
      
      # Try both lowercase and capitalized key names
      if ("ilp" %in% names(rdm_dat)) {
        cat("DEBUG: Found ilp data (lowercase) in rdm_dat\n")
        ilp_data <- rdm_dat$ilp
      } else if ("ILP" %in% names(rdm_dat)) {
        cat("DEBUG: Found ILP data (capitalized) in rdm_dat\n")
        ilp_data <- rdm_dat$ILP
      } else {
        # Alternative approach: try to extract from the main data frame
        cat("DEBUG: Looking for ilp data in repeating instruments\n")
        if ("redcap_repeat_instrument" %in% names(rdm_dat)) {
          ilp_data <- rdm_dat %>%
            filter(tolower(redcap_repeat_instrument) == "ilp")
          if (nrow(ilp_data) > 0) {
            cat("DEBUG: Extracted", nrow(ilp_data), "rows of ILP data from main dataframe\n")
          } else {
            cat("DEBUG: No ILP data found in main dataframe\n")
            NULL
          }
        } else {
          cat("DEBUG: No ILP data found and no repeating instruments in rdm_dat\n")
          NULL
        }
      }
      
      # Debug the extracted data
      if (exists("ilp_data")) {
        cat("DEBUG: ILP data class:", class(ilp_data), "\n")
        if (is.data.frame(ilp_data)) {
          cat("DEBUG: ILP data has", nrow(ilp_data), "rows and", ncol(ilp_data), "columns\n")
        }
        ilp_data
      } else {
        NULL
      }
    }
  }, error = function(e) {
    cat("DEBUG: Error extracting ILP data:", e$message, "\n")
    NULL
  })
  
  # Extract s_eval data from rdm_dat
  s_eval_data <- tryCatch({
    cat("DEBUG: Starting s_eval data extraction\n")
    if (is.null(rdm_dat)) {
      cat("DEBUG: rdm_dat is NULL, cannot extract s_eval data\n")
      NULL
    } else {
      cat("DEBUG: rdm_dat contains these keys:", paste(names(rdm_dat), collapse=", "), "\n")
      
      # Try both lowercase and capitalized key names
      if ("s_eval" %in% names(rdm_dat)) {
        cat("DEBUG: Found s_eval data (lowercase) in rdm_dat\n")
        s_eval_data <- rdm_dat$s_eval
      } else if ("S_eval" %in% names(rdm_dat)) {
        cat("DEBUG: Found S_eval data (capitalized) in rdm_dat\n")
        s_eval_data <- rdm_dat$S_eval
      } else {
        # Alternative approach: try to extract from the main data frame
        cat("DEBUG: Looking for s_eval data in repeating instruments\n")
        if ("redcap_repeat_instrument" %in% names(rdm_dat)) {
          s_eval_data <- rdm_dat %>%
            filter(tolower(redcap_repeat_instrument) == "s_eval")
          if (nrow(s_eval_data) > 0) {
            cat("DEBUG: Extracted", nrow(s_eval_data), "rows of s_eval data from main dataframe\n")
          } else {
            cat("DEBUG: No s_eval data found in main dataframe\n")
            NULL
          }
        } else {
          cat("DEBUG: No s_eval data found and no repeating instruments in rdm_dat\n")
          NULL
        }
      }
      
      # Debug the extracted data
      if (exists("s_eval_data")) {
        cat("DEBUG: s_eval data class:", class(s_eval_data), "\n")
        if (is.data.frame(s_eval_data)) {
          cat("DEBUG: s_eval data has", nrow(s_eval_data), "rows and", ncol(s_eval_data), "columns\n")
        }
        s_eval_data
      } else {
        NULL
      }
    }
  }, error = function(e) {
    cat("DEBUG: Error extracting s_eval data:", e$message, "\n")
    NULL
  })
  
  # In the load_imres_data function, modify the scholarship data extraction:
  schol_data <- tryCatch({
    cat("DEBUG: Starting scholarship data extraction\n")
    if (is.null(rdm_dat)) {
      cat("DEBUG: rdm_dat is NULL, cannot extract scholarship data\n")
      NULL
    } else {
      cat("DEBUG: rdm_dat is not NULL\n")
      cat("DEBUG: rdm_dat contains these keys:", paste(names(rdm_dat), collapse=", "), "\n")
      
      # Try both "scholarship" and "Scholarship" keys
      if ("scholarship" %in% names(rdm_dat)) {
        cat("DEBUG: Found scholarship data (lowercase) in rdm_dat\n")
        schol_data <- rdm_dat$scholarship
      } else if ("Scholarship" %in% names(rdm_dat)) {
        cat("DEBUG: Found Scholarship data (capitalized) in rdm_dat\n")
        schol_data <- rdm_dat$Scholarship
      } else {
        # Alternative approach: try to extract from the main data frame
        cat("DEBUG: Looking for scholarship data in repeating instruments\n")
        if ("redcap_repeat_instrument" %in% names(rdm_dat)) {
          schol_data <- rdm_dat %>%
            filter(tolower(redcap_repeat_instrument) == "scholarship")
          if (nrow(schol_data) > 0) {
            cat("DEBUG: Extracted", nrow(schol_data), "rows of scholarship data from main dataframe\n")
          } else {
            cat("DEBUG: No scholarship data found in main dataframe\n")
            NULL
          }
        } else {
          cat("DEBUG: No scholarship data found and no repeating instruments in rdm_dat\n")
          NULL
        }
      }
      
      # Debug the extracted data
      if (exists("schol_data")) {
        cat("DEBUG: scholarship data class:", class(schol_data), "\n")
        if (is.data.frame(schol_data)) {
          cat("DEBUG: scholarship data has", nrow(schol_data), "rows and", ncol(schol_data), "columns\n")
        }
        schol_data
      } else {
        NULL
      }
    }
  }, error = function(e) {
    cat("DEBUG: Error extracting scholarship data:", e$message, "\n")
    NULL
  })
  
  # Function to safely pull resident data from REDCap API
  resident_data <- tryCatch({
    cat("Attempting to pull assessment data...\n")
    ass_dat <- full_api_pull(config$eval_token, config$url)
    cat("Successfully pulled assessment data\n")
    
    cat("Wrangling assessment data...\n")
    ass_dat <- wrangle_assessment_data(ass_dat)
    cat("Assessment data wrangled\n")
    
    # We already have rdm_dat from earlier
    cat("Creating resident data...\n")
    result <- create_res_data(ass_dat, rdm_dat)
    cat("Resident data created with", nrow(result), "rows\n")
    
    result
  }, error = function(e) {
    cat("Error in resident data API pull:", e$message, "\n")
    NULL
  })
  
  # Load milestone data
  miles <- tryCatch({
    cat("Getting all milestones...\n")
    result <- get_all_milestones(config$rdm_token, config$url)
    cat("All milestones retrieved\n")
    
    cat("Filling missing resident data in milestones...\n")
    result <- fill_missing_resident_data(result)
    cat("Resident data filled in milestones\n")
    
    result
  }, error = function(e) {
    cat("Error loading milestones:", e$message, "\n")
    NULL
  })
  
  # Process milestones
  p_miles <- NULL
  s_miles <- NULL
  if (!is.null(miles)) {
    p_miles <- tryCatch({
      cat("Processing program milestones...\n")
      result <- process_milestones(miles, type = "program")
      cat("Program milestones processed\n")
      result
    }, error = function(e) {
      cat("Error processing program milestones:", e$message, "\n")
      NULL
    })
    
    s_miles <- tryCatch({
      cat("Processing self milestones...\n")
      result <- process_milestones(miles, type = "self")
      cat("Self milestones processed\n")
      result
    }, error = function(e) {
      cat("Error processing self milestones:", e$message, "\n")
      NULL
    })
  }
  
  # Return all the data
  list(
    rdm_dict = rdm_dict,
    ass_dict = ass_dict,
    resident_data = resident_data,
    miles = miles,
    schol_data = schol_data,
    ilp = ilp_data,  # Updated to use the extracted ilp_data 
    s_eval = s_eval_data,  # Updated to use the extracted s_eval_data
    p_miles = p_miles,
    s_miles = s_miles,
    url = config$url,
    eval_token = config$eval_token,
    rdm_token = config$rdm_token,
    fac_token = config$fac_token
  )
}

# ---------- HELPER FUNCTIONS ----------
# Load helper functions
source("R/helpers.R")



