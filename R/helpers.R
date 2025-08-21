# helpers.R - Functions NOT available in gmed package yet
# These are required for the coach app to work


# ============================================================================
# MILESTONE GOALS FUNCTIONS - NOT in gmed
# ============================================================================

get_milestone_goals <- function(resident_name, current_period, resident_data, rdm_dict = NULL) {
  message(paste("Getting milestone goals for:", resident_name, "period:", current_period))
  
  # Filter resident data just by name - no period filtering
  filtered_data <- resident_data %>%
    filter(name == resident_name)
  
  # If no rows found, return empty data
  if (nrow(filtered_data) == 0) {
    message("No data found for resident:", resident_name)
    return(list(
      pc_mk_goal = NULL,
      pc_mk_action = NULL,
      sbp_pbl_goal = NULL,
      sbp_pbl_action = NULL,
      prof_ics_goal = NULL,
      prof_ics_action = NULL
    ))
  }
  
  # Define exactly the columns we want to check
  pc_mk_cols <- c("pc1_r1", "pc1_r2", "pc2_r1", "pc2_r2", "pc3_r1", "pc3_r2", "pc4_r1", "pc4_r2",
                  "pc5_r1", "pc5_r2", "pc5_r3", "pc6_r1", "pc6_r2", "mk1_r1", "mk2_r1", "mk3_r1", "mk3_r2")
  
  sbp_pbl_cols <- c("sbp1_r1", "sbp1_r2", "sbp1_r3", "sbp2_r1", "sbp2_r2", "sbp2_r3", "sbp3_r1", 
                    "sbp3_r2", "pbl1_r1", "pbl2_r1", "pbl2_r2", "pbl2_r3")
  
  prof_ics_cols <- c("prof1_r1", "prof2_r1", "prof3_r1", "prof4_r1", "prof4_r2", "ics1_r1", 
                     "ics1_r2", "ics2_r1", "ics2_r2", "ics3_r1", "ics3_r2")
  
  # Function to find the first non-NA value among the columns
  find_first_non_na <- function(data, columns) {
    existing_cols <- intersect(columns, names(data))
    
    if (length(existing_cols) == 0) {
      return(NULL)
    }
    
    for (col in existing_cols) {
      for (i in 1:nrow(data)) {
        val <- data[[col]][i]
        if (!is.na(val) && val != "") {
          return(list(column = col, value = val))
        }
      }
    }
    return(NULL)
  }
  
  # Find goals
  pc_mk_goal <- find_first_non_na(filtered_data, pc_mk_cols)
  sbp_pbl_goal <- find_first_non_na(filtered_data, sbp_pbl_cols)
  prof_ics_goal <- find_first_non_na(filtered_data, prof_ics_cols)
  
  # Find action plans
  pc_mk_action <- NULL
  sbp_pbl_action <- NULL
  prof_ics_action <- NULL
  
  if ("how_pcmk" %in% names(filtered_data)) {
    for (i in 1:nrow(filtered_data)) {
      val <- filtered_data$how_pcmk[i]
      if (!is.na(val) && val != "") {
        pc_mk_action <- val
        break
      }
    }
  }
  
  if ("how_sbppbl" %in% names(filtered_data)) {
    for (i in 1:nrow(filtered_data)) {
      val <- filtered_data$how_sbppbl[i]
      if (!is.na(val) && val != "") {
        sbp_pbl_action <- val
        break
      }
    }
  }
  
  if ("how_profics" %in% names(filtered_data)) {
    for (i in 1:nrow(filtered_data)) {
      val <- filtered_data$how_profics[i]
      if (!is.na(val) && val != "") {
        prof_ics_action <- val
        break
      }
    }
  }
  
  # Return the results
  list(
    pc_mk_goal = pc_mk_goal,
    pc_mk_action = pc_mk_action,
    sbp_pbl_goal = sbp_pbl_goal,
    sbp_pbl_action = sbp_pbl_action,
    prof_ics_goal = prof_ics_goal,
    prof_ics_action = prof_ics_action
  )
}

# ============================================================================
# KNOWLEDGE DATA PROCESSING - NOT in gmed
# ============================================================================


#' Parse REDCap choices string into named vector
#' @param choices_text REDCap choices string (e.g., "1, Choice 1 | 2, Choice 2")
#' @return Named vector with codes as names and labels as values
parse_redcap_choices <- function(choices_text) {
  if (is.na(choices_text) || choices_text == "") {
    return(character(0))
  }
  
  # Split on pipe character
  choice_pairs <- strsplit(choices_text, "\\|")[[1]]
  choice_pairs <- trimws(choice_pairs)
  
  # Parse each "code, label" pair
  labels <- character(0)
  
  for (pair in choice_pairs) {
    if (grepl(",", pair)) {
      parts <- strsplit(pair, ",", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        code <- trimws(parts[1])
        label <- trimws(paste(parts[-1], collapse = ","))  # Handle labels with commas
        labels[code] <- label
      }
    }
  }
  
  return(labels)
}




# ============================================================================
# INTERN INTRO HELPER FUNCTIONS
# ============================================================================

#' Check if current review is an intern intro
#' 
#' @param resident_level Resident level (e.g., "Intern", "PGY2", "PGY3")
#' @param current_period Current review period
#' @return Logical indicating if this is an intern intro review
is_intern_intro_review <- function(resident_level, current_period) {
  is_intern <- resident_level == "Intern"
  is_intro_period <- current_period %in% c("Intern Intro", "7", "Intro")
  
  result <- is_intern && is_intro_period
  message("Intern intro check: Level=", resident_level, ", Period=", current_period, ", Result=", result)
  
  return(result)
}

# Fixed submission function in helpers.R

# Coach App Specific Submission Functions
# Add these to your coach app helpers.R file
# These use smaller gmed functions as building blocks

#' Submit coach review data using coach app logic
#' @param resident_name Name of the resident
#' @param inputs Shiny input values
#' @param app_data Application data
#' @param is_intern_intro Whether this is an intern intro review
#' @return List with submission result
submit_coach_review_coach_app <- function(resident_name, inputs, app_data, is_intern_intro = FALSE) {
  
  tryCatch({
    message("=== COACH APP SUBMISSION ===")
    message("Resident: ", resident_name)
    message("Is intern intro: ", is_intern_intro)
    
    # Find resident record
    resident_record <- app_data$resident_data %>%
      filter(name == resident_name) %>%
      slice(1)
    
    if (nrow(resident_record) == 0) {
      return(list(
        success = FALSE,
        message = paste("Could not find resident:", resident_name)
      ))
    }
    
    # Get basic info
    record_id <- as.character(resident_record$record_id)
    level <- resident_record$level %||% resident_record$Level %||% "Intern"
    
    # Determine period and instance
    if (is_intern_intro) {
      period <- "Intern Intro"
    } else {
      # Use gmed function to get current period
      period <- gmed::get_current_period()
    }
    
    # Use existing gmed function to get REDCap instance
    instance <- gmed::get_redcap_instance(
      level = level,
      period = period,
      review_type = "scheduled"
    )
    
    message("Using record_id: ", record_id)
    message("Using level: ", level)
    message("Using period: ", period)
    message("Using instance: ", instance)
    
    # Map inputs to REDCap fields
    if (is_intern_intro) {
      coach_data <- map_intern_intro_inputs_coach_app(inputs)
    } else {
      coach_data <- map_regular_review_inputs_coach_app(inputs)
    }
    
    # Add metadata
    coach_data$coach_date <- format(Sys.Date(), "%Y-%m-%d")
    coach_data$coach_period <- instance
    coach_data$coach_rev_complete <- "2"  # Complete
    
    # Build REDCap record
    redcap_record <- build_coach_redcap_record(record_id, instance, coach_data)
    
    # Submit to REDCap
    result <- submit_to_redcap_coach_app(
      redcap_url = app_data$redcap_url %||% "https://redcapsurvey.slu.edu/api/",
      redcap_token = app_data$rdm_token,
      record_data = redcap_record
    )
    
    if (result$success) {
      message("✅ Successfully submitted coach review")
      return(list(
        success = TRUE,
        message = paste("Coach review submitted for", resident_name),
        instance = instance
      ))
    } else {
      message("❌ Failed to submit coach review: ", result$message)
      return(result)
    }
    
  }, error = function(e) {
    error_msg <- paste("Coach app submission error:", e$message)
    message(error_msg)
    return(list(
      success = FALSE,
      message = error_msg
    ))
  })
}

#' Map milestone period to REDCap instance number
#' @param milestone_period Milestone period name
#' @return Instance number as character
map_milestone_to_instance <- function(milestone_period) {
  
  instance_map <- c(
    "Mid Intern" = "1",
    "End Intern" = "2",
    "Mid PGY2" = "3",
    "End PGY2" = "4",
    "Mid PGY3" = "5",
    "Graduation" = "6",
    "Graduating" = "6",
    "Intern Intro" = "7"
  )
  
  if (!is.na(milestone_period) && milestone_period %in% names(instance_map)) {
    return(instance_map[milestone_period])
  }
  
  # Fallback
  warning("Could not map milestone period '", milestone_period, "' to instance, using '1'")
  return("1")
}

#' Map intern intro inputs to coach_rev fields
#' @param inputs Shiny input values
#' @return Named list of coach review fields
map_intern_intro_inputs_coach_app <- function(inputs) {
  
  list(
    # Intern intro specific fields
    coach_intro_back = inputs$coach_intro_back %||% "",
    coach_coping = inputs$coach_coping %||% "",
    coach_ilp_final = inputs$coach_ilp_final %||% "",
    
    # Set other fields to empty
    coach_pre_rev = "",
    coach_wellness = "",
    coach_evaluations = "",
    coach_p_d_comments = "",
    coach_ls_and_topic = "",
    coach_step_board = "",
    coach_scholarship = "",
    coach_mile_goal = "",
    coach_summary = "",
    coach_career = ""
  )
}

#' Map regular review inputs to coach_rev fields
#' @param inputs Shiny input values
#' @return Named list of coach review fields
map_regular_review_inputs_coach_app <- function(inputs) {
  
  list(
    # Regular review fields
    coach_pre_rev = inputs$coach_pre_rev %||% "",
    coach_coping = inputs$coach_coping %||% "",
    coach_wellness = inputs$coach_wellness %||% "",
    coach_evaluations = inputs$coach_evaluations %||% "",
    coach_summary = inputs$coach_summary %||% "",
    coach_ilp_final = inputs$coach_ilp_final %||% "",
    
    # Set intern intro specific field to empty
    coach_intro_back = "",
    
    # Set other fields
    coach_p_d_comments = inputs$coach_p_d_comments %||% "",
    coach_ls_and_topic = inputs$coach_ls_and_topic %||% "",
    coach_step_board = inputs$coach_step_board %||% "",
    coach_scholarship = inputs$coach_scholarship %||% "",
    coach_mile_goal = inputs$coach_mile_goal %||% "",
    coach_career = inputs$coach_career %||% ""
  )
}

#' Build REDCap record for coach review submission
#' @param record_id Resident record ID
#' @param instance REDCap repeat instance number
#' @param coach_data Coach review data fields
#' @return Data frame ready for REDCap submission
build_coach_redcap_record <- function(record_id, instance, coach_data) {
  
  # Start with base record structure
  redcap_record <- data.frame(
    record_id = as.character(record_id),
    redcap_repeat_instrument = "coach_rev",
    redcap_repeat_instance = as.character(instance),
    stringsAsFactors = FALSE
  )
  
  # Add coach data fields
  for (field_name in names(coach_data)) {
    field_value <- coach_data[[field_name]]
    
    # Convert to character and handle NULL/NA
    if (is.null(field_value) || is.na(field_value)) {
      redcap_record[[field_name]] <- ""
    } else {
      redcap_record[[field_name]] <- as.character(field_value)
    }
  }
  
  message("Built REDCap record with ", nrow(redcap_record), " rows and ", ncol(redcap_record), " columns")
  
  return(redcap_record)
}

#' Submit data to REDCap API
#' @param redcap_url REDCap API URL
#' @param redcap_token REDCap API token
#' @param record_data Data frame to submit
#' @return List with submission result
submit_to_redcap_coach_app <- function(redcap_url, redcap_token, record_data) {
  
  tryCatch({
    message("Submitting to REDCap...")
    message("URL: ", redcap_url)
    message("Record rows: ", nrow(record_data))
    message("Record cols: ", ncol(record_data))
    
    # Validate inputs
    if (is.null(redcap_token) || redcap_token == "") {
      return(list(success = FALSE, message = "REDCap token is missing"))
    }
    
    if (nrow(record_data) != 1) {
      return(list(success = FALSE, message = paste("Expected 1 row, got", nrow(record_data))))
    }
    
    # Convert to JSON
    json_data <- jsonlite::toJSON(record_data, auto_unbox = TRUE, na = "string")
    message("JSON preview: ", substr(json_data, 1, 200), "...")
    
    # Submit via POST
    response <- httr::POST(
      url = redcap_url,
      body = list(
        token = redcap_token,
        content = "record",
        action = "import",
        format = "json",
        type = "flat",
        overwriteBehavior = "overwrite",
        data = json_data,
        returnContent = "count",
        returnFormat = "json"
      ),
      encode = "form"
    )
    
    status_code <- httr::status_code(response)
    response_text <- httr::content(response, "text", encoding = "UTF-8")
    
    message("HTTP Status: ", status_code)
    message("Response: ", response_text)
    
    if (status_code == 200) {
      return(list(
        success = TRUE,
        message = "Successfully submitted to REDCap",
        response = response_text
      ))
    } else {
      return(list(
        success = FALSE,
        message = paste("HTTP", status_code, ":", response_text)
      ))
    }
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      message = paste("Submission error:", e$message)
    ))
  })
}

#' Validate coach review inputs
#' @param inputs Shiny input values
#' @param is_intern_intro Whether this is intern intro review
#' @return List with validation result
validate_coach_inputs <- function(inputs, is_intern_intro = FALSE) {
  
  if (is_intern_intro) {
    # Validate intern intro fields
    missing_fields <- character(0)
    
    if (is.null(inputs$coach_intro_back) || nchar(trimws(inputs$coach_intro_back)) < 5) {
      missing_fields <- c(missing_fields, "Background Information")
    }
    
    if (is.null(inputs$coach_coping) || nchar(trimws(inputs$coach_coping)) < 5) {
      missing_fields <- c(missing_fields, "Coping and Adjustment")
    }
    
    if (is.null(inputs$coach_ilp_final) || nchar(trimws(inputs$coach_ilp_final)) < 5) {
      missing_fields <- c(missing_fields, "Individual Learning Plan Summary")
    }
    
  } else {
    # Validate regular review fields
    missing_fields <- character(0)
    
    # At least one substantive field should be filled
    substantive_fields <- c("coach_pre_rev", "coach_coping", "coach_wellness", 
                            "coach_evaluations", "coach_summary", "coach_ilp_final")
    
    filled_fields <- sum(sapply(substantive_fields, function(field) {
      !is.null(inputs[[field]]) && nchar(trimws(inputs[[field]])) > 5
    }))
    
    if (filled_fields == 0) {
      missing_fields <- c(missing_fields, "At least one substantive field must be completed")
    }
  }
  
  if (length(missing_fields) == 0) {
    return(list(valid = TRUE, message = "Validation passed"))
  } else {
    return(list(
      valid = FALSE,
      message = paste("Please complete:", paste(missing_fields, collapse = ", "))
    ))
  }
}

# Update the submit_intern_intro_data function to use coach app submission
submit_intern_intro_data <- function(resident_name, inputs, app_data) {
  
  # Validate inputs first
  validation <- validate_coach_inputs(inputs, is_intern_intro = TRUE)
  if (!validation$valid) {
    return(list(
      success = FALSE,
      message = validation$message
    ))
  }
  
  # Use coach app submission
  result <- submit_coach_review_coach_app(
    resident_name = resident_name,
    inputs = inputs,
    app_data = app_data,
    is_intern_intro = TRUE
  )
  
  return(result)
}



# ============================================================================
# ENHANCED COACH APP - HELPER FUNCTIONS
# These functions are defined outside the server to avoid scoping issues
# ============================================================================

# ============================================================================
# ENHANCED STEP-BASED REVIEW CREATION
# ============================================================================
create_enhanced_review_step <- function(step, resident, app_data) {
  
  switch(as.character(step),
         "1" = {
           # Step 1: Getting Started
           card(
             card_header("Getting Started"),
             card_body(
               div(
                 class = "alert alert-primary mb-4",
                 icon("play-circle", class = "me-2"),
                 "Welcome to the enhanced coaching review process."
               ),
               
               div(
                 class = "mb-4",
                 h5("Pre-review Discussion", class = "text-primary"),
                 p("Document your pre-review discussion with the resident", class = "text-muted"),
                 textAreaInput(
                   "coach_pre_rev",
                   label = NULL,
                   placeholder = "Document your pre-review discussion with the resident...",
                   height = "120px"
                 )
               )
             )
           )
         },
         
         "2" = {
           # Step 2: Coping & Wellness  
           card(
             card_header("Coping and Wellness Assessment"),
             card_body(
               div(
                 class = "alert alert-success mb-4",
                 icon("heart", class = "me-2"),
                 "Assess the resident's wellbeing and coping strategies."
               ),
               
               div(
                 class = "mb-4",
                 h5("How is the resident dealing with residency?", class = "text-success"),
                 p("Any concerns or issues? Are they supported?", class = "text-muted"),
                 textAreaInput(
                   "coach_coping",
                   label = NULL,
                   placeholder = "Document how the resident is coping with residency demands...",
                   height = "120px"
                 )
               ),
               
               div(
                 class = "mb-4",
                 h5("Wellness and wellbeing concerns", class = "text-warning"),
                 p("Any wellness considerations or concerns?", class = "text-muted"),
                 textAreaInput(
                   "coach_wellness",
                   label = NULL,
                   placeholder = "Any wellness or wellbeing concerns to document?",
                   height = "120px"
                 )
               )
             )
           )
         },
         
         "3" = {
           # Step 3: Evaluations with Enhanced Plus/Delta
           card(
             card_header("Rotation & Clinical Evaluations"),
             card_body(
               div(
                 class = "alert alert-info mb-4",
                 icon("clipboard-check", class = "me-2"),
                 "Review the resident's evaluation feedback with enhanced plus/delta visualization."
               ),
               
               # Enhanced Plus/Delta display
               div(
                 class = "mb-4",
                 h5("Plus/Delta Feedback", class = "text-info"),
                 div(
                   class = "d-flex justify-content-between align-items-center mb-3",
                   span("Recent evaluation feedback from rotations"),
                   actionButton("open_detailed_plus_delta", 
                                tagList(icon("external-link-alt"), " View Detailed Feedback"), 
                                class = "btn-outline-info btn-sm")
                 ),
                 uiOutput("enhanced_plus_delta_display")
               ),
               
               div(
                 class = "mb-4",
                 h5("Your Assessment of Evaluations", class = "text-primary"),
                 textAreaInput(
                   "coach_evaluations",
                   label = NULL,
                   placeholder = "Your thoughts on how the resident has done with evaluations...",
                   height = "120px"
                 )
               )
             )
           )
         },
         
         "4" = {
           # Step 4: Knowledge & Board Prep
           card(
             card_header("Knowledge and Board Preparation"),
             card_body(
               div(
                 class = "alert alert-warning mb-4",
                 icon("brain", class = "me-2"),
                 "Assess knowledge gaps and board preparation progress."
               ),
               
               div(
                 class = "mb-4",
                 h5("Knowledge Assessment", class = "text-warning"),
                 uiOutput("knowledge_topics_display")
               ),
               
               div(
                 class = "mb-4",
                 h5("Board Preparation Discussion", class = "text-primary"),
                 textAreaInput(
                   "coach_board_prep",
                   label = NULL,
                   placeholder = "Discuss board preparation strategies and progress...",
                   height = "120px"
                 )
               )
             )
           )
         },
         
         "5" = {
           # Step 5: Scholarship
           card(
             card_header("Scholarship Activities"),
             card_body(
               div(
                 class = "alert alert-secondary mb-4",
                 icon("graduation-cap", class = "me-2"),
                 "Review scholarly activities and research involvement."
               ),
               
               div(
                 class = "mb-4",
                 h5("Scholarship Discussion", class = "text-secondary"),
                 uiOutput("scholarship_activities_display")
               ),
               
               div(
                 class = "mb-4",
                 h5("Scholarship Comments", class = "text-primary"),
                 textAreaInput(
                   "coach_scholarship",
                   label = NULL,
                   placeholder = "Comments on resident's scholarly activities...",
                   height = "120px"
                 )
               )
             )
           )
         },
         
         "6" = {
           # Step 6: Enhanced Milestone Goals with Spider Plot
           card(
             card_header("Milestone Goals Assessment"),
             card_body(
               div(
                 class = "alert alert-primary mb-4",
                 icon("target", class = "me-2"),
                 "Review milestone progress with enhanced visualization."
               ),
               
               # Enhanced milestone visualization
               div(
                 class = "row mb-4",
                 div(
                   class = "col-md-6",
                   h5("Current Milestone Assessment", class = "text-primary"),
                   div(
                     class = "border rounded p-3",
                     plotly::plotlyOutput("enhanced_milestone_plot", height = "400px")
                   )
                 ),
                 div(
                   class = "col-md-6",
                   h5("Milestone Goals", class = "text-success"),
                   uiOutput("milestone_goals_display")
                 )
               ),
               
               div(
                 class = "mb-4",
                 h5("Comments on Milestone-Based Goals", class = "text-primary"),
                 textAreaInput(
                   "coach_milestone_comments",
                   label = NULL,
                   placeholder = "Comment on milestone goals and development areas...",
                   height = "120px"
                 )
               )
             )
           )
         },
         
         "7" = {
           # Step 7: Career Planning
           card(
             card_header("Career Planning Discussion"),
             card_body(
               div(
                 class = "alert alert-info mb-4",
                 icon("route", class = "me-2"),
                 "Discuss career goals and professional development."
               ),
               
               div(
                 class = "mb-4",
                 h5("Career Discussion", class = "text-info"),
                 textAreaInput(
                   "coach_career",
                   label = NULL,
                   placeholder = "Discuss career goals, fellowship interests, future plans...",
                   height = "120px"
                 )
               )
             )
           )
         },
         
         "8" = {
           # Step 8: Summary & Submission
           card(
             card_header("Final Summary and Submission"),
             card_body(
               div(
                 class = "alert alert-success mb-4",
                 icon("check-circle", class = "me-2"),
                 "Complete your review with a final summary."
               ),
               
               div(
                 class = "mb-4",
                 h5("Overall Summary", class = "text-success"),
                 textAreaInput(
                   "coach_summary",
                   label = NULL,
                   placeholder = "Overall summary of the coaching session...",
                   height = "120px"
                 )
               ),
               
               div(
                 class = "mb-4",
                 h5("Individual Learning Plan Summary", class = "text-primary"),
                 textAreaInput(
                   "coach_ilp_final",
                   label = NULL,
                   placeholder = "Summarize the individual learning plan discussion...",
                   height = "120px"
                 )
               ),
               
               div(
                 class = "text-center mt-4 p-4 bg-light rounded",
                 h5("Ready to Submit?", class = "text-primary mb-3"),
                 p("Review your entries above, then submit your coaching review.", class = "text-muted"),
                 actionButton(
                   "submit_review",
                   tagList(icon("paper-plane"), " Submit Complete Review"),
                   class = "btn-success btn-lg px-5"
                 )
               )
             )
           )
         }
  )
}

# ============================================================================
# ENHANCED INTERN INTRO INTERFACE CREATION
# ============================================================================
create_enhanced_intern_intro_interface <- function(resident_name, app_data) {
  
  # Get intro data using existing function
  intro_data <- tryCatch({
    get_intern_intro_data(resident_name, app_data)
  }, error = function(e) {
    message("Error getting intern intro data: ", e$message)
    list(has_data = FALSE)
  })
  
  card(
    card_header("Enhanced Intern Introduction Review"),
    card_body(
      # Show self-evaluation data if available
      if (intro_data$has_data) {
        div(
          class = "mb-4",
          h5("Resident's Self-Evaluation Data", class = "text-info mb-3"),
          
          # Goals display
          if (exists("create_goals_display_safe") && 
              (nchar(intro_data$goals$goal1) > 0 || nchar(intro_data$goals$goal2) > 0 || nchar(intro_data$goals$goal3) > 0)) {
            tryCatch(create_goals_display_safe(intro_data), error = function(e) NULL)
          },
          
          # Enhanced preparedness display
          if (exists("create_preparedness_display_safe")) {
            tryCatch(create_preparedness_display_safe(intro_data, app_data$rdm_dict), error = function(e) NULL)
          },
          
          # Topics and learning styles
          if (exists("create_topics_and_learning_display_safe")) {
            tryCatch(create_topics_and_learning_display_safe(intro_data, app_data$rdm_dict), error = function(e) NULL)
          }
        )
      } else {
        div(
          class = "alert alert-warning mb-4",
          icon("info-circle", class = "me-2"),
          "No self-evaluation data found for this resident."
        )
      },
      
      # Coach input section with enhanced styling
      div(
        class = "border-top pt-4",
        h5("Your Coaching Input", class = "text-primary mb-4"),
        
        div(
          class = "mb-4",
          h6("Background Information", class = "text-success"),
          p("Where is this resident from, what are they excited about?", class = "text-muted small"),
          textAreaInput(
            "coach_intro_back",
            label = NULL,
            height = "120px",
            placeholder = "Document background information about the resident..."
          )
        ),
        
        div(
          class = "mb-4",
          h6("Coping and Adjustment", class = "text-warning"),
          p("How is the resident adjusting to residency?", class = "text-muted small"),
          textAreaInput(
            "coach_coping",
            label = NULL,
            height = "120px",
            placeholder = "Document how the resident is coping..."
          )
        ),
        
        div(
          class = "mb-4",
          h6("Individual Learning Plan Summary", class = "text-info"),
          p("Summarize key elements and next steps.", class = "text-muted small"),
          textAreaInput(
            "coach_ilp_final",
            label = NULL,
            height = "120px",
            placeholder = "Summarize the individual learning plan and next steps..."
          )
        ),
        
        div(
          class = "text-center mt-4 p-4 bg-light rounded",
          actionButton(
            "submit_review",
            tagList(icon("save"), " Submit Enhanced Intern Review"),
            class = "btn-success btn-lg px-5"
          )
        )
      )
    )
  )
}

# ============================================================================
# BASIC COMPLETION STATUS CHECKING FUNCTION
# ============================================================================
check_completion_status_enhanced <- function(resident_name, period_code, app_data, level = NULL) {
  tryCatch({
    # Basic implementation - you can enhance this based on your data structure
    self_eval_complete <- FALSE
    coach_review_complete <- FALSE
    
    # Check if gmed functions are available
    if (exists("check_self_eval_complete", where = "package:gmed")) {
      self_eval_complete <- tryCatch({
        gmed::check_self_eval_complete(
          app_data$resident_data, 
          resident_name, 
          period_code
        )
      }, error = function(e) FALSE)
    }
    
    if (exists("check_coach_review_complete_status", where = "package:gmed")) {
      coach_review_complete <- tryCatch({
        if (!is.null(level)) {
          gmed::check_coach_review_complete_status(
            app_data$resident_data, 
            resident_name, 
            period_code,
            level = level
          )
        } else {
          gmed::check_coach_review_complete_status(
            app_data$resident_data, 
            resident_name, 
            period_code
          )
        }
      }, error = function(e) FALSE)
    }
    
    return(list(
      self_eval = self_eval_complete,
      coach_review = coach_review_complete
    ))
  }, error = function(e) {
    message("Error in check_completion_status_enhanced: ", e$message)
    return(list(self_eval = FALSE, coach_review = FALSE))
  })
}

# ============================================================================
# ENHANCED PLUS/DELTA CREATION
# ============================================================================
create_manual_plus_delta_display <- function(plus_text, delta_text) {
  div(
    class = "row",
    div(
      class = "col-md-6",
      div(
        class = "card border-success",
        div(class = "card-header bg-success text-white", 
            icon("plus-circle", class = "me-2"), "Strengths (Plus)"),
        div(class = "card-body",
            if (nchar(plus_text) > 0) plus_text else "No strengths feedback provided")
      )
    ),
    div(
      class = "col-md-6",
      div(
        class = "card border-warning",
        div(class = "card-header bg-warning text-dark", 
            icon("triangle-exclamation", class = "me-2"), "Areas for Improvement (Delta)"),
        div(class = "card-body",
            if (nchar(delta_text) > 0) delta_text else "No improvement feedback provided")
      )
    )
  )
}

# ============================================================================
# INTERN INTRO REVIEW DETECTION
# ============================================================================
is_intern_intro_review_enhanced <- function(resident_level, current_period) {
  is_intern <- resident_level == "Intern"
  is_intro_period <- current_period %in% c("Intern Intro", "7", "Intro")
  
  result <- is_intern && is_intro_period
  message("Enhanced intern intro check: Level=", resident_level, ", Period=", current_period, ", Result=", result)
  
  return(result)
}