# ============================================================================
# DATA RETRIEVAL FUNCTIONS
# R/helpers/data_retrievers.R
# ============================================================================

#' Get Milestone Goals Data for Resident
#' 
#' Retrieves milestone goals from resident data
#' @param resident_name Name of the resident
#' @param period Current period
#' @param resident_data Resident data frame
#' @param rdm_dict Data dictionary
#' @return List of milestone goals
get_milestone_goals_data <- function(resident_name, period, resident_data, rdm_dict) {
  tryCatch({
    message("Getting milestone goals for: ", resident_name, " period: ", period)
    
    # Look for existing milestone goals in resident data
    resident_rows <- resident_data %>%
      filter(name == resident_name)
    
    if (nrow(resident_rows) == 0) {
      return(list(pc_mk_goal = NULL, sbp_pbl_goal = NULL, prof_ics_goal = NULL, 
                  pc_mk_action = NULL, sbp_pbl_action = NULL, prof_ics_action = NULL))
    }
    
    # Look for goal fields in the data
    goal_fields <- names(resident_rows)[grepl("goal|mile", names(resident_rows), ignore.case = TRUE)]
    message("Found goal fields: ", paste(goal_fields, collapse = ", "))
    
    # Extract goals if they exist
    pc_mk_goal <- NULL
    sbp_pbl_goal <- NULL
    prof_ics_goal <- NULL
    
    # Try to find goals in the data structure
    if ("coach_mile_goal" %in% names(resident_rows)) {
      goal_text <- resident_rows$coach_mile_goal[1]
      if (!is.na(goal_text) && goal_text != "") {
        pc_mk_goal <- list(column = "PC/MK Goal", value = goal_text)
      }
    }
    
    return(list(
      pc_mk_goal = pc_mk_goal,
      sbp_pbl_goal = sbp_pbl_goal,
      prof_ics_goal = prof_ics_goal,
      pc_mk_action = NULL,
      sbp_pbl_action = NULL,
      prof_ics_action = NULL
    ))
    
  }, error = function(e) {
    message("Error getting milestone goals: ", e$message)
    return(list(pc_mk_goal = NULL, sbp_pbl_goal = NULL, prof_ics_goal = NULL,
                pc_mk_action = NULL, sbp_pbl_action = NULL, prof_ics_action = NULL))
  })
}

#' Get Knowledge Topics Data for Resident
#' 
#' Retrieves knowledge assessment topics and discussion points
#' @param resident_name Name of the resident
#' @param period Current period
#' @param resident_data Resident data frame
#' @return Data frame of knowledge topics or NULL
get_knowledge_topics_data <- function(resident_name, period, resident_data) {
  tryCatch({
    message("Getting knowledge topics for: ", resident_name, " period: ", period)
    
    resident_rows <- resident_data %>%
      filter(name == resident_name)
    
    if (nrow(resident_rows) == 0) {
      return(NULL)
    }
    
    # Look for self-evaluation discussion topics
    knowledge_fields <- names(resident_rows)[grepl("s_e_|discussion|topic|knowledge", names(resident_rows), ignore.case = TRUE)]
    
    if (length(knowledge_fields) > 0) {
      message("Found knowledge-related fields: ", paste(knowledge_fields, collapse = ", "))
      
      # Try to extract discussion topics
      discussion_topics <- c()
      
      for (field in knowledge_fields) {
        values <- resident_rows[[field]][!is.na(resident_rows[[field]]) & resident_rows[[field]] != ""]
        if (length(values) > 0) {
          discussion_topics <- c(discussion_topics, values)
        }
      }
      
      if (length(discussion_topics) > 0) {
        return(data.frame(
          Topic = paste("Discussion Topic", 1:length(discussion_topics)),
          Content = discussion_topics,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    return(NULL)
    
  }, error = function(e) {
    message("Error getting knowledge topics: ", e$message)
    return(NULL)
  })
}

#' Get Scholarship Data for Resident
#' 
#' Retrieves scholarship activities and research projects
#' @param resident_name Name of the resident
#' @param resident_data Resident data frame
#' @return Data frame of scholarship activities or NULL
get_scholarship_data <- function(resident_name, resident_data) {
  tryCatch({
    message("Getting scholarship data for: ", resident_name)
    
    resident_rows <- resident_data %>%
      filter(name == resident_name)
    
    if (nrow(resident_rows) == 0) {
      return(NULL)
    }
    
    # Look for scholarship-related fields
    schol_fields <- names(resident_rows)[grepl("schol|research|project|safety|qip", names(resident_rows), ignore.case = TRUE)]
    
    if (length(schol_fields) > 0) {
      message("Found scholarship fields: ", paste(schol_fields, collapse = ", "))
      
      # Extract scholarship activities
      activities <- c()
      
      for (field in schol_fields) {
        values <- resident_rows[[field]][!is.na(resident_rows[[field]]) & resident_rows[[field]] != ""]
        if (length(values) > 0) {
          activities <- c(activities, paste(field, ":", values))
        }
      }
      
      if (length(activities) > 0) {
        return(data.frame(
          Activity = paste("Activity", 1:length(activities)),
          Description = activities,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    return(NULL)
    
  }, error = function(e) {
    message("Error getting scholarship data: ", e$message)
    return(NULL)
  })
}

#' Create Manual Plus/Delta Display
#' 
#' Fallback function for displaying plus/delta feedback
#' @param plus_text Positive feedback text
#' @param delta_text Areas for improvement text
#' @return UI element
create_manual_plus_delta_display <- function(plus_text, delta_text) {
  div(
    class = "row",
    div(
      class = "col-md-6",
      div(
        class = "card border-success",
        div(
          class = "card-header bg-success text-white",
          icon("plus-circle", class = "me-2"),
          "Positive Feedback (Plus)"
        ),
        div(
          class = "card-body",
          if (plus_text != "") {
            p(plus_text)
          } else {
            p(class = "text-muted", "No positive feedback recorded yet.")
          }
        )
      )
    ),
    div(
      class = "col-md-6",
      div(
        class = "card border-warning",
        div(
          class = "card-header bg-warning text-dark",
          icon("triangle-exclamation", class = "me-2"),
          "Areas for Growth (Delta)"
        ),
        div(
          class = "card-body",
          if (delta_text != "") {
            p(delta_text)
          } else {
            p(class = "text-muted", "No growth areas recorded yet.")
          }
        )
      )
    )
  )
}

#' Check if Intern Intro Review Enhanced
#' 
#' Enhanced function to determine if this is an intern intro review
#' @param level Resident level
#' @param period Current period
#' @return Boolean
is_intern_intro_review_enhanced <- function(level, period) {
  is_intern <- level == "Intern"
  is_intro_period <- period %in% c("Intern Intro", "7", "Intro")
  return(is_intern && is_intro_period)
}