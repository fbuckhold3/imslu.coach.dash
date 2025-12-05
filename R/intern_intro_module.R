# intern_intro_module.R - COMPLETE CLEANED VERSION
# Keep this file in your coach app, not in gmed package

# ============================================================================
# INTERN INTRO DATA PROCESSING FUNCTIONS
# ============================================================================

#' Get intern intro self-evaluation data for a resident
#' @param resident_name Name of the resident
#' @param app_data Application data containing s_eval data
#' @return List containing processed intern intro data
get_intern_intro_data <- function(resident_name, app_data) {
  message("Getting intern intro data for: ", resident_name)
  
  # Initialize empty structure
  intro_data <- list(
    goals = list(goal1 = "", goal2 = "", goal3 = ""),
    preparedness = list(),
    topics = list(selected = character(0), other = ""),
    learning_styles = list(selected = character(0), other = ""),
    concerns = "",
    career_path = list(selected = character(0), other = ""),
    fellowship = list(selected = character(0), other = ""),
    track = list(interested = FALSE, types = character(0)),
    has_data = FALSE
  )
  
  # STEP 1: Find the resident's record_id from the resident_data
  message("Step 1: Finding record_id for ", resident_name)
  
  resident_record <- app_data$resident_data %>%
    filter(name == resident_name) %>%
    slice(1)
  
  if (nrow(resident_record) == 0) {
    message("❌ Resident not found in resident_data")
    return(intro_data)
  }
  
  resident_record_id <- resident_record$record_id
  message("✅ Found record_id: ", resident_record_id)
  
  # STEP 2: Find s_eval records
  message("Step 2: Finding S Eval records")
  
  s_eval_data <- app_data$raw_data %>%
    filter(!is.na(redcap_repeat_instrument) & redcap_repeat_instrument == "S Eval")
  
  message("Found ", nrow(s_eval_data), " total S Eval records")
  
  if (nrow(s_eval_data) == 0) {
    message("❌ No S Eval records found")
    return(intro_data)
  }
  
  # STEP 3: Find the specific resident's s_eval record using record_id
  message("Step 3: Finding S Eval record for record_id ", resident_record_id)
  
  resident_s_eval <- s_eval_data %>%
    filter(record_id == resident_record_id) %>%
    arrange(desc(s_e_date)) %>%
    slice(1)
  
  if (nrow(resident_s_eval) == 0) {
    message("❌ No S Eval data found for record_id: ", resident_record_id)
    return(intro_data)
  }
  
  message("✅ Found S Eval data for ", resident_name, " (record_id: ", resident_record_id, ")")
  intro_data$has_data <- TRUE
  
  row <- resident_s_eval[1, ]
  
  # Helper function
  safe_extract <- function(value) {
    if (is.null(value) || is.na(value) || value == "") {
      return("")
    }
    return(as.character(value))
  }
  
  # Extract goals
  goal1 <- safe_extract(row$s_e_ume_goal1)
  goal2 <- safe_extract(row$s_e_ume_goal2)
  goal3 <- safe_extract(row$s_e_ume_goal3)
  
  intro_data$goals$goal1 <- goal1
  intro_data$goals$goal2 <- goal2
  intro_data$goals$goal3 <- goal3
  
  message("  Goal 1: '", goal1, "'")
  message("  Goal 2: '", goal2, "'")
  message("  Goal 3: '", goal3, "'")
  
  # Extract preparedness ratings (as text)
  prep_count <- 0
  prep_fields <- grep("^s_e_prep_[0-9]+$", names(row), value = TRUE)
  
  for (field in prep_fields) {
    value <- safe_extract(row[[field]])
    if (value != "" && value != "NA") {
      # Extract number from field name
      field_num <- gsub("s_e_prep_", "", field)
      intro_data$preparedness[[field_num]] <- value
      prep_count <- prep_count + 1
      message("  Added preparedness: field ", field_num, " = '", value, "'")
    }
  }
  
  message("  Preparedness ratings found: ", prep_count)
  
  # Extract topic selections (look for non-empty values)
  selected_topics <- character(0)
  topic_fields <- grep("^s_e_topic_sel___[0-9]+$", names(row), value = TRUE)
  
  for (field in topic_fields) {
    value <- safe_extract(row[[field]])
    # If the field has a value (not NA, empty, or just the topic name), consider it selected
    if (value != "" && value != "NA" && !is.na(value)) {
      topic_num <- gsub("s_e_topic_sel___", "", field)
      selected_topics <- c(selected_topics, topic_num)
      message("  Selected topic ", topic_num, ": '", value, "'")
    }
  }
  
  intro_data$topics$selected <- selected_topics
  intro_data$topics$other <- safe_extract(row$s_e_topic_oth)
  message("  Topics selected: ", length(selected_topics))
  
  # Extract learning styles (look for non-empty values)
  selected_styles <- character(0)
  style_fields <- grep("^s_e_learn_style___[0-9]+$", names(row), value = TRUE)
  
  for (field in style_fields) {
    value <- safe_extract(row[[field]])
    if (value != "" && value != "NA" && !is.na(value)) {
      style_num <- gsub("s_e_learn_style___", "", field)
      selected_styles <- c(selected_styles, style_num)
      message("  Selected learning style ", style_num, ": '", value, "'")
    }
  }
  
  intro_data$learning_styles$selected <- selected_styles
  intro_data$learning_styles$other <- safe_extract(row$s_e_learn_oth)
  message("  Learning styles selected: ", length(selected_styles))
  
  # Extract concerns
  intro_data$concerns <- safe_extract(row$s_e_ume_concern)
  message("  Concerns: '", intro_data$concerns, "'")
  
  # Extract career paths (look for non-empty values)
  selected_career <- character(0)
  career_fields <- grep("^s_e_career_path___[0-9]+$", names(row), value = TRUE)
  
  for (field in career_fields) {
    value <- safe_extract(row[[field]])
    if (value != "" && value != "NA" && !is.na(value)) {
      career_num <- gsub("s_e_career_path___", "", field)
      selected_career <- c(selected_career, career_num)
      message("  Selected career ", career_num, ": '", value, "'")
    }
  }
  
  intro_data$career_path$selected <- selected_career
  intro_data$career_path$other <- safe_extract(row$s_e_career_oth)
  
  # Extract fellowship interests
  selected_fellow <- character(0)
  fellow_fields <- grep("^s_e_fellow___[0-9]+$", names(row), value = TRUE)
  
  for (field in fellow_fields) {
    value <- safe_extract(row[[field]])
    if (value != "" && value != "NA" && !is.na(value)) {
      fellow_num <- gsub("s_e_fellow___", "", field)
      selected_fellow <- c(selected_fellow, fellow_num)
      message("  Selected fellowship ", fellow_num, ": '", value, "'")
    }
  }
  intro_data$fellowship$selected <- selected_fellow
  intro_data$fellowship$other <- safe_extract(row$s_e_fellow_oth)
  
  # Extract track information
  track_value <- safe_extract(row$s_e_track)
  intro_data$track$interested <- (track_value != "" && track_value != "NA" && !is.na(track_value))
  
  if (intro_data$track$interested) {
    selected_tracks <- character(0)
    track_fields <- grep("^s_e_track_type___[0-9]+$", names(row), value = TRUE)
    
    for (field in track_fields) {
      value <- safe_extract(row[[field]])
      if (value != "" && value != "NA" && !is.na(value)) {
        track_num <- gsub("s_e_track_type___", "", field)
        selected_tracks <- c(selected_tracks, track_num)
      }
    }
    intro_data$track$types <- selected_tracks
  }
  
  message("✅ Successfully extracted intern intro data for ", resident_name)
  
  return(intro_data)
}

# ============================================================================
# DISPLAY FUNCTIONS (WORKING VERSIONS)
# ============================================================================
# 
#' Get fellowship labels from data dictionary
#' @param rdm_dict RDM 2.0 data dictionary
#' @return Named vector of fellowship choice labels
get_fellowship_labels_safe <- function(rdm_dict) {
  tryCatch({
    if (is.null(rdm_dict) || nrow(rdm_dict) == 0) {
      return(create_fallback_fellowship_labels())
    }
    
    # Use the correct API column names
    field_col <- "field_name"
    choices_col <- "select_choices_or_calculations"
    
    if (field_col %in% names(rdm_dict) && choices_col %in% names(rdm_dict)) {
      fellowship_field <- rdm_dict %>%
        filter(.data[[field_col]] == "s_e_fellow")
      
      if (nrow(fellowship_field) > 0) {
        choices_text <- fellowship_field[[choices_col]][1]
        
        if (!is.na(choices_text) && choices_text != "") {
          # Parse choices
          labels <- parse_redcap_choices_safe(choices_text)
          
          if (length(labels) > 0) {
            message("Successfully extracted ", length(labels), " fellowship labels from data dictionary")
            return(labels)
          }
        }
      }
    }
    
    message("Fellowship field not found in data dictionary, using fallback")
    return(create_fallback_fellowship_labels())
    
  }, error = function(e) {
    message("Error in get_fellowship_labels_safe: ", e$message)
    return(create_fallback_fellowship_labels())
  })
}

#' Create fallback fellowship labels
create_fallback_fellowship_labels <- function() {
  return(c(
    "1" = "Cardiology",
    "2" = "Endocrinology", 
    "3" = "Gastroenterology",
    "4" = "Hematology/Oncology",
    "5" = "Infectious Disease",
    "6" = "Nephrology",
    "7" = "Pulmonary/Critical Care",
    "8" = "Rheumatology",
    "9" = "Geriatrics",
    "10" = "Hospitalist Medicine",
    "11" = "Not interested in fellowship",
    "12" = "Still exploring"
  ))
}


#' Create preparedness display - shows text values sorted by preparation level
create_preparedness_display_safe <- function(intro_data, rdm_dict) {
  tryCatch({
    prep_labels <- get_preparedness_labels_safe(rdm_dict)
    
    # Check if we have any preparedness data at all
    if (is.null(intro_data$preparedness) || length(intro_data$preparedness) == 0) {
      return(div(
        class = "preparedness-display mb-4",
        div(
          class = "card shadow-sm",
          div(
            class = "card-header bg-primary text-white",
            h5("📊 Preparedness Self-Assessment", class = "mb-0 card-title")
          ),
          div(
            class = "card-body",
            div(class = "text-muted font-italic text-center py-3", 
                icon("info-circle"), " No preparedness ratings provided")
          )
        )
      ))
    }
    
    # Create data frame for sorting
    prep_data <- data.frame(
      key = character(0),
      area_label = character(0),
      rating_text = character(0),
      sort_order = numeric(0),
      stringsAsFactors = FALSE
    )
    
    # Get the range of preparedness items actually present in data
    prep_keys <- names(intro_data$preparedness)
    
    for (key in prep_keys) {
      rating_text <- intro_data$preparedness[[key]]
      
      # Skip if rating is empty
      if (is.null(rating_text) || rating_text == "" || is.na(rating_text)) {
        next
      }
      
      # Get label for this preparedness area
      area_key <- as.character(key)
      area_label <- if (area_key %in% names(prep_labels)) {
        prep_labels[area_key]
      } else {
        paste("Preparedness Area", key)
      }
      
      # Assign sort order (lower = shows first)
      sort_order <- if (grepl("not.*prepared", tolower(rating_text))) {
        1  # Not prepared - show first
      } else if (grepl("slightly", tolower(rating_text))) {
        2  # Slightly prepared
      } else if (grepl("moderately", tolower(rating_text))) {
        3  # Moderately prepared
      } else if (grepl("well", tolower(rating_text))) {
        4  # Well prepared
      } else {
        5  # Very well prepared or unknown
      }
      
      prep_data <- rbind(prep_data, data.frame(
        key = key,
        area_label = area_label,
        rating_text = rating_text,
        sort_order = sort_order,
        stringsAsFactors = FALSE
      ))
    }
    
    # Sort by preparation level (least prepared first)
    prep_data <- prep_data[order(prep_data$sort_order, prep_data$area_label), ]
    
    # Create display items with FIXED COLORS for readability
    prep_items <- lapply(1:nrow(prep_data), function(i) {
      row <- prep_data[i, ]
      
      # FIXED: Better color scheme for readability
      if (grepl("not.*prepared", tolower(row$rating_text))) {
        color_class <- "text-white"  # White text for contrast
        bg_class <- "bg-danger"      # Dark red background
        icon_name <- "exclamation-triangle"
      } else if (grepl("slightly", tolower(row$rating_text))) {
        color_class <- "text-dark"   # Dark text
        bg_class <- "bg-warning bg-opacity-75"  # Light orange
        icon_name <- "exclamation-circle"
      } else if (grepl("moderately", tolower(row$rating_text))) {
        color_class <- "text-dark"   # Dark text
        bg_class <- "bg-info bg-opacity-50"     # Light blue
        icon_name <- "info-circle"
      } else if (grepl("well", tolower(row$rating_text))) {
        color_class <- "text-dark"   # Dark text
        bg_class <- "bg-success bg-opacity-50"  # Light green
        icon_name <- "check-circle"
      } else {
        color_class <- "text-dark"   # Dark text
        bg_class <- "bg-light"       # Light gray
        icon_name <- "circle"
      }
      
      div(
        class = paste("d-flex justify-content-between align-items-center p-3 mb-2 rounded", bg_class),
        div(
          class = "flex-grow-1",
          div(class = paste("fw-medium", color_class), row$area_label)
        ),
        div(
          class = paste("fw-bold d-flex align-items-center", color_class),
          icon(icon_name, class = "me-2"),
          row$rating_text
        )
      )
    })
    
    # If no items were processed, show a message
    if (length(prep_items) == 0) {
      prep_items <- list(div(class = "text-muted font-italic text-center py-3", 
                             icon("info-circle"), " No preparedness ratings found"))
    }
    
    return(div(
      class = "preparedness-display mb-4",
      div(
        class = "card shadow-sm",
        div(
          class = "card-header bg-primary text-white",
          h5("📊 Preparedness Self-Assessment", class = "mb-0 card-title"),
          tags$small("Sorted by preparation level (least prepared first)", class = "text-white-50")
        ),
        div(class = "card-body", prep_items)
      )
    ))
    
  }, error = function(e) {
    message("Error in create_preparedness_display_safe: ", e$message)
    return(div(
      class = "alert alert-warning mb-4",
      icon("exclamation-triangle"), " Error displaying preparedness data: ", e$message
    ))
  })
}

#' Create topics and learning styles display - side by side
create_topics_and_learning_display_safe <- function(intro_data, rdm_dict) {
  tryCatch({
    topic_labels <- get_topic_labels_safe(rdm_dict)
    learning_labels <- get_learning_style_labels_safe(rdm_dict)
    
    # Topics display
    topics_content <- div(
      class = "card shadow-sm h-100",
      div(
        class = "card-header bg-warning text-dark",
        h5("⚠️ Topics Feeling Least Confident About", class = "mb-0 card-title")
      ),
      div(
        class = "card-body",
        if (length(intro_data$topics$selected) > 0) {
          tagList(
            div(
              class = "list-group list-group-flush",
              lapply(intro_data$topics$selected, function(topic_num) {
                # Get the label from data dictionary if available
                topic_label <- if (topic_num %in% names(topic_labels)) {
                  topic_labels[topic_num]
                } else {
                  paste("Topic", topic_num)
                }
                div(
                  class = "list-group-item border-0 px-0 py-2",
                  icon("circle-dot", class = "text-warning me-2"),
                  topic_label
                )
              })
            ),
            if (nchar(intro_data$topics$other) > 0) {
              div(
                class = "mt-3 p-2 bg-light rounded",
                strong(icon("plus-circle", class = "text-warning me-2"), "Other: "), 
                intro_data$topics$other
              )
            }
          )
        } else {
          div(
            class = "text-muted font-italic text-center py-3", 
            icon("info-circle"), " No topics selected"
          )
        }
      )
    )
    
    # Learning styles display
    selected_styles <- character(0)
    for (style_num in intro_data$learning_styles$selected) {
      if (style_num %in% names(learning_labels)) {
        selected_styles <- c(selected_styles, learning_labels[style_num])
      } else {
        selected_styles <- c(selected_styles, paste("Learning Style", style_num))
      }
    }
    
    learning_content <- div(
      class = "card shadow-sm h-100",
      div(
        class = "card-header bg-success text-white",
        h5("🎯 Desired Learning Experiences", class = "mb-0 card-title")
      ),
      div(
        class = "card-body",
        if (length(selected_styles) > 0) {
          tagList(
            div(
              class = "list-group list-group-flush",
              lapply(selected_styles, function(style) {
                div(
                  class = "list-group-item border-0 px-0 py-2",
                  icon("check-circle", class = "text-success me-2"),
                  style
                )
              })
            ),
            if (nchar(intro_data$learning_styles$other) > 0) {
              div(
                class = "mt-3 p-2 bg-light rounded",
                strong(icon("plus-circle", class = "text-success me-2"), "Other: "), 
                intro_data$learning_styles$other
              )
            }
          )
        } else {
          div(
            class = "text-muted font-italic text-center py-3", 
            icon("info-circle"), " No learning experiences selected"
          )
        }
      )
    )
    
    # Return side-by-side layout
    return(div(
      class = "topics-learning-display mb-4",
      div(
        class = "row g-3",
        div(class = "col-md-6", topics_content),
        div(class = "col-md-6", learning_content)
      )
    ))
    
  }, error = function(e) {
    message("Error in create_topics_and_learning_display_safe: ", e$message)
    return(div(
      class = "alert alert-warning mb-4",
      icon("exclamation-triangle"), " Error displaying topics and learning styles: ", e$message
    ))
  })
}

#' Create goals display - ENHANCED VERSION
create_goals_display_safe <- function(intro_data) {
  tryCatch({
    return(div(
      class = "goals-display mb-4",
      div(
        class = "card shadow-sm",
        div(
          class = "card-header bg-info text-white",
          h5("🎯 Goals for First 6 Months", class = "mb-0 card-title")
        ),
        div(
          class = "card-body",
          if (nchar(intro_data$goals$goal1) > 0) {
            div(
              class = "goal-item mb-3 p-3 border-start border-info border-4 bg-light",
              div(class = "d-flex align-items-center mb-2",
                  span(class = "badge bg-info me-2", "1"),
                  strong("Goal #1", class = "text-info")
              ),
              div(class = "ms-4", intro_data$goals$goal1)
            )
          },
          if (nchar(intro_data$goals$goal2) > 0) {
            div(
              class = "goal-item mb-3 p-3 border-start border-info border-4 bg-light",
              div(class = "d-flex align-items-center mb-2",
                  span(class = "badge bg-info me-2", "2"),
                  strong("Goal #2", class = "text-info")
              ),
              div(class = "ms-4", intro_data$goals$goal2)
            )
          },
          if (nchar(intro_data$goals$goal3) > 0) {
            div(
              class = "goal-item mb-2 p-3 border-start border-info border-4 bg-light",
              div(class = "d-flex align-items-center mb-2",
                  span(class = "badge bg-info me-2", "3"),
                  strong("Goal #3", class = "text-info")
              ),
              div(class = "ms-4", intro_data$goals$goal3)
            )
          },
          if (all(sapply(intro_data$goals, function(x) nchar(x) == 0))) {
            div(
              class = "text-muted font-italic text-center py-3", 
              icon("info-circle"), " No goals provided"
            )
          }
        )
      )
    ))
  }, error = function(e) {
    message("Error in create_goals_display_safe: ", e$message)
    return(div(
      class = "alert alert-warning mb-4",
      icon("exclamation-triangle"), " Error displaying goals: ", e$message
    ))
  })
}

#' Create track interests and career info display
#' Create track interests and career info display - FIXED VERSION
create_track_and_career_display_safe <- function(intro_data, rdm_dict) {
  tryCatch({
    career_labels <- get_career_path_labels_safe(rdm_dict)
    fellowship_labels <- get_fellowship_labels_safe(rdm_dict)  # Add fellowship labels
    
    # Process career paths
    selected_careers <- character(0)
    for (career_num in intro_data$career_path$selected) {
      if (career_num %in% names(career_labels)) {
        selected_careers <- c(selected_careers, career_labels[career_num])
      } else {
        selected_careers <- c(selected_careers, paste("Career Path", career_num))
      }
    }
    
    # Process fellowship paths - FIXED TO USE LABELS
    selected_fellowships <- character(0)
    for (fellowship_num in intro_data$fellowship$selected) {
      if (fellowship_num %in% names(fellowship_labels)) {
        selected_fellowships <- c(selected_fellowships, fellowship_labels[fellowship_num])
      } else {
        selected_fellowships <- c(selected_fellowships, paste("Fellowship", fellowship_num))
      }
    }
    
    # Check if we have any data to display
    has_career_data <- length(selected_careers) > 0 || nchar(intro_data$career_path$other) > 0
    has_fellowship_data <- length(selected_fellowships) > 0 || nchar(intro_data$fellowship$other) > 0
    has_track_data <- intro_data$track$interested
    
    if (!has_career_data && !has_fellowship_data && !has_track_data) {
      return(div(
        class = "track-career-display mb-4",
        div(
          class = "card shadow-sm",
          div(
            class = "card-header bg-secondary text-white",
            h5("🚀 Career Path & Track Interests", class = "mb-0 card-title")
          ),
          div(
            class = "card-body",
            div(
              class = "text-muted font-italic text-center py-3", 
              icon("info-circle"), " No career or track information provided"
            )
          )
        )
      ))
    }
    
    content_items <- list()
    
    # Career paths
    if (has_career_data) {
      content_items <- append(content_items, list(
        div(
          class = "mb-3",
          div(class = "d-flex align-items-center mb-2",
              icon("briefcase", class = "text-primary me-2"),
              strong("Career Paths Considering:", class = "text-primary")
          ),
          div(
            class = "ms-4",
            if (length(selected_careers) > 0) {
              tagList(
                lapply(selected_careers, function(career) {
                  span(class = "badge bg-primary me-1 mb-1", career)
                }),
                if (nchar(intro_data$career_path$other) > 0) {
                  span(class = "badge bg-outline-primary me-1 mb-1", intro_data$career_path$other)
                }
              )
            } else if (nchar(intro_data$career_path$other) > 0) {
              span(class = "badge bg-primary", intro_data$career_path$other)
            }
          )
        )
      ))
    }
    
    # Fellowship interests - FIXED TO SHOW LABELS
    if (has_fellowship_data) {
      content_items <- append(content_items, list(
        div(
          class = "mb-3",
          div(class = "d-flex align-items-center mb-2",
              icon("graduation-cap", class = "text-success me-2"),
              strong("Fellowship Interests:", class = "text-success")
          ),
          div(
            class = "ms-4",
            tagList(
              if (length(selected_fellowships) > 0) {
                lapply(selected_fellowships, function(fellowship) {
                  span(class = "badge bg-success me-1 mb-1", fellowship)
                })
              },
              if (nchar(intro_data$fellowship$other) > 0) {
                span(class = "badge bg-outline-success me-1 mb-1", intro_data$fellowship$other)
              }
            )
          )
        )
      ))
    }
    
    # Track information
    if (has_track_data) {
      content_items <- append(content_items, list(
        div(
          class = "mb-2",
          div(class = "d-flex align-items-center mb-2",
              icon("route", class = "text-warning me-2"),
              strong("Program Track Interest:", class = "text-warning")
          ),
          div(
            class = "ms-4",
            if (length(intro_data$track$types) > 0) {
              tagList(
                span(class = "badge bg-warning text-dark me-2", "Yes, interested"),
                br(),
                div(class = "mt-2", "Specific tracks: "),
                lapply(intro_data$track$types, function(track) {
                  span(class = "badge bg-outline-warning me-1 mb-1", track)
                })
              )
            } else {
              span(class = "badge bg-warning text-dark", "Yes, but tracks not specified")
            }
          )
        )
      ))
    }
    
    return(div(
      class = "track-career-display mb-4",
      div(
        class = "card shadow-sm",
        div(
          class = "card-header bg-secondary text-white",
          h5("🚀 Career Path & Track Interests", class = "mb-0 card-title")
        ),
        div(class = "card-body", content_items)
      )
    ))
    
  }, error = function(e) {
    message("Error in create_track_and_career_display_safe: ", e$message)
    return(div(
      class = "alert alert-warning mb-4",
      icon("exclamation-triangle"), " Error displaying career and track information: ", e$message
    ))
  })
}

#' Create concerns display - ENHANCED VERSION
create_concerns_display_safe <- function(intro_data) {
  tryCatch({
    return(div(
      class = "concerns-display mb-4",
      div(
        class = "card shadow-sm",
        div(
          class = "card-header bg-light text-dark",
          h5("💭 Comments and Concerns", class = "mb-0 card-title")
        ),
        div(
          class = "card-body",
          if (nchar(intro_data$concerns) > 0) {
            div(
              class = "p-3 bg-light rounded border-start border-secondary border-4",
              icon("comment-alt", class = "text-secondary me-2"),
              intro_data$concerns
            )
          } else {
            div(
              class = "text-muted font-italic text-center py-3", 
              icon("info-circle"), " No specific concerns or comments provided"
            )
          }
        )
      )
    ))
  }, error = function(e) {
    message("Error in create_concerns_display_safe: ", e$message)
    return(div(
      class = "alert alert-warning mb-4",
      icon("exclamation-triangle"), " Error displaying concerns: ", e$message
    ))
  })
}

# ============================================================================
# HELPER FUNCTIONS FOR LABEL MAPPING
# ============================================================================

#' Parse REDCap choices string into named vector
#' @param choices_text REDCap choices string (e.g., "1, Choice 1 | 2, Choice 2")
#' @return Named vector with codes as names and labels as values
parse_redcap_choices_safe <- function(choices_text) {
  if (is.null(choices_text) || is.na(choices_text) || choices_text == "") {
    return(character(0))
  }
  
  tryCatch({
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
  }, error = function(e) {
    message("Error parsing REDCap choices: ", e$message)
    return(character(0))
  })
}

#' Create fallback preparedness labels
create_fallback_preparedness_labels <- function() {
  return(c(
    "1" = "Taking a history",
    "2" = "Performing a physical exam", 
    "3" = "Forming differential diagnoses",
    "4" = "Ordering appropriate tests",
    "5" = "Interpreting test results",
    "6" = "Making treatment decisions",
    "7" = "Communicating with patients",
    "8" = "Working with the healthcare team",
    "9" = "Managing time effectively",
    "10" = "Handling stress and workload",
    "11" = "Professional behavior",
    "12" = "Electronic health records",
    "13" = "Medical knowledge application",
    "14" = "Clinical reasoning",
    "15" = "Patient safety practices",
    "16" = "Quality improvement",
    "17" = "Practice-based learning"
  ))
}

#' Create fallback topic labels
create_fallback_topic_labels <- function() {
  return(c(
    "1" = "Cardiovascular diseases",
    "2" = "Respiratory conditions", 
    "3" = "Acute coronary syndrome",
    "4" = "Acute kidney injury",
    "5" = "Altered mental status",
    "6" = "Infectious diseases",
    "7" = "Cirrhosis",
    "8" = "Gastrointestinal disorders",
    "9" = "Diabetes",
    "10" = "Endocrine disorders",
    "11" = "Neurological conditions",
    "12" = "Emergency medicine procedures"
  ))
}

#' Create fallback learning style labels
create_fallback_learning_style_labels <- function() {
  return(c(
    "1" = "Case discussion sessions",
    "2" = "Bedside teaching",
    "3" = "Simulation exercises",
    "4" = "Reading assignments",
    "5" = "Online modules",
    "6" = "Hands-on procedures",
    "7" = "Peer teaching opportunities",
    "8" = "Research projects"
  ))
}

#' Create fallback career path labels
create_fallback_career_path_labels <- function() {
  return(c(
    "1" = "Hospital medicine",
    "2" = "Primary care/outpatient",
    "3" = "Fellowship training",
    "4" = "Academic medicine",
    "5" = "Private practice",
    "6" = "Research career",
    "7" = "Administration/leadership",
    "8" = "Still exploring options"
  ))
}

#' Get preparedness labels from data dictionary
#' @param rdm_dict RDM 2.0 data dictionary
#' @return Named vector of preparedness area labels
get_preparedness_labels_safe <- function(rdm_dict) {
  tryCatch({
    if (is.null(rdm_dict) || nrow(rdm_dict) == 0) {
      return(create_fallback_preparedness_labels())
    }
    
    # Use the correct API column names
    field_col <- "field_name"
    choices_col <- "select_choices_or_calculations"
    
    if (field_col %in% names(rdm_dict) && choices_col %in% names(rdm_dict)) {
      prep_field <- rdm_dict %>%
        filter(.data[[field_col]] == "s_e_prep_1")  # Use first prep field as template
      
      if (nrow(prep_field) > 0) {
        choices_text <- prep_field[[choices_col]][1]
        
        if (!is.na(choices_text) && choices_text != "") {
          # This would be for the rating scale (1-5), but we want the area labels
          # For now, use fallback labels for preparedness areas
          return(create_fallback_preparedness_labels())
        }
      }
    }
    
    return(create_fallback_preparedness_labels())
    
  }, error = function(e) {
    message("Error in get_preparedness_labels_safe: ", e$message)
    return(create_fallback_preparedness_labels())
  })
}

#' Get topic labels from data dictionary
#' @param rdm_dict RDM 2.0 data dictionary
#' @return Named vector of topic choice labels
get_topic_labels_safe <- function(rdm_dict) {
  tryCatch({
    if (is.null(rdm_dict) || nrow(rdm_dict) == 0) {
      return(create_fallback_topic_labels())
    }
    
    # Use the correct API column names
    field_col <- "field_name"
    choices_col <- "select_choices_or_calculations"
    
    if (field_col %in% names(rdm_dict) && choices_col %in% names(rdm_dict)) {
      topic_field <- rdm_dict %>%
        filter(.data[[field_col]] == "s_e_topic_sel")
      
      if (nrow(topic_field) > 0) {
        choices_text <- topic_field[[choices_col]][1]
        
        if (!is.na(choices_text) && choices_text != "") {
          # Parse choices
          labels <- parse_redcap_choices_safe(choices_text)
          
          if (length(labels) > 0) {
            message("Successfully extracted ", length(labels), " topic labels from data dictionary")
            return(labels)
          }
        }
      }
    }
    
    message("Topic selection field not found in data dictionary, using fallback")
    return(create_fallback_topic_labels())
    
  }, error = function(e) {
    message("Error in get_topic_labels_safe: ", e$message)
    return(create_fallback_topic_labels())
  })
}

#' Get learning style labels from data dictionary
#' @param rdm_dict RDM 2.0 data dictionary
#' @return Named vector of learning style choice labels
get_learning_style_labels_safe <- function(rdm_dict) {
  tryCatch({
    if (is.null(rdm_dict) || nrow(rdm_dict) == 0) {
      return(create_fallback_learning_style_labels())
    }
    
    # Use the correct API column names
    field_col <- "field_name"
    choices_col <- "select_choices_or_calculations"
    
    if (field_col %in% names(rdm_dict) && choices_col %in% names(rdm_dict)) {
      style_field <- rdm_dict %>%
        filter(.data[[field_col]] == "s_e_learn_style")
      
      if (nrow(style_field) > 0) {
        choices_text <- style_field[[choices_col]][1]
        
        if (!is.na(choices_text) && choices_text != "") {
          # Parse choices
          labels <- parse_redcap_choices_safe(choices_text)
          
          if (length(labels) > 0) {
            message("Successfully extracted ", length(labels), " learning style labels from data dictionary")
            return(labels)
          }
        }
      }
    }
    
    message("Learning style field not found in data dictionary, using fallback")
    return(create_fallback_learning_style_labels())
    
  }, error = function(e) {
    message("Error in get_learning_style_labels_safe: ", e$message)
    return(create_fallback_learning_style_labels())
  })
}

#' Get career path labels from data dictionary
#' @param rdm_dict RDM 2.0 data dictionary
#' @return Named vector of career path choice labels
get_career_path_labels_safe <- function(rdm_dict) {
  tryCatch({
    if (is.null(rdm_dict) || nrow(rdm_dict) == 0) {
      return(create_fallback_career_path_labels())
    }
    
    # Use the correct API column names
    field_col <- "field_name"
    choices_col <- "select_choices_or_calculations"
    
    if (field_col %in% names(rdm_dict) && choices_col %in% names(rdm_dict)) {
      career_field <- rdm_dict %>%
        filter(.data[[field_col]] == "s_e_career_path")
      
      if (nrow(career_field) > 0) {
        choices_text <- career_field[[choices_col]][1]
        
        if (!is.na(choices_text) && choices_text != "") {
          # Parse choices
          labels <- parse_redcap_choices_safe(choices_text)
          
          if (length(labels) > 0) {
            message("Successfully extracted ", length(labels), " career path labels from data dictionary")
            return(labels)
          }
        }
      }
    }
    
    message("Career path field not found in data dictionary, using fallback")
    return(create_fallback_career_path_labels())
    
  }, error = function(e) {
    message("Error in get_career_path_labels_safe: ", e$message)
    return(create_fallback_career_path_labels())
  })
}

# ============================================================================
# MAIN INTERN INTRO INTERFACE FUNCTION - ENHANCED VERSION
# ============================================================================

#' Create complete intern intro review interface - SIMPLIFIED VERSION
#' @param resident_name Name of the resident
#' @param app_data Application data containing s_eval and rdm_dict
#' @return Complete UI for intern intro review
create_intern_intro_interface_with_submit <- function(resident_name, app_data) {
  
  tryCatch({
    message("Creating intern intro interface for: ", resident_name)
    
    # Get the intro data
    intro_data <- get_intern_intro_data(resident_name, app_data)
    
    # Get data dictionary for dynamic labels
    rdm_dict <- app_data$rdm_dict
    
    return(div(
      class = "intern-intro-review",
      
      # Custom CSS for enhanced styling
      tags$style(HTML("
        .intern-intro-review {
          font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        }
        .card {
          transition: all 0.3s ease;
          border: none !important;
        }
        .card:hover {
          transform: translateY(-2px);
          box-shadow: 0 8px 25px rgba(0,0,0,0.15) !important;
        }
        .goal-item {
          transition: all 0.2s ease;
        }
        .goal-item:hover {
          background-color: #f8f9fa !important;
        }
        .list-group-item {
          transition: all 0.2s ease;
        }
        .list-group-item:hover {
          background-color: #f8f9fa;
          padding-left: 1rem !important;
        }
        .badge {
          font-size: 0.85em;
        }
        .coach-input-section .card {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
        }
      ")),
      
      # Header - SIMPLIFIED
      div(
        class = "alert mb-4",
        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border: none; color: white;",
        div(class = "d-flex align-items-center",
            icon("user-graduate", class = "me-3", style = "font-size: 2rem;"),
            div(
              h3("🎓 Intern Introduction Review", class = "alert-heading mb-1"),
              p("Complete review for new interns during their introduction period", class = "mb-0 opacity-75")
            )
        )
      ),
      
      # Self-evaluation data display - NO TABS, JUST CONTENT
      if (intro_data$has_data) {
        div(
          class = "self-eval-section mb-4",
          div(class = "d-flex align-items-center mb-3",
              icon("clipboard-list", class = "text-primary me-2", style = "font-size: 1.2rem;"),
              h4("Resident's Self-Evaluation", class = "text-primary mb-0")
          ),
          
          # Goals
          create_goals_display_safe(intro_data),
          
          # Preparedness - with fixed colors
          create_preparedness_display_safe(intro_data, rdm_dict),
          
          # Topics and Learning Styles - side by side
          create_topics_and_learning_display_safe(intro_data, rdm_dict),
          
          # Track and Career Info - with fellowship labels fixed
          create_track_and_career_display_safe(intro_data, rdm_dict),
          
          # Concerns
          create_concerns_display_safe(intro_data)
        )
      } else {
        div(
          class = "alert alert-warning mb-4 shadow-sm",
          div(class = "d-flex align-items-center",
              icon("exclamation-triangle", class = "me-3", style = "font-size: 1.5rem;"),
              div(
                h5("No Self-Evaluation Data", class = "alert-heading mb-1"),
                p("No self-evaluation data found for this resident.", class = "mb-0")
              )
          )
        )
      },
      
      # Coach input section - SIMPLIFIED DESIGN
      div(
        class = "coach-input-section",
        div(
          class = "card shadow-lg",
          div(
            class = "card-header text-white",
            style = "background: linear-gradient(135deg, #28a745 0%, #20c997 100%);",
            div(class = "d-flex align-items-center",
                icon("user-edit", class = "me-2"),
                h4("Your Coaching Input", class = "mb-0")
            )
          ),
          div(
            class = "card-body bg-white",
            
            # Background
            div(
              class = "mb-4",
              div(class = "d-flex align-items-center mb-2",
                  icon("user", class = "text-info me-2"),
                  tags$label("Background Information", class = "form-label fw-bold text-dark mb-0")
              ),
              p("Where is this resident from, what are they excited about?", 
                class = "text-muted mb-2 small"),
              textAreaInput(
                "coach_intro_back",
                label = NULL,
                height = "100px",
                placeholder = "Document background information about the resident..."
              )
            ),
            
            # Coping/Adjustment
            div(
              class = "mb-4",
              div(class = "d-flex align-items-center mb-2",
                  icon("heart", class = "text-success me-2"),
                  tags$label("Coping and Adjustment", class = "form-label fw-bold text-dark mb-0")
              ),
              p("How is the resident adjusting to residency?", 
                class = "text-muted mb-2 small"),
              textAreaInput(
                "coach_coping",
                label = NULL,
                height = "100px",
                placeholder = "Document how the resident is coping..."
              )
            ),
            
            # ILP Final Summary
            div(
              class = "mb-4",
              div(class = "d-flex align-items-center mb-2",
                  icon("lightbulb", class = "text-warning me-2"),
                  tags$label("Individual Learning Plan Summary", class = "form-label fw-bold text-dark mb-0")
              ),
              p("Summarize key elements and next steps.", 
                class = "text-muted mb-2 small"),
              textAreaInput(
                "coach_ilp_final",
                label = NULL,
                height = "120px",
                placeholder = "Summarize the individual learning plan and next steps..."
              )
            ),
            
            # Submit section - SIMPLIFIED
            div(
              class = "text-center mt-4 p-3 bg-light rounded",
              actionButton(
                "submit_intern_intro",
                label = tagList(
                  icon("save", class = "me-2"),
                  "Submit Review"
                ),
                class = "btn btn-success btn-lg px-4",
                style = "font-weight: bold;"
              ),
              br(), br(),
              tags$small(
                "This will save your coaching input and return to the coach dashboard.",
                class = "text-muted"
              )
            )
          )
        )
      )
    ))
    
  }, error = function(e) {
    message("Error in create_intern_intro_interface_with_submit: ", e$message)
    
    # Return a safe fallback interface
    return(div(
      class = "alert alert-danger mb-4",
      h4("Interface Loading Error"),
      p("There was an error loading the intern introduction interface."),
      p("Error: ", e$message)
    ))
  })
}


