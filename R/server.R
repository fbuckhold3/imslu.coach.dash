# ============================================================================
# ENHANCED COACH APP - STREAMLINED SERVER
# server.R
# ============================================================================

server <- function(input, output, session) {
  
  # ============================================================================
  # INITIALIZATION
  # ============================================================================
  
  showNotification(
    "Loading enhanced coaching application...",
    id = "loading",
    type = "message",
    duration = NULL
  )
  
  # ============================================================================
  # REACTIVE VALUES
  # ============================================================================
  values <- reactiveValues(
    is_authenticated = FALSE,
    selected_coach = NULL,
    selected_resident = NULL,
    review_type = NULL,
    current_period = NULL,
    redcap_period = NULL,
    redcap_instance = NULL,
    gmed_tested = FALSE,
    dropdown_updated = FALSE,
    current_step = 1,
    total_steps = 8,
    step_data = list(),
    period_override_active = FALSE
  )
  
  # ============================================================================
  # INITIALIZE COMPONENTS
  # ============================================================================
  
  # Initialize authentication
  initialize_authentication(input, output, session, values)
  
  # Initialize data processing and get reactive expressions
  data_reactives <- initialize_data_processing(input, output, session, values)
  processed_resident_data <- data_reactives$processed_resident_data
  
  # Initialize table generation
  initialize_table_generation(input, output, session, values, processed_resident_data)
  
  # Initialize navigation
  initialize_navigation(input, output, session, values, processed_resident_data)
  
  # Initialize milestone visualization
  initialize_milestone_visualization(input, output, session, values)
  
  # Initialize evaluation data displays
  initialize_evaluation_displays(input, output, session, values)
  
  # Initialize debug outputs
  initialize_debug_outputs(input, output, session, values)
  
  # Initialize submission handlers
  initialize_submission_handlers(input, output, session, values)
  
  # ============================================================================
  # ENHANCED REVIEW CONTENT RENDERING
  # ============================================================================
  output$review_content <- renderUI({
    req(values$selected_resident)
    
    message("=== RENDERING ENHANCED REVIEW CONTENT ===")
    message("Resident: ", values$selected_resident$name)
    message("Level: ", values$selected_resident$level)
    message("Step: ", values$current_step)
    
    # Check if this is an intern intro review
    is_intro <- tryCatch({
      is_intern_intro_review_enhanced(values$selected_resident$level, values$current_period)
    }, error = function(e) {
      message("Error in intern intro check: ", e$message)
      FALSE
    })
    
    if (is_intro) {
      message("Creating enhanced intern intro interface...")
      
      # Use enhanced intern intro module with gmed components
      tryCatch({
        create_enhanced_intern_intro_interface(
          resident_name = values$selected_resident$name,
          app_data = app_data()
        )
      }, error = function(e) {
        message("Error creating intern intro interface: ", e$message)
        
        # Enhanced fallback interface
        create_enhanced_intern_intro_interface(values$selected_resident$name, app_data())
      })
      
    } else {
      message("Creating enhanced regular review interface...")
      
      # Enhanced regular review interface with step-based navigation
      create_enhanced_review_step(values$current_step, values$selected_resident, app_data())
    }
  })
  
  # ============================================================================
  # SESSION CLEANUP
  # ============================================================================
  session$onSessionEnded(function() {
    message("Enhanced coach app session ended")
  })
}