server <- function(input, output, session) {
    redcap_url <- "https://redcapsurvey.slu.edu/api/"
    
    # Development mode flag (set to FALSE for production)
    dev_mode <- TRUE
    
    # Show loading notification
    showNotification("Loading data... may take a sec", type = "message", duration = NULL, id = "loading")
    
    # Reactive values to store session state
    values <- reactiveValues(
        is_authenticated = FALSE,
        selected_coach = NULL,
        selected_resident = NULL,
        current_tab = "pre_review",
        review_type = NULL,
        primary_review_data = NULL,
        tab_order = c("pre_review", "wellness", "career", "scholarship", 
                      "knowledge", "ilp", "summary", "milestones")
    )
    
    # Authentication logic
    #--------------------------------------------------
    # Access code validation
    observeEvent(input$submit_access, {
        # Get the access code based on mode
        stored_access_code <- if(dev_mode) "default123" else Sys.getenv("ACCESS_CODE")
        
        # Compare the input with the stored access code
        if (input$access_code == stored_access_code) {
            values$is_authenticated <- TRUE
            shinyjs::hide("login-page")
            shinyjs::show("coach-selection-page")
        } else {
            shinyjs::show("access_error")
        }
    })
    
    # Coach selection
    #--------------------------------------------------
    # Populate coach dropdown
    observe({
        # TODO: Get actual coach list from data
        coaches <- c("Coach 1", "Coach 2", "Coach 3")  # Replace with actual coach data
        updateSelectInput(session, "coach_name", choices = coaches)
    })
    
    observeEvent(input$submit_coach, {
        req(input$coach_name)
        values$selected_coach <- input$coach_name
        shinyjs::hide("coach-selection-page")
        shinyjs::show("resident-selection-page")
    })
    
    # Period selection module
    #--------------------------------------------------
    output$period_select_ui <- renderUI({
        # TODO: Insert period selection module
        # This will be replaced with your actual period selection module
        selectInput("period", "Select Period", choices = c("Period 1", "Period 2", "Period 3"))
    })
    
    # Resident selection
    #--------------------------------------------------
    # Render resident table
    output$resident_table <- DT::renderDataTable({
        req(values$selected_coach)
        # TODO: Filter residents by selected coach
        # This will be replaced with actual filtered resident data
        data.frame(
            name = c("Resident 1", "Resident 2", "Resident 3"),
            access_code = c("R001", "R002", "R003"),
            year = c("PGY-1", "PGY-2", "PGY-3"),
            coach = rep(values$selected_coach, 3)
        )
    }, selection = 'single')
    
    observeEvent(input$submit_resident, {
        req(input$resident_table_rows_selected)
        
        # Get selected resident data
        # TODO: Replace with actual data extraction
        resident_data <- data.frame(
            name = c("Resident 1", "Resident 2", "Resident 3"),
            access_code = c("R001", "R002", "R003"),
            year = c("PGY-1", "PGY-2", "PGY-3"),
            coach = rep(values$selected_coach, 3)
        )[input$resident_table_rows_selected, ]
        
        values$selected_resident <- resident_data
        values$review_type <- input$review_type
        
        # Show appropriate review content
        shinyjs::hide("resident-selection-page")
        shinyjs::show("review-pages")
        
        if (values$review_type == "primary") {
            shinyjs::show("primary-review-content")
            shinyjs::hide("second-review-content")
        } else {
            shinyjs::hide("primary-review-content")
            shinyjs::show("second-review-content")
            
            # TODO: Load primary review data for second review
        }
    })
    
    # Display resident info in header
    #--------------------------------------------------
    output$display_resident_name <- renderText({
        req(values$selected_resident)
        values$selected_resident$name
    })
    
    output$display_coach_name <- renderText({
        req(values$selected_coach)
        values$selected_coach
    })
    
    output$display_access_code <- renderText({
        req(values$selected_resident)
        values$selected_resident$access_code
    })
    
    # Check if resident is an intern
    output$is_intern <- reactive({
        req(values$selected_resident)
        # TODO: Replace with actual logic to determine if resident is an intern
        return(values$selected_resident$year == "PGY-1")
    })
    outputOptions(output, "is_intern", suspendWhenHidden = FALSE)
    
    # Primary Review - Meeting Pre-review
    #--------------------------------------------------
    # Milestone plots
    output$self_milestones_plot <- renderPlot({
        req(values$selected_resident)
        # TODO: Generate self-reported milestones spider plot
        plot(1:5, 1:5, type = "n", xlab = "", ylab = "", main = "Self-Reported Milestones")
        text(3, 3, "Milestone Plot Placeholder")
    })
    
    output$program_milestones_plot <- renderPlot({
        req(values$selected_resident)
        # TODO: Generate program milestones spider plot
        plot(1:5, 1:5, type = "n", xlab = "", ylab = "", main = "Program Milestones")
        text(3, 3, "Milestone Plot Placeholder")
    })
    
    # Prior CCC notes
    output$prior_ccc_notes <- renderText({
        req(values$selected_resident)
        # TODO: Pull prior CCC notes
        "Prior CCC notes will be displayed here."
    })
    
    # Prior ILP
    output$prior_ilp <- renderText({
        req(values$selected_resident)
        # TODO: Pull prior ILP
        "Prior ILP will be displayed here."
    })
    
    # Primary Review - Data displays
    #--------------------------------------------------
    # Career plan data
    output$career_data <- renderText({
        req(values$selected_resident)
        # TODO: Pull career plan data
        "Career plan data will be displayed here."
    })
    
    # Scholarship data
    output$scholarship_data <- renderText({
        req(values$selected_resident)
        # TODO: Pull scholarship data
        "Scholarship data will be displayed here."
    })
    
    # Exam data
    output$exam_data <- renderText({
        req(values$selected_resident)
        # TODO: Pull exam data
        "Exam and knowledge acquisition data will be displayed here."
    })
    
    # ILP data
    output$ilp_data <- renderText({
        req(values$selected_resident)
        # TODO: Pull ILP data
        "ILP data will be displayed here."
    })
    
    # Milestone module
    output$milestone_module_ui <- renderUI({
        req(values$selected_resident)
        # TODO: Insert milestone module UI
        # This will be replaced with your actual milestone module
        div(
            p("Milestone module will be inserted here."),
            actionButton("mock_milestone_submit", "Submit Milestones (Mock)")
        )
    })
    
    # Second Review
    #--------------------------------------------------
    # Display primary review data
    output$primary_review_data <- renderText({
        req(values$selected_resident)
        # TODO: Pull primary review data
        "Primary review data will be displayed here."
    })
    
    # Navigation and tab management
    #--------------------------------------------------
    # Tab navigation
    observeEvent(input$next_tab, {
        req(values$current_tab)
        current_index <- match(values$current_tab, values$tab_order)
        if (!is.na(current_index) && current_index < length(values$tab_order)) {
            # Check if self-evaluation is completed before allowing to proceed
            if (values$current_tab == "pre_review" && !input$self_eval_completed) {
                showNotification("Please confirm the resident has completed their self-evaluation.", 
                                 type = "warning")
                return()
            }
            
            next_tab <- values$tab_order[current_index + 1]
            values$current_tab <- next_tab
            updateNavs(session, "primary_review_tabs", selected = next_tab)
            
            # Show submit button on last tab
            if (next_tab == values$tab_order[length(values$tab_order)]) {
                shinyjs::show("submit_primary_review")
                shinyjs::hide("next_tab")
            }
        }
    })
    
    observeEvent(input$prev_tab, {
        req(values$current_tab)
        current_index <- match(values$current_tab, values$tab_order)
        if (!is.na(current_index) && current_index > 1) {
            prev_tab <- values$tab_order[current_index - 1]
            values$current_tab <- prev_tab
            updateNavs(session, "primary_review_tabs", selected = prev_tab)
            
            # Hide submit button if not on last tab
            if (values$current_tab != values$tab_order[length(values$tab_order)]) {
                shinyjs::hide("submit_primary_review")
                shinyjs::show("next_tab")
            }
        }
    })
    
    # Update current tab when tabs are clicked directly
    observeEvent(input$primary_review_tabs, {
        values$current_tab <- input$primary_review_tabs
        
        # Show/hide submit button based on whether we're on the last tab
        if (values$current_tab == values$tab_order[length(values$tab_order)]) {
            shinyjs::show("submit_primary_review")
            shinyjs::hide("next_tab")
        } else {
            shinyjs::hide("submit_primary_review")
            shinyjs::show("next_tab")
        }
    })
    
    # Form submissions
    #--------------------------------------------------
    # Submit primary review
    observeEvent(input$submit_primary_review, {
        # TODO: Collect all form data and save to REDCap
        # Mock milestone submission
        observeEvent(input$mock_milestone_submit, {
            showNotification("Milestones submitted!", type = "message")
        })
        
        # Show completion page
        shinyjs::hide("review-pages")
        shinyjs::show("done-page")
    })
    
    # Submit second review
    observeEvent(input$submit_second_review, {
        # TODO: Collect second review data and save to REDCap
        
        # Show completion page
        shinyjs::hide("review-pages")
        shinyjs::show("done-page")
    })
    
    # Start new review
    observeEvent(input$start_new_review, {
        # Reset app state
        values$selected_resident <- NULL
        values$current_tab <- "pre_review"
        values$review_type <- NULL
        
        # Reset UI
        shinyjs::hide("done-page")
        shinyjs::show("resident-selection-page")
        updateNavs(session, "primary_review_tabs", selected = "pre_review")
        
        # Reset form inputs
        # TODO: Reset all input values
    })
}
