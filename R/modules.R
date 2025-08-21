# modules.R - Corrected Card Modules for Coaching App

# ============================================================================
# RESIDENT INFORMATION CARD MODULE
# ============================================================================

# UI Component for Card 2: Resident Information
card2UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        card(
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              h4("Resident Information", class = "mb-0"),
              gmed::gmed_status_badge(
                text = "Step 2 of 8",
                status = "in-progress"
              )
            )
          ),
          card_body(
            uiOutput(ns("resident_questions"))
          )
        )
      )
    )
  )
}

# Server logic for Card 2
card2Server <- function(id, resident_name, is_intro_period) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Generate dynamic questions based on whether it's an intro period
    output$resident_questions <- renderUI({
      req(resident_name())
      
      if (is_intro_period()) {
        # Introduction period questions
        tagList(
          div(
            class = "mb-4",
            h5(paste("Let's learn more about", resident_name()), 
               class = "text-primary mb-3"),
            p("For new residents, please collect information about their background:", 
              class = "text-muted")
          ),
          textAreaInput(
            ns("background"), 
            label = paste0("Where is ", resident_name(), " from, what are they excited about, and what do they do outside of the program?"),
            height = "150px",
            width = "100%",
            placeholder = "Enter information about the resident's background, interests, and activities outside the program..."
          )
        )
      } else {
        # Regular period questions (shown for all periods)
        tagList(
          div(
            class = "mb-4",
            h5(paste("Check-in with", resident_name()), 
               class = "text-primary mb-3"),
            p("Please document how the resident is progressing in residency:", 
              class = "text-muted")
          ),
          
          div(
            class = "mb-4",
            textAreaInput(
              ns("dealing_with_residency"), 
              label = paste0("How is ", resident_name(), " dealing with residency? Any concerns or issues? Are they supported?"),
              height = "120px",
              width = "100%",
              placeholder = "Document how the resident is adapting to residency responsibilities, any challenges, and support systems..."
            )
          ),
          
          textAreaInput(
            ns("wellbeing"), 
            label = paste0("How is ", resident_name(), "'s wellbeing? Any concerns or considerations?"),
            height = "120px",
            width = "100%",
            placeholder = "Document any wellbeing concerns, stress levels, or considerations that may affect the resident's performance..."
          )
        )
      }
    })
    
    # Return entered values for use in parent module/app
    return(
      reactive({
        if (is_intro_period()) {
          list(
            background = input$background %||% ""
          )
        } else {
          list(
            dealing_with_residency = input$dealing_with_residency %||% "",
            wellbeing = input$wellbeing %||% ""
          )
        }
      })
    )
  })
}

# ============================================================================
# EVALUATIONS CARD MODULE
# ============================================================================

card3UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        card(
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              h4("Rotation & Clinical Evaluations", class = "mb-0"),
              gmed::gmed_status_badge(
                text = "Step 3 of 8",
                status = "in-progress"
              )
            )
          ),
          card_body(
            # Plus/Delta display area
            div(
              class = "mb-4",
              uiOutput(ns("plus_delta_display"))
            ),
            
            # Coach assessment inputs
            div(
              class = "mt-4",
              h5("Your Assessment of Evaluations:", class = "text-primary"),
              textAreaInput(
                ns("evaluations_assessment"), 
                label = "Please enter your thoughts about how the resident has done in collecting evaluations, doing faculty evaluations, and completing peer evaluations:",
                rows = 4,
                width = "100%",
                placeholder = "Enter your assessment of the resident's evaluation performance..."
              ),
              
              textAreaInput(
                ns("evaluations_comments"), 
                label = "Comments about Plus/Delta data and the resident's reflection:",
                rows = 4,
                width = "100%",
                placeholder = "Document your discussion about the resident's evaluations. Consider: Do they have accurate self-assessment? What patterns do you see in their evaluations? What strategies would help them improve?"
              )
            )
          )
        )
      )
    )
  )
}

card3Server <- function(id, resident_name, current_period, app_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Display plus/delta data
    output$plus_delta_display <- renderUI({
      req(resident_name(), app_data())
      
      # Get self-evaluation data for plus/delta
      plus_text <- ""
      delta_text <- ""
      
      if (!is.null(app_data()$s_eval)) {
        s_eval_data <- app_data()$s_eval %>%
          filter(name == resident_name()) %>%
          arrange(desc(s_e_date)) %>%
          slice(1)
        
        if (nrow(s_eval_data) > 0) {
          plus_text <- s_eval_data$s_e_plus[1] %||% ""
          delta_text <- s_eval_data$s_e_delta[1] %||% ""
        }
      }
      
      # Use gmed plus/delta display component
      gmed::gmed_plus_delta_display(
        plus_text = plus_text,
        delta_text = delta_text
      )
    })
    
    # Return entered values
    return(
      reactive({
        list(
          evaluations_assessment = input$evaluations_assessment %||% "",
          evaluations_comments = input$evaluations_comments %||% ""
        )
      })
    )
  })
}

# ============================================================================
# MILESTONE GOALS CARD MODULE
# ============================================================================

card6UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        card(
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              h4("Milestone Goals", class = "mb-0"),
              gmed::gmed_status_badge(
                text = "Step 6 of 8",
                status = "in-progress"
              )
            )
          ),
          card_body(
            # Previous goals section
            div(
              class = "mb-4",
              h5("Previous Milestone Goals", class = "text-info"),
              uiOutput(ns("previous_goals_display"))
            ),
            
            # Current goals section
            div(
              class = "mb-4",
              h5("Current Milestone Goals", class = "text-primary"),
              
              # PC/MK Goal
              div(
                class = "mb-3",
                h6("1. Patient Care / Medical Knowledge Milestone Goal", class = "text-secondary"),
                uiOutput(ns("pc_mk_goal_display"))
              ),
              
              # SBP/PBL Goal
              div(
                class = "mb-3",
                h6("2. Systems-Based Practice / Practice-Based Learning Milestone Goal", class = "text-secondary"),
                uiOutput(ns("sbp_pbl_goal_display"))
              ),
              
              # Prof/ICS Goal
              div(
                class = "mb-3",
                h6("3. Professionalism / Interpersonal Communication Skills Milestone Goal", class = "text-secondary"),
                uiOutput(ns("prof_ics_goal_display"))
              )
            ),
            
            # Comments section
            textAreaInput(
              ns("milestone_goals_comments"), 
              label = "Comments on Milestone-Based Goals",
              rows = 5,
              width = "100%",
              placeholder = "Take a moment to comment on all the milestone-based goals. Consider: Are they appropriate for the resident's level? Do they address key areas for development? How will you help them achieve these goals?"
            )
          )
        )
      )
    )
  )
}

card6Server <- function(id, resident_name, current_period, app_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Get milestone goals data
    milestone_goals <- reactive({
      req(resident_name(), app_data())
      get_milestone_goals(
        resident_name = resident_name(),
        current_period = current_period(),
        resident_data = app_data()$resident_data
      )
    })
    
    # Display previous goals
    output$previous_goals_display <- renderUI({
      # You can implement previous goals logic here
      div(
        class = "alert alert-light",
        "Previous milestone goals will be displayed here when available."
      )
    })
    
    # Display PC/MK goal
    output$pc_mk_goal_display <- renderUI({
      goals <- milestone_goals()
      
      if (!is.null(goals$pc_mk_goal) && !is.null(goals$pc_mk_goal$value)) {
        div(
          class = "p-3 bg-light rounded",
          tags$strong("Goal: "), goals$pc_mk_goal$value,
          if (!is.null(goals$pc_mk_action)) {
            tagList(
              tags$br(), tags$br(),
              tags$strong("Action Plan: "), goals$pc_mk_action
            )
          }
        )
      } else {
        div(
          class = "text-muted",
          "No PC/MK milestone goal found for this period."
        )
      }
    })
    
    # Display SBP/PBL goal
    output$sbp_pbl_goal_display <- renderUI({
      goals <- milestone_goals()
      
      if (!is.null(goals$sbp_pbl_goal) && !is.null(goals$sbp_pbl_goal$value)) {
        div(
          class = "p-3 bg-light rounded",
          tags$strong("Goal: "), goals$sbp_pbl_goal$value,
          if (!is.null(goals$sbp_pbl_action)) {
            tagList(
              tags$br(), tags$br(),
              tags$strong("Action Plan: "), goals$sbp_pbl_action
            )
          }
        )
      } else {
        div(
          class = "text-muted",
          "No SBP/PBL milestone goal found for this period."
        )
      }
    })
    
    # Display Prof/ICS goal
    output$prof_ics_goal_display <- renderUI({
      goals <- milestone_goals()
      
      if (!is.null(goals$prof_ics_goal) && !is.null(goals$prof_ics_goal$value)) {
        div(
          class = "p-3 bg-light rounded",
          tags$strong("Goal: "), goals$prof_ics_goal$value,
          if (!is.null(goals$prof_ics_action)) {
            tagList(
              tags$br(), tags$br(),
              tags$strong("Action Plan: "), goals$prof_ics_action
            )
          }
        )
      } else {
        div(
          class = "text-muted",
          "No Prof/ICS milestone goal found for this period."
        )
      }
    })
    
    # Return entered values
    return(
      reactive({
        list(
          milestone_goals_comments = input$milestone_goals_comments %||% ""
        )
      })
    )
  })
}