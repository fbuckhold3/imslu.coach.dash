# ============================================================================
# STEP CREATION FUNCTIONS
# R/helpers/step_creators.R
# ============================================================================

#' Create Enhanced Review Step Content
#' 
#' Main function to create step-based review interface
#' @param step Current step number (1-8)
#' @param resident Selected resident information
#' @param app_data Application data
#' @return UI element for the step
create_enhanced_review_step <- function(step, resident, app_data) {
  switch(as.character(step),
         "1" = create_step_getting_started(resident),
         "2" = create_step_coping_wellness(resident),
         "3" = create_step_evaluations(resident, app_data),
         "4" = create_step_knowledge(resident),
         "5" = create_step_scholarship(resident),
         "6" = create_step_milestone_goals(resident),
         "7" = create_step_career_planning(resident),
         "8" = create_step_summary_submission(resident),
         create_step_getting_started(resident)  # Default fallback
  )
}

#' Create Step 1: Getting Started
#' @param resident Selected resident information
#' @return UI element
create_step_getting_started <- function(resident) {
  card(
    card_header(
      div(
        class = "d-flex justify-content-between align-items-center",
        span("Step 1: Getting Started"),
        span(class = "badge bg-primary", "1/8")
      )
    ),
    card_body(
      div(
        class = "mb-4",
        h5("Pre-review Discussion", class = "text-primary"),
        p("Document your initial discussion with the resident.", class = "text-muted"),
        textAreaInput(
          "coach_pre_rev",
          label = NULL,
          placeholder = "What did you discuss before starting the formal review?",
          height = "120px"
        )
      ),
      
      div(
        class = "d-flex justify-content-between mt-4",
        div(),  # Empty div for spacing
        actionButton(
          "next_step",
          tagList(icon("arrow-right"), " Next: Coping & Wellness"),
          class = "btn-primary"
        )
      )
    )
  )
}

#' Create Step 2: Coping & Wellness
#' @param resident Selected resident information
#' @return UI element
create_step_coping_wellness <- function(resident) {
  card(
    card_header(
      div(
        class = "d-flex justify-content-between align-items-center",
        span("Step 2: Coping & Wellness"),
        span(class = "badge bg-success", "2/8")
      )
    ),
    card_body(
      div(
        class = "mb-4",
        h5("Coping with Residency", class = "text-success"),
        p("How is the resident handling the demands of residency?", class = "text-muted"),
        textAreaInput(
          "coach_coping",
          label = NULL,
          placeholder = "Document how the resident is coping with residency demands...",
          height = "120px"
        )
      ),
      
      div(
        class = "mb-4",
        h5("Wellness and Wellbeing", class = "text-success"),
        p("Any wellness concerns or positive practices to note?", class = "text-muted"),
        textAreaInput(
          "coach_wellness",
          label = NULL,
          placeholder = "Document wellness observations and any concerns...",
          height = "120px"
        )
      ),
      
      div(
        class = "d-flex justify-content-between mt-4",
        actionButton(
          "prev_step",
          tagList(icon("arrow-left"), " Previous"),
          class = "btn-outline-secondary"
        ),
        actionButton(
          "next_step",
          tagList(icon("arrow-right"), " Next: Evaluations"),
          class = "btn-primary"
        )
      )
    )
  )
}

#' Create Step 3: Evaluations
#' @param resident Selected resident information
#' @param app_data Application data
#' @return UI element
create_step_evaluations <- function(resident, app_data) {
  card(
    card_header(
      div(
        class = "d-flex justify-content-between align-items-center",
        span("Step 3: Evaluations"),
        span(class = "badge bg-info", "3/8")
      )
    ),
    card_body(
      div(
        class = "mb-4",
        h5("Plus/Delta Feedback Summary", class = "text-info"),
        uiOutput("enhanced_plus_delta_display")
      ),
      
      # Add debug section (can be removed later)
      div(
        class = "mb-4",
        details(
          summary("Debug: Data Structure Info"),
          verbatimTextOutput("debug_plus_delta_structure")
        )
      ),
      
      div(
        class = "mb-4",
        h5("Assessment of Evaluations", class = "text-info"),
        p("Your thoughts on the resident's evaluation feedback.", class = "text-muted"),
        textAreaInput(
          "coach_evaluations",
          label = NULL,
          placeholder = "How has the resident performed based on evaluations?",
          height = "120px"
        )
      ),
      
      div(
        class = "d-flex justify-content-between mt-4",
        actionButton(
          "prev_step",
          tagList(icon("arrow-left"), " Previous"),
          class = "btn-outline-secondary"
        ),
        actionButton(
          "next_step",
          tagList(icon("arrow-right"), " Next: Knowledge"),
          class = "btn-primary"
        )
      )
    )
  )
}

#' Create Step 4: Knowledge & Board Prep
#' @param resident Selected resident information
#' @return UI element
create_step_knowledge <- function(resident) {
  card(
    card_header(
      div(
        class = "d-flex justify-content-between align-items-center",
        span("Step 4: Knowledge & Board Prep"),
        span(class = "badge bg-warning", "4/8")
      )
    ),
    card_body(
      div(
        class = "mb-4",
        h5("Knowledge Assessment", class = "text-warning"),
        uiOutput("knowledge_topics_display")
      ),
      
      # Add debug section for knowledge data
      div(
        class = "mb-4",
        details(
          summary("Debug: Knowledge Data Fields"),
          verbatimTextOutput("debug_knowledge_fields")
        )
      ),
      
      div(
        class = "mb-4",
        h5("Board Preparation", class = "text-warning"),
        p("Discuss board exam preparation and knowledge gaps.", class = "text-muted"),
        textAreaInput(
          "coach_knowledge",
          label = NULL,
          placeholder = "Document knowledge strengths and areas for improvement...",
          height = "120px"
        )
      ),
      
      div(
        class = "d-flex justify-content-between mt-4",
        actionButton(
          "prev_step",
          tagList(icon("arrow-left"), " Previous"),
          class = "btn-outline-secondary"
        ),
        actionButton(
          "next_step",
          tagList(icon("arrow-right"), " Next: Scholarship"),
          class = "btn-primary"
        )
      )
    )
  )
}

#' Create Step 5: Scholarship
#' @param resident Selected resident information
#' @return UI element
create_step_scholarship <- function(resident) {
  card(
    card_header(
      div(
        class = "d-flex justify-content-between align-items-center",
        span("Step 5: Scholarship"),
        span(class = "badge bg-purple", "5/8")
      )
    ),
    card_body(
      div(
        class = "mb-4",
        h5("Scholarship Activities", class = "text-purple"),
        uiOutput("scholarship_activities_display")
      ),
      
      # Add debug section for scholarship data
      div(
        class = "mb-4",
        details(
          summary("Debug: Scholarship Data Fields"),
          verbatimTextOutput("debug_scholarship_fields")
        )
      ),
      
      div(
        class = "mb-4",
        h5("Research and Projects", class = "text-purple"),
        p("Discuss current and planned scholarly activities.", class = "text-muted"),
        textAreaInput(
          "coach_scholarship",
          label = NULL,
          placeholder = "Document scholarship progress and plans...",
          height = "120px"
        )
      ),
      
      div(
        class = "d-flex justify-content-between mt-4",
        actionButton(
          "prev_step",
          tagList(icon("arrow-left"), " Previous"),
          class = "btn-outline-secondary"
        ),
        actionButton(
          "next_step",
          tagList(icon("arrow-right"), " Next: Milestone Goals"),
          class = "btn-primary"
        )
      )
    )
  )
}

#' Create Step 6: Milestone Goals
#' @param resident Selected resident information
#' @return UI element
create_step_milestone_goals <- function(resident) {
  card(
    card_header(
      div(
        class = "d-flex justify-content-between align-items-center",
        span("Step 6: Milestone Goals"),
        span(class = "badge bg-danger", "6/8")
      )
    ),
    card_body(
      div(
        class = "mb-4",
        h5("Enhanced Milestone Visualization", class = "text-danger"),
        
        # Tab interface for different milestone views
        navset_card_tab(
          nav_panel(
            "Interactive Spider Plot",
            plotly::plotlyOutput("enhanced_milestone_plot", height = "500px")
          ),
          nav_panel(
            "Self-Assessment",
            plotOutput("self_milestone_plot", height = "400px")
          ),
          nav_panel(
            "Program Assessment", 
            plotOutput("program_milestone_plot", height = "400px")
          )
        )
      ),
      
      div(
        class = "mb-4",
        h5("Current Milestone Goals", class = "text-danger"),
        uiOutput("milestone_goals_display")
      ),
      
      # Add debug section for milestone data
      div(
        class = "mb-4",
        details(
          summary("Debug: Milestone Data Structure"),
          verbatimTextOutput("debug_milestone_structure")
        )
      ),
      
      div(
        class = "mb-4",
        h5("Goal Setting Discussion", class = "text-danger"),
        p("Set specific milestone goals for the next period.", class = "text-muted"),
        textAreaInput(
          "coach_milestone_goals",
          label = NULL,
          placeholder = "Document milestone goals and action plans...",
          height = "120px"
        )
      ),
      
      div(
        class = "d-flex justify-content-between mt-4",
        actionButton(
          "prev_step",
          tagList(icon("arrow-left"), " Previous"),
          class = "btn-outline-secondary"
        ),
        actionButton(
          "next_step",
          tagList(icon("arrow-right"), " Next: Career Planning"),
          class = "btn-primary"
        )
      )
    )
  )
}

#' Create Step 7: Career Planning
#' @param resident Selected resident information
#' @return UI element
create_step_career_planning <- function(resident) {
  card(
    card_header(
      div(
        class = "d-flex justify-content-between align-items-center",
        span("Step 7: Career Planning"),
        span(class = "badge bg-dark", "7/8")
      )
    ),
    card_body(
      div(
        class = "mb-4",
        h5("Career Development", class = "text-dark"),
        p("Discuss career goals and development plans.", class = "text-muted"),
        textAreaInput(
          "coach_career",
          label = NULL,
          placeholder = "Document career discussions and next steps...",
          height = "120px"
        )
      ),
      
      div(
        class = "d-flex justify-content-between mt-4",
        actionButton(
          "prev_step",
          tagList(icon("arrow-left"), " Previous"),
          class = "btn-outline-secondary"
        ),
        actionButton(
          "next_step",
          tagList(icon("arrow-right"), " Next: Summary"),
          class = "btn-primary"
        )
      )
    )
  )
}

#' Create Step 8: Summary & Submission
#' @param resident Selected resident information
#' @return UI element
create_step_summary_submission <- function(resident) {
  card(
    card_header(
      div(
        class = "d-flex justify-content-between align-items-center",
        span("Step 8: Summary & Submission"),
        span(class = "badge bg-success", "8/8")
      )
    ),
    card_body(
      div(
        class = "mb-4",
        h5("Overall Summary", class = "text-success"),
        p("Provide an overall summary of the coaching session.", class = "text-muted"),
        textAreaInput(
          "coach_summary",
          label = NULL,
          placeholder = "Overall summary of the coaching session...",
          height = "120px"
        )
      ),
      
      div(
        class = "mb-4",
        h5("Individual Learning Plan Summary", class = "text-success"),
        p("Summarize the learning plan discussion and next steps.", class = "text-muted"),
        textAreaInput(
          "coach_ilp_final",
          label = NULL,
          placeholder = "Summarize the individual learning plan...",
          height = "120px"
        )
      ),
      
      div(
        class = "d-flex justify-content-between mt-4",
        actionButton(
          "prev_step",
          tagList(icon("arrow-left"), " Previous"),
          class = "btn-outline-secondary"
        ),
        actionButton(
          "submit_review",
          tagList(icon("check"), " Submit Review"),
          class = "btn-success btn-lg"
        )
      )
    )
  )
}

#' Create Enhanced Intern Intro Interface
#' @param resident_name Name of the resident
#' @param app_data Application data
#' @return UI element for intern intro review
create_enhanced_intern_intro_interface <- function(resident_name, app_data) {
  card(
    card_header("Intern Introduction Review"),
    card_body(
      div(
        class = "alert alert-info mb-4",
        icon("info-circle", class = "me-2"),
        "This is an enhanced intern introduction review with milestone visualization."
      ),
      
      div(
        class = "mb-4",
        h5("Background Information", class = "text-primary"),
        p("Where is this resident from, what are they excited about?", class = "text-muted"),
        textAreaInput(
          "coach_intro_back",
          label = NULL,
          placeholder = "Document background information about the resident...",
          height = "120px"
        )
      ),
      
      div(
        class = "mb-4", 
        h5("Coping and Adjustment", class = "text-success"),
        p("How is the resident adjusting to residency?", class = "text-muted"),
        textAreaInput(
          "coach_coping", 
          label = NULL,
          placeholder = "Document how the resident is coping...",
          height = "120px"
        )
      ),
      
      div(
        class = "mb-4",
        h5("Individual Learning Plan Summary", class = "text-info"),
        p("Summarize the learning plan and next steps", class = "text-muted"),
        textAreaInput(
          "coach_ilp_final",
          label = NULL,
          placeholder = "Summarize the individual learning plan...",
          height = "120px"
        )
      ),
      
      div(
        class = "text-center mt-4",
        actionButton(
          "submit_review",
          tagList(icon("save"), " Submit Intern Intro Review"),
          class = "btn-success btn-lg"
        )
      )
    )
  )
}