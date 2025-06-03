# Use bslib for a modern look with Bootstrap 5
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    primary = imres_colors$primary,
    secondary = imres_colors$secondary,
    success = imres_colors$success,
    warning = imres_colors$warning,
    danger = imres_colors$danger,
    bg = imres_colors$background,
    fg = imres_colors$text
  ),
  
  # Enable shinyjs
  useShinyjs(),
  
  # Load CSS from separate file
  includeCSS("www/styles.css"),
  
  # Load JavaScript from separate file
  includeScript("www/app.js"),
  
  # Page title
  tags$head(
    tags$title("IMSLU Coaching")
  ),
  
  # Application header
  fluidRow(
    column(12,
           div(
             class = "p-3 bg-primary text-white",
             h1("IMSLU Coaching", class = "text-center")
           )
    )
  ),
  
  # Main content div - will be used to show/hide sections
  div(
    id = "main-content",
    
    # Login page
    div(
      id = "login-page",
      fluidRow(
        column(
          width = 6,
          offset = 3,
          card(
            card_header("Welcome to IMSLU Coaching"),
            card_body(
              p("This application is designed to facilitate coaching sessions with IMSLU residents."),
              p("Please enter your access code to continue."),
              
              textInput("access_code", "Access Code"),
              actionButton("submit_access", "Submit", class = "btn-primary"),
              
              br(), br(),
              div(id = "access_error", class = "alert alert-danger", style = "display: none;",
                  "Invalid access code. Please try again.")
            )
          )
        )
      )
    ),
    
    # Coach selection page - initially hidden
    div(
      id = "coach-selection-page",
      style = "display: none; padding-bottom: 100px;",
      fluidRow(
        column(
          width = 10,
          offset = 1,
          card(
            card_header("Select Your Profile"),
            card_body(
              div(
                class = "coach-selection-container",
                style = "min-height: 250px; padding: 30px 0;", # Make this area 2.5x taller
                # Using selectizeInput for searchable dropdown
                selectizeInput("coach_name", "Select Your Name", 
                               choices = c("Loading coach names..." = ""),
                               options = list(
                                 placeholder = "Select or type to search for your name",
                                 create = FALSE,
                                 openOnFocus = TRUE,
                                 onInitialize = I('function() { this.setValue(""); }')
                               ))
              )
            )
          )
        )
      ),
      
      # Conditional panel that appears after coach selection
      conditionalPanel(
        condition = "input.coach_name !== null && input.coach_name !== ''",
        fluidRow(
          column(
            width = 10,
            offset = 1,
            # Coach dashboard header
            div(
              class = "p-3 bg-primary text-white mt-4 mb-3",
              h2("Coaching Dashboard for: ", textOutput("dashboard_coach_name", inline = TRUE), 
                 class = "text-center")
            ),
            
            # Residents table card
            card(
              card_header("Your Assigned Residents"),
              card_body(
                # Add instructions for users
                p(
                  class = "mb-3 text-center fw-bold table-instruction",
                  "Click on a resident row to start the review process"
                ),
                
                # Residents table
                DT::dataTableOutput("coach_residents_table"),
                
                # Helper text below table
                p(
                  class = "text-muted mt-3",
                  "Primary review: You are the main coach",
                  br(),
                  "Secondary review: You are reviewing another coach's feedback"
                )
              )
            )
          )
        )
      )
    ),
    
    # Review pages - initially hidden
    div(
      id = "review-pages",
      style = "display: none;",
      
      # Resident info panel - always visible during review
      fluidRow(
        column(
          width = 12,
          div(
            class = "resident-info-panel",
            fluidRow(
              column(2, h4("Resident:", textOutput("display_resident_name", inline = TRUE))),
              column(2, h4("Level:", textOutput("display_resident_level", inline = TRUE))),
              column(3, h4("Coach:", textOutput("display_coach_name", inline = TRUE))),
              column(2, h4("Access Code:", textOutput("display_access_code", inline = TRUE))),
              column(3, h4("Period:", textOutput("display_current_period", inline = TRUE)))
            )
          )
        )
      ),
      
      # Progress bar
      fluidRow(
        column(
          width = 12,
          div(
            class = "step-indicator",
            textOutput("current_step_text")
          ),
          div(
            class = "progress review-progress",
            div(
              id = "review_progress_bar",
              class = "progress-bar progress-bar-striped progress-bar-animated",
              role = "progressbar",
              style = "width: 0%",
              "aria-valuenow" = "0",
              "aria-valuemin" = "0",
              "aria-valuemax" = "100"
            )
          )
        )
      ),
      
      # Primary Review Tabs
      div(
        id = "primary-review-content",
        style = "display: none;",
        
        navset_card_tab(
          id = "primary_review_tabs",
          
          # First nav panel - Meeting Pre-review
          nav_panel(
            title = "1. Meeting Pre-review",
            value = "pre_review",
            
            # Self-evaluation status
            uiOutput("self_eval_status_ui"),
            
            # Manual override if needed
            checkboxInput("self_eval_completed", 
                          "Override: I confirm the resident has completed their self-evaluation", 
                          FALSE),
            
            # Dashboard link
            p("Review the resident's dashboard using their access code:"),
            a("Open Resident Dashboard", 
              href = "https://fbuckhold3-imsluresidentdashboard.share.connect.posit.cloud", 
              target = "_blank", 
              class = "btn btn-info"),
            
            p("Note: it will be helpful to have this open during coaching session to review evaluations and progress"),
            
            # Milestone spider plots with explanatory text
            card(
              card_header("Milestone Assessments"),
              card_body(
                p("Review the resident's self-assessment for the current period and the program assessment from the previous period."),
                fluidRow(
                  column(6, 
                         h5("Current Self-Assessment"),
                         plotOutput("self_milestones_plot", height = "400px")
                  ),
                  column(6, 
                         h5("Previous Program Assessment"),
                         plotOutput("program_milestones_plot", height = "400px")
                  )
                )
              )
            ),
            
            # Prior CCC notes
            card(
              card_header("Prior CCC Notes"),
              card_body(
                uiOutput("prior_ccc_notes")
              )
            ),
            
            # Prior ILP
            card(
              card_header("Prior ILP"),
              card_body(
                uiOutput("prior_ilp")
              )
            ),
            
            # Discussion points - moved to its own card for more emphasis
            card(
              card_header("Discussion Points"),
              card_body(
                p("Enter 1-3 key points to discuss with the resident based on your review of their data."),
                textAreaInput(
                  "discussion_points", 
                  NULL,  # No label needed here since it's in the card header
                  rows = 5, 
                  placeholder = "Enter discussion points here...",
                  width = "100%"
                )
              )
            )
          ),
          
          # Second nav panel - Wellness
          nav_panel(
            title = "2. Wellness",
            value = "wellness",
            
            # Intern intro section - shows only for interns in intro period
            conditionalPanel(
              condition = "output.is_intern_intro === true",
              card(
                card_header("Resident Background"),
                card_body(
                  p("For new residents, please collect information about their background:"),
                  textAreaInput(
                    "resident_background", 
                    label = "Where is the resident from, what are they excited about, and what do they do outside of the program?",
                    rows = 5,
                    width = "100%",
                    placeholder = "Enter information about resident's background, interests, and activities outside the program..."
                  )
                )
              )
            ),
            
            # These questions show for all residents (both interns and other levels)
            card(
              card_header("Residency Progress"),
              card_body(
                textAreaInput(
                  "dealing_with_residency", 
                  label = "How is the resident dealing with residency? Any concerns or issues? Are they supported?",
                  rows = 5,
                  width = "100%",
                  placeholder = "Document how the resident is adapting to residency responsibilities, any challenges, and support systems..."
                )
              )
            ),
            
            card(
              card_header("Resident Wellbeing"),
              card_body(
                textAreaInput(
                  "resident_wellbeing", 
                  label = "How is the resident's wellbeing? Any concerns or considerations?",
                  rows = 5,
                  width = "100%",
                  placeholder = "Document any wellbeing concerns, stress levels, or considerations that may affect the resident's performance..."
                )
              )
            )
          ),
          
          # Third nav panel - Evaluations
          nav_panel(
            title = "3. Evaluations",
            value = "evaluations",
            
            card(
              card_header(
                tagList(
                  "Rotation & Clinical Evaluations",
                  actionButton("open_eval_modal", label = NULL, icon = icon("table"), 
                               class = "btn btn-link card-header-action", title = "View Plus/Delta Assessment Table")
                )
              ),
              card_body(
                p("Review the resident's evaluations from rotations, outpatient clinics, and other experiences."),
                
                # Link to resident dashboard for evaluations
                div(
                  class = "mb-4",
                  p("Access the resident's evaluation dashboard for more detailed information:"),
                  a("Open Resident Dashboard", 
                    href = "https://fbuckhold3-imsluresidentdashboard.share.connect.posit.cloud", 
                    target = "_blank", 
                    class = "btn btn-outline-primary btn-sm")
                ),
                
                # Display the resident's self-reflection on strengths
                div(
                  class = "mt-4",
                  h4(textOutput("resident_plus_header"), class = "text-success"), 
                  div(
                    class = "p-3 border-left border-success bg-light",
                    style = "border-left: 4px solid #28a745 !important;",
                    verbatimTextOutput("resident_plus_assessment", placeholder = TRUE)
                  )
                ),
                
                # Display the resident's self-reflection on areas for improvement
                div(
                  class = "mt-4 mb-4",
                  h4(textOutput("resident_delta_header"), class = "text-danger"),
                  div(
                    class = "p-3 border-left border-danger bg-light",
                    style = "border-left: 4px solid #dc3545 !important;",
                    verbatimTextOutput("resident_delta_assessment", placeholder = TRUE)
                  )
                ),
                
                # Button to view the plus/delta table
                div(
                  class = "text-center mt-4",
                  actionButton(
                    "reopen_eval_modal", 
                    "View Full Plus/Delta Table", 
                    icon = icon("table"), 
                    class = "btn-secondary"
                  )
                ),
                
                # Divider
                hr(),
                
                # Coach's assessment of resident's evaluations
                h4("Your Assessment of Evaluations:", class = "mt-4"),
                textAreaInput(
                  "evaluations_assessment", 
                  label = "Please enter your thoughts about how the resident has done in collecting evaluations, doing faculty evaluations, and completing peer evaluations:",
                  rows = 4,
                  width = "100%",
                  placeholder = "Enter your assessment of the resident's evaluation performance..."
                ),
                
                # Coach's comments about plus/delta feedback
                textAreaInput(
                  "evaluations_comments", 
                  label = "Comments about Plus/Delta data and the resident's reflection:",
                  rows = 4,
                  width = "100%",
                  placeholder = "Document your discussion about the resident's evaluations. Consider: Do they have accurate self-assessment? What patterns do you see in their evaluations? What strategies would help them improve?"
                )
              )
            )
          ),
          
          # Fourth nav panel - Exams & Knowledge
          nav_panel(
            title = "4. Exams & Knowledge",
            value = "knowledge",
            
            # First card: Topics and Learning Styles
            card(
              card_header("Knowledge Assessment"),
              card_body(
                # Render the topics and learning styles lists
                uiOutput("knowledge_topics_ui"),
                
                # Comments on topics and learning styles
                textAreaInput(
                  "knowledge_topics_comments", 
                  label = "Comments on topics and learning styles:",
                  rows = 3,
                  width = "100%",
                  placeholder = "Enter your comments about the resident's identified challenging topics and preferred learning styles..."
                )
              )
            ),
            
            # Second card: Exam and Board Prep Data
            card(
              card_header("Exam Performance & Board Preparation"),
              card_body(
                fluidRow(
                  # Board Prep Data (left column)
                  column(
                    width = 6,
                    h4("Board Preparation Status"),
                    tableOutput("board_prep_data")
                  ),
                  
                  # Exam Scores (right column)
                  column(
                    width = 6,
                    h4("Exam Scores"),
                    tableOutput("exam_scores_data")
                  )
                ),
                
                # Warnings about board prep
                fluidRow(
                  column(
                    width = 12,
                    uiOutput("board_prep_warnings")
                  )
                ),
                
                # Comments on Step 3 and Board Prep
                textAreaInput(
                  "board_prep_comments", 
                  label = "Notes about Step 3 and Board Preparation:",
                  rows = 3,
                  width = "100%",
                  placeholder = "Enter your comments about the resident's board preparation status and exam performance..."
                )
              )
            )
          ),
          
          # Fifth nav panel - Scholarship
          nav_panel(
            title = "5. Scholarship",
            value = "scholarship",
            
            # Instructions/explanation card
            card(
              card_header("Scholarship & Patient Safety"),
              card_body(
                p("This section displays the resident's scholarship activities and patient safety achievements."),
                p("Residents are expected to complete a Patient Safety Review and a Root Cause Analysis during their training."),
                p("All research, quality improvement, and other scholarly activities are also tracked here.")
              )
            ),
            
            # Scholarship data will be pulled and rendered here
            uiOutput("scholarship_data"),
            
            # Comments
            card(
              card_header("Scholarship & Patient Safety Comments"),
              card_body(
                textAreaInput("scholarship_comments", "Comments", 
                              rows = 8, 
                              width = "100%",
                              placeholder = "Enter your comments about the resident's scholarship activities, patient safety work, and any recommendations for further development...")
              )
            )
          ),
          
          # Sixth nav panel - Milestone Goals
          nav_panel(
            title = "6. Milestone Goals",
            value = "ilp",
            
            # Previous milestone goals card
            card(
              card_header("Previous Milestone Goals"),
              card_body(
                p("Review the resident's previous milestone-based goals:"),
                uiOutput("previous_milestone_goals")
              )
            ),
            
            # Current milestone goals card
            card(
              card_header("Current Milestone Goals"),
              card_body(
                # Patient Care / Medical Knowledge Goal
                div(
                  class = "mb-4",
                  h5("1. Patient Care / Medical Knowledge Milestone Goal", class = "text-primary"),
                  uiOutput("pc_mk_goal_ui")
                ),
                
                # Systems-Based Practice / PBLI Goal
                div(
                  class = "mb-4",
                  h5("2. Systems-Based Practice / Practice-Based Learning Milestone Goal", class = "text-primary"),
                  uiOutput("sbp_pbl_goal_ui")
                ),
                
                # Professionalism / ICS Goal
                div(
                  class = "mb-4",
                  h5("3. Professionalism / Interpersonal Communication Skills Milestone Goal", class = "text-primary"),
                  uiOutput("prof_ics_goal_ui")
                ),
                
                # Only one comments section at the bottom
                textAreaInput(
                  "milestone_goals_comments", 
                  label = "Comments on Milestone-Based Goals",
                  rows = 5,
                  width = "100%",
                  placeholder = "Take a moment to comment on all the milestone-based goals. Consider: Are they appropriate for the resident's level? Do they address key areas for development? How will you help them achieve these goals?"
                )
              )
            )
          ),
          
          # Seventh nav panel - Career Plan
          nav_panel(
            title = "7. Career Plan",
            value = "career",
            
            # Career path information section - populated by server
            card(
              card_header("Career Interests and Planning"),
              card_body(
                # This will be populated dynamically by the server.R code
                uiOutput("career_data_ui")
              )
            ),
            
            # Discussion guide section
            card(
              card_header("Career Discussion Guide"),
              card_body(
                fluidRow(
                  column(
                    width = 6,
                    h4("For Early Stage Residents", class = "text-success mb-3"),
                    tags$ul(
                      class = "list-group",
                      tags$li(class = "list-group-item", "Discuss career paths available"),
                      tags$li(class = "list-group-item", "Gauge interest in fellowship"),
                      tags$li(class = "list-group-item", "Identify mentors in areas of interest"),
                      tags$li(class = "list-group-item", "Discuss research opportunities")
                    )
                  ),
                  
                  column(
                    width = 6,
                    h4("For Graduating Residents", class = "text-primary mb-3"),
                    tags$ul(
                      class = "list-group",
                      tags$li(class = "list-group-item", "Confirm post-graduation plans"),
                      tags$li(class = "list-group-item", "Review job search/fellowship match results"),
                      tags$li(class = "list-group-item", "Discuss credentialing requirements"),
                      tags$li(class = "list-group-item", "Address transition to practice or fellowship")
                    )
                  )
                )
              )
            ),
            
            # Comments section
            card(
              card_header("Career Plan Discussion"),
              card_body(
                p("Based on the resident's career interests, provide your guidance and feedback:"),
                textAreaInput(
                  "career_comments", 
                  label = NULL,
                  rows = 5, 
                  width = "100%",
                  placeholder = "Enter comments regarding the resident's career planning. Consider: Are their goals realistic? What resources or connections might help them? Any suggested experiences or opportunities?"
                )
              )
            )
          ),
          
          # Eighth nav panel - Summary
          nav_panel(
            title = "8. Summary",
            value = "summary",
            
            # Summary instructions card
            card(
              card_header("Summary Instructions"),
              card_body(
                div(
                  class = "alert alert-primary",
                  tags$i(class = "fas fa-exclamation-circle me-2"),
                  tags$strong("Important: "),
                  "Please spend a few minutes reviewing and fully updating the resident's \"Individualized Learning Plan\". This must include a reflection on up-to-date progress, knowledge acquisition and board prep, scholarship, Milestone-based goals, and career plans. This summary will be used by the CCC to review and assess the resident."
                )
              )
            ),
            
            # Additional concerns card - moved directly here from discussion_topics_ui
            card(
              card_header("Additional Concerns from Self-Evaluation"),
              card_body(
                # Display the discussion topics (use uiOutput for dynamic rendering)
                uiOutput("discussion_topics_display"),
                
                # Add comments input
                textAreaInput(
                  "discussion_topics_comments", 
                  label = "Comments on Discussion Topics:",
                  rows = 4,
                  width = "100%",
                  placeholder = "Add your comments about these discussion topics..."
                )
              )
            ),
            
            # Summary input card
            card(
              card_header("Summary and ILP Update"),
              card_body(
                textAreaInput(
                  "summary_comments", 
                  label = NULL,
                  rows = 12, 
                  width = "100%",
                  placeholder = "Please include the following elements in your summary:\n\n1. PROGRESS: Overall assessment of resident's progress\n2. KNOWLEDGE: Assessment of medical knowledge and board preparation\n3. SCHOLARSHIP: Progress on scholarly activities\n4. MILESTONE GOALS: Update on goals for each competency area\n5. CAREER PLANS: Progress towards career objectives\n6. CONCERNS: Any areas of concern that need program attention\n7. RESOURCES: Specific resources the resident needs for success"
                )
              )
            ),
            
            # Confirmation checkbox and validation UI
            card(
              card_body(
                div(
                  id = "summary_validation_box",
                  class = "summary-validation",
                  uiOutput("summary_validation_ui")
                ),
                checkboxInput(
                  "summary_complete", 
                  label = tags$span(
                    tags$strong("I confirm"), 
                    "that I have completed a thorough review of this resident's progress and updated their Individualized Learning Plan."
                  ),
                  value = FALSE
                ),
                
                # Submit button directly in the summary tab for better UX
                div(
                  class = "text-center mt-4",
                  actionButton(
                    "submit_summary", 
                    tagList(icon("save"), "Submit Coach Review"), 
                    class = "btn-primary btn-lg"
                  )
                )
              )
            )
          ),
          
          # Ninth nav panel - Milestone Review
          nav_panel(
            title = "9. Milestone Review",
            value = "milestones",
            
            # Previous milestone plots (for reference)
            card(
              card_header("Previous Milestone Assessments"),
              card_body(
                fluidRow(
                  column(6, 
                         h5("Current Self-Assessment"),
                         plotOutput("self_milestones_plot_m", height = "400px")
                  ),
                  column(6, 
                         h5("Previous Program Assessment"),
                         plotOutput("program_milestones_plot_m", height = "400px")
                  )
                )
              )
            ),
            
            # Current milestone goals
            card(
              card_header("Current Milestone Goals"),
              card_body(
                # Patient Care / Medical Knowledge Goal
                div(
                  class = "mb-4",
                  h5("1. Patient Care / Medical Knowledge Milestone Goal", class = "text-primary"),
                  uiOutput("pc_mk_goal_ui_m")
                ),
                
                # Systems-Based Practice / PBLI Goal
                div(
                  class = "mb-4",
                  h5("2. Systems-Based Practice / Practice-Based Learning Milestone Goal", class = "text-primary"),
                  uiOutput("sbp_pbl_goal_ui_m")
                ),
                
                # Professionalism / ICS Goal
                div(
                  class = "mb-4",
                  h5("3. Professionalism / Interpersonal Communication Skills Milestone Goal", class = "text-primary"),
                  uiOutput("prof_ics_goal_ui_m")
                )
              )
            ),
            
            # Drop in the milestone module UI here
            card(
              card_header("Complete Milestone Assessment"),
              card_body(
                uiOutput("milestone_module_ui")
              )
            )
          )
        ),  # End of navset_card_tab
        
        # Navigation buttons
        fluidRow(
          column(6,
                 actionButton("prev_tab", "Previous", class = "btn-secondary")
          ),
          column(6,
                 div(
                   style = "float: right;",
                   # Show Next button on all tabs except the last one (milestone tab)
                   # The condition below checks if not on summary AND not on milestones tab
                   conditionalPanel(
                     condition = "input.primary_review_tabs !== 'summary' && input.primary_review_tabs !== 'milestones'",
                     actionButton("next_tab", "Next", class = "btn-primary")
                   )
                   # Removed the submit_primary_review button completely
                 )
          )
        )
      ),  # End of primary-review-content div
      
      # Second Review Content
      div(
        id = "second-review-content",
        style = "display: none;",
        
        card(
          card_header("Secondary Coach Review"),
          card_body(
            # Back button at the top
            div(
              class = "mb-4",
              actionButton("second_review_back", 
                           tagList(icon("arrow-left"), " Back to Coach Dashboard"), 
                           class = "btn-secondary btn-sm")
            ),
            
            # Resident information display
            div(
              class = "alert alert-info mb-4",
              tags$h4("Review Information", class = "alert-heading"),
              tags$p(
                tags$strong("You are conducting a secondary review for:"),
                textOutput("secondary_review_resident_name", inline = TRUE)
              ),
              tags$p(
                tags$strong("Period:"), 
                textOutput("secondary_review_period", inline = TRUE)
              ),
              tags$p(
                tags$strong("Primary Coach:"), 
                textOutput("secondary_review_primary_coach", inline = TRUE)
              )
            ),
            
            # Current milestone assessment plot
            card(
              card_header("Current Milestone Assessment"),
              card_body(
                plotOutput("secondary_current_milestones_plot", height = "400px")
              )
            ),
            
            # Primary coach's ILP final comments
            card(
              card_header(
                div(
                  class = "d-flex justify-content-between align-items-center",
                  tags$h5("Primary Coach's ILP Summary", class = "mb-0"),
                  tags$small("Review the primary coach's assessment", class = "text-muted")
                )
              ),
              card_body(
                # Improved display for primary coach comments
                div(
                  class = "primary-coach-summary-container",
                  uiOutput("primary_coach_comments_ui")  # Changed from verbatimTextOutput
                ),
                
                # Secondary coach's comments section - also improved
                div(
                  class = "mt-4",
                  div(
                    class = "d-flex align-items-center mb-3",
                    tags$h5("Your Comments on the ILP", class = "mb-0 me-3"),
                    tags$small("Required", class = "badge bg-warning text-dark")
                  ),
                  div(
                    class = "secondary-comments-container",
                    textAreaInput(
                      "secondary_coach_comments", 
                      label = NULL,
                      rows = 6,
                      width = "100%",
                      placeholder = "Enter your detailed comments about the resident's Individualized Learning Plan and the primary coach's assessment. Consider:\n\n- Do you agree with the primary coach's assessment?\n- Are there additional observations or concerns?\n- What recommendations do you have?\n- Are there resources the resident needs?"
                    )
                  )
                )
              )
            ),
            
            
            # Milestone approval section
            card(
              card_header("Milestone Assessment Approval"),
              card_body(
                div(
                  class = "mb-3",
                  radioButtons(
                    "approve_milestones", 
                    "Do you approve the milestone assessments as submitted by the primary coach?",
                    choices = c(
                      "Yes - I agree with the milestone assessments" = "yes",
                      "No - I have concerns about the milestone assessments" = "no"
                    ),
                    selected = character(0)
                  )
                ),
                
                # Conditional panel that shows only when "No" is selected
                conditionalPanel(
                  condition = "input.approve_milestones == 'no'",
                  div(
                    class = "alert alert-warning",
                    tags$p(
                      tags$strong("Please explain your concerns:"),
                      "Your comments will be shared with the CCC for review."
                    ),
                    textAreaInput(
                      "milestone_concerns", 
                      label = NULL,
                      rows = 4,
                      width = "100%",
                      placeholder = "Explain which milestone assessments you disagree with and why..."
                    )
                  )
                )
              )
            ),
            
            # Submit button section with validation
            div(
              class = "mt-4 text-center",
              uiOutput("secondary_review_validation_ui"),
              actionButton(
                "submit_secondary_review", 
                "Submit Secondary Review", 
                class = "btn-success btn-lg mt-3",
                icon = icon("save")
              )
            )
          )
        )
      )
    ),  # End of review-pages div
  
    # Done page - initially hidden
    div(
      id = "done-page",
      style = "display: none;",
      fluidRow(
        column(
          width = 8,
          offset = 2,
          card(
            card_header("Review Submitted"),
            card_body(
              div(
                class = "text-center",
                icon("check-circle", class = "fa-4x text-success"),
                h3("Thank you!"),
                p("Your review has been successfully submitted."),
                actionButton("start_new_review", "Start New Review", class = "btn-primary")
              )
            )
          )
        )
      )
    )
  )  # End of main-content div
) # End of page_fluid