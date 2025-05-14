# Complete Fixed UI Code with Reorganized Tabs and Progress Bar

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
  
  # Page title
  tags$head(
    tags$title("IMSLU Coaching"),
    # Add custom CSS
    tags$style(HTML("
      .card-header {
        background-color: #0072B2;
        color: white;
      }
      .nav-tabs .nav-link.active {
        background-color: #56B4E9;
        color: white;
      }
      .resident-info-panel {
        background-color: #f8f9fa;
        border-left: 4px solid #0072B2;
        padding: 15px;
        margin-bottom: 20px;
      }
      /* New styles for the coach dashboard */
    .coach-dashboard-header {
        background-color: #0072B2;
        color: white;
        padding: 15px;
        margin-bottom: 20px;
        border-radius: 5px;
    }
    
    /* Table row hover effect */
    .dataTable tbody tr:hover {
        background-color: rgba(86, 180, 233, 0.2) !important;
        cursor: pointer;
    }
    
    /* Selected row style */
    .dataTable tbody tr.selected {
        background-color: rgba(86, 180, 233, 0.4) !important;
    }
    
    /* Resident details section */
    .resident-details {
        background-color: #f8f9fa;
        border-left: 4px solid #56B4E9;
        margin-top: 10px;
        margin-bottom: 20px;
        padding: 15px;
        border-radius: 0 5px 5px 0;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
    }
    
    /* Hide resident details section initially */
    #resident_details_section {
        display: none;
    }
    ")),
    tags$style(HTML("
        /* Make table rows more obviously clickable */
        #coach_residents_table .dataTables_wrapper tbody tr {
            cursor: pointer;
        }
        
        #coach_residents_table .dataTables_wrapper tbody tr:hover {
            background-color: rgba(0, 114, 178, 0.15) !important; 
        }
        
        #coach_residents_table .dataTables_wrapper tbody tr.selected {
            background-color: rgba(0, 114, 178, 0.3) !important;
        }
        
        /* Clear button for coach selection */
        .selectize-control.single .selectize-input:after {
            content: '×';
            font-size: 16px;
            opacity: 0.5;
            border: none;
            right: 8px;
        }
        
        /* Emphasize table instruction */
        .table-instruction {
            color: #0072B2;
            border-bottom: 1px solid #0072B2;
            padding-bottom: 8px;
            margin-bottom: 16px;
        }
          /* Make text areas wider and more prominent */
        .form-control.shiny-bound-input[type='textarea'] {
          width: 100%;
          border: 1px solid #ddd;
          padding: 10px;
          font-size: 14px;
          border-radius: 4px;
        }
        
        /* Add focus style */
        .form-control.shiny-bound-input[type='textarea']:focus {
          border-color: #0072B2;
          box-shadow: 0 0 0 0.2rem rgba(0, 114, 178, 0.25);
        }
    /* Career plan section styling */
  .career-section {
    margin-bottom: 20px;
    padding: 15px;
    border-radius: 5px;
    background-color: #f8f9fa;
  }
  
  .career-section h5 {
    color: #0072B2;
    border-bottom: 1px solid #0072B2;
    padding-bottom: 8px;
    margin-bottom: 15px;
  }
  
  .career-section ul {
    padding-left: 20px;
  }
  
  .career-section li {
    margin-bottom: 5px;
  }
  
  .discussion-topics {
    background-color: #e9ecef;
    padding: 15px;
    border-radius: 5px;
    border-left: 4px solid #0072B2;
  }
  
  /* Add nice styling to the career plan comment box */
  #career_comments {
    border: 1px solid #ddd;
    border-left: 3px solid #0072B2;
  }
  
  #career_comments:focus {
    border-color: #0072B2;
    box-shadow: 0 0 0 0.2rem rgba(0, 114, 178, 0.25);
  }
  
  /* Fix tab visibility with z-index and margin */
  .nav-tabs {
    position: relative;
    z-index: 10;
    margin-top: 15px;
  }
  
  /* Progress bar styling */
  .review-progress {
    margin-bottom: 20px;
    height: 10px;
  }
  
  .review-progress .progress-bar {
    transition: width 0.5s ease;
  }
  
  /* Step indicator formatting */
  .step-indicator {
    font-size: 0.85rem;
    margin-bottom: 5px;
    font-weight: bold;
    color: #0072B2;
  }
  
  /* Add this to your existing CSS */
.big-modal .modal-dialog {
    width: 90%;
    max-width: 1200px;
}

/* Style for the plus/delta sections */
#resident_plus_assessment, #resident_delta_assessment {
    background-color: #f8f9fa;
    border-left: 4px solid #0072B2;
    padding: 10px 15px;
    margin-bottom: 20px;
    border-radius: 0 4px 4px 0;
    white-space: pre-wrap;
    font-size: 14px;
    max-height: 200px;
    overflow-y: auto;
}

#resident_plus_assessment {
    border-left-color: #28a745; /* Green for positive feedback */
}

#resident_delta_assessment {
    border-left-color: #dc3545; /* Red for areas of improvement */
}

/* Table button styling */
.btn-link.card-header-action {
    color: white;
    float: right;
    padding: 0 5px;
    margin: -5px 0;
}

.btn-link.card-header-action:hover {
    color: #f8f9fa;
    text-decoration: none;
}
"))
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
      style = "display: none;",
      fluidRow(
        column(
          width = 10,
          offset = 1,
          card(
            card_header("Select Your Profile"),
            card_body(
              # Using selectizeInput for searchable dropdown
              selectizeInput("coach_name", "Select Your Name", 
                             choices = c("Loading coach names..." = ""),  # Add a placeholder option
                             options = list(
                               placeholder = "Select or type to search for your name",
                               create = FALSE,
                               openOnFocus = TRUE,
                               onInitialize = I('function() { this.setValue(""); }')
                             ))
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
        
        navset_card_tab(  # Start of navset_card_tab
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
                uiOutput("prior_ccc_notes")  # Change this from verbatimTextOutput to uiOutput
              )
            ),
            
            # Prior ILP
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
          
          # Third nav panel - Evaluations Review (New Panel)
          nav_panel(
            title = "3. Evaluations",
            value = "evaluations",
            
            card(
              card_header(
                tagList(
                  "Rotation & Clinical Evaluations",
                  actionButton("open_eval_modal", label = NULL, icon = icon("table"), 
                               class = "btn btn-link", title = "View Plus/Delta Assessment Table")
                )
              ),
              card_body(
                p("Review the resident's evaluations from rotations, outpatient clinics, and other experiences."),
                
                # Display the resident's self-reflection on strengths
                h4("What the resident thinks they're doing well:"),
                verbatimTextOutput("resident_plus_assessment"),
                
                # Display the resident's self-reflection on areas for improvement
                h4("What the resident thinks they need to improve:"),
                verbatimTextOutput("resident_delta_assessment"),
                
                # Divider
                hr(),
                
                # Coach's comments about evaluations
                h4("Your Assessment of Evaluations:"),
                textAreaInput(
                  "evaluations_comments", 
                  label = NULL,
                  rows = 5,
                  width = "100%",
                  placeholder = "Document your discussion about the resident's evaluations. Consider: Do they have accurate self-assessment? What patterns do you see in their evaluations? What strategies would help them improve?"
                ),
                
                # Button to view the plus/delta table at the bottom as well
                actionButton("reopen_eval_modal", "View Full Plus/Delta Table", 
                             icon = icon("table"), class = "btn-secondary mt-3")
              )
            )
          ),
          
          # Fourth nav panel - Exams & Knowledge (moved up)
          nav_panel(
            title = "4. Exams & Knowledge",
            value = "knowledge",
            
            # Exam data will be pulled here
            verbatimTextOutput("exam_data"),
            
            # Comments
            textAreaInput("knowledge_comments", "Knowledge Acquisition Comments", 
                          rows = 5, placeholder = "Enter comments regarding resident's knowledge development...")
          ),
          
          # Fifth nav panel - Scholarship (moved)
          nav_panel(
            title = "5. Scholarship",
            value = "scholarship",
            
            # Scholarship data will be pulled here
            verbatimTextOutput("scholarship_data"),
            
            # Comments
            textAreaInput("scholarship_comments", "Scholarship Comments", 
                          rows = 5, placeholder = "Enter comments regarding resident's scholarship activities...")
          ),
          
          # Sixth nav panel - ILP (moved)
          nav_panel(
            title = "6. ILP",
            value = "ilp",
            
            # ILP data will be pulled here
            verbatimTextOutput("ilp_data"),
            
            # Comments
            textAreaInput("ilp_comments", "ILP Comments", 
                          rows = 5, placeholder = "Enter comments regarding resident's individualized learning plan...")
          ),
          
          # Seventh nav panel - Career Plan (moved)
          nav_panel(
            title = "7. Career Plan",
            value = "career",
            
            # Career path section
            card(
              card_header("Career Path Information"),
              card_body(
                uiOutput("career_data_ui")  # This will be populated dynamically in server.R
              )
            ),
            
            # Comments section
            card(
              card_header("Career Plan Discussion"),
              card_body(
                p("Based on the resident's career interests above, provide your guidance and feedback:"),
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
            
            p("Please take a moment to briefly summarize your discussion with the resident and your thoughts on their progress. List any resources, needs, or concerns the Program needs to know to ensure their success."),
            
            textAreaInput("summary_comments", "Summary", 
                          rows = 10, placeholder = "Enter summary of discussion, including resources, needs, or concerns...")
          ),
          
          # Ninth nav panel - Milestone Review
          nav_panel(
            title = "9. Milestone Review",
            value = "milestones",
            
            # Milestone module will be inserted here
            uiOutput("milestone_module_ui")
          )
        ),  # End of navset_card_tab
        
        # Navigation buttons
        fluidRow(
          column(6,
                 actionButton("prev_tab", "Previous", class = "btn-secondary")
          ),
          column(6,
                 div(style = "float: right;",
                     actionButton("next_tab", "Next", class = "btn-primary"),
                     actionButton("submit_primary_review", "Submit Review", class = "btn-success", style = "display: none;")
                 )
          )
        )
      ),  # End of primary-review-content div
      
      # Second Review Content
      div(
        id = "second-review-content",
        style = "display: none;",
        
        card(
          card_header("Second Review"),
          card_body(
            # Back button at the top
            div(class = "mb-4",
                actionButton("second_review_back", "← Back to Coach Dashboard", 
                             class = "btn-secondary btn-sm")
            ),
            
            # Display primary review data
            verbatimTextOutput("primary_review_data"),
            
            # Agreement with milestones
            radioButtons("agree_with_milestones", "Do you agree with the milestones as reported?",
                         choices = c("Yes" = "yes", "No" = "no")),
            
            # Conditional comments for disagreement
            conditionalPanel(
              condition = "input.agree_with_milestones == 'no'",
              textAreaInput("milestone_disagreement", "Please explain your disagreement with the milestones:", rows = 5)
            ),
            
            # Overall comments
            textAreaInput("second_review_comments", "Overall Comments", rows = 5,
                          placeholder = "Enter any additional comments about this resident..."),
            
            actionButton("submit_second_review", "Submit Review", class = "btn-success")
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
              icon("check-circle", class = "fa-4x text-success"),
              h3("Thank you!"),
              p("Your review has been successfully submitted."),
              actionButton("start_new_review", "Start New Review", class = "btn-primary")
            )
          )
        )
      )
    )
  )  # End of main-content div
)  # Closing parenthesis for page_fluid