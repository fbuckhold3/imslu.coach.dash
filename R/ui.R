# ui.R - IMSLU Coaching Application UI

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
                            selectInput("coach_name", "Select Your Name", choices = NULL),
                            actionButton("submit_coach", "Continue", class = "btn-primary")
                        )
                    )
                )
            )
        ),
        
        # Resident selection page - initially hidden
        div(
            id = "resident-selection-page",
            style = "display: none;",
            fluidRow(
                column(
                    width = 10,
                    offset = 1,
                    card(
                        card_header("Resident Selection"),
                        card_body(
                            p("Below is a list of residents assigned to you. Select a resident to review and the type of review."),
                            
                            # Period selection module placeholder
                            uiOutput("period_select_ui"),
                            
                            # Resident table will be rendered here
                            DT::dataTableOutput("resident_table"),
                            
                            # Review type selection
                            radioButtons("review_type", "Review Type",
                                         choices = c("Primary Review" = "primary", 
                                                     "Second Review" = "second")),
                            
                            actionButton("submit_resident", "Begin Review", class = "btn-primary")
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
                            column(4, h4("Resident:", textOutput("display_resident_name", inline = TRUE))),
                            column(4, h4("Coach:", textOutput("display_coach_name", inline = TRUE))),
                            column(4, h4("Access Code:", textOutput("display_access_code", inline = TRUE)))
                        )
                    )
                )
            ),
            
            # Primary Review Tabs
            div(
                id = "primary-review-content",
                style = "display: none;",
                
                navs_tab_card(
                    id = "primary_review_tabs",
                    nav_panel(
                        title = "1. Meeting Pre-review",
                        value = "pre_review",
                        # Self-evaluation check
                        checkboxInput("self_eval_completed", "Resident completed self-evaluation", FALSE),
                        
                        conditionalPanel(
                            condition = "!input.self_eval_completed",
                            div(
                                class = "alert alert-warning",
                                "The resident has not completed their self-evaluation. Please have them complete it before proceeding with the review."
                            )
                        ),
                        
                        # Dashboard link
                        p("Review the resident's dashboard using their access code:"),
                        a("Open Resident Dashboard", 
                          href = "https://fbuckhold3-imsluresidentdashboard.share.connect.posit.cloud", 
                          target = "_blank", 
                          class = "btn btn-info"),
                        
                        # Milestone spider plots
                        fluidRow(
                            column(6, 
                                   h4("Self-Reported Milestones"),
                                   plotOutput("self_milestones_plot")
                            ),
                            column(6, 
                                   h4("Program Milestones"),
                                   plotOutput("program_milestones_plot")
                            )
                        ),
                        
                        # Prior CCC notes
                        card(
                            card_header("Prior CCC Notes"),
                            card_body(
                                verbatimTextOutput("prior_ccc_notes")
                            )
                        ),
                        
                        # Prior ILP
                        card(
                            card_header("Prior ILP"),
                            card_body(
                                verbatimTextOutput("prior_ilp")
                            )
                        ),
                        
                        # Discussion points
                        textAreaInput("discussion_points", "Notes for Discussion (1-3 points)", 
                                      rows = 5, placeholder = "Enter 1-3 key points to discuss with the resident...")
                    ),
                    
                    nav_panel(
                        title = "2. Wellness",
                        value = "wellness",
                        
                        # Intern intro notes
                        conditionalPanel(
                            condition = "output.is_intern === true",
                            card(
                                card_header("Intern Introduction"),
                                card_body(
                                    p("Use this section to document background information for new interns."),
                                    textAreaInput("intern_background", "Background Notes", rows = 5)
                                )
                            )
                        ),
                        
                        # Professional wellness
                        card(
                            card_header("Professional Wellness"),
                            card_body(
                                textAreaInput("professional_wellness", "Professional Wellness Notes", 
                                              rows = 5, placeholder = "Document professional wellness areas...")
                            )
                        ),
                        
                        # Personal concerns
                        card(
                            card_header("Personal Concerns"),
                            card_body(
                                textAreaInput("personal_concerns", "Personal Concerns (if any)", 
                                              rows = 5, placeholder = "Document any personal concerns or challenges...")
                            )
                        )
                    ),
                    
                    nav_panel(
                        title = "3. Career Plan",
                        value = "career",
                        
                        # Career data will be pulled here
                        verbatimTextOutput("career_data"),
                        
                        # Comments
                        textAreaInput("career_comments", "Career Plan Comments", 
                                      rows = 5, placeholder = "Enter comments regarding resident's career planning...")
                    ),
                    
                    nav_panel(
                        title = "4. Scholarship",
                        value = "scholarship",
                        
                        # Scholarship data will be pulled here
                        verbatimTextOutput("scholarship_data"),
                        
                        # Comments
                        textAreaInput("scholarship_comments", "Scholarship Comments", 
                                      rows = 5, placeholder = "Enter comments regarding resident's scholarship activities...")
                    ),
                    
                    nav_panel(
                        title = "5. Exams & Knowledge",
                        value = "knowledge",
                        
                        # Exam data will be pulled here
                        verbatimTextOutput("exam_data"),
                        
                        # Comments
                        textAreaInput("knowledge_comments", "Knowledge Acquisition Comments", 
                                      rows = 5, placeholder = "Enter comments regarding resident's knowledge development...")
                    ),
                    
                    nav_panel(
                        title = "6. ILP",
                        value = "ilp",
                        
                        # ILP data will be pulled here
                        verbatimTextOutput("ilp_data"),
                        
                        # Comments
                        textAreaInput("ilp_comments", "ILP Comments", 
                                      rows = 5, placeholder = "Enter comments regarding resident's individualized learning plan...")
                    ),
                    
                    nav_panel(
                        title = "7. Summary",
                        value = "summary",
                        
                        p("Please take a moment to briefly summarize your discussion with the resident and your thoughts on their progress. List any resources, needs, or concerns the Program needs to know to ensure their success."),
                        
                        textAreaInput("summary_comments", "Summary", 
                                      rows = 10, placeholder = "Enter summary of discussion, including resources, needs, or concerns...")
                    ),
                    
                    nav_panel(
                        title = "8. Milestone Review",
                        value = "milestones",
                        
                        # Milestone module will be inserted here
                        uiOutput("milestone_module_ui")
                    )
                ),
                
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
            ),
            
            # Second Review Content
            div(
                id = "second-review-content",
                style = "display: none;",
                
                card(
                    card_header("Second Review"),
                    card_body(
                        
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
        ),
        
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
    )
)
