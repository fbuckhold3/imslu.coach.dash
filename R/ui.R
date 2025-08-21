# ============================================================================
# ENHANCED COACH APP - UI COMPONENT
# ui.R
# ============================================================================

ui <- tryCatch({
  page_fluid(
    title = "IMSLU Coaching - Enhanced",
    
    # Try to use gmed theme, fallback to bslib default
    theme = tryCatch(gmed::create_gmed_theme(), error = function(e) {
      message("gmed theme failed, using default: ", e$message)
      bslib::bs_theme(version = 5)
    }),
    
    # Load enhanced styles
    tags$head(
      # Only load gmed CSS if files exist
      tryCatch(tags$link(rel = "stylesheet", type = "text/css", href = "gmed/css/gmed-components.css"), 
               error = function(e) NULL),
      tryCatch(tags$link(rel = "stylesheet", type = "text/css", href = "gmed/css/gmed-core.css"), 
               error = function(e) NULL),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$title("IMSLU Coaching Dashboard - Enhanced"),
      tryCatch(tags$script(src = "gmed/js/gmed-components.js"), 
               error = function(e) NULL),
      
      # Custom JavaScript for progress bar updates
      tags$script(HTML("
        Shiny.addCustomMessageHandler('updateProgressBar', function(data) {
          $('#progress_bar').css('width', data.width);
          if (data.text) {
            $('#progress_bar div').text(data.text);
          }
        });
      "))
    ),
    
    shinyjs::useShinyjs(),
    
    # Enhanced header with gradient
    div(
      class = "gmed-header p-4 text-white mb-4",
      style = "background: linear-gradient(135deg, #003d5c 0%, #0066a1 100%); border-radius: 0 0 16px 16px; position: relative; overflow: hidden;",
      div(
        style = "position: absolute; top: 0; right: 0; opacity: 0.1; font-size: 8rem; transform: rotate(15deg); color: white;",
        icon("user-md")
      ),
      h1("IMSLU Coaching Dashboard", class = "text-center mb-2", style = "position: relative; z-index: 1;"),
      h4("Enhanced Resident Coaching and Assessment Platform", class = "text-center mb-0 opacity-75", style = "position: relative; z-index: 1;")
    ),
    
    # ============================================================================
    # LOGIN PAGE (Enhanced with fallbacks)
    # ============================================================================
    div(
      id = "login-page",
      fluidRow(
        column(
          width = 6,
          offset = 3,
          # Use regular card if gmed_card fails
          tryCatch(
            gmed::gmed_card(
              title = "Welcome to Enhanced IMSLU Coaching",
              div(
                class = "text-center p-3",
                icon("lock", class = "fa-3x text-primary mb-3"),
                p("This enhanced application provides comprehensive coaching tools with milestone visualizations and enhanced feedback systems."),
                p("Please enter your access code to continue."),
                
                textInput("access_code", "Access Code", placeholder = "Enter your access code"),
                actionButton("submit_access", "Submit", class = "btn-primary"),
                
                br(), br(),
                div(id = "access_error", class = "alert alert-danger", style = "display: none;",
                    "Invalid access code. Please try again.")
              )
            ),
            error = function(e) {
              message("gmed_card failed, using regular card: ", e$message)
              card(
                card_header("Welcome to Enhanced IMSLU Coaching"),
                card_body(
                  div(
                    class = "text-center p-3",
                    icon("lock", class = "fa-3x text-primary mb-3"),
                    p("This enhanced application provides comprehensive coaching tools."),
                    p("Please enter your access code to continue."),
                    
                    textInput("access_code", "Access Code", placeholder = "Enter your access code"),
                    actionButton("submit_access", "Submit", class = "btn-primary"),
                    
                    br(), br(),
                    div(id = "access_error", class = "alert alert-danger", style = "display: none;",
                        "Invalid access code. Please try again.")
                  )
                )
              )
            }
          )
        )
      )
    ),
    
    # ============================================================================
    # COACH SELECTION PAGE (Enhanced with fallbacks)
    # ============================================================================
    div(
      id = "coach-selection-page",
      style = "display: none; padding-bottom: 100px;",
      fluidRow(
        column(
          width = 10,
          offset = 1,
          # Use fallbacks for gmed components
          tryCatch(
            gmed::gmed_card(
              title = "Select Your Profile",
              div(
                class = "text-center p-4",
                tags$label(
                  "Select Your Name",
                  class = "form-label text-center mb-4",
                  style = "font-size: 1.25rem; font-weight: 600; color: #003d5c; display: block;"
                ),
                selectizeInput(
                  "coach_name", 
                  label = NULL,
                  choices = c("Loading coach names..." = ""),
                  options = list(
                    placeholder = "Select or type to search for your name",
                    create = FALSE,
                    searchField = c('text', 'value')
                  ),
                  width = "100%"
                )
              )
            ),
            error = function(e) {
              message("gmed components failed, using fallback: ", e$message)
              card(
                card_header("Select Your Profile"),
                card_body(
                  div(
                    class = "text-center p-4",
                    tags$label(
                      "Select Your Name",
                      class = "form-label text-center mb-4",
                      style = "font-size: 1.25rem; font-weight: 600; color: #003d5c; display: block;"
                    ),
                    selectizeInput(
                      "coach_name", 
                      label = NULL,
                      choices = c("Loading coach names..." = ""),
                      options = list(
                        placeholder = "Select or type to search for your name",
                        create = FALSE,
                        searchField = c('text', 'value')
                      ),
                      width = "100%"
                    )
                  )
                )
              )
            }
          )
        )
      ),
      
      # Enhanced period override section
      conditionalPanel(
        condition = "input.coach_name !== null && input.coach_name !== '' && input.coach_name !== 'Loading coach names...'",
        fluidRow(
          column(
            width = 10,
            offset = 1,
            
            # Period override control
            div(
              class = "mb-4",
              card(
                card_header("Period Override (Optional)"),
                card_body(
                  checkboxInput("enable_period_override", 
                                "Enable period override for testing/special reviews", 
                                value = FALSE),
                  conditionalPanel(
                    condition = "input.enable_period_override",
                    div(
                      class = "alert alert-warning mt-3",
                      icon("exclamation-triangle"), " Period override enabled. You can now select any evaluation period, even if it's not currently active."
                    ),
                    selectInput(
                      "override_period",
                      "Select Period to Review:",
                      choices = list(
                        "Current Period (Automatic)" = "auto",
                        "Intern Intro" = "Intern Intro",
                        "Mid Intern" = "Mid Intern", 
                        "End Intern" = "End Intern",
                        "Mid PGY2" = "Mid PGY2",
                        "End PGY2" = "End PGY2", 
                        "Mid PGY3" = "Mid PGY3",
                        "Graduation" = "Graduation"
                      ),
                      selected = "auto"
                    )
                  )
                )
              )
            ),
            
            # Enhanced coach dashboard header with icon
            div(
              class = "gmed-card p-3 text-white mt-4 mb-3",
              style = "background: linear-gradient(135deg, #003d5c 0%, #0066a1 100%); border-radius: 12px;",
              div(
                class = "d-flex align-items-center justify-content-center",
                icon("user-tie", class = "fa-2x me-3"),
                h2("Coaching Dashboard for: ", textOutput("dashboard_coach_name", inline = TRUE))
              )
            ),
            
            # Enhanced residents table card
            card(
              card_header("Your Assigned Residents"),
              card_body(
                div(
                  class = "mb-3 text-center p-3 bg-light rounded",
                  div(
                    class = "d-flex align-items-center justify-content-center mb-2",
                    icon("mouse-pointer", class = "text-primary me-2"),
                    strong("Click on a resident row to start the review process", class = "text-primary")
                  ),
                  textOutput("period_status_text", inline = TRUE)
                ),
                
                DT::dataTableOutput("coach_residents_table"),
                
                div(
                  class = "text-muted mt-3 p-2 bg-light rounded",
                  div(
                    class = "row",
                    div(class = "col-md-6", 
                        icon("user", class = "text-primary me-2"), 
                        "Primary review: You are the main coach"),
                    div(class = "col-md-6", 
                        icon("user-check", class = "text-secondary me-2"), 
                        "Secondary review: You are reviewing another coach's feedback")
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # ============================================================================
    # ENHANCED REVIEW PAGES
    # ============================================================================
    div(
      id = "review-pages",
      style = "display: none;",
      
      # Enhanced resident info panel with gmed styling
      fluidRow(
        column(
          width = 12,
          div(
            class = "gmed-card p-3 mb-4",
            style = "background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%); border-left: 4px solid #0066a1; border-radius: 0 12px 12px 0; box-shadow: 0 4px 16px rgba(0, 61, 92, 0.08);",
            fluidRow(
              column(2, 
                     div(class = "text-center",
                         icon("user", class = "text-primary fa-lg mb-2"),
                         div(h6("Resident:", class = "text-primary mb-1"), textOutput("resident_name_display"))
                     )
              ),
              column(2, 
                     div(class = "text-center",
                         icon("graduation-cap", class = "text-success fa-lg mb-2"),
                         div(h6("Level:", class = "text-success mb-1"), textOutput("resident_level_display"))
                     )
              ),
              column(2, 
                     div(class = "text-center",
                         icon("calendar", class = "text-info fa-lg mb-2"),
                         div(h6("Period:", class = "text-info mb-1"), textOutput("resident_period_display"))
                     )
              ),
              column(3, 
                     div(class = "text-center",
                         icon("user-tie", class = "text-warning fa-lg mb-2"),
                         div(h6("Coach:", class = "text-warning mb-1"), textOutput("resident_coach_display"))
                     )
              ),
              column(3, 
                     div(class = "text-center",
                         icon("key", class = "text-secondary fa-lg mb-2"),
                         div(h6("Access Code:", class = "text-secondary mb-1"), textOutput("resident_access_display"))
                     )
              )
            )
          )
        )
      ),
      
      # Enhanced progress indicator with gmed styling
      fluidRow(
        column(
          width = 12,
          div(
            class = "text-center mb-3",
            style = "font-size: 1.1rem; font-weight: 600; color: #0066a1;",
            textOutput("current_step_text")
          ),
          div(
            class = "mb-4 position-relative",
            style = "height: 20px; border-radius: 12px; background: linear-gradient(90deg, #e9ecef, #f8f9fa); overflow: hidden; box-shadow: inset 0 2px 4px rgba(0, 0, 0, 0.1);",
            div(
              id = "progress_bar",
              style = "width: 12.5%; background: linear-gradient(90deg, #0066a1, #4a90a4); transition: width 0.6s ease; border-radius: 12px; height: 100%; position: relative;",
              div(
                style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); color: white; font-size: 0.8rem; font-weight: bold;",
                textOutput("progress_text", inline = TRUE)
              )
            )
          )
        )
      ),
      
      # Dynamic review content - this will be rendered by server
      fluidRow(
        column(
          width = 12,
          uiOutput("review_content")
        )
      ),
      
      # Enhanced navigation buttons
      fluidRow(
        column(
          width = 12,
          div(
            class = "d-flex justify-content-between align-items-center mt-4 pt-3 border-top",
            actionButton("back_to_dashboard", 
                         tagList(icon("arrow-left"), " Back to Dashboard"), 
                         class = "btn-outline-secondary"),
            div(
              class = "btn-group",
              actionButton("prev_step", 
                           tagList(icon("chevron-left"), " Previous"), 
                           class = "btn-outline-primary"),
              actionButton("next_step", 
                           tagList("Next ", icon("chevron-right")), 
                           class = "btn-primary")
            )
          )
        )
      )
    ),
    
    # ============================================================================
    # ENHANCED DONE PAGE
    # ============================================================================
    div(
      id = "done-page",
      style = "display: none;",
      fluidRow(
        column(
          width = 8,
          offset = 2,
          card(
            card_header("Review Submitted Successfully"),
            card_body(
              div(
                class = "text-center p-4",
                div(
                  class = "mb-4",
                  icon("check-circle", class = "fa-5x text-success mb-3"),
                  h3("Thank you!", class = "text-success"),
                  p("Your coaching review has been successfully submitted to REDCap.", class = "lead")
                ),
                div(
                  class = "d-flex justify-content-center gap-3",
                  actionButton("start_new_review", 
                               tagList(icon("plus"), " Start New Review"), 
                               class = "btn-primary"),
                  actionButton("view_dashboard", 
                               tagList(icon("dashboard"), " View Dashboard"), 
                               class = "btn-outline-primary")
                )
              )
            )
          )
        )
      )
    )
  )
}, error = function(e) {
  message("UI creation failed: ", e$message)
  
  # Ultimate fallback
  page_fluid(
    title = "IMSLU Coaching - Error",
    div(
      class = "alert alert-danger m-4",
      h3("UI Loading Error"),
      p("There was an error loading the user interface."),
      p("Error message: ", e$message),
      p("Please contact support or try refreshing the page.")
    )
  )
})