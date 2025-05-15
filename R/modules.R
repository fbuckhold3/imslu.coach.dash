# UI Component for Card 2: Resident Information

card2UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 12,
        card(
          card_header("Resident Information"),
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
          h4(paste("Let's learn more about", resident_name())),
          textAreaInput(
            ns("background"), 
            label = paste0("Where is ", resident_name(), " from, what are they excited about, and what do they do outside of the program?"),
            height = "150px",
            width = "100%"
          )
        )
      } else {
        # Regular period questions (shown for all periods)
        tagList(
          h4(paste("Check-in with", resident_name())),
          textAreaInput(
            ns("dealing_with_residency"), 
            label = paste0("How is ", resident_name(), " dealing with residency? Any concerns or issues? Are they supported?"),
            height = "150px",
            width = "100%"
          ),
          textAreaInput(
            ns("wellbeing"), 
            label = paste0("How is ", resident_name(), "'s wellbeing? Any concerns or considerations?"),
            height = "150px",
            width = "100%"
          )
        )
      }
    })
    
    # Return entered values for use in parent module/app
    return(
      reactive({
        if (is_intro_period()) {
          list(
            background = input$background
          )
        } else {
          list(
            dealing_with_residency = input$dealing_with_residency,
            wellbeing = input$wellbeing
          )
        }
      })
    )
  })
}

