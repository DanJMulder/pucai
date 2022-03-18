## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##   Shiny App for Quickly Calculating Pediatric Ulcerative Colitis Activity Index (PUCIA)   ##
##   Written by Daniel Mulder, April-May 2021                                                ##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

## Preamble
# This script does not constitute medical advice and is only to be used for the purposes of learning or preparing personal templates
# This script contains no real medical information, any information contained within is fictional example information

# Load required packages ----
library(shiny) # for interactive web application framework
library(tidyverse) # for basic data organization
library(glue) # for gluing together text


ui <- fluidPage(
  br(),
  tabsetPanel(type = "tabs",
              tabPanel("Selection",
                       br(),
                  tags$b("Pediatric Ulcerative Colitis Activity Index (PUCAI):"),
                  br(),
                  br(),
                  selectInput("pucai_pain", tags$span(style = "font-weight: 400", "Abdominal Pain"), choices = c("No pain (0)",
                                                                                                                 "Pain can be ignored (5)",
                                                                                                                 "Pain cannot be ignored (10)")),
                  selectInput("pucai_blood", tags$span(style = "font-weight: 400", "Rectal Bleeding"), choices = c("None (0)",
                                                                                                                   "Small amount only, in <50% of stools (10)",
                                                                                                                   "Small amount with most stools (20)",
                                                                                                                   "Large amount (>50% of stool content) (30)")),
                  selectInput("pucai_consistency", tags$span(style = "font-weight: 400", "Consistency of Most Stools"), choices = c("Formed (0)",
                                                                                                                                    "Partially formed (5)",
                                                                                                                                    "Completely unformed (10)")),
                  selectInput("pucai_number", tags$span(style = "font-weight: 400", "Number of Stools per 24h"), choices = c("0-2 (0)",
                                                                                                                             "3-5 (5)",
                                                                                                                             "6-8 (10)",
                                                                                                                             ">8 (15)")),
                  selectInput("pucai_nocturnal", tags$span(style = "font-weight: 400", "Nocturnal Stools (causing waking)"), choices = c("No (0)",
                                                                                                                                         "Yes (10)")),
                  selectInput("pucai_activity", tags$span(style = "font-weight: 400", "Activity Level"), choices = c("No limitation (0)",
                                                                                                                     "Occasional limitation (5)",
                                                                                                                     "Severe restriction (10)")),
                  textOutput("pucai_total_score"),
                  paste("(< 10 denotes clinical remission, 10-34 is clinically mild disease, 35-64 is clinically moderate disease, and 65-85 is clinically severe disease)"),
                  br(),
                  br(),
                  paste("References: Turner, et al. Gastroenterology. 2007;133:423–432; Turner et al. Inflamm Bowel Dis 2009;15:1218–1223")
                  ),
  
  tabPanel("Text Output (for copy/paste)",
           
           # Tab 2: Output (text note) Preview ----
           br(),
           tagAppendAttributes(textOutput("full_note"), style = "white-space:pre-wrap;")
  )
  )
)

server <- function(input, output, session) {
  
  output$pucai_total_score <- renderPrint({
    pain_score <- if (input$pucai_pain == "No pain (0)") {
      0
    } else if (input$pucai_pain == "Pain can be ignored (5)") {
      5
    } else if (input$pucai_pain == "Pain cannot be ignored (10)") {
      10
    }
    
    blood_score <- if (input$pucai_blood == "None (0)") {
      0
    } else if (input$pucai_blood == "Small amount only, in <50% of stools (10)") {
      10
    } else if (input$pucai_blood == "Small amount with most stools (20)") {
      20
    } else if (input$pucai_blood == "Large amount (>50% of stool content) (30)") {
      30
    }
    
    consistency_score <- if (input$pucai_consistency == "Formed (0)") {
      0
    } else if (input$pucai_consistency == "Partially formed (5)") {
      5
    } else if (input$pucai_consistency == "Completely unformed (10)") {
      10
    }
    
    number_score <- if (input$pucai_number == "0-2 (0)") {
      0
    } else if (input$pucai_number == "3-5 (5)") {
      5
    } else if (input$pucai_number == "6-8 (10)") {
      10
    } else if (input$pucai_number == ">8 (15)") {
      15
    }
    
    nocturnal_score <- if (input$pucai_nocturnal == "No (0)") {
      0
    } else if (input$pucai_nocturnal == "Yes (10)") {
      10
    }
    
    activity_score <- if (input$pucai_activity == "No limitation (0)") {
      0
    } else if (input$pucai_activity == "Occasional limitation (5)") {
      5
    } else if (input$pucai_activity == "Severe restriction (10)") {
      10
    }
    
    sum_pucai_score <- sum(pain_score, blood_score, consistency_score, number_score, nocturnal_score, activity_score)
    
    glue("PUCAI = ", sum_pucai_score, "/85 (denotes ",
           if (sum_pucai_score <10) {
             "clinical remission)"
           } else if (sum_pucai_score >=10 & sum_pucai_score < 35) {
             "clinically mild disease activity)"
           } else if (sum_pucai_score >=35 & sum_pucai_score < 65) {
             "clinically moderate disease activity)"
           } else if (sum_pucai_score >=65) {
             "clinically severe disease activity)"
           })
  })
  
  formData <- reactive({
   
      pain_score <- if (input$pucai_pain == "No pain (0)") {
        0
      } else if (input$pucai_pain == "Pain can be ignored (5)") {
        5
      } else if (input$pucai_pain == "Pain cannot be ignored (10)") {
        10
      }
      
      pain_text <- input$pucai_pain
      
      blood_score <- if (input$pucai_blood == "None (0)") {
        0
      } else if (input$pucai_blood == "Small amount only, in <50% of stools (10)") {
        10
      } else if (input$pucai_blood == "Small amount with most stools (20)") {
        20
      } else if (input$pucai_blood == "Large amount (>50% of stool content) (30)") {
        30
      }
      
      blood_text <- input$pucai_blood
      
      consistency_score <- if (input$pucai_consistency == "Formed (0)") {
        0
      } else if (input$pucai_consistency == "Partially formed (5)") {
        5
      } else if (input$pucai_consistency == "Completely unformed (10)") {
        10
      }
      
      consistency_text <- input$pucai_consistency
      
      number_score <- if (input$pucai_number == "0-2 (0)") {
        0
      } else if (input$pucai_number == "3-5 (5)") {
        5
      } else if (input$pucai_number == "6-8 (10)") {
        10
      } else if (input$pucai_number == ">8 (15)") {
        15
      }
      
      number_text <- input$pucai_number
      
      nocturnal_score <- if (input$pucai_nocturnal == "No (0)") {
        0
      } else if (input$pucai_nocturnal == "Yes (10)") {
        10
      }
      
      nocturnal_text <- input$pucai_nocturnal
      
      activity_score <- if (input$pucai_activity == "No limitation (0)") {
        0
      } else if (input$pucai_activity == "Occasional limitation (5)") {
        5
      } else if (input$pucai_activity == "Severe restriction (10)") {
        10
      }
      
      activity_text <- input$pucai_activity
      
      total_pucai_score <- pain_score +
        blood_score +
        consistency_score +
        number_score +
        nocturnal_score +
        activity_score

        return(glue("Pediatric Ulcerative Colitis Activity Index (PUCAI):",
                    "\n",
                    "\n",
                    paste("Abdominal Pain:", pain_text),
                    "\n",
                    paste("Rectal Bleeding:", blood_text),
                    "\n",
                    paste("Consistency of Most Stools:", consistency_text),
                    "\n",
                    paste("Number of Stools per 24h:", number_text),
                    "\n",
                    paste("Nocturnal Stools (causing waking):", nocturnal_text),
                    "\n",
                    paste("Activity Level:", activity_text),
                    "\n",
                    "\n",
                    "Total PUCAI = ", 
                    paste(total_pucai_score),
                    "/85 (denotes ",
                      paste(
                        if (total_pucai_score <10) {
                          "clinical remission)"
                        } else if (total_pucai_score >=10 & total_pucai_score < 35) {
                          "clinically mild disease activity)"
                        } else if (total_pucai_score >=35 & total_pucai_score < 65) {
                          "clinically moderate disease activity)"
                        } else if (total_pucai_score >=65) {
                          "clinically severe disease activity)"
                        }),
                    "\n",
                    "\n")
               )
    })
    
  output$full_note <- renderPrint(formData())
  
  }

shinyApp(ui, server)