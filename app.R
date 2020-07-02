#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(
    tabsetPanel(
        tabPanel("Etudiant",
    titlePanel("Evaluation Cefim par Etudiant"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "formation",
                "formation",
                unique(quest_rep$formation),
                selected = "AEC"
                #multiple = T
                
            ),
            selectInput(
                "Session_id",
                "id_etudiant",
                unique(quest_rep$nom_prenom),
                selected = "Jeoffrey Jeoffrey"
                #multiple = T
                
            ),
            dateRangeInput("date_filter",
                           "Date",
                           min="2020-01-01",
                           #max="2021-01-01", 
                           start = "2020-01-01",
                           end   = "2020-12-31")
            ),
        
        # Show a plot of the generated distribution
        mainPanel(plotOutput("histo"),
                  dataTableOutput("reponses"))
    )),
    tabPanel("Global"),
    titlePanel("Evaluation Cefim Global"),
    
    sidebarLayout(
        sidebarPanel(selectInput(
            "questionId",
            "Choisissez votre question",
            c(unique(quest_rep$libelle)[4:46])
        ),),
        
        
        mainPanel(plotOutput("histoglobal"),
                  dataTableOutput("txtQestion"))
    )

    ))

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    df <- reactive({
        quest_rep %>%
            filter(nom_prenom == input$Session_id,
                   session_date >= ymd(input$date_filter[1]),
                   session_date <= ymd(input$date_filter[2]),
                   formation == input$formation)
    })
    
    observeEvent(c(input$formation,input$date_filter), {
        choix <- quest_rep %>% 
            filter(formation == input$formation,
                   session_date >= ymd(input$date_filter[1]),
                   session_date <= ymd(input$date_filter[2])) %>% 
            pull(nom_prenom) %>% 
            unique()
        #choix<-c('a','b')
        #print(choix)
        updateSelectInput(session,"Session_id",choices = choix)
    })
    
    output$histo <- renderPlot({
        df() %>%
            filter(type == "score") %>%
            select("libelle", "reponse") %>%
            ggplot() +
            geom_histogram(aes(
                x = as.numeric(reponse),
                y = ..prop..,
                fill = ..prop..,
                group = 1
            ), stat = "count") +
            #facet_wrap(~ input$Session_id)+
            labs(
                title = paste("Repartition des notes données par", input$Session_id),
                x = "Notes",
                y = "Proportions"
            )
        
    })
    
    output$reponses <- renderDataTable({
        # generate bins based on input$bins from ui.R
        df() %>%
            select("libelle", "reponse")
    })
    dfbis <- reactive({
        quest_rep %>%
            filter(libelle == input$questionId)
        
    })
    
    output$histoglobal <- renderPlot({
        dfbis() %>%
            filter(type == "score") %>%
            ggplot() +
            geom_bar(aes(
                x = factor(reponse),
                y = stat(prop),
                fill = stat(prop),
                group = 1
            ), width = 0.5) +
            labs(x = "Notes",
                 y = "Pourcentage")
        
    })
    
    output$txtQestion <- renderDataTable({
        dfbis() %>%
            filter(type == "texte_long" | type == "texte_long") %>%
            select(nom_prenom,reponse) %>%
            unique()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
