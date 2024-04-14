
library(DT)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = "bootstrap.css",
    navbarPage(
        "Défaillance des entreprises agricoles", 
        id  = "main_navbar",
        
        tabPanel(id = "1_Statistics",
                 fluidRow(column(3,
                                 uiOutput("h2_title1"), class = "container")
                 ), 
                 mainPanel(
                     sidebarLayout(
                         sidebarPanel(
                             width = 3, 
                             selectInput("foo1",
                                         label = "Choose", 
                                         choices = c(1,2,3,4,5), 
                                         selected = 3)
                         ),
                         
                         mainPanel(
                             p("#Agriculture  #changement climatique  # procedure collective"),
                             br(), 
                             br(),
                             p("En difficulté : une entreprise agricole qui se voit concerné par une procédure collecte dans les x derniers mois (source : tribunaux de commerce)."),
                             br(), 
                             br(), 
                             p("Ce présent projet vise à étudier l'impact des chocs climatiques sur le déclenchement de procédure collective des entreprises agricoles."),
                         
                             DT::dataTableOutput("tab1_PT"))
                 )
        )
    ),
    
    tabPanel(id = "2_Models",
             fluidRow(column(3,
                             uiOutput("h2_title2"), class = "container")
             ), 
             mainPanel(
                 sidebarLayout(
                     sidebarPanel(
                         width = 3, 
                         selectInput("foo2",
                                     label = "Choose", 
                                     choices = c(1,2,3,4,5), 
                                     selected = 3)
                     ),
                     mainPanel(DT::dataTableOutput("def_table2"))
                 )
             )
    ), 
    
    tabPanel(id = "3_FranceMet_map",
             fluidRow(column(3,
                             uiOutput("h2_title3"), class = "container")
             ), 
             mainPanel(
                 sidebarLayout(
                     sidebarPanel(
                         width = 3, 
                         selectInput("foo3",
                                     label = "Choose", 
                                     choices = c(1,2,3,4,5), 
                                     selected = 3)
                     ),
                     mainPanel(DT::dataTableOutput("def_table3"))
                 )
             )
    ), 
    
    
    tabPanel(id = "4_Prev",
             fluidRow(column(3,
                             uiOutput("h2_title4"), class = "container")
             ), 
             mainPanel(
                 sidebarLayout(
                     sidebarPanel(
                         width = 3, 
                         selectInput("foo4",
                                     label = "Choose", 
                                     choices = c(1,2,3,4,5), 
                                     selected = 3)
                     ),
                     mainPanel(
                         p("A partir du modèle sélectionné, nous pouvons estimé que les entreprises suivantes vont connaître des difficultés",
                           br()),
                         DT::dataTableOutput("def_table4"),
                         p("Rajouter une carte de la france"))
                 )
             )
    )
)
)
