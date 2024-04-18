
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
                         mainPanel( p("Onglet qui vise à expliciter la méthodologie suivie pour sélectionner les modèles ensuite utilisés"),
                                    DT::dataTableOutput("def_table2"))
                     )
                 )
        ), 
        
        tabPanel(id = "3_ModelsPREDICTIONS",
                 fluidRow(column(3,
                                 uiOutput("h2_title3"), class = "container")
                 ), 
                 mainPanel(
                     sidebarLayout(
                         sidebarPanel(
                             width = 3, 
                             selectInput("choiceModels",
                                         label = "Choix de la modélisation?", 
                                         choices = c("RandomForest"),
                                         selected = 'RandomForest', 
                                         multiple = TRUE),
                             selectInput("choiceIndicators",
                                         label = "IndicateurChoisi", 
                                         choices = c("ChoixMajoritaire",
                                                     "Moyenne_Pourcentage",
                                                     "Un_Modele_Suffit"),
                                         selected = 'ChoixMajoritaire', 
                                         multiple = FALSE),
                             
                             uiOutput("slider_ui_randomForest"),
                             
                             
                             sliderInput("slider_Threshold", 
                                         "Fixation du seuil",
                                         min = 0, max = 1, step = 0.01,
                                         value = 0.2)
                         ),
                         mainPanel(
                             p("Avec le modèle sélectionné (forêts aléatoires), nous allons restimer sur l'ensemble de la population (modulo un extrait des données qui nous permettra de voir si notre traitement ne fait pas de sur-appprentissage."),
                             br(), 
                             p("Nous allons estimer plusieurs modèles et produire des combinaisons de modèles"),
                             br(), 
                             p("Sur la population :"),
                             
                             br(), 
                             p("Sur des données jamais vu : "),
                             DT::dataTableOutput("mat_conf1"))
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
