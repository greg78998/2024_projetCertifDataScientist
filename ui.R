

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = "bootstrap.css",
    navbarPage(
        "Des entreprises agricoles en difficultés ?", 
        id  = "main_navbar",
        tabPanel(
            id = "1_Statistics",
            fluidRow(column(3,
                            uiOutput("h2_title1"), class = "container")), 
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
                        p(uiOutput("sous_texte_1")),
                        br(), 
                        br(),
                        p("En difficulté : une entreprise agricole qui se voit concerné par une procédure collecte dans les 12 derniers mois (source : tribunaux de commerce)."),
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
                                    br(),
                                    h3("Comparaison des roc_auc des différents modèles en fonction de la forme choisie :"),
                                    DT::dataTableOutput("model_results_pour_Shiny"))
                     )
                 )
        ), 
        
        tabPanel(
            id = "2b_Calibrage",
            fluidRow(column(3, uiOutput("h2_title_Calibrage"), class = "container")),
            mainPanel(
                sidebarLayout(
                    sidebarPanel(
                        width = 3,
                        # Sélection du learning rate 
                        selectInput("learning_rate", "Choisir Learning Rate pour les courbes de densité :",
                                    choices = lr_rate_mapping$learning_rate, selected = "0.15")
                    ),
                    mainPanel(
                        # Graphique de densité
                        plotOutput("density_plot", height="400px", width="700px"),
                        br(),
                        plotOutput("density_plot_2", height="400px", width="700px"),
                        br(),
                        br(),
                        br(),
                        
                        # Affichage des métriques en fonction du learning rate sélectionné
                        h2("Métriques considérées"), 
                        DT::dataTableOutput("selected_metrics")
                    )
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
                             sliderInput("sm_logit", 
                                         "Nombre de logit:",
                                         min = 0, max = 2, value = 0),
                             sliderInput("sm_rf", 
                                         "Nombre de forêts aléatoires:",
                                         min = 0, max = 10, value = 5),
                             sliderInput("sm_xgb", 
                                         "Nombre de gradient boosting:",
                                         min = 0, max = 10, value = 5),
                             
                             selectInput("choiceIndicators",
                                         label = "Méthode d'agrégation des modèles", 
                                         choices = c(choice_A,
                                                     choice_B,
                                                     choice_C),
                                         selected = choice_A, 
                                         multiple = FALSE),
                             
                             
                             sliderInput("slider_Threshold", 
                                         "Seuil de probabilité",
                                         min = 0, max = 0.5, step = 0.001,
                                         value = 0.2)
                         ),
                         mainPanel(
                             p("Le présent outil permet de combiner les modèles"),
                             br(), 
                             p("sur les données déjà rencontrées : "),
                             plotOutput("confusionMatrix_1"), 
                             p("Sur des données jamais rencontrées : "),
                             plotOutput("confusionMatrix_2"), 
                             p("Explorer les mal-predits"), 
                             downloadButton("download_selected_explore_mal_predit", "Données mal classées")
                         )
                     ))
        ), 
        
        tabPanel(id = "4_Prev",
                 fluidRow(column(3,
                                 uiOutput("h2_title4"), class = "container")
                 ), 
                 mainPanel(
                     sidebarLayout(
                         sidebarPanel(
                             width = 3,
                             downloadButton("download_selected", "Télécharger les données filtrées"),
                             br(),
                             pickerInput("filter_region","Filtrer sur la région", 
                                         choices=unique(region_departement$region), 
                                         options = list(
                                             `actions-box` = TRUE,
                                             `live-search` = TRUE,
                                             `style` = "btn-info"),
                                         multiple = TRUE, 
                                         selected = unique(region_departement$region)),
                             uiOutput("department_select"),
                             selectInput("filter_ape", 
                                         label = "Filtre sur le code APE", 
                                         choices = demain_ape$ape, 
                                         selectize = TRUE,
                                         multiple = TRUE, 
                                         selected = demain_ape$ape),
                             selectInput("filter_nj", 
                                         label = "Filter sur la nature juridique", 
                                         choices = demain_nj$nj, 
                                         selectize = TRUE, 
                                         multiple = TRUE, 
                                         selected = demain_nj$nj)
                         ),
                         mainPanel(
                             p("A partir du modèle sélectionné dans le quatrième onglet, nous pouvons faire des prévisions sur les entreprises en difficulté demain."),
                             br(),
                             DT::dataTableOutput("def_table4")
                         )
                     )
                 )
        )
        
    )
)
