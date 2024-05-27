
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
#            mainPanel(
#                sidebarLayout(
#                    sidebarPanel(
#                        width = 3, 
#                        selectInput("foo1",
#                                    label = "Choose", 
#                                    choices = c(1,2,3,4,5), 
#                                    selected = 3)
#                    ),
                    
                    mainPanel(
                        p(uiOutput("sous_texte_1")),
                        br(), 
                        br(),
                        p("En difficulté : une entreprise agricole qui se voit concerné par une procédure collecte dans les 12 derniers mois (source : tribunaux de commerce)."),
                        br(), 
                        br(), 
                        p("Ce présent projet vise à étudier l'impact des chocs climatiques sur le déclenchement de procédure collective des entreprises agricoles."),
                        DT::dataTableOutput("tab1_PT"),
                        br(), 
                        br(),
                        br(),
                        p("Comparaison du nombre de défaillances annuelles entre les données de défaillances utilisées (source SIETE) 
                          et les données publiques de défillances de la Banque de France :"),
                        plotOutput("tab1_plot_comparaison"),
                        br(), 
                        br(),
                        br(),
                        p("Matrices de corrélation des variables financières (source ESANE) avant et après retrait des variables trop corrélées :"),
                        column(10, plotOutput("tab1_cor_matrix_avt",height="400px", width="700px")),
                        column(2, plotOutput("tab1_cor_matrix_aps",height="400px", width="700px")),
                        br(),
                        br(),
                        br(),
                        p("Boîtes à moustaches des variables financières avant et après élimination des observations très atypiques :"),
                        column(12,plotOutput("tab1_boxplot_var1")),
                        column(6,plotOutput("tab1_boxplot_var2a")),
                        column(6,plotOutput("tab1_boxplot_var2b")),
                        column(6,plotOutput("tab1_boxplot_var3a")),
                        column(6,plotOutput("tab1_boxplot_var3b")),
                        column(6,plotOutput("tab1_boxplot_var4a")),
                        column(6,plotOutput("tab1_boxplot_var4b")),
                        column(6,plotOutput("tab1_boxplot_var5a")),
                        column(6,plotOutput("tab1_boxplot_var5b")),
                        column(6,plotOutput("tab1_boxplot_var6a")),
                        column(6,plotOutput("tab1_boxplot_var6b")),
                        column(6,plotOutput("tab1_boxplot_var7a")),
                        column(6,plotOutput("tab1_boxplot_var7b"))
                        )
        ),
        
        tabPanel(id = "2_Models",
                 fluidRow(column(3,
                                 uiOutput("h2_title2"), class = "container")
                 ), 
#                 mainPanel(
#                     sidebarLayout(
#                         sidebarPanel(
#                             width = 3, 
#                             selectInput("foo2",
#                                         label = "Choose", 
#                                         choices = c(1,2,3,4,5), 
#                                         selected = 3)
#                         ),
                         mainPanel( p("Onglet qui vise à expliciter la méthodologie suivie pour sélectionner les modèles ensuite utilisés"),
                                    br(),
                                    h3("Comparaison des roc_auc des différents modèles en fonction de la forme choisie :"),
                                    DT::dataTableOutput("model_results_pour_Shiny"))
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
                        p("Suite à la comparaison des modèles à l'onglet suivant, le Extreme Gradient Boosting pour la forme mêlant interactions et polynômes nous apparaît être le plus performant."),
                        p("Il sera notre modèle principal (qui pourra toutefois être mélangé avec d'autres modèles selon le souhait du métier, cf. onglet Résultats)."),
                        p("Avoir un modèle unique de Machine Learning permet une appropriation plus simple par le métier."),
                        br(),
                        br(),
                        br(),
                        p("Identification des variables ayant un effet significatif sur la capacité du modèle à discriminer les sociétés en défaillances ou pas. 
                          La méthode VIP (Variable Importance in Projection) a été utilisée pour le modèle XG Boost sur les données d'apprentissage :"),
                        plotOutput("importance_plot", height="400px", width="700px"),
                        br(),
                        br(),
                        br(),
                        # Graphique de densité
                        p("La courbe de densité des prédictions permet d'évaluer la capacité discriminante du modèle de discrimation XG Boost.
                        Elle permet de voir la distribution des prédictions de chaque classe et d'identifier facilement si le modèle a tendance
                        à bien séparer les classes."),
                        p("La courbe de densité est donc utilisée pour déterminer le meilleur taux d'apprentissage (learning rate) de notre modèle, qui est son hyperparamètre le plus important."),
                        p("Ci-dessous, la courbe de densité sur les données de test :"),
                        plotOutput("density_plot", height="400px", width="700px"),
                        br(),
                        br(),
                        br(),
                        
                        # Affichage des métriques en fonction du learning rate sélectionné
                        p("Ci-dessous, les métriques (précision et rappel) calculées sur les données de test pour les différents taux d'apprentissage :"),
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
