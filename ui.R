library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(corrplot)

# UI
ui <- dashboardPage(
    
    dashboardHeader(title = "Dashboard"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Statistiques", tabName = "stats", icon = icon("chart-bar")),
            menuItem("Choix de modèles", tabName = "models", icon = icon("cogs")),
            menuItem("Fine Tuning du XGBoost", tabName = "fine_tuning_xgb", icon = icon("sliders-h")),
            menuItem("Comprendre le Modèle", tabName = "comprendre_modele", icon = icon("info-circle")),
            menuItem("Extension par des ensembles", tabName = "results", icon = icon("table")),
            menuItem("Prévisions", tabName = "predictions", icon = icon("chart-line"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "stats",
                    fluidRow(
                        box(
                            title = "Statistiques", status = "primary", solidHeader = TRUE, width = 12,
                            column(3, uiOutput("h2_title1")),
                            tabBox(
                                id = "tab_stats", width = 12,
                                
                                tabPanel("Approche exploratoire des défaillances",
                                         p(uiOutput("sous_texte_1")),
                                         p("En difficulté : une entreprise agricole qui se voit concernée par une procédure collective (RJ ou LJ) dans les 12 derniers mois (méthodologie BDF)."),
                                         DT::dataTableOutput("tab1_PT"),
                                         br(),
                                         br(),
                                         p("Comparaison du nombre de défaillances annuelles entre les données de défaillances utilisées (source SIETE) et les données publiques de défaillances de la Banque de France :"),
                                         plotOutput("tab1_plot_comparaison")),
                                
                                tabPanel("Sous/sur représentation ?",
                                         sidebarPanel(
                                             width = 3,
                                             selectInput("graph_inter", 
                                                         "Variables choisies", 
                                                         choices = c("region","nj_ret","ape_ret","ent_age_ret"),
                                                         selected = c("region")
                                             )
                                         ),
                                         plotOutput("graph_inter")
                                ),
                                tabPanel("Variables financières",
                                         p("Matrices de corrélation des variables financières (source ESANE) avant et après retrait des variables trop corrélées :"),
                                         fluidRow(
                                             column(6, plotOutput("tab1_cor_matrix_avt", height = "400px")),
                                             column(6, plotOutput("tab1_cor_matrix_aps", height = "400px"))
                                         ),
                                         br(),
                                         br(),
                                         br(),
                                         p("Boîtes à moustaches des variables financières avant et après élimination des observations très atypiques :"),
                                         fluidRow(
                                             column(6, plotOutput("tab1_boxplot_var2a")),
                                             column(6, plotOutput("tab1_boxplot_var2b")),
                                             column(6, plotOutput("tab1_boxplot_var3a")),
                                             column(6, plotOutput("tab1_boxplot_var3b")),
                                             column(6, plotOutput("tab1_boxplot_var4a")),
                                             column(6, plotOutput("tab1_boxplot_var4b")),
                                             column(6, plotOutput("tab1_boxplot_var5a")),
                                             column(6, plotOutput("tab1_boxplot_var5b")),
                                             column(6, plotOutput("tab1_boxplot_var6a")),
                                             column(6, plotOutput("tab1_boxplot_var6b")),
                                             column(6, plotOutput("tab1_boxplot_var7a")),
                                             column(6, plotOutput("tab1_boxplot_var7b"))
                                         )
                                ),
                                tabPanel("Variables climatiques",
                                         pickerInput("chirps_region", 
                                                     "Filtrer sur la région", 
                                                     choices = unique(region_departement$region), 
                                                     options = list(`actions-box` = TRUE, `live-search` = TRUE, `style` = "btn-info"), 
                                                     multiple = TRUE, 
                                                     selected = "Occitanie"),
                                         sliderInput("choice_interval", "Combien en mois", min = 11, max = 50, value = 20),
                                         
                                         plotOutput("graphi_chirps2"),
                                         plotOutput("graphi_chirps")
                                )
                            )
                        )
                    )
            ),
            tabItem(tabName = "models",
                    fluidRow(
                        box(
                            title = "Choix de modèles", status = "primary", solidHeader = TRUE, width = 12,
                            column(3, uiOutput("h2_title2")),
                            p("Onglet qui vise à expliciter la méthodologie suivie pour sélectionner les modèles ensuite utilisés"),
                            h3("Comparaison des roc_auc des différents modèles en fonction de la forme choisie :"),
                            DT::dataTableOutput("model_results_pour_Shiny")
                        )
                    )
            ),
            tabItem(tabName = "fine_tuning_xgb",
                    fluidRow(
                        box(
                            title = "Fine Tuning du XGBoost", status = "primary", solidHeader = TRUE, width = 12,
                            column(3, uiOutput("h2_title_Calibrage")),
                            sidebarLayout(
                                sidebarPanel(
                                    width = 3,
                                    selectInput("learning_rate", "Choisir Learning Rate pour les courbes de densité :", choices = lr_rate_mapping$learning_rate, selected = "0.15")
                                ),
                                mainPanel(
                                    p("La courbe de densité des prédictions permet d'évaluer la capacité discriminante du modèle XGBoost. Elle permet de voir la distribution des prédictions de chaque classe et d'identifier facilement si le modèle a tendance à bien séparer les classes."),
                                    p("La courbe de densité est donc utilisée pour déterminer le meilleur taux d'apprentissage (learning rate) de notre modèle, qui est son hyperparamètre le plus important."),
                                    plotOutput("density_plot", height = "400px", width = "700px"),
                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    p("Ci-dessous, les métriques (précision et rappel) calculées sur les données de test pour les différents taux d'apprentissage."),
                                    p("Le seuil est la probabilité au-delà de laquelle une instance est prédite comme positive. Augmenter le seuil diminue le nombre de défaillances prédites (et donc le rappel), et inversément."),
                                    p("On observe que les probabilité de défaillance prédites diminuent en fonction du learning rate. Il faudra donc adapter le seuil suivant le learning rate choisi."),
                                    h2("Métriques considérées"),
                                    DT::dataTableOutput("selected_metrics")
                                )
                            )
                        )
                    )
            ),
            tabItem(tabName = "comprendre_modele",
                    fluidRow(
                        box(
                            title = "Comprendre le Modèle", 
                            status = "primary", 
                            solidHeader = TRUE, 
                            width = 12,
                            sidebarPanel(
                                selectInput("xgb_choice","Choix d'un xgb?", choices = 1:10, selected = 1),
                                sliderInput("fe_importance_nb", "Nombre de features?", min = 10, max = 50, value = 20)
                            ),
                            mainPanel(
                            p("Suite à la comparaison des modèles à l'onglet suivant, le Extreme Gradient Boosting pour la forme mêlant interactions et polynômes nous apparaît être le plus performant."),
                            p("Il sera notre modèle principal (qui pourra toutefois être mélangé avec d'autres modèles selon le souhait du métier, cf. onglet Résultats)."),
                            p("Avoir un modèle unique de Machine Learning permet une appropriation plus simple par le métier."),
                            p("Identification des variables ayant un effet significatif sur la capacité du modèle à discriminer les sociétés en défaillances ou pas. La méthode VIP (Variable Importance in Projection) a été utilisée pour le modèle XGBoost sur les données d'apprentissage :"),
                            
                            plotOutput("importance_plot", height = "600px", width = "900px"),
                            br(),
                            br(),
                            p("L'importance des variables est ici calculée avec la méthode basée sur le gain. Cette méthode permet d'identifier quelles variables sont les plus influentes en termes de réduction de la fonction de coût et donc pour les prédictions du modèle."),
                            p("Par exemple, une variable avec une importance de 0.25 contribue 5 fois plus qu'une variable avec une importance de 0.05 à la réduction de la perte globale du modèle.")
                            )
                    )
            )
            ),
            tabItem(tabName = "results",
                    fluidRow(
                        box(
                            title = "Modèle d'ensemble", status = "primary", solidHeader = TRUE, width = 12,
                            column(3, uiOutput("h2_title3")),
                            sidebarLayout(
                                sidebarPanel(
                                    width = 3,
                                    sliderInput("sm_logit", "Nombre de logit:", min = 0, max = 1, value = 0),
                                    sliderInput("sm_rf", "Accroître le nombre d'arbres:", min = 0, max = 10, value = 1),
                                    sliderInput("sm_xgb", "Nombre de gradient boosting:", min = 0, max = 10, value = 5),
                                    selectInput("choiceIndicators", "Méthode d'agrégation des modèles", choices = c(choice_A, choice_B, choice_C), selected = choice_A),
                                    sliderInput("slider_Threshold", "Seuil de probabilité", min = 0, max = 0.5, step = 0.001, value = 0.2)
                                ),
                                mainPanel(
                                    p("Le présent outil permet de combiner les modèles"),
                                    p("Sur les données déjà rencontrées : "),
                                    plotOutput("confusionMatrix_1"),
                                    p("Sur des données jamais rencontrées : "),
                                    plotOutput("confusionMatrix_2"),
                                    p("Explorer les mal-predits"),
                                    downloadButton("download_selected_explore_mal_predit", "Données mal classées")
                                )
                            )
                        )
                    )
            ),
            tabItem(tabName = "predictions",
                    fluidRow(
                        box(
                            title = "Prévisions", status = "primary", solidHeader = TRUE, width = 12,
                            column(3, uiOutput("h2_title4")),
                            sidebarLayout(
                                sidebarPanel(
                                    width = 3,
                                    downloadButton("download_selected", "Télécharger les données filtrées"),
                                    pickerInput("filter_region", "Filtrer sur la région", choices = unique(region_departement$region), options = list(`actions-box` = TRUE, `live-search` = TRUE, `style` = "btn-info"), multiple = TRUE, selected = unique(region_departement$region)),
                                    uiOutput("department_select"),
                                    selectInput("filter_ape", "Filtrer sur le code APE", choices = demain_ape$ape, selectize = TRUE, multiple = TRUE, selected = demain_ape$ape),
                                    selectInput("filter_nj", "Filtrer sur la nature juridique", choices = demain_nj$nj, selectize = TRUE, multiple = TRUE, selected = demain_nj$nj)
                                ),
                                mainPanel(
                                    p("A partir du modèle sélectionné dans le quatrième onglet, nous pouvons faire des prévisions sur les entreprises en difficulté demain."),
                                    DT::dataTableOutput("def_table4")
                                )
                            )
                        )
                    )
            )
        )
    )
)
