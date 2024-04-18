library(dygraphs)
library(ggplot2)
library(dplyr)
library(shiny)
library(htmlwidgets)
library(shiny)
library(htmlwidgets)
library(pivottabler)
library(tidyr)




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$h2_title1 <- renderUI({
        h3(paste0("Statistiques", sep = ""))
    })
    output$h2_title2 <- renderUI({
        h3(paste0("Choix de modèles", sep = ""))
    })
    output$h2_title3 <- renderUI({
        h3(paste0("Résultats", sep = ""))
    })
    output$h2_title4 <- renderUI({
        h3(paste0("En difficulté demain?", sep = ""))
    })
    
    
    output$slider_ui_randomForest <- renderUI({
        if (input$choiceModels %in% "RandomForest") {
            sliderInput("sm_rf", 
                        "Nombre de forêts aléatoires:",
                        min = 0, max = 10, value = 5)
        } else {
            tag(NULL)
        }
    })
    
    
    
    
    
    
    
    
    
    output$tab1_PT <- DT::renderDataTable({
        
        datatable(
            db_defaillance %>% pivot_wider(id_cols = annee, names_from = mois, values_from = nb_defaillance),
            caption = "Nombre de défaillance sur la période récente", 
            rownames = FALSE)
    })
    
    output$def_table2 <- DT::renderDataTable({
        db_defaillance
    })
    output$def_table3 <- DT::renderDataTable({
        db_defaillance
    })
    output$def_table4 <- DT::renderDataTable({
        db_defaillance
    })
    
    # Matrice de confusion
    output$mat_conf1 <- DT::renderDataTable({
        nb_rf <- min(1,input$sm_rf)
        selected_DB <- select(DF_entrainement, matches(c("Y",
                                                         paste0("^rf_[1-",nb_rf,"]$"))))
        
        selected_DB_v2 <- add_var_model(selected_DB, input$slider_Threshold)
        
        selected_DB_v3 <- selected_DB_v2 %>% 
            rename(ChoixMajoritaire = top_voteMajo, 
                   Moyenne_Pourcentage = top_percMean, 
                   Un_Modele_Suffit = top_votePosi ) %>%
            group_by(Y,!!sym(input$choiceIndicators)) %>%
            summarise(n = n())
    
        selected_DB_v3
    })
    
})