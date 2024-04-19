library(dygraphs)
library(ggplot2)
library(dplyr)
library(shiny)
library(htmlwidgets)
library(shiny)
library(htmlwidgets)
library(pivottabler)
library(tidyr)
library(caret)



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
    
    
    output$slider_ui_randomforest <- renderUI({
        if(input$top_slider_rf) {
            sliderInput("sm_rf", 
                        "Nombre de forêts aléatoires:",
                        min = 0, max = 10, value = 5)
        } 
    })
    output$slider_ui_logit <- renderUI({
        if(input$top_slider_logit) {
            sliderInput("sm_logit", 
                        "Nombre de logit:",
                        min = 0, max = 2, value = 1)
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
        
        nb_rf <- input$sm_rf
        nb_logit <- input$sm_logit
        
        selected_DB <- select(DF_entrainement, matches(c("Y",
                                                         paste0("^rf_[0-",nb_rf,"]$"),
                                                         paste0("^logit_[0-",nb_logit,"]$"))
        )
        )
        
        selected_DB_v2 <- add_var_model(selected_DB, input$slider_Threshold)
        
        selected_DB_v3 <- selected_DB_v2
        selected_DB_v3[,"Y"] <- factor(selected_DB_v3$Y, levels = c(0,1), labels = ls_label)
        selected_DB_v3[,choice_A] <- factor(selected_DB_v3$top_voteMajo, levels = c(0,1), labels = ls_label)
        selected_DB_v3[,choice_C] <- factor(selected_DB_v3$top_percMean, levels = c(0,1), labels = ls_label)
        selected_DB_v3[,choice_B] <- factor(selected_DB_v3$top_votePosi, levels = c(0,1), labels = ls_label)
        
        
        y <- data.frame(
            round(
                table(selected_DB_v3[,c("Y",input$choiceIndicators)])/dim(selected_DB_v3)[1]*100,
                2)
        )
        y
        
        
    })
    
    output$mat_conf2 <- DT::renderDataTable({
        
        nb_rf <- input$sm_rf
        nb_logit <- input$sm_logit
        
        test_DB <- select(DF_test, 
                          matches(c("Y",
                                    paste0("^rf_[0-",nb_rf,"]$"),
                                    paste0("^logit_[0-",nb_logit,"]$"))
                          )
        )
        
        test_DB_v2 <- add_var_model(test_DB, input$slider_Threshold)
        test_DB_v3 <- test_DB_v2
        test_DB_v3[,"Y"] <- factor(test_DB_v3$Y, levels = c(0,1), labels = ls_label)
        test_DB_v3[,choice_A] <- factor(test_DB_v3$top_voteMajo, levels = c(0,1), labels = ls_label)
        test_DB_v3[,choice_C] <- factor(test_DB_v3$top_percMean, levels = c(0,1), labels = ls_label)
        test_DB_v3[,choice_B] <- factor(test_DB_v3$top_votePosi, levels = c(0,1), labels = ls_label)
        
        x <- data.frame(
            round(
                table(test_DB_v3[,c("Y",input$choiceIndicators)])/dim(test_DB_v3)[1]*100,
                2)
        )
        
        x
    })
    
})
