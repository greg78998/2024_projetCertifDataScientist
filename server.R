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
library(shinyWidgets)



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
    
    # Matrice de confusion
    
    output$confusionMatrix_1 <- renderPlot({
        
        
        matrixPlot1 <- X_compute_confusion_matrix(para_nb_rf = input$sm_rf, 
                                                  para_nb_logit = input$sm_logit, 
                                                  para_nb_xgb = input$sm_xgb,
                                                  para_threshold = input$slider_Threshold, 
                                                  para_DB = DF_entrainement, 
                                                  para_var_selected = input$choiceIndicators)
        
        
        plot_cm_1 <- X_draw_plot_confusion_matrix(matrixPlot1)
        
        print(plot_cm_1)
        
        
    })
    
    output$confusionMatrix_2 <- renderPlot({
        
        
        matrixPlot2 <- X_compute_confusion_matrix(para_nb_rf = input$sm_rf, 
                                                  para_nb_logit = input$sm_logit,
                                                  para_nb_xgb = input$sm_xgb,
                                                  para_threshold = input$slider_Threshold, 
                                                  para_DB = DF_test, 
                                                  para_var_selected = input$choiceIndicators)
        
        
        plot_cm_2 <- X_draw_plot_confusion_matrix(matrixPlot2)
        
        print(plot_cm_2)
        
        
    })
    
    
    
    
    filtered_data <- reactive({
        
        predire_les_defaillances_demain <- X_compute_confusion_matrix(
            para_nb_rf = input$sm_rf, 
            para_nb_logit = input$sm_logit,
            para_nb_xgb = input$sm_xgb,
            para_threshold = input$slider_Threshold, 
            para_DB = predictionDemain, 
            para_var_selected = input$choiceIndicators)
        
        pred_db2 <- predire_les_defaillances_demain %>% 
            cbind(demain) %>% 
            filter(predicted_labels == 1) %>% 
            mutate(dep = substr(adr_depcom,1,2)) %>% 
            filter(nj %in% input$filter_nj, 
                   ape %in% input$filter_ape,
                   dep %in% input$filter_dep) %>% 
            select(siren, nj, ape, adr_depcom)
        
        pred_db2
    })
    
    selected_departments <- reactive({
        subset(region_departement, region == input$filter_region)$department
    })
    
    
    output$department_select <- renderUI({
        pickerInput(
            "filter_dep", "Choisissez un département :", 
            choices = selected_departments(),
            multiple = TRUE, # Autoriser la sélection unique
            options = list(
                `actions-box` = TRUE,
                `live-search` = TRUE, # Activer la recherche
                `style` = "btn-warning" # Changer le style du menu
            )
        )
    }
    )
    
    output$def_table4 <- DT::renderDataTable({
        
        datatable(filtered_data(), options = list(pageLength = 20, autoWidth = TRUE))
        
    })
    
    output$download_selected <- downloadHandler(
        filename = function() {
            paste0("data_",".csv") 
        },
        content = function(file) {
            write.csv(filtered_data(), file, row.names = FALSE)
        }
    )    
    
})
