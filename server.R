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
    output$h2_title_Calibrage <- renderUI({
        h3(paste0("Calibrage du XGBoost", sep = ""))
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
    
    output$density_plot <- renderPlot({
        # Récupérer les données de densité normalisées correspondant au learning rate sélectionné
        selected_variable <- lr_rate_mapping$variable_name[lr_rate_mapping$learning_rate==input$learning_rate]
        
        # Récupérer les données de densité correspondant au nom de variable
        selected_density <- DF_test[[selected_variable]]
        
        # Création du graphique
        ggplot(DF_test, aes_string(x = selected_variable, fill = "factor(Y)")) + 
            geom_density(alpha = 0.5) +
            scale_fill_manual(values = c("blue", "red")) +
            labs(title = paste0("Densité des prédictions normalisées selon Y pour learning rate de ", input$learning_rate),
                 x = "Prédictions (YP)",
                 y = "Densité") +
            theme_minimal() +
            theme(plot.title=element_text(size=20))
    })
    
    output$density_plot_2 <- renderPlot({
        # Récupérer les données de densité NON normalisées correspondant au learning rate sélectionné
        selected_variable <- lr_rate_mapping$variable_name_2[lr_rate_mapping$learning_rate==input$learning_rate]
        
        # Récupérer les données de densité correspondant au nom de variable
        selected_density <- DF_test[[selected_variable]]
        
        # Création du graphique
        ggplot(DF_test, aes_string(x = selected_variable, fill = "factor(Y)")) + 
            geom_density(alpha = 0.5) +
            scale_fill_manual(values = c("blue", "red")) +
            labs(title = paste0("Densité des prédictions selon Y pour learning rate de ", input$learning_rate),
                 x = "Prédictions (YP)",
                 y = "Densité") +
            theme_minimal() +
            theme(plot.title=element_text(size=20))
    })
    
    # Onglet CALIBRAGE XGBoost : réagir aux changements de la sélection du learning rate (abandonné a priori)
    observe({
        # Calculer les métriques en fonction du learning rate sélectionné
        #compute_metrics <- function(learning_rate){
        #    selected_metrics <- metriques_pour_Shiny[metriques_pour_Shiny$learn_rate==learning_rate,]
        #    return(selected_metrics)
        #}
        #selected_metrics <- compute_metrics(input$learning_rate)
        
        # Afficher les métriques
        output$selected_metrics <- renderDataTable({
            metriques_pour_Shiny
        })
    })
    
    # Onglet Choix du modèle : afficher les métriques
    output$model_results_pour_Shiny <- DT::renderDataTable({
        datatable(model_results_pour_Shiny, options = list(
            columnDefs = list(
                list(targets = c(2,6), render = DT::JS("function(data, type, row, meta) { return type === 'display' ? parseFloat(data).toFixed(6) : data; }"))
            )
        ))
    })
    
})
