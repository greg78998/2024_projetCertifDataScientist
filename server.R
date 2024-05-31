shinyServer(function(input, output) {
  
    output$h2_title1 <- renderUI({
        h3(paste0("Statistiques", sep = ""))
    })
    
    output$h2_title2 <- renderUI({
        h3(paste0("Choix de modèles", sep = ""))
    })
    output$h2_title_Calibrage <- renderUI({
        h3(paste0("Focus sur XGBoost : variables significatives et calibrage", sep = ""))
    })
    output$h2_title3 <- renderUI({
        h3(paste0("Résultats", sep = ""))
    })
    output$h2_title4 <- renderUI({
        h3(paste0("En difficulté demain?", sep = ""))
    })
    
    output$sous_texte_1 <- renderUI({
        h3(paste0("#Agriculture#changementClimatique#procedureCollective", sep = ""))
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
    
    output$tab1_plot_comparaison <- renderPlot({
        ggplot(data = compar_siete_webstat_2, aes(x = Annee, y = Defaillances, color = Source)) +
            geom_line(size = 1.2) + 
            geom_point(size = 3) +
            theme_minimal() +
            labs(title = "Évolution des Défaillances par Année",
                 x = "Année",
                 y = "Nombre de Défaillances",
                 color = "Source") +
            scale_color_manual(values = c("Defaillances_SIETE" = "steelblue", "Defaillances_WEBSTAT" = "darkorange")) +
            theme(
                plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 12),
                legend.title = element_text(size = 12),
                legend.text = element_text(size = 10)
            )
    })
    
    output$tab1_cor_matrix_avt <- renderPlot({
        corrplot(cor_matrix_avt, method = "color", type = "upper", 
                 tl.col = "black", tl.srt = 45, 
                 addCoef.col = "black", number.cex = 0.7, 
                 col = colorRampPalette(c("red", "white", "blue"))(200))
    })
    
    output$tab1_cor_matrix_aps <- renderPlot({
        corrplot(cor_matrix_aps, method = "color", type = "upper", 
                 tl.col = "black", tl.srt = 45, 
                 addCoef.col = "black", number.cex = 0.7, 
                 col = colorRampPalette(c("red", "white", "blue"))(200))
    })
    
    output$tab1_boxplot_var1 <- renderPlot({
        ggplot(DB_stats, aes(x = "", y = ca_moy_2014_2021)) + geom_boxplot() + labs(title = "ca_moy_2014_2021", y = "ca_moy_2014_2021") + theme(axis.text.x = element_blank())
    })
    
    output$tab1_boxplot_var2a <- renderPlot({
        ggplot(DB_stats, aes(x = "", y = effectifs_moy_2014_2021)) + geom_boxplot() + labs(title = "effectifs_moy_2014_2021 avant retraitement", y = "effectifs_moy_2014_2021") + theme(axis.text.x = element_blank())
    })
    
    output$tab1_boxplot_var2b <- renderPlot({
        ggplot(DB, aes(x = "", y = effectifs_moy_2014_2021)) + geom_boxplot() + labs(title = "effectifs_moy_2014_2021 après retraitement", y = "effectifs_moy_2014_2021") + theme(axis.text.x = element_blank())
    })
    
    output$tab1_boxplot_var3a <- renderPlot({
        ggplot(DB_stats, aes(x = "", y = Besoin_en_FDR_moy)) + geom_boxplot() + labs(title = "Besoin en fond de roulement (DFDR) moyen avant retraitement", y = "Besoin_en_FDR_moy") + theme(axis.text.x = element_blank())    
    })
    
    output$tab1_boxplot_var3b <- renderPlot({
        ggplot(DB, aes(x = "", y = Besoin_en_FDR_moy)) + geom_boxplot() + labs(title = "Besoin en fond de roulement (DFDR) moyen après retraitement", y = "Besoin_en_FDR_moy") + theme(axis.text.x = element_blank())
    })

    output$tab1_boxplot_var4a <- renderPlot({
        ggplot(DB_stats, aes(x = "", y = Total_actif_circulant_moy)) + geom_boxplot() + labs(title = "Total de l'actif circulant moyen avant retraitement", y = "Total_actif_circulant_moy") + theme(axis.text.x = element_blank())    
    })
    
        output$tab1_boxplot_var4b <- renderPlot({
        ggplot(DB, aes(x = "", y = Total_actif_circulant_moy)) + geom_boxplot() + labs(title = "Total de l'actif circulant moyen après retraitement", y = "Total_actif_circulant_moy") + theme(axis.text.x = element_blank())
    })

        output$tab1_boxplot_var5a <- renderPlot({
            ggplot(DB_stats, aes(x = "", y = Dettes_four_et_comptes_ratt_moy)) + geom_boxplot() + labs(title = "Dettes fournisseurs et comptes rattachées moyen avant retraitement", y = "Dettes_four_et_comptes_ratt_moy") + theme(axis.text.x = element_blank())
    })    
        output$tab1_boxplot_var5b <- renderPlot({
            ggplot(DB, aes(x = "", y = Dettes_four_et_comptes_ratt_moy)) + geom_boxplot() + labs(title = "Dettes fournisseurs et comptes rattachées moyen après retraitement", y = "Dettes_four_et_comptes_ratt_moy") + theme(axis.text.x = element_blank())
    })    
      
        output$tab1_boxplot_var6a <- renderPlot({
            ggplot(DB_stats, aes(x = "", y = Total_dettes_moy)) + geom_boxplot() + labs(title = "Total des dettes avant retraitement", y = "Total_dettes_moy") + theme(axis.text.x = element_blank())        
    })    
        
        output$tab1_boxplot_var6b <- renderPlot({
            ggplot(DB, aes(x = "", y = Total_dettes_moy)) + geom_boxplot() + labs(title = "Total des dettes après retraitement", y = "Total_dettes_moy") + theme(axis.text.x = element_blank())        
    })    
    
        output$tab1_boxplot_var7a <- renderPlot({
            ggplot(DB_stats, aes(x = "", y = Resultat_comptable_moy)) + geom_boxplot() + labs(title = "Résultat comptable (bénéfice ou perte) avant retraitement", y = "Resultat_comptable_moy") + theme(axis.text.x = element_blank())
    })    
        
        output$tab1_boxplot_var7b <- renderPlot({
            ggplot(DB, aes(x = "", y = Resultat_comptable_moy)) + geom_boxplot() + labs(title = "Résultat comptable (bénéfice ou perte) après retraitement", y = "Resultat_comptable_moy") + theme(axis.text.x = element_blank())
    })  
        

    
    # Matrice de confusion
    
    output$confusionMatrix_1 <- renderPlot({
        
        matrixPlot1 <- X_compute_confusion_matrix(para_nb_rf = input$sm_rf, 
                                                  para_nb_logit = input$sm_logit, 
                                                  para_nb_xgb = input$sm_xgb,
                                                  para_threshold = input$slider_Threshold, 
                                                  para_db = DF_entrainement, 
                                                  para_var_selected = input$choiceIndicators)
        
        
        plot_cm_1 <- X_draw_plot_confusion_matrix(matrixPlot1)
        
        print(plot_cm_1)
        
    })
    
    output$confusionMatrix_2 <- renderPlot({
        
        matrixPlot2 <- X_compute_confusion_matrix(para_nb_rf = input$sm_rf, 
                                                  para_nb_logit = input$sm_logit,
                                                  para_nb_xgb = input$sm_xgb,
                                                  para_threshold = input$slider_Threshold, 
                                                  para_db = DF_test, 
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
            para_db = DF_demain_pred, 
            para_var_selected = input$choiceIndicators)
        
        pred_db2 <- predire_les_defaillances_demain %>% 
            cbind(DF_demain_brut) %>% 
            filter(predicted_labels == 1) %>% 
            filter(nj %in% input$filter_nj, 
                   ape %in% input$filter_ape,
                   dep_num_name %in% input$filter_dep) %>% 
            select(siren, nj, ape,department, dep_num_name, region, ent_age, emails)
        
        pred_db2
    })
    
    selected_departments <- reactive({
        subset(region_departement, region == input$filter_region)$dep_num_name
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
            ), 
            selected = selected_departments()
        )
    })
    
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
    
    df_explore_FT <- reactive({
        
        xx <- X_compute_confusion_matrix(para_nb_rf = input$sm_rf, 
                                         para_nb_logit = input$sm_logit,
                                         para_nb_xgb = input$sm_xgb,
                                         para_threshold = input$slider_Threshold, 
                                         para_db = DF_test, 
                                         para_var_selected = input$choiceIndicators)
        
        explore_FT <- test_set %>% bind_cols(xx) %>%
            select(-c(Y,dt)) %>%
            filter(predicted_labels == 0 & true_labels == 1)
        
        explore_FT
    })
    
    output$exploration_result <- DT::renderDataTable({
        datatable(df_explore_FT(), 
                  options = list(pageLength = 20, 
                                 autoWidth = TRUE))
    })
    
    output$download_selected_explore_mal_predit <- downloadHandler(
        filename = function() {
            paste0("donnees_mal_predite",".csv") 
        },
        content = function(file) {
            write.csv(df_explore_FT(), file, row.names = FALSE)
        }
    )
    
    output$importance_plot <- renderPlot({

      # Importance des variables (méthode VIP) dans le modèle XGBoost
        print(importance_plot)
    })    
    
    output$density_plot <- renderPlot({
        # Récupérer les données de densité NON normalisées correspondant au learning rate sélectionné
        selected_variable <- lr_rate_mapping$variable_name[lr_rate_mapping$learning_rate==input$learning_rate]
        
        # Récupérer les données de densité correspondant au nom de variable
        selected_density <- DF_test_S[[selected_variable]]
        
        # Création du graphique
        ggplot(DF_test_S, aes_string(x = selected_variable, fill = "factor(Y)")) + 
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
        output$selected_metrics <- DT::renderDataTable({
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
