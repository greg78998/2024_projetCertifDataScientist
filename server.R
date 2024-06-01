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
        h3(paste0("#Agriculture#changementClimatique#ProceduresCollectives", sep = ""))
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
    
    
    output$tab1_boxplot_var2a <- renderPlot({
      Graph_fi_B1    
      })
    
    output$tab1_boxplot_var2b <- renderPlot({
      Graph_fi_B2 
      })
    
    output$tab1_boxplot_var3a <- renderPlot({
      Graph_fi_C1    })
    
    output$tab1_boxplot_var3b <- renderPlot({
      Graph_fi_C2    })

    output$tab1_boxplot_var4a <- renderPlot({
      Graph_fi_D1    })
    
        output$tab1_boxplot_var4b <- renderPlot({
          Graph_fi_D2    })

        output$tab1_boxplot_var5a <- renderPlot({
          Graph_fi_E1    })    
        output$tab1_boxplot_var5b <- renderPlot({
          Graph_fi_E2    })    
      
        output$tab1_boxplot_var6a <- renderPlot({
          Graph_fi_F1    })    
        
        output$tab1_boxplot_var6b <- renderPlot({
          Graph_fi_F2    })    
    
        output$tab1_boxplot_var7a <- renderPlot({
          Graph_fi_H1    })    
        
        output$tab1_boxplot_var7b <- renderPlot({
          Graph_fi_H2    })  
        

    
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
    
    fi_data_graph <- reactive({
      xgb_fit <- readRDS(file = paste0(path_pg_models_save,"/",forme_dt,"_vf_xgb_n",input$xgb_choice,".RDS"))
      model <- extract_fit_parsnip(xgb_fit)
      model
    }) 
    
    
    filtered_data <- reactive({
        
        predire_les_defaillances_demain <- X_compute_confusion_matrix(
            para_nb_rf = input$sm_rf, 
            para_nb_logit = input$sm_logit,
            para_nb_xgb = input$sm_xgb,
            para_threshold = input$slider_Threshold, 
            para_db = DF_demain_pred, 
            para_var_selected = input$choiceIndicators)
        
        df_mean_prob <- DF_demain_pred 
        df_mean_prob$prob <- rowMeans(df_mean_prob %>% select(-Y)) 
        
        pred_db2 <- predire_les_defaillances_demain %>% 
            cbind(DF_demain_brut, 
                  df_mean_prob %>% select(prob)) %>% 
            filter(predicted_labels == 1) %>% 
            filter(nj %in% input$filter_nj, 
                   ape %in% input$filter_ape,
                   dep_num_name %in% input$filter_dep) %>% 
            select(siren, nj, ape,department, dep_num_name, region, ent_age, emails, prob)
        
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
    
    
    df_graph_interactive <- reactive({
      x_0_0 <- DB_forGraph %>% 
        rename(selected_var = input$graph_inter) %>%
        mutate(nbA = n()) %>%
        group_by(selected_var, nbA) %>%
        summarise(nb_top_A = n(), 
                  perc_A = nb_top_A / nbA*100) %>% 
        ungroup() %>% 
        distinct()
      
      x_0_1 <- DB_forGraph %>%
        rename(selected_var = input$graph_inter) %>%
        filter(top_defaillance == 1) %>% 
        mutate(nbB = n()) %>%
        group_by(selected_var,nbB) %>%
        summarise(nb_top_B = n(), 
                  perc_B = round(nb_top_B / nbB*100,1)) %>% 
        ungroup() %>% 
        distinct()
      
      xbis <- x_0_0 %>% 
        left_join(x_0_1, by = "selected_var") %>%
        mutate(top_statut = ifelse(
          (perc_B-perc_A)>1,"sur-representé", 
          ifelse((perc_B-perc_A)<1, "sous-representé","equi-representé")))
      
      xter <- xbis %>% select(selected_var, perc_A, perc_B, top_statut) %>% 
        pivot_longer(cols = c(perc_A, perc_B), names_to ="pop", values_to = "perc") %>% 
        mutate(perc = round(perc,2), 
               pop = factor(pop, levels = c("perc_A","perc_B"), labels = c("Ensemble de la population", "Entreprises en difficulté")))
      
    })
    
    output$graph_inter <- renderPlot({
      
      ggplot(data=df_graph_interactive(), 
             aes(x=selected_var, y = perc, col = top_statut, fill = top_statut)) +
        theme_test() +
        geom_bar(stat = 'identity') + 
        geom_text(aes(label = perc), nudge_y = 1) + 
        labs(title = "Ventilation et représentation") + 
        xlab("") + 
        ylab("Proportion (en %)") +
        facet_wrap(~pop) + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
      
    })
    
    chirps_graph <- reactive({
      graph_chirps <- DB_stats %>% 
        filter(region %in% input$chirps_region) %>%
        select(siren, region, top_defaillance, starts_with("rf_")) %>%
        pivot_longer(cols = starts_with("rf_M"), 
                     names_to = "per_ref") %>%
        mutate(per_ref2 = as.integer(substr(per_ref,6,7))) %>%   # pour que les valeurs soient comparables
        filter(per_ref2 <= input$choice_interval, 
               per_ref2 > 11) 
      
    })
    
    output$graphi_chirps <- renderPlot({
      
      ggplot() + 
        geom_line(data=chirps_graph(),
                  aes(x = desc(per_ref2), y = value, col = siren), show.legend = FALSE) +
        xlab("Lag") + 
        ylab("Rainfalls standardized") + 
        geom_hline(yintercept=0, linetype="dashed", 
                   color = "red", size=1) + 
        geom_vline(xintercept=-12, linetype="dashed", color = "red", size =1) +
        facet_grid(~top_defaillance) + 
        labs(title = "Analyse des séries de précipitation.",
             caption = "Un point représente une donnée climatique relative à un SIREN (source CHIRPS) et données géographiques")
      
    })
    
    output$graphi_chirps2 <- renderPlot({
      data <- chirps_graph() %>%
        group_by(per_ref2,top_defaillance) %>%
        summarize(
          mean_chirps = mean(value),
          sd_chirps = sd(value),
          min_chirps = min(value),
          max_chirps = max(value)
        )
      
      line_plot <- ggplot(data, 
                          aes(x = desc(per_ref2), y = mean_chirps)) +
        geom_line(color = "blue") +
        geom_ribbon(aes(ymin = mean_chirps - sd_chirps, ymax = mean_chirps + sd_chirps), alpha = 0.2) +
        labs(title = "Moyenne", x = "Lag", y = "rf") +
        theme_minimal() + 
        facet_grid(~top_defaillance) + 
        labs(title = "Des années avec une pluviométrie faible et d'autres avec une pluviométrie importante (effet local contrôlé).")
      
      line_plot
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
      importance_plot <- vip(fi_data_graph(), 
                             num_features=input$fe_importance_nb, 
                             geom="col", 
                             horiz=TRUE, 
                             aesthetics = list(fill="steelblue"))
      
      importance_plot
    })    
    
    output$density_plot <- renderPlot({
        # Récupérer les données de densité NON normalisées correspondant au learning rate sélectionné
        selected_variable <- lr_rate_mapping$variable_name_2[lr_rate_mapping$learning_rate==input$learning_rate]
        
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
 
        output$selected_metrics <- DT::renderDataTable({
            metriques_pour_Shiny
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
