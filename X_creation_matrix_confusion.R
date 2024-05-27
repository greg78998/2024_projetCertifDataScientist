
X_compute_confusion_matrix <- function(para_nb_rf, para_nb_logit, para_nb_xgb, 
                                       para_threshold, para_db, para_var_selected){
#  para_nb_rf <- 0
#  para_nb_logit <- 0
#  para_nb_xgb <- 3
#  para_threshold <- 0.1
#  para_db <- DF_test
#  para_var_selected <- choice_A
# rm(para_nb_rf, para_nb_logit, para_nb_xgb, para_db, para_var_selected, tab_plot_1, tab_plot_2, tab_plot_3)  
  
  tab_plot_1 <- select(para_db, 
                       matches(c("Y",
                                 paste0("^rf_mod_",0:para_nb_rf),
                                 paste0("^logit_mod_",0:para_nb_logit), 
                                 paste0("^xgb_mod_",0:para_nb_xgb)
                                 
                       )))

#  rm(para_DB)
#  rm(tab_plot_2)
#  rm(tab_plot_3)

  tab_plot_2 <- add_var_model(tab_plot_1, para_threshold)
  
  tab_plot_3 <- tab_plot_2 %>%
    select(Y, top_voteMajo, top_votePosi, top_percMean) %>%
    rename(Choix_Majoritaire = top_voteMajo,
           Un_modèle_suffit = top_votePosi,
           Sur_moyenne_probabilités = top_percMean) %>%
    rename(true_labels = Y, predicted_labels = para_var_selected)
  
  return(tab_plot_3)
}


X_draw_plot_confusion_matrix <- function(para_db){
  
  conf_matrix <- para_db %>%
    group_by(true_labels, predicted_labels) %>%
    summarise(nb = n()) %>%
    ungroup() %>%
    mutate(perc = nb/dim(para_db)[1],
           statut = case_when(
             true_labels == 1 & predicted_labels == 1 ~ "True positive",
             true_labels == 0 & predicted_labels == 1 ~ "False positive",
             true_labels == 0 & predicted_labels == 0 ~ "True negative",
             true_labels == 1 & predicted_labels == 0 ~ "False negative"
           ))
  
  
  recall <- as.numeric(
    conf_matrix %>%
      filter(true_labels == 1 & predicted_labels == 1) %>%
      select(nb)) / as.numeric(conf_matrix %>%
                                 filter(true_labels ==1) %>%
                                 summarise(tot = sum(nb)))*100
  unit <- 0.45
  
  # Visualiser la matrice de confusion; avec ggplot2
  tau <- ggplot(conf_matrix,
                aes(x = predicted_labels , y = true_labels)) +
    geom_point(colour = "red", alpha=0.15, show.legend = FALSE, size = 50) +
    # Afficher les valeurs dans la matrice
    geom_text(aes(label = nb), vjust = 0, size = 3.5) +
    geom_text(aes(label = statut), vjust = +2, size = 3.5) +
    # Étiquettes des axes
    labs(x = "Prédiction", y = "Vérité") +
    # Style de thème
    theme_minimal() +
    geom_rect(aes(xmin = 0-unit+0.1, xmax = 1+unit-0.1, ymin = 1-unit, ymax = 1+unit),
              fill = NA,colour = "blue", size = 1) +
    geom_rect(aes(xmin = -1+0.45, xmax = 2-0.5, ymin = -1+0.25, ymax = 2-0.25),
              fill = NA,colour = "black", size = 1) +
    xlim(-1,2) +
    annotate("text", x= 0.5, y = -0.5, label = "Prédiction", colour = "black", size = 5) + 
    annotate("text", x= -0.5, y = 0.5, label = "Vérité", colour = "black", size = 5, angle = 90) + 
    ylim(-1,2) +
    annotate("text", x = 0.5, y = 0.7, label = sprintf("Recall = TP / (TP + FN) = %.1f%%",recall),
             colour = "blue", size = 3.5) +
    theme_void()
  
    return(tau)
}
