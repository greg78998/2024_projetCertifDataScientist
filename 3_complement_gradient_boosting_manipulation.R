
# 3 | DIVERSES ANALYSES SUR DF_TEST POUR SHINY
#### Table de paramétrage issue de l'hypertuning du programme 3 à éventuellement automatiser
param_xgboost <- data.frame(
  learn_rate=c(0.15,0.20,0.25,0.3,0.4,0.5,0.6),
  mtry=c(88,67,92,80,88,93,93),
  min_n=c(23,23,29,29,23,29,29)
)

##### XGBOOST
results_list_xgb <- list()
#DF_entrainement_S <- data.frame(Y = training$Y)
DF_test_S <- data.frame(Y = test_set$Y)
training$Y <- factor(training$Y)
test_set$Y <- factor(test_set$Y)
train_set$Y <- factor(train_set$Y)
set.seed(1)

# Créer une fonction pour entraîner un modèle avec des paramètres spécifiques
calibrage_shiny <- function(parametres) {
  # Définir les paramètres du modèle
  xgb_mod <- boost_tree(learn_rate = parametres$learn_rate,
                        mtry = parametres$mtry,
                        min_n = parametres$min_n) %>% #min_n=profondeur d'arbre, mtry=nb de feuilles
    set_engine('xgboost') %>%
    set_mode('classification')
  
  # Entraîner le modèle
  xgb_fit <- workflow() %>%
    add_recipe(rec) %>%
    add_model(xgb_mod) %>%
    fit(training)
  
  return(xgb_fit)
}

# Boucle sur les différentes combinaisons de paramètres
for (pho in 1:nrow(param_xgboost)) {
  print(paste0("tour de boucle :", pho))
  
  # Entraîner le modèle avec les paramètres spécifiques
  xgb_fit <- calibrage_shiny(param_xgboost[pho, ])
  
  # Obtenir les prédictions sur l'ensemble d'entraînement
  #DF_entrainement_S[, paste("xgb_mod_", pho, sep = "")] <- predict(xgb_fit, training, type = "prob")[, 2]
  
  # Obtenir les prédictions sur l'ensemble de test
  DF_test_S[, paste("xgb_mod_", pho, sep = "")] <- predict(xgb_fit, test_set, type = "prob")[, 2]

  }



# Courbes ROC
#library(pROC)
#roc_data <- roc(Y~.,data=DF_test)
#plot(roc_data$xgb_mod_1)


# Boucle sur les différentes combinaisons de paramètres dans param_xgboost
for (i in 1:nrow(param_xgboost)) {
  # Affichage des courbes de densité
  print(ggplot(DF_test_S, aes_string(x = paste0("xgb_mod_", i), fill = "factor(Y)")) + 
          geom_density(alpha = 0.5) +
          scale_fill_manual(values = c("blue", "red")) +
          labs(title = paste0("Densité des prédictions selon Y pour learning rate de ", param_xgboost[i,"learn_rate"]),
               x = "Prédictions (YP)",
               y = "Densité") +
          theme_minimal())
}

# Table de mapping des densités aux learning rates pour Shiny
lr_rate_mapping <- data.frame(
  learning_rate=c("0.15","0.20","0.25","0.3","0.4","0.5","0.6"),
  variable_name=c("xgb_mod_1","xgb_mod_2","xgb_mod_3","xgb_mod_4","xgb_mod_5","xgb_mod_6","xgb_mod_7")
)

###### Sauvegarde de metriques_pour_Shiny pour l'application SHINY
saveRDS(lr_rate_mapping,
        file = paste0(path_data_vf,"/","lr_rate_mapping.RDS") )

# Transformer les valeurs de Y en 0 et 1 (à faire en dehors d'une boucle)
#DF_test_S$Y <- as.numeric(DF_test_S$Y)
#DF_test_S$Y <- ifelse(DF_test_S$Y == 2, 1, 0)


# Initialisation de la table pour stocker les métriques pour Shiny (à faire en dehors d'une boucle)
metriques_pour_Shiny <- data.frame(learn_rate = numeric(),
                                   roc_auc=numeric(),
                                   mtry = numeric(),
                                   min_n = numeric(),
                                   accuracy = numeric(),
                                   accuracy_thresh = numeric(),
                                   recall = numeric())

# Comparaison des métriques des différents modèles
for (i in 1:nrow(param_xgboost)) {
  accuracy <- function(X,Y,seuil=0.5){
    Xc=X*0
    Xc[X>seuil] <- 1
    sum(Xc==Y)/length(Y)*100
  }
  
  recall <- function(X,Y,seuil=0.5){
    Xc=X*0
    Xc[X>seuil] <- 1
    
    vrai_positif <- sum(Xc == 1 & Y == 1)
    positif_real <- sum(Y==1)
    
    vrai_positif / positif_real*100
  }  
  
  
  # Extraire les paramètres pour cette itération
  learn_rate <- param_xgboost[i, "learn_rate"]
  mtry <- param_xgboost[i, "mtry"]
  min_n <- param_xgboost[i, "min_n"]
  
  # Calcul des prédictions du modèle actuel
  predictions <- DF_test_S[, paste0("xgb_mod_", i)]
  
  
  # Calcul du ROC AUC 
  roc_auc <- roc(DF_test_S$Y, predictions)
  
  ###### Sauvegarde des DF_entrainement_S et DF_test_S pour l'application SHINY
  saveRDS(DF_test_S,
          file = paste0(path_data_vf,"/",dt_placement,'_',forme_dt,'_',"basesPREVISION_test_S",".RDS") )
  
  # Calcul des métriques pour le modèle actuel
  acc <- accuracy(X=predictions, Y=DF_test_S$Y)
  recall_0.07 <- recall(X=predictions, Y=DF_test_S$Y, seuil = 0.07)
  recall_0.06 <- recall(X=predictions, Y=DF_test_S$Y, seuil = 0.06)
  recall_0.05 <- recall(X=predictions, Y=DF_test_S$Y, seuil = 0.05)
  recall_0.04 <- recall(X=predictions, Y=DF_test_S$Y, seuil = 0.04)
  recall_0.03 <- recall(X=predictions, Y=DF_test_S$Y, seuil = 0.03)
  recall_0.02 <- recall(X=predictions, Y=DF_test_S$Y, seuil = 0.02)
  recall_0.01 <- recall(X=predictions, Y=DF_test_S$Y, seuil = 0.01)
  recall_0.005 <- recall(X=predictions, Y=DF_test_S$Y, seuil = 0.005)
  recall_0.0025 <- recall(X=predictions, Y=DF_test_S$Y, seuil = 0.0025)
  
  
  # Stocker les métriques dans la table metriques_pour_Shiny
  metriques_pour_Shiny <- rbind(metriques_pour_Shiny,
                                data.frame(learn_rate = learn_rate,
                                           roc_auc = roc_auc$auc,
                                           mtry = mtry,
                                           min_n = min_n,
                                           accuracy = acc,
                                           recall_seuil_0.07 = recall_0.07,
                                           recall_seuil_0.06 = recall_0.06,
                                           recall_seuil_0.05 = recall_0.05,
                                           recall_seuil_0.04 = recall_0.04,
                                           recall_seuil_0.03 = recall_0.03,
                                           recall_seuil_0.02 = recall_0.02,
                                           recall_seuil_0.01 = recall_0.01,
                                           recall_seuil_0.005 = recall_0.005,
                                           recall_seuil_0.0025 = recall_0.0025)
  )
}

# Formater et afficher les métriques pour Shiny
metriques_pour_Shiny <- round(metriques_pour_Shiny, digits=2)
print(metriques_pour_Shiny)


###### Sauvegarde de metriques_pour_Shiny pour l'application SHINY
saveRDS(metriques_pour_Shiny,
        file = paste0(path_data_vf,"/","metriques_pour_Shiny.RDS") )