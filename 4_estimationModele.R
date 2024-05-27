matricule <- "X822385"

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF", sep = "")
} 
if (matricule == "X822385"){
  path_USER <- "C:/Users/X822385/Desktop/DS/Projet"
}

# Chargement des utilisateurs 

chargement_modeles <- TRUE 
nb_model <- 1

forme_dt_ls <- c("simple","add_succ_surplus","add_succ_chocs","poly") 
forme_dt  <- paste(forme_dt_ls, collapse = "_") # pour les dénomination des modèles
dt_placement <-  as.Date("2023-12-31")

# SIMPLE : Pas de feature engineering (sauf retraitement)
# ADD : Ajout de variables            (ajout de variables)
# POLY :  polygon sur les variables météo

# ------------------------------------------------------------------------------ 


# 0 | chargement des libraries  -----

source(paste0(path_USER,"/pg_propre/","_before_chemins.R"))
source(paste0(path_USER,"/pg_propre/","_before_libraries.R"))


# 1 | chargement de la table + chargement des paramètres

source(paste0(path_USER,"/pg_propre/","X3_MeF_predictionModels.R"))
source(paste0(path_USER,"/pg_propre/","X6_assemblage_table.R"))


dt_placement <- readRDS(file = paste0(path_data_vf,"/","para_dt_placement.RDS"))
interval_month <- readRDS( file = paste0(path_data_vf,"/","para_interval_month.RDS"))
annee_nb <- readRDS( file = paste0(path_data_vf,"/","para_annee_nb.RDS"))

# ATTENTION, dépend de la forme choisie au programme 3 (où on fait les retraitements de DB)

DB <- X6_construction_base(forme_dt_ls,para_succ_nb_periode = 3) # output du programme 2, comme ça pas besoin de le relancer

DB <- DB %>% 
  select(-c(siren,dt,ea_ul)) %>% # je retire "ea_ul" qui n'a plus d'utilité (var quali : region, nj_ret, ape_ret, ent_age_ret)
  rename(Y = top_defaillance) %>% 
  mutate(Y = factor(Y))


names(DB)
need <- TRUE    # Pour assurer la reproductivité 
if (need){
  set.seed(1234)
  
  data_split <- initial_split(DB, strata = Y, prop = 0.8)
  
  training <- training(data_split) # data frame qui permet de faire le premier découpage
  test_set <- testing(data_split) # extraire le test set
  
  training_split <- initial_split(training, strata = Y, prop = 0.8)
  
  train_set <- training(training_split)
  eval_set <- testing(training_split)
}

rec <- recipe(data = training, Y~.) %>%
  step_dummy(ape_ret) %>%
  step_dummy(nj_ret) %>%
  step_dummy(region) %>%
  step_dummy(ent_age_ret)


# 2 | CALCUL DES PREDICTIONS SUR TRAINING (DF_ENTRAINEMENT) ET SUR TEST (DF_TEST) + GESTION DU DE NB_MODEL

##### XGBOOST
results_list_xgb <- list()
DF_entrainement <- data.frame(Y=training$Y)
DF_test <- data.frame(Y=test_set$Y)

xgb_mod <- boost_tree(learn_rate=0.25, mtry=92, min_n=29) %>% #min_n=profondeur d'arbre, mtry=nb de feuilles
  set_engine('xgboost')%>%
  set_mode('classification')
log_mod <- logistic_reg()%>%
  set_engine('glm')%>%
  set_mode('classification')


for (pho in 1:nb_model){
  
  print(paste0("tour de boucle :",pho))

  if (chargement_modeles == TRUE){
    
    print(paste0("Entrainement XGBOOST n°",pho, sep= ""))
  
    set.seed(pho)

    xgb_fit <- workflow() %>% 
      add_recipe(rec) %>%
      add_model(xgb_mod) %>%
      fit(training)
  
    saveRDS(xgb_fit, file = paste0(path_pg_models_save,"/",forme_dt,"_vf_xgb_n",pho,".RDS"))
  } else {
    xgb_fit <- readRDS(paste0(path_pg_models_save,"/",forme_dt,"_vf_xgb_n",pho,".RDS"))
  }

  # ----------------------------------------------------------------------------
  
  DF_entrainement[,paste("xgb_mod_",pho, sep = "")] <- predict(xgb_fit, training, type = "prob")[2]
  DF_test[,paste("xgb_mod_",pho, sep = "")] <- predict(xgb_fit, test_set, type = "prob")[2]


  if (pho < 2){
    if (chargement_modeles == TRUE){
    
    print(paste0("Entrainement Logit n°",pho, sep= ""))
    
    set.seed(pho)
    
    logit_fit <- workflow() %>% 
      add_recipe(rec) %>%
      add_model(log_mod) %>%
      fit(training)
    
    saveRDS(logit_fit, file = paste0(path_pg_models_save,"/",forme_dt,"_vf_logit_n",pho,".RDS"))
    } else {
      logit_fit <- readRDS(paste0(path_pg_models_save,"/",forme_dt,"_vf_logit_n",pho,".RDS"))
    }
    DF_entrainement[,paste("logit_mod_",pho, sep = "")] <- predict(logit_fit, training, type = "prob")[2]
    DF_test[,paste("logit_mod_",pho, sep = "")] <- predict(logit_fit, test_set, type = "prob")[2]
  }


    # ----------------------------------------------------------------------------

    if (chargement_modeles == TRUE){
    
    print(paste0("Entrainement randomForest n°",pho, sep= ""))
    
    set.seed(pho)
    
    rf_mod_fit <- ranger(Y ~ ., data=training, probability = TRUE)
    
    saveRDS(rf_mod_fit, file = paste0(path_pg_models_save,"/",forme_dt,"_vf_random_forest_n",pho,".RDS"))
  } else {
    rf_mod_fit <- readRDS(paste0(path_pg_models_save,"/",forme_dt,"_vf_random_forest_n",pho,".RDS"))
  }

  # ----------------------------------------------------------------------------
  
  DF_entrainement[,paste("rf_mod_",pho, sep = "")] <- predict(rf_mod_fit, training)$predictions[,2]
  DF_test[,paste("rf_mod",pho, sep = "")] <- predict(rf_mod_fit, test_set)$predictions[,2]
 
}



# Transformer les valeurs de Y en 0 et 1 (à faire en dehors d'une boucle)
#DF_entrainement$Y <- as.numeric(DF_entrainement$Y)
#DF_entrainement$Y <- ifelse(DF_entrainement$Y == 2, 1, 0)
#DF_test$Y <- as.numeric(DF_test$Y)
#DF_test$Y <- ifelse(DF_test$Y == 2, 1, 0)

###### Sauvegarde des DF_entrainement et DF_test pour l'application SHINY
saveRDS(DF_entrainement,
        file = paste0(path_data_vf,"/",dt_placement,'_',forme_dt,'_',"basesPREVISION_train",".RDS"))
saveRDS(DF_test,
        file = paste0(path_data_vf,"/",dt_placement,'_',forme_dt,'_',"basesPREVISION_test",".RDS") )

#DF_entrainement <- readRDS(file = paste0(path_data_vf,"/",dt_placement,'_',forme_dt,'_',"basesPREVISION_train",".RDS"))
#DF_test <- readRDS(file = paste0(path_data_vf,"/",dt_placement,'_',forme_dt,'_',"basesPREVISION_test",".RDS") )
#rm(DF_test,DF_entrainement)


# 3 | DIVERSES ANALYSES SUR DF_TEST POUR SHINY
#### Table de paramétrage issue de l'hypertuning du programme 3 à éventuellement automatiser
param_xgboost <- data.frame(
  learn_rate=c(0.15,0.20,0.25,0.3,0.4),
  mtry=c(88,88,92,92,88),
  min_n=c(23,23,29,29,23)
)



##### XGBOOST
results_list_xgb <- list()
DF_entrainement_S <- data.frame(Y = training$Y)
DF_test_S <- data.frame(Y = test_set$Y)
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
  DF_entrainement_S[, paste("xgb_mod_", pho, sep = "")] <- predict(xgb_fit, training, type = "prob")[, 2]
  
  # Obtenir les prédictions sur l'ensemble de test
  DF_test_S[, paste("xgb_mod_", pho, sep = "")] <- predict(xgb_fit, test_set, type = "prob")[, 2]
}

# Courbes ROC
#library(pROC)
#roc_data <- roc(Y~.,data=DF_test)
#plot(roc_data$xgb_mod_1)


# Boucle sur les différentes combinaisons de paramètres dans param_xgboost
for (i in 1:nrow(param_xgboost)) {
  # Calcul des densités normalisées pour Y=0
  DF_test_S[DF_test_S$Y==0, paste0("xgb_mod_", i, "_mean_Y0")] <- mean(DF_test_S[DF_test_S$Y==0, paste0("xgb_mod_", i)])
  DF_test_S[DF_test_S$Y==0, paste0("xgb_mod_", i, "_sd_Y0")] <- sd(DF_test_S[DF_test_S$Y==0, paste0("xgb_mod_", i)])
  DF_test_S[DF_test_S$Y==0, paste0("xgb_mod_", i, "_norm")] <- (DF_test_S[DF_test_S$Y==0, paste0("xgb_mod_", i)] - DF_test_S[DF_test_S$Y==0, paste0("xgb_mod_", i, "_mean_Y0")]) / DF_test_S[DF_test_S$Y==0, paste0("xgb_mod_", i, "_sd_Y0")]
  
  # Calcul des densités normalisées pour Y=1
  DF_test_S[DF_test_S$Y==1, paste0("xgb_mod_", i, "_mean_Y1")] <- mean(DF_test_S[DF_test_S$Y==1, paste0("xgb_mod_", i)])
  DF_test_S[DF_test_S$Y==1, paste0("xgb_mod_", i, "_sd_Y1")] <- sd(DF_test_S[DF_test_S$Y==1, paste0("xgb_mod_", i)])
  DF_test_S[DF_test_S$Y==1, paste0("xgb_mod_", i, "_norm")] <- (DF_test_S[DF_test_S$Y==1, paste0("xgb_mod_", i)] - DF_test_S[DF_test_S$Y==1, paste0("xgb_mod_", i, "_mean_Y1")]) / DF_test_S[DF_test_S$Y==1, paste0("xgb_mod_", i, "_sd_Y1")]

  # Affichage des courbes de densité normalisées
  print(ggplot(DF_test_S, aes_string(x = paste0("xgb_mod_", i, "_norm"), fill = "factor(Y)")) + 
          geom_density(alpha = 0.5) +
          scale_fill_manual(values = c("blue", "red")) +
          labs(title = paste0("Densité des prédictions normalisées selon Y pour learning rate de ", param_xgboost[i,"learn_rate"]),
               x = "Prédictions (YP)",
               y = "Densité") +
          theme_minimal())
  
  # Affichage des courbes de densité NON normalisées
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
    learning_rate=c("0.15","0.20","0.25","0.3","0.4"),
    variable_name=c("xgb_mod_1_norm","xgb_mod_2_norm","xgb_mod_3_norm","xgb_mod_4_norm","xgb_mod_5_norm"),
    variable_name_2=c("xgb_mod_1","xgb_mod_2","xgb_mod_3","xgb_mod_4","xgb_mod_5")
  )
  saveRDS(lr_rate_mapping,
          file = paste0(path_data_vf,"/","lr_rate_mapping.RDS"))


  # Transformer les valeurs de Y en 0 et 1 (à faire en dehors d'une boucle)
  DF_test_S$Y <- as.numeric(DF_test_S$Y)
  DF_test_S$Y <- ifelse(DF_test_S$Y == 2, 1, 0)
  
  
  # Initialisation de la table pour stocker les métriques pour Shiny (à faire en dehors d'une boucle)
  metriques_pour_Shiny <- data.frame(learn_rate = numeric(),
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
  
  
  ###### Sauvegarde des DF_entrainement, DF_test et metriques_pour_Shiny pour l'application SHINY
  saveRDS(DF_entrainement_S,
          file = paste0(path_data_vf,"/",dt_placement,'_',forme_dt,'_',"basesPREVISION_train_S",".RDS"))
  saveRDS(DF_test_S,
          file = paste0(path_data_vf,"/",dt_placement,'_',forme_dt,'_',"basesPREVISION_test_S",".RDS") )
  saveRDS(metriques_pour_Shiny,
          file = paste0(path_data_vf,"/","metriques_pour_Shiny.RDS") )
  
  #DF_entrainement_S <- readRDS(file = paste0(path_data_vf,"/",dt_placement,'_',forme_dt,'_',"basesPREVISION_train_S",".RDS"))
  #DF_test_S <- readRDS(file = paste0(path_data_vf,"/",dt_placement,'_',forme_dt,'_',"basesPREVISION_test_S",".RDS") )
  #metriques_pour_Shiny <- readRDS(file = paste0(path_data_vf,"/","metriques_pour_Shiny.RDS") )