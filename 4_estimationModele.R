matricule <- "N818398"

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF", sep = "")
} 
if (matricule == "X822385"){
  path_USER <- "C:/Users/X822385/Desktop/DS/Projet"
}

# Chargement des utilisateurs 

chargement_modeles <- FALSE 
nb_model <- 10

forme_dt_ls <- c("simple","add_succ_surplus","add_succ_chocs","poly") 
forme_dt  <- paste(forme_dt_ls, collapse = "_") # pour les dénomination des modèles

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

rm(list = c("DB","data_split","training_split"))

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
  rm(xgb_fit)
  
  if (pho < 2){
    
    if (chargement_modeles == TRUE){
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
  rm(rf_mod_fit)
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

#rm(DF_test,DF_entrainement)


