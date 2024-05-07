
###################################################
# NECESSITE DE LANCER LE PROGRAMME 3 AU PREALABLE #
###################################################

matricule <- "N818398"

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF", sep = "")
} 
if (matricule == "X822385"){
  path_USER <- "C:/Users/X822385/Desktop/DS/Projet"
}

# Chargement des utilisateurs 

chargement_modeles <- TRUE
nb_model <- 10

forme_dt_ls <- c("simple","add_succ_surplus","add_succ_chocs","poly") 
forme_dt  <- paste(forme_dt_ls, collapse = "_") # pour les dénomination des modèles


# SIMPLE : Pas de feature engineering (sauf retraitement)
# ADD : Ajout de variables            (ajout de variables)
# POLY :  polygon sur les variables météo
# Interaction : encore à définir

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

DB <- X6_construction_base(forme_dt_ls,para_succ_nb_periode = 3)
forme_dt  <- paste(forme_dt_ls, collapse = "_")

DB <- DB %>% 
  select(-c(siren,dt,ea_ul)) %>% # je retire "ea_ul" qui n'a plus d'utilité (var quali : region, nj_ret, ape_ret, ent_age_ret)
  rename(Y = top_defaillance) %>% 
  mutate(Y = factor(Y))


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
  step_dummy(region) %>%
  step_dummy(nj_ret) %>%
  step_dummy(ape_ret) %>%
  step_dummy(ent_age_ret)

# 2 | CALCUL DES PREDICTIONS SUR TRAINING (DF_ENTRAINEMENT) ET SUR TEST (DF_TEST) + GESTION DU DE NB_MODEL


##### XGBOOST
results_list_xgb <- list()
DF_entrainement <- data.frame(Y=training$Y)
DF_test <- data.frame(Y=test_set$Y)

xgb_mod <- boost_tree(mtry=81, min_n=23) %>% #min_n=profondeur d'arbre, mtry=nb de feuilles
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
  }
  
  # ----------------------------------------------------------------------------
  
  DF_entrainement[,paste("rf_mod_",pho, sep = "_")] <- predict(rf_mod_fit, training)[2]
  DF_test[,paste("rf_mod",pho, sep = "_")] <- predict(rf_mod_fit, test_set)$predictions[,2]
  
  
}




###### Sauvegarde des DF_entrainement et DF_test pour l'application SHINY
saveRDS(DF_entrainement,
        file = paste0(path_data_vf,"/",dt_placement,'_',forme_dt,'_',"basesPREVISION_train",".RDS"))
saveRDS(DF_test,
        file = paste0(path_data_vf,"/",dt_placement,'_',forme_dt,'_',"basesPREVISION_test",".RDS") )






# 3 | DIVERS ANALYSES SUR DF_TEST
# DF_test <- readRDS( file = paste0(path_data_vf,"/",dt_placement,'_',forme_dt,"basesPREVISION_test",".RDS"))

# Courbes ROC
library(pROC)
roc_data <- roc(Y~.,data=DF_test)
plot(roc_data$xgb_mod_1)

roc_data_2 <- roc(DF_test$Y,DF_test$xgb_1)
optimal_threshold_sensitivity <- coords(roc_data_2, "best", ret="threshold")
print(optimal_threshold_sensitivity) # 0.006736428


# Courbes en densité normalisé des prédictions pour Y=0 et Y=1
DF_test$xgb_1_mean <- mean(DF_test$xgb_1)
DF_test$xgb_1_sd <- sd(DF_test$xgb_1)
DF_test$xgb_1_norm <- (DF_test$xgb_1 - DF_test$xgb_1_mean)/ DF_test$xgb_1_sd


ggplot(DF_test, aes(x = xgb_1_norm, fill = factor(Y))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Densité des prédictions selon la valeur de Y",
       x = "Prédictions (YP)",
       y = "Densité") +
  theme_minimal()


# Comparaison des métriques des différents modèles
DF_test$Y <- as.numeric(DF_test$Y)

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

apply(X = DF_test,2,accuracy,Y=DF_test$Y) # seuil=0.5

apply(X = DF_test,2,accuracy,Y=DF_test$Y,seuil = 0.01)

apply(X = DF_test,2,recall,Y=DF_test$Y,seuil = 0.0055)


