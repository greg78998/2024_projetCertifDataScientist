
###################################################
# NECESSITE DE LANCER LE PROGRAMME 3 AU PREALABLE #
###################################################

matricule <- "X822385"

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

dt_placement <- readRDS(file = paste0(path_data_vf,"/","para_dt_placement.RDS"))
interval_month <- readRDS( file = paste0(path_data_vf,"/","para_interval_month.RDS"))
annee_nb <- readRDS( file = paste0(path_data_vf,"/","para_annee_nb.RDS"))

dt_placement <- readRDS(file = paste0(path_data_vf,"/","para_dt_placement.RDS"))
interval_month <- readRDS( file = paste0(path_data_vf,"/","para_interval_month.RDS"))
annee_nb <- readRDS( file = paste0(path_data_vf,"/","para_annee_nb.RDS"))

# ATTENTION, dépend de la forme choisie au programme 3 (où on fait les retraitements de DB)
DB <- readRDS(paste0(path_data_vf,"/",dt_placement,"_DB_postRET.RDS")) 



# 2 | CALCUL DES PREDICTIONS SUR TRAINING (DF_ENTRAINEMENT) ET SUR TEST (DF_TEST) + GESTION DU DE NB_MODEL
training$Y <- factor(training$Y)
test_set$Y <- factor(test_set$Y)
train_set$Y <- factor(train_set$Y)

##### XGBOOST
results_list_xgb <- list()
DF_entrainement <- data.frame(Y=training$Y)
DF_test <- data.frame(Y=test_set$Y)
for (pho in 1:nb_model){
  print(paste0("numero :",pho))
  if (chargement_modeles == TRUE){
    print(paste0("Entrainement XGBOOST n°",pho, sep= ""))
    set.seed(pho)
    grid_xgb <- data.frame(mtry = 81, min_n = 23)
    
    wf_set_xgb <- workflow_set(
      preproc = list(basic = rec),
      models = list(xgb = xgb_mod))
    
    res_wf_set_xgb <- wf_set_xgb %>%
      workflow_map(
        resamples = folds,
        metrics = metric_set(accuracy, roc_auc, recall),
        grid = grid_xgb,
        control=control_grid(save_pred=TRUE),
        verbose = TRUE)
 
    saveRDS(res_wf_set_xgb, file = paste0(path_pg_models_save,"/",forme_dt,"_vf_xgb_n",pho,".RDS"))
    print("Ce modèle XG Boost est sauvegardé")
    
    predictions_entrainement <- res_wf_set_xgb %>% collect_predictions()
    results_list_xgb[[pho]] <- list(predictions=predictions_entrainement)
    DF_entrainement[,paste0("xgb_",pho)] <- results_list_xgb[[pho]]$predictions[,c(".pred_1")]
    
    best_model_xgb <- 
      res_wf_set_xgb %>%
      extract_workflow_set_result('basic_xgb')%>%
      select_best(metric='roc_auc')

    last_fit_xgb <- res_wf_set_xgb %>%
      extract_workflow("basic_xgb")%>%
      finalize_workflow(best_model_xgb) %>%
      last_fit(split=data_split, metrics=metric_set(accuracy, roc_auc))
    
    predictions_test <- last_fit_xgb %>% collect_predictions()
    results_list_xgb[[pho]] <- list(predictions=predictions_test)
    DF_test[,paste0("xgb_",pho)] <- results_list_xgb[[pho]]$predictions[,c(".pred_1")]
    }
}



###### RANDOM_FOREST (JE NE L'AI PAS ENCORE LANCE DEPUIS QUE LE CODE A CHANGE... PREND DU TEMPS)

wf_set_rf <- workflow_set(
  preproc = list(basic = rec),
  models = list(rf = rf_mod) # 
)

res_wf_set_rf_vf <- wf_set_rf %>%
  workflow_map(
    resamples = folds,
    metrics = metric_set(accuracy, roc_auc, recall),
    grid = grid_rf,
    verbose = TRUE
  )

best_model_rf_vf <- 
  res_wf_set_rf_vf %>%
  extract_workflow_set_result('basic_rf')%>%
  select_best(metric='roc_auc')

last_fit_rf_vf <- res_wf_set_rf_vf %>%
  extract_workflow("basic_rf")%>%
  finalize_workflow(best_model_rf_vf) %>%
  last_fit(split=data_split)

last_fit_rf_vf%>%collect_metrics()
last_fit_rf_vf%>%collect_predictions()


results_list_rf <- list()
#DF_entrainement <- data.frame(Y=training$Y)
#DF_test <- data.frame(Y=test_set$Y)
for (pho in 1:nb_model){
  print(paste0("numero :",pho))
  if (chargement_modeles == TRUE){
    print(paste0("Entrainement RANDOM_FOREST n°",pho, sep= ""))
    set.seed(pho)
    grid_rf <- data.frame(trees = 500, min_n = 29) # on bloque à 500, pas encore fait retourner l'hypertuning où on bloque trees à 500 
    
    wf_set_rf <- workflow_set(
      preproc = list(basic = rec),
      models = list(xgb = rf_mod))
    
    res_wf_set_rf <- wf_set_rf %>%
      workflow_map(
        resamples = folds,
        metrics = metric_set(accuracy, roc_auc, recall),
        grid = grid_rf,
        control=control_grid(save_pred=TRUE),
        verbose = TRUE)
    
    saveRDS(res_wf_set_rf, file = paste0(path_pg_models_save,"/",forme_dt,"_vf_rf_n",pho,".RDS"))
    print("Ce modèle RANDOM_FOREST est sauvegardé")
    
    predictions_entrainement <- res_wf_set_rf %>% collect_predictions()
    results_list_rf[[pho]] <- list(predictions=predictions_entrainement)
    DF_entrainement[,paste0("rf_",pho)] <- results_list_rf[[pho]]$predictions[,c(".pred_1")]
    
    best_model_rf <- 
      res_wf_set_rf %>%
      extract_workflow_set_result('basic_rf')%>%
      select_best(metric='roc_auc')
    
    last_fit_rf <- res_wf_set_rf %>%
      extract_workflow("basic_rf")%>%
      finalize_workflow(best_model_rf) %>%
      last_fit(split=data_split, metrics=metric_set(accuracy, roc_auc))
    
    predictions_test <- last_fit_rf %>% collect_predictions()
    results_list_rf[[pho]] <- list(predictions=predictions_test)
    DF_test[,paste0("rf_",pho)] <- results_list_rf[[pho]]$predictions[,c(".pred_1")]
  }
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
plot(roc_data$xgb_1)

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


