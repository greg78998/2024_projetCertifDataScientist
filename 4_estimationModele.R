

matricule <- "X822385"

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF", sep = "")
} 
if (matricule == "X822385"){
  path_USER <- "C:/Users/X822385/Desktop/DS/Projet"
}

# Chargement des utilisateurs 

chargement_modeles <- TRUE
nb_model <- 2

forme_dt_ls <- c("simple","add","poly")  
#forme_dt_ls <- c("simple")  


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

forme_dt  <- paste(forme_dt_ls, collapse = "_") # pour les dénomination des modèles

dt_placement <- readRDS(file = paste0(path_data_vf,"/","para_dt_placement.RDS"))
interval_month <- readRDS( file = paste0(path_data_vf,"/","para_interval_month.RDS"))
annee_nb <- readRDS( file = paste0(path_data_vf,"/","para_annee_nb.RDS"))

DB <- readRDS(paste0(path_data_vf,"/",dt_placement,"_DB_postRET.RDS"))          # = forme simple

source(paste0(path_USER,"/pg_propre/","Y1_assemblageTables.R"))


# 2 | Remise en page du dataframe 

DB <- DB %>% 
  select(-c(siren,dt)) %>% 
  rename(Y = top_defaillance) %>%
  select(-c(ea_ul)) # ligne qui va disparaître à terme

need <- TRUE # Pour assurer la reproductivite
if (need){
  set.seed(1234)

  data_split <- initial_split(DB, strata = Y, prop = 0.8)
  
  training <- training(data_split) # data frame qui permet de faire le premier découpage
  test_set <- testing(data_split) # extraire le test set
}

# 3 | MODELE FINAL estimé sur l'ensemble des données (training=test_set+eval_set) POUR TROUVER LES HYPERPARAMETRES XGBOOST ET RANDOMFOREST

#### creating and fitting a workflowset, evaluating of all wf in the wf_set
xgb_mod <- boost_tree(mtry=tune(), min_n=tune())%>% #min_n=profondeur d'arbre, mtry=nb de feuilles
  set_engine('xgboost')%>%
  set_mode('classification')

rf_mod <- rand_forest(trees=500,min_n=tune())%>% # trees=nd d'arbres, min_n=profondeur d'arbre
  set_engine('ranger')%>%
  set_mode('classification')

wf_set <- workflow_set(
    preproc = list(basic = rec),
    models = list(rf= rf_mod, xgb = xgb_mod)
    )

set.seed(1234)
res_wf_set <-
  wf_set%>%
  workflow_map(
    resamples=folds,
    metrics=metric_set(accuracy, roc_auc, recall),
    grid=5,
    fn="tune_grid",
    verbose=T
  )

saveRDS(res_wf_set, file = paste0(path_pg_models_save,"/",forme_dt,"_wf_set.RDS"))
# res_wf_set <- readRDS(file = paste0(path_pg_models_save,"/",forme_dt,"_wf_set.RDS"))
rank_results(res_wf_set, rank_metric='roc_auc')

#### estimation du modèle XGBOOST sur l'ensemble des données (training=test_set+eval_set) pour déterminer les paramètres
best_model_xgb_training <- 
  res_wf_set %>%
  extract_workflow_set_result('basic_xgb')%>%
  select_best(metric='roc_auc') # meilleurs hyperparamètres : grid_xgb <- data.frame(mtry = 81, min_n = 23)

#### estimation du modèle RANDOM FOREST sur l'ensemble des données pour déterminer les paramètres
best_model_rf_training <- 
  res_wf_set %>%
  extract_workflow_set_result('basic_rf')%>%
  select_best(metric='roc_auc') # meilleurs hyperparamètres : grid_rf <- data.frame(trees = 808, min_n = 29) // je dois refaire tourner avec trees=500



# 4 | ESTIMATION DES MODELES EN FONCTION DE NB_MODEL
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



###### RANDOM_FOREST

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

# Sauvegarde des DF_entrainement et DF_test pour l'application SHINY
saveRDS(DF_entrainement,
        file = paste0(path_data_vf,"/",dt_placement,'_',forme_dt,"basesPREVISION_train",".RDS"))
saveRDS(DF_test,
        file = paste0(path_data_vf,"/",dt_placement,'_',forme_dt,"basesPREVISION_test",".RDS") )


# Comparaison des métriques pour information
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

apply(X = DF_test,2,accuracy,Y=DF_test$Y,seuil = 0.001)

apply(X = DF_test,2,recall,Y=DF_test$Y,seuil = 0.0055)

