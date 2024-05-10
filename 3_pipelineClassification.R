# pipeline - Classification 

matricule <- "X822385"

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF", sep = "")
} 
if (matricule == "X822385"){
  path_USER <- "C:/Users/X822385/Desktop/DS/Projet"
}

  # SIMPLE : Pas de feature engineering (sauf retraitement)
  # ADD : Ajout de variables
  # POLY :  polygon sur les variables météo
  # Interaction : on en fait déjà implicitement avec les add, la rec_inter fonctionne mal avec ridge/lasso (à creuser mais rien de significatif à attendre)
  
# 0 | chargement des libraries  -----

source(paste0(path_USER,"/pg_propre/","_before_chemins.R"))
source(paste0(path_USER,"/pg_propre/","_before_libraries.R"))

source(paste0(path_USER,"/pg_propre/","X6_assemblage_table.R"))

# 1 | chargement de la table + chargement des paramètres

dt_placement <- readRDS(file = paste0(path_data_vf,"/","para_dt_placement.RDS"))
interval_month <- readRDS( file = paste0(path_data_vf,"/","para_interval_month.RDS"))
annee_nb <- readRDS( file = paste0(path_data_vf,"/","para_annee_nb.RDS"))

#Lorsqu'on veut concaténer les DB sur plusieurs années
#DB_2021 <- DB
#DB <- rbind(DB_2021,DB_2022,DB_2023)

chargement_modeles <- TRUE

# forme_dt_ls <- c("simple","add_surplus","add_chocs","add_succ_surplus","add_succ_chocs","poly")
forme_dt_ls <- c("simple","add_succ_surplus","add_succ_chocs","poly") 
DB <- X6_construction_base(forme_dt_ls,para_succ_nb_periode = 3) # output du programme 2, comme ça pas besoin de le relancer
forme_dt  <- paste(forme_dt_ls, collapse = "_")

# 1 | chargement des modèles pour la cross validation

if (chargement_modeles == FALSE){
  
  log_fitted_cv <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_mod_log_tuned.RDS"))
  ridge_fitted_cv <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_mod_ridge_tuned.RDS"))
  lasso_fitted_cv <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_mod_lasso_tuned.RDS"))
  #elasNet_fitted_cv <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_mod_elasticNet_tuned.RDS"))
  rf_fitted_cv <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_mod_rf1_tuned.RDS"))
  xgb_fitted_cv <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_mod_xgb_tuned.RDS"))
}


DB <- DB %>% 
  select(-c(siren,dt,ea_ul,b500_moy)) %>% # je retire "ea_ul" qui n'a plus d'utilité et b500_moy avec des NA (var quali : region, nj_ret, ape_ret, ent_age_ret)
  rename(Y = top_defaillance)# %>%
# summary(DB)
# names(DB)
DB$Y <- factor(DB$Y) # TRES IMPORTANT !
# table(DB$region)
# table (DB$nj_ret)
# table (DB$ape_ret)
# table (DB$ent_age_ret)
# saveRDS(DB, file = paste0(path_data_vf,"/",dt_placement,"_DB_postRET.RDS"))

# 2 | Mise en place du training + test
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

# Recipe sur training (et pas sur train_set car on fait de la validation croisée)
training$Y <- factor(training$Y)
test_set$Y <- factor(test_set$Y)
train_set$Y <- factor(train_set$Y)

rec <- recipe(data = training, Y~.) %>%
  step_dummy(region) %>%
  step_dummy(nj_ret) %>%
  step_dummy(ape_ret) %>%
  step_dummy(ent_age_ret)
class(rec)
prep(rec)
head(juice(prep(rec)))
formula(juice(prep(rec)))

#rec_spline <- rec_inter%>%step_ns(deg_free =tune())

# Ajout de la VC avec X blocs
folds <- vfold_cv(training, v=10, strata=Y)


# 3 | Mise en place (dont hypertuning à l'aide de la random grid search => pour grid je met à 5 mais la prod met à 20)

### REGRESSION LOGISTIQUE (PAS D'HYPERTUNING POUR CE MODELE)
if (chargement_modeles == TRUE){
log_mod <- logistic_reg()%>%
  set_engine('glm')%>%
  set_mode('classification')
log_wf <- workflow()%>%
  add_recipe(rec_inter)%>%
  add_model(log_mod)
log_fitted_cv <- log_wf %>%
  fit_resamples(
    resamples = folds,
    metrics = metric_set(roc_auc)
  )
saveRDS(log_fitted_cv, file = paste0(path_pg_models_save,"/",forme_dt,"_mod_log_tuned_v2.RDS"))
# log_fitted_cv<- readRDS(file = paste0(path_pg_models_save,"/",forme_dt,"_mod_log_tuned_v2.RDS"))
log_fitted_cv %>% collect_metrics()
best_model_log <- show_best(log_fitted_cv, metric="roc_auc") %>% mutate(Model="Logistique") # roc_auc=0,637
}


### XGBOOST
if (chargement_modeles == TRUE){
xgb_mod <- boost_tree(learn_rate=0.5, mtry=215, min_n=30)%>% #learn_rate=tune(), min_n=profondeur d'arbre, mtry=nb de feuilles
  set_engine('xgboost')%>%
  set_mode('classification')
xgb_wf <- workflow()%>%
  add_recipe(rec)%>%
  add_model(xgb_mod)
xgb_fitted_cv <- xgb_wf %>%
  tune_grid(
    resamples = folds,
    grid=5,
    metrics = metric_set(roc_auc),
    control=control_grid(verbose=TRUE, save_pred=TRUE)
  )
saveRDS(xgb_fitted_cv, file = paste0(path_pg_models_save,"/",forme_dt,"_mod_xgb_tuned_v2.RDS"))
# xgb_fitted_cv<- readRDS(file = paste0(path_pg_models_save,"/",forme_dt,"_mod_xgb_tuned_v2.RDS"))
xgb_fitted_cv %>% collect_metrics()
xgb_fitted_cv %>% collect_predictions()
best_models_xgb <- xgb_fitted_cv %>% collect_metrics() %>% filter (.metric =='roc_auc') %>% arrange (desc(mean)) # mtry=114 ; min_n=19 ; roc_auc = 0,781
best_model_xgb <- best_models_xgb[1,] %>% mutate(Model="XG_Boost")
}

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

apply(X = DF_test,2,recall,Y=DF_test$Y,seuil = 0.001)
rm(DF_test)

### LASSO
if (chargement_modeles == TRUE){
lasso_mod <- logistic_reg(mode="classification", engine="glmnet", penalty=1.30e-5, mixture=1) 
lasso_wf <- workflow()%>%
  add_recipe(rec_inter)%>%
  add_model(lasso_mod)
lasso_fitted_cv <- lasso_wf %>%
  fit_resamples(
    resamples = folds,
    metrics = metric_set(roc_auc),
    control=control_grid(verbose=TRUE)
  ) #tune_grid et grid=5
saveRDS(lasso_fitted_cv, file = paste0(path_pg_models_save,"/",forme_dt,"_mod_lasso_tuned_v2.RDS"))
# lasso_fitted_cv<- readRDS(file = paste0(path_pg_models_save,"/",forme_dt,"_mod_lasso_tuned.RDS"))
lasso_fitted_cv %>% collect_metrics()
best_models_lasso <- show_best(lasso_fitted_cv, metric="roc_auc") # penalty=4.81e- 5  ; roc_auc = 0.686
best_model_lasso <- best_models_lasso[1,] %>% mutate(Model="Lasso")
} 

### RIDGE
if (chargement_modeles == TRUE){
ridge_mod <- logistic_reg(mode="classification", engine="glmnet", penalty=tune(), mixture=0) 
ridge_wf <- workflow()%>%
  add_recipe(rec_inter)%>%
  add_model(ridge_mod)
ridge_fitted_cv <- ridge_wf %>%
  tune_grid(
    resamples = folds,
    grid=5,
    metrics = metric_set(roc_auc) # f_meas,recall
  )
saveRDS(ridge_fitted_cv, file = paste0(path_pg_models_save,"/",forme_dt,"_mod_ridge_tuned_v2.RDS"))
ridge_fitted_cv %>% collect_metrics()
best_models_ridge <- ridge_fitted_cv %>% collect_metrics() %>% filter (.metric =='roc_auc') %>% arrange (desc(mean)) # penalty=1.24e-10 ; roc_auc = 0,672
best_model_ridge <- best_models_ridge[1,] %>% mutate(Model="Ridge")
}


### RANDOM FOREST (ATTENTION !!! SOIT A LA MANO SOIT ON NE FAIT PAS D'HYPERTUNING (500 arbres max notamment))
if (chargement_modeles == TRUE){
  rf_mod <- rand_forest(trees=500,min_n=tune())%>% # trees=nd d'arbres, min_n=profondeur d'arbre
    set_engine('ranger')%>%
    set_mode('classification')
  rf_wf <- workflow()%>%
    add_recipe(rec_inter)%>%
    add_model(rf_mod)
  rf_fitted_cv <- rf_wf %>%
    tune_grid(
      resamples = folds, 
      metrics = metric_set(accuracy, roc_auc),
      grid=5
    ) 
  saveRDS(rf_fitted_cv, file = paste0(path_pg_models_save,"/",forme_dt,"_mod_rf1_tuned_v2.RDS"))
  rf_fitted_cv %>% collect_metrics()
  best_models_rf <- rf_fitted_cv %>% collect_metrics() %>% filter (.metric =='roc_auc') %>% arrange (desc(mean)) # trees=1760 ; min_n =14 ; roc_auc =0,738 
  best_model_rf <- best_models_rf[1,] %>% mutate(Model="Random_Forest")
}


### SPLINE (DONNE DE TRES MAUVAIS RESULTATS, VOIR SI SVM NE MARCHE PAS MIEUX)
if (chargement_modeles == TRUE){
spline_mod <- decision_tree(
  mode = "classification",
  engine = "rpart")
spline_wf <- workflow()%>%
  add_recipe(rec_spline)%>%
  add_model(spline_mod)
spline_fitted_cv <- spline_wf %>%
  tune_grid(
    resamples = folds,
    grid=5,
    metrics = metric_set(roc_auc)
  )
saveRDS(spline_fitted_cv, file = paste0(path_pg_models_save,"/",forme_dt,"_mod_spline_tuned.RDS"))
spline_fitted_cv %>% collect_metrics()
best_models_spline <- spline_fitted_cv %>% collect_metrics() %>% filter (.metric =='roc_auc') %>% arrange (desc(mean)) # roc_auc = 0,5 à tous les coups...
best_model_spline <- best_models_spline[1,] %>% mutate(Model="Spline")
}




### ELASTICNET (NE MARCHE PAS)
if (chargement_modeles == TRUE){
elasticnet_mod <- logistic_reg(mode="classification", engine="glmnet", penalty=tune(), mixture=tune()) 
elasticnet_wf <- workflow()%>%
  add_recipe(rec)%>%
  add_model(elasticnet_mod)
grid <- expand.grid(
  penalty=c(0,0.5,1),
  mixture=seq(0,1, by=0.1)
)
elasticnet_fitted_cv <- elasticnet_wf %>%
  tune_grid(
    resamples = folds,
    grid=grid,
    metrics = metric_set(roc_auc)
  )
saveRDS(elasticnet_fitted_cv, file = paste0(path_pg_models_save,"/",forme_dt,"_mod_elasticnet_tuned.RDS"))
elasticnet_fitted_cv %>% collect_metrics()
elasticnet_fitted_cv %>% collect_metrics() %>% filter (.metric =='roc_auc') %>% arrange (desc(mean)) # penalty= ; roc_auc = 
}



# 4 | Comparaison des modèles
model_results <- bind_rows(best_model_log, best_model_lasso, best_model_ridge, best_model_rf, best_model_xgb) %>% arrange (desc(mean))
model_results_pour_Shiny <- model_results[,c(".metric", "mean", "n", ".config", "Model", "penalty", "mtry", "min_n")] 
model_results_pour_Shiny <- round(model_results_pour_Shiny, digits=2)


# 5 | MODELES FINAUX estimé sur l'ensemble des données (training=test_set+eval_set) POUR TROUVER LES HYPERPARAMETRES XGBOOST ET RANDOMFOREST

#### creating and fitting a workflowset, evaluating of all wf in the wf_set
xgb_mod <- boost_tree(learn_rate=0.25,mtry=tune(), min_n=tune())%>% #min_n=profondeur d'arbre, mtry=nb de feuilles
  set_engine('xgboost')%>%
  set_mode('classification')

rf_mod <- rand_forest(trees=500,min_n=tune())%>% # trees=nd d'arbres, min_n=profondeur d'arbre
  set_engine('ranger')%>%
  set_mode('classification')

wf_set <- workflow_set(
  preproc = list(basic = rec),
  models = list(xgb = xgb_mod) #rf=rf_mod
)

set.seed(1234)
res_wf_set <-
  wf_set%>%
  workflow_map(
    resamples=folds,
    metrics=metric_set(roc_auc),
    grid=5,
    fn="tune_grid",
    verbose=T,
    control=control_grid(verbose=TRUE, save_pred=TRUE)
      )

saveRDS(res_wf_set, file = paste0(path_pg_models_save,"/",forme_dt,"_wf_set.RDS"))
# res_wf_set <- readRDS(file = paste0(path_pg_models_save,"/",forme_dt,"_wf_set.RDS"))
rank_results(res_wf_set, rank_metric='roc_auc')


#### estimation du modèle XGBOOST sur l'ensemble des données (training=test_set+eval_set) pour déterminer les paramètres
best_model_xgb_training <- 
  res_wf_set %>%
  extract_workflow_set_result('basic_xgb')%>%
  select_best(metric='roc_auc') # meilleurs hyperparamètres : grid_xgb <- data.frame(mtry = 81, min_n = 23)
  
predictions_entrainement_tuning <- res_wf_set %>% collect_predictions()
predictions_entrainement_tuning_2 <- predictions_entrainement_tuning[predictions_entrainement_tuning$.config=="Preprocessor1_Model3",]
predictions_entrainement_tuning_2 <- predictions_entrainement_tuning_2[,c("Y",".pred_1")]
predictions_entrainement_tuning_2$Y <- as.numeric(predictions_entrainement_tuning_2$Y)

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
predictions_entrainement_tuning_2$Y <- ifelse(predictions_entrainement_tuning_2$Y==2,1,0)
apply(X = predictions_entrainement_tuning_2,2,accuracy,Y=predictions_entrainement_tuning_2$Y) # seuil=0.5

apply(X = predictions_entrainement_tuning_2,2,accuracy,Y=predictions_entrainement_tuning_2$Y,seuil = 0.024)

apply(X = predictions_entrainement_tuning_2,2,recall,Y=predictions_entrainement_tuning_2$Y,seuil = 0.0055)

#### estimation du modèle RANDOM FOREST sur l'ensemble des données pour déterminer les paramètres
best_model_rf_training <- 
  res_wf_set %>%
  extract_workflow_set_result('basic_rf')%>%
  select_best(metric='roc_auc') # meilleurs hyperparamètres : grid_rf <- data.frame(trees = 808, min_n = 29) // je dois refaire tourner avec trees=500

