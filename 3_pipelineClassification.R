# pipeline - Classification 

matricule <- "X822385"

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF", sep = "")
} 
if (matricule == "X822385"){
  path_USER <- "C:/Users/X822385/Desktop/DS/Projet"
}


chargement_modeles <- TRUE
forme_dt_ls <- c("simple","add","poly")  


  # SIMPLE : Pas de feature engineering (sauf retraitement)
  # ADD : Ajout de variables            (ajout de variables)
  # POLY :  polygon sur les variables météo
  # Interaction : encore à définir
  
# 0 | chargement des libraries  -----

source(paste0(path_USER,"/pg_propre/","_before_chemins.R"))
source(paste0(path_USER,"/pg_propre/","_before_libraries.R"))

# 1 | chargement de la table + chargement des paramètres

# choix de la table
forme_dt  <- paste(forme_dt_ls, collapse = "_")


dt_placement <- readRDS(file = paste0(path_data_vf,"/","para_dt_placement.RDS"))
interval_month <- readRDS( file = paste0(path_data_vf,"/","para_interval_month.RDS"))
annee_nb <- readRDS( file = paste0(path_data_vf,"/","para_annee_nb.RDS"))

DB <- readRDS(paste0(path_data_vf,"/",dt_placement,"_DB_postRET.RDS"))          # = forme simple

source(paste0(path_USER,"/pg_propre/","Y1_assemblageTables.R"))


# 1 | chargement des modèles pour la cross validation

if (chargement_modeles == FALSE){
  
  logit_fitted_cv <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_mod_logit_tuned.RDS"))
  
  ridge_fitted_cv <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_mod_ridge_tuned.RDS"))
  lasso_fitted_cv <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_mod_lasso_tuned.RDS"))
  elasNet_fitted_cv <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_mod_elasticNet_tuned.RDS"))
  rF_mod1_fitted_cv <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_mod_rf1_tuned.RDS"))
  xgb_fitted_cv <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_mod_xgb_tuned.RDS"))
}


DB <- DB %>% 
  select(-c(siren,dt)) %>% 
  rename(Y = top_defaillance) %>%
  select(-c(ea_ul,date_creation,region,nj_ret,ape_ret)) # pour l'instant, je retire toutes les quali mais à ajuster
# summary(DB)


# 2 | Mise en place du training + test

# pour véirfier que ça tourne 
#DB <- training(initial_split(DB, strata = Y, prop = 1))

DB <- na.omit(DB) # ajout pour debugger

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
rec <- recipe(data = training, Y~.)
class(rec)
prep(rec)
juice(prep(rec))


# Ajout de la VC avec X blocs
folds <- vfold_cv(training, v=5, strata=Y)


# 3 | Mise en place (dont hypertuning à l'aide de la random grid search => pour grid je met à 5 mais la prod met à 20)

### REGRESSION LOGISTIQUE (PAS D'HYPERTUNING POUR CE MODELE)
if (chargement_modeles == TRUE){
log_mod <- logistic_reg(mode="classification", engine="glm") # voir si ça fonctionne avec glmnet
  log_wf <- workflow()%>%
    add_recipe(rec)%>%
    add_model(log_mod)
  log_fitted_cv <- log_wf %>%
    tune_grid(
      resamples = folds,
      grid=5,
      metrics = metric_set(accuracy, roc_auc)
    )
saveRDS(log_fitted_cv, file = paste0(path_pg_models_save,"/",forme_dt,"_mod_log_tuned.RDS"))
log_fitted_cv %>% collect_metrics()
best_params_log <- show_best(log_fitted_cv, metric="roc_auc") # penalty=  ; roc_auc =
best_params_log
}

### RANDOM FOREST
rf_mod <- rand_forest(trees=tune(),min_n=tune())%>% # trees=nd d'arbres, min_n=profondeur d'arbre
  set_engine('ranger')%>%
  set_mode('classification')
rf_wf <- workflow()%>%
  add_recipe(rec)%>%
  add_model(rf_mod)
rf_fitted_cv <- rf_wf %>%
  tune_grid(
    resamples = folds, 
    metrics = metric_set(accuracy, roc_auc),
    grid=5
  ) # la prof met un grid à 20, f_meas et recall
saveRDS(rf_fitted_cv, file = paste0(path_pg_models_save,"/",forme_dt,"_mod_rf1_tuned.RDS"))
rf_fitted_cv %>% collect_metrics()
rf_fitted_cv %>% collect_metrics() %>% filter (.metric =='roc_auc') %>% arrange (desc(mean)) # trees=1760 ; min_n =14 ; roc_auc =0,738 
}

### XGBOOST
if (chargement_modeles == TRUE){
xg_mod <- boost_tree(mtry=tune(), min_n=tune())%>% #min_n=profondeur d'arbre, mtry=nb de feuilles
  set_engine('xgboost')%>%
  set_mode('classification')
xg_wf <- workflow()%>%
  add_recipe(rec)%>%
  add_model(xg_mod)
xg_fitted_cv <- xg_wf %>%
  tune_grid(
    resamples = folds,
    grid=5,
    metrics = metric_set(accuracy, roc_auc)
  )
saveRDS(xg_fitted_cv, file = paste0(path_pg_models_save,"/",forme_dt,"_mod_xgb_tuned.RDS"))
xg_fitted_cv %>% collect_metrics()
xg_fitted_cv %>% collect_metrics() %>% filter (.metric =='roc_auc') %>% arrange (desc(mean)) # mtry=114 ; min_n=19 ; roc_auc = 0,781
}

### LASSO
if (chargement_modeles == TRUE){
lasso_mod <- logistic_reg(mode="classification", engine="glmnet", penalty=tune(), mixture=1) 
lasso_wf <- workflow()%>%
  add_recipe(rec)%>%
  add_model(lasso_mod)
lasso_fitted_cv <- lasso_wf %>%
  tune_grid(
    resamples = folds,
    grid=5,
    metrics = metric_set(accuracy, roc_auc)
  )
saveRDS(lasso_fitted_cv, file = paste0(path_pg_models_save,"/",forme_dt,"_mod_lasso_tuned.RDS"))
lasso_fitted_cv %>% collect_metrics()
best_params_lasso <- show_best(lasso_fitted_cv, metric="roc_auc") # penalty=4.81e- 5  ; roc_auc = 0.686
best_params_lasso
}

### RIDGE
if (chargement_modeles == TRUE){
ridge_mod <- logistic_reg(mode="classification", engine="glmnet", penalty=tune(), mixture=0) 
ridge_wf <- workflow()%>%
  add_recipe(rec)%>%
  add_model(ridge_mod)
ridge_fitted_cv <- ridge_wf %>%
  tune_grid(
    resamples = folds,
    grid=5,
    metrics = metric_set(accuracy, roc_auc, f_meas,recall)
  )
saveRDS(ridge_fitted_cv, file = paste0(path_pg_models_save,"/",forme_dt,"_mod_ridge_tuned.RDS"))
ridge_fitted_cv %>% collect_metrics()
ridge_fitted_cv %>% collect_metrics() %>% filter (.metric =='roc_auc') %>% arrange (desc(mean)) # penalty=1.24e-10 ; roc_auc = 0,672
}

### ELASTICNET (IL ME RESISTE MAIS JE SUIS DESSUS)
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

# 4 | Comparaison des modèles (reste à faire)

