# pipeline - Classification 

matricule <- "N818398"

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF", sep = "")
} 
if (matricule == ""){
  path_USER <- ""
}


chargement_modeles <- FALSE
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


nb_bloc <- 2 

# 1 | chargement des modèles pour la cross validation

if (chargement_modeles == FALSE){
  
  logit_mod <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_mod_logit.RDS"))
  
  ridge_mod <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_mod_ridge.RDS"))
  lasso_mod <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_mod_lasso.RDS"))
  elasNet_mod <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_mod_elasticNet.RDS"))
  
  rpart_mdl <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_mod_arbre.RDS"))
  
  rF_mod1 <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_mod_rf1.RDS"))
  xgb_mod <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_mod_xgb.RDS"))
}


DB <- DB %>% 
  select(-c(siren,dt)) %>% 
  rename(Y = top_defaillance) %>%
  select(-c(ea_ul)) # ligne qui va disparaître à terme




# 2 | Mise en place du training + test

# pour véirfier que ça tourne 
#DB <- training(initial_split(DB, strata = Y, prop = 1))

need <- TRUE    # Pour assurer la reproductivité 
if (need){
  set.seed(1234)
  blocs <- sample(rep(1:nb_bloc, length(nrow(DB))))
  
  data_split <- initial_split(DB, strata = Y, prop = 0.8)
  
  training <- training(data_split) # data frame qui permet de faire le premier découpage
  test_set <- testing(data_split) # extraire le test set
  
  training_split <- initial_split(training, strata = Y, prop = 0.8)
  
  train_set <- training(training_split)
  eval_set <- testing(training_split)
}


tabPREV <- eval_set %>% select(Y)

XXA <- model.matrix(Y~., data = train_set)    # Utilisation pour le ridge, elastic, lasso
YYA <- train_set$Y
XXT <- model.matrix(Y~., data = eval_set)
YYT <- eval_set$Y

# 3 | Mise en place

if (chargement_modeles == TRUE){
  logit_mod <- glm(Y~.,data=train_set,family="binomial")
  saveRDS(logit_mod, file = paste0(path_pg_models_save,"/",forme_dt,"_mod_logit.RDS"))
}
tabPREV$logit <- predict(logit_mod, eval_set, type = 'response')

if (chargement_modeles == TRUE){
  ridge_mod <- cv.glmnet(XXA,YYA,alpha=0,family="binomial", type.measure = "auc")
  saveRDS(ridge_mod, file = paste0(path_pg_models_save,"/",forme_dt,"_mod_ridge.RDS"))
}
tabPREV$ridge <- predict(ridge_mod,XXT,s="lambda.min",type="response")

if (chargement_modeles == TRUE){
  lasso_mod <- cv.glmnet(XXA,YYA,alpha=1,family="binomial", type.measure = "auc")
  saveRDS(lasso_mod, file = paste0(path_pg_models_save,"/",forme_dt,"_mod_lasso.RDS"))
}
tabPREV$lasso <- predict(lasso_mod,XXT,s="lambda.min",type="response")

if (chargement_modeles == TRUE){
  elasNet_mod <- cv.glmnet(XXA,YYA,alpha=0.5,family="binomial", type.measure = "auc")
  saveRDS(elasNet_mod, file = paste0(path_pg_models_save,"/",forme_dt,"_mod_elasticNet.RDS"))
}
tabPREV$elasticNet <- predict(elasNet_mod,XXT,s="lambda.min",type="response")

if (chargement_modeles == TRUE){
  rpart_mdl <- rpart::rpart(as.factor(Y)~., data=train_set)
  saveRDS(rpart_mdl, file = paste0(path_pg_models_save,"/",forme_dt,"_mod_arbre.RDS"))
}
tabPREV$arbre <- predict(rpart_mdl,eval_set,type="prob")[,2]

if (chargement_modeles == TRUE){
  rF_mod1 <- ranger::ranger(as.factor(Y)~., data=train_set, probability = TRUE)
  saveRDS(rF_mod1, file = paste0(path_pg_models_save,"/",forme_dt,"_mod_rf1.RDS"))
}
tabPREV$rF_mod1 <- predict(rF_mod1,eval_set)$prediction[,2]

xgb_train = xgb.DMatrix(data = XXA, label = YYA)      # Utilisation pour le xgb
xgb_test = xgb.DMatrix(data = XXT, label = YYT)

if (chargement_modeles == TRUE){
  xgb_cv <- xgb.cv(data=xgb_train,nrounds=100,max_depth=2,nfold=10,verbose=TRUE,eta=0.1)
  iteropt <- which.min(xgb_cv$evaluation_log$test_rmse_mean)
  
  xgb_mod <- xgboost(data = xgb_train, max.depth = 2, 
                     nrounds = iteropt, eta=0.1,
                     verbose = TRUE, 
                     objective = "binary:logistic")
  saveRDS(xgb_mod, file = paste0(path_pg_models_save,"/",forme_dt,"_mod_xgb.RDS"))
}
tabPREV$xgb_mod <- predict(xgb_mod,xgb_test)






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

apply(X = tabPREV,2,accuracy,Y=tabPREV$Y)
apply(X = tabPREV,2,accuracy,Y=tabPREV$Y,seuil = 0.001)
apply(X = tabPREV,2,recall,Y=tabPREV$Y,seuil = 0.001)

