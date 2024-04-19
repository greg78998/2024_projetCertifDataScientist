

matricule <- "N818398"

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF", sep = "")
} 
if (matricule == ""){
  path_USER <- ""
}

# Chargement des utilisateurs 

chargement_modeles <- FALSE
forme_dt <- "simple"   # ou poly
nb_model <- 10


# ------------------------------------------------------------------------------ 


# 0 | chargement des libraries  -----

source(paste0(path_USER,"/pg_propre/","_before_chemins.R"))
source(paste0(path_USER,"/pg_propre/","_before_libraries.R"))

# 1 | chargement de la table + chargement des paramètres

source(paste0(path_USER,"/pg_propre/","X3_MeF_predictionModels.R"))

DB <- readRDS(paste0(path_data_vf,"/","base_postRET.RDS"))

if (forme_dt == "poly"){
  DB_poly <- readRDS(paste0(path_data_vf,"/","base_precipitationcarre.RDS"))
  DB <- DB %>% cbind(DB_poly)
}

dt_placement <- readRDS(file = paste0(path_data_vf,"/","para_dt_placement.RDS"))
interval_month <- readRDS( file = paste0(path_data_vf,"/","para_interval_month.RDS"))
annee_nb <- readRDS( file = paste0(path_data_vf,"/","para_annee_nb.RDS"))

# 2 | Remise en page du dataframe 

DB <- DB %>% 
  select(-c(siren,dt)) %>% 
  rename(Y = top_defaillance) %>%
  select(-c(ea_ul)) # ligne qui va disparaître à terme

set.seed(1234)
data_split <- initial_split(DB, strata = Y, prop = 0.8)

training <- training(data_split) # data frame qui permet de faire le premier découpage
test_set <- testing(data_split) # extraire le test set


# 3 | estimation de modèle sur l'ensemble du modèle

for (pho in 1:nb_model){
  print(paste0("numero :",pho))
  if (chargement_modeles == TRUE){
    
    if (pho < 3){
      print(paste0("Entrainement logit n°",pho, sep= ""))
      logit_mod_vf <- glm(Y~.,data=training,family="binomial")
      saveRDS(logit_mod_vf, file = paste0(path_data_vf,"/",forme_dt,"_vf_logit_n",pho,".RDS"))
    }
    
    print(paste0("Entrainement d'une forêt aléatoire n°",pho, sep= ""))
    rF_mod_vf <- ranger::ranger(as.factor(Y)~., data=training, probability = TRUE)
    saveRDS(rF_mod_vf, file = paste0(path_data_vf,"/",forme_dt,"_vf_rf_n",pho,".RDS"))
  }
}

# 4 | Mise en place de dataframe DF entrainement 
DF_entrainement <- training %>% select(Y)## correspond à la population sur laquelle on travaille on produit les estim
DF_test <- test_set %>% select(Y) ## on garde un dataframe sur lequel les modèles n'ont jamais vu les données


# 5 - Charger les modèles 
for (pho in 1:nb_model){
  
  print(paste0("Import des modèles entrainés : n°",pho))
  
  if (pho < 3){
    print("logit")
    logit_mod_vf <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_vf_logit_n",pho,".RDS"))
    DF_entrainement[, paste0("logit_",pho)] <- predict(logit_mod_vf,training, type = "response")
  }

  print("randomForest")
  rF_mod <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_vf_rf_n",pho,".RDS"))
  DF_entrainement[, paste0("rf_",pho)] <- predict(rF_mod,training)$prediction[,2]
  
  print(paste0("Données jamais vu : n°",pho))
  
  if (pho < 3){
    print("logit")
    logit_mod_vf <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_vf_logit_n",pho,".RDS"))
    DF_test[, paste0("logit_",pho)] <- predict(logit_mod_vf,test_set, type = "response")
  }
  
  print("randomForest")
  rF_mod <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_vf_rf_n",pho,".RDS"))
  DF_test[, paste0("rf_",pho)] <- predict(rF_mod,test_set)$prediction[,2]
}

seuil <- 0.005
DF_entrainement_aug <- add_var_model(para_db = DF_entrainement, para_threshold = seuil)
DF_test_aug <- add_var_model(para_db = DF_test, para_threshold = seuil)

# test 
saveRDS(DF_entrainement,
        file = paste0(path_data_vf,"/","basesPREVISION_train",".RDS") )
saveRDS(DF_test,
        file = paste0(path_data_vf,"/","basesPREVISION_test",".RDS") )

# Matrice de confusion
table(DF_entrainement_aug$Y,DF_entrainement_aug$top_voteMajo)/dim(DF_entrainement_aug)[1]*100 # Un vote majoritaire
table(DF_entrainement_aug$Y,DF_entrainement_aug$top_votePosi)/dim(DF_entrainement_aug)[1]*100 # Si un modele estime que la proba est supérieure au seuil
table(DF_entrainement_aug$Y,DF_entrainement_aug$top_percMean)/dim(DF_entrainement_aug)[1]*100 # Si la moyenne des proba est supérieur au seuil



round(table(DF_test_aug$Y,DF_test_aug$top_voteMajo)/dim(DF_test_aug)[1]*100,2)
round(table(DF_test_aug$Y,DF_test_aug$top_votePosi)/dim(DF_test_aug)[1]*100,2)
round(table(DF_test_aug$Y,DF_test_aug$top_percMean)/dim(DF_test_aug)[1]*100,2)





