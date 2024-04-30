

matricule <- "N818398"

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF", sep = "")
} 
if (matricule == ""){
  path_USER <- ""
}

# Chargement des utilisateurs 

chargement_modeles <- TRUE
nb_model <- 10

forme_dt_ls <- c("simple")  
#forme_dt_ls <- c("simple","add","poly")  


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


forme_dt_ls <- c("simple","add_surplus","add_chocs","add_succ_surplus","add_succ_chocs","poly")  
forme_dt  <- paste(forme_dt_ls, collapse = "_") # pour les dénomination des modèles

DB <- X6_construction_base(forme_dt_ls,para_succ_nb_periode = 3)




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

# 3 | estimation de modèle sur l'ensemble du modèle

for (pho in 1:nb_model){
  print(paste0("numero :",pho))
  if (chargement_modeles == TRUE){
    if (pho < 3){
      print(paste0("Entrainement logit n°",pho, sep= ""))
      logit_mod_vf <- glm(Y~.,data=training,family=binomial(link = "probit"))
      saveRDS(logit_mod_vf, file = paste0(path_pg_models_save,"/",forme_dt,"_vf_logit_n",pho,".RDS"))
      print("Ce modèle logit est sauvegardé")
    }
    
    print(paste0("Entrainement d'une forêt aléatoire n°",pho, sep= ""))
    rF_mod_vf <- ranger::ranger(as.factor(Y)~., data=training, probability = TRUE)
    saveRDS(rF_mod_vf, file = paste0(path_pg_models_save,"/",forme_dt,"_vf_rf_n",pho,".RDS"))
    print("Ce modèle randomForest est sauvegardé")
  }
}

# 4 | Mise en place de dataframe DF entrainement 
DF_entrainement <- training %>% select(Y)## correspond à la population sur laquelle on travaille on produit les estim
DF_test <- test_set %>% select(Y) ## on garde un dataframe sur lequel les modèles n'ont jamais vu les données

# La liste des modèles entrainés pour cette combi
ls_model_vf <- list.files(
  path = path_pg_models_save,
  pattern = paste0(forme_dt,"_vf_"))

for (ii in (ls_model_vf)){
  
  mod_vf_load <- str_split(ii,pattern = "_")[[1]][3]
  mod_num <- str_sub(str_split(ii,pattern = "_")[[1]][4],1,-5)

  print(paste(mod_vf_load,mod_num, sep ="_"))
  
  mod <- readRDS(file = paste0(path_pg_models_save,"/",ii))
  
  if (mod_vf_load == "logit"){
    print("Toute pop")
    DF_entrainement[,paste(mod_vf_load,mod_num, sep ="_")] <- predict(mod,training, type = "response")
    print("Données jamais vu")
    DF_test[,paste(mod_vf_load,mod_num, sep ="_")] <- predict(mod,test_set, type = "response")
  }
  if (mod_vf_load == "rf"){
    print("TOute population")
    DF_entrainement[, paste(mod_vf_load,mod_num, sep ="_")] <- predict(mod,training)$prediction[,2]
    print("Données jamais vu")
    DF_test[, paste(mod_vf_load,mod_num, sep ="_")] <- predict(mod,test_set)$prediction[,2]
  }
  
}

# test 
saveRDS(DF_entrainement,
        file = paste0(path_data_vf,"/",dt_placement,'_',forme_dt,"pourSHINY_training",".RDS") )
saveRDS(DF_test,
        file = paste0(path_data_vf,"/",dt_placement,'_',forme_dt,"pourSHINY_test",".RDS") )






seuil <- 0.005
DF_entrainement_aug <- add_var_model(para_db = DF_entrainement, para_threshold = seuil)
DF_test_aug <- add_var_model(para_db = DF_test, para_threshold = seuil)

# Matrice de confusion
table(DF_entrainement_aug$Y,DF_entrainement_aug$top_voteMajo)/dim(DF_entrainement_aug)[1]*100 # Un vote majoritaire
table(DF_entrainement_aug$Y,DF_entrainement_aug$top_votePosi)/dim(DF_entrainement_aug)[1]*100 # Si un modele estime que la proba est supérieure au seuil
table(DF_entrainement_aug$Y,DF_entrainement_aug$top_percMean)/dim(DF_entrainement_aug)[1]*100 # Si la moyenne des proba est supérieur au seuil



round(table(DF_test_aug$Y,DF_test_aug$top_voteMajo)/dim(DF_test_aug)[1]*100,2)
round(table(DF_test_aug$Y,DF_test_aug$top_votePosi)/dim(DF_test_aug)[1]*100,2)
round(table(DF_test_aug$Y,DF_test_aug$top_percMean)/dim(DF_test_aug)[1]*100,2)





