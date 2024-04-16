

matricule <- "N818398"

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF", sep = "")
} 
if (matricule == ""){
  path_USER <- ""
}

# Chargement des utilisateurs 

chargement_modeles <- TRUE
forme_dt <- "simple"   # ou poly
nb_model <- 10


# ------------------------------------------------------------------------------ 


# 0 | chargement des libraries  -----

source(paste0(path_USER,"/pg_propre/","_before_chemins.R"))
source(paste0(path_USER,"/pg_propre/","_before_libraries.R"))

# 1 | chargement de la table + chargement des paramètres

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
    rF_mod_vf <- ranger::ranger(as.factor(Y)~., data=training, probability = TRUE)
    saveRDS(rF_mod_vf, file = paste0(path_data_vf,"/",forme_dt,"_vf_rf_n",pho,".RDS"))
  }
}

# 4 | Mise en place de dataframe REAL ---- NEVER_TRY
real <- training %>% select(Y)
never_try <- test_set %>% select(Y)
  

# 5 - Charger les modèles 
for (pho in 1:nb_model){
  print(paste0("numero :",pho))
  rF_mod <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_vf_rf_n",pho,".RDS"))
  real[, paste0("rf_",pho)] <- predict(rF_mod,training)$prediction[,2]
}

seuil <- 0.005

real_db <- data.frame(real %>% select(-Y) > seuil)
db_propUN <- data.frame(rf_voteMajo = rowSums(real_db)/dim(real)[2])
db_propMOY <- data.frame(perc_mean = rowMeans(real %>% select(-Y)))

real_db <- real %>% 
  bind_cols(db_propUN, 
            db_propMOY) %>%
  mutate(top_voteMajo = ifelse(rf_voteMajo>0.5,1,0),
         top_votePosi = ifelse(rf_voteMajo>0,1,0), 
         top_percMean = ifelse(perc_mean > seuil,1,0)) 


table(real_db$Y,real_db$top_voteMajo)
table(real_db$Y,real_db$top_votePosi)
table(real_db$Y,real_db$top_percMean)



### id seuil naturel

aze <- sort(real_db$perc_mean, decreasing = FALSE)

tab_aze <- table(real_db$Y)[2]/dim(real_db)[1]
indice <- ceiling((1-tab_aze)*length(aze))

seuil_naturel <- aze[indice]
seuil <- seuil_naturel








for (pho in 1:nb_model){
  print(paste0("numero :",pho))
  rF_mod <- readRDS(file = paste0(path_data_vf,"/",forme_dt,"_vf_rf_n",pho,".RDS"))
  never_try[, paste0("rf_",pho)] <- predict(rF_mod,test_set)$prediction[,2]
}


nevertry_db <- data.frame(never_try %>% select(-Y) > seuil)
db_propUN <- data.frame(rf_voteMajo = rowSums(never_try)/dim(never_try)[2])
db_propMOY <- data.frame(perc_mean = rowMeans(never_try %>% select(-Y)))

never_try_db <- never_try %>% 
  bind_cols(db_propUN, db_propMOY) %>%
  mutate(top_voteMajo = ifelse(rf_voteMajo>0.5,1,0),
         top_votePosi = ifelse(rf_voteMajo>0,1,0), 
         top_percMean = ifelse(perc_mean > seuil,1,0)) 


round(table(never_try_db$Y,never_try_db$top_voteMajo)/dim(never_try_db)[1]*100,2)
round(table(never_try_db$Y,never_try_db$top_votePosi)/dim(never_try_db)[1]*100,2)
round(table(never_try_db$Y,never_try_db$top_percMean)/dim(never_try_db)[1]*100,2)





