################################################################################
# Date : 05/04/2024
# Titre : construction de la table CHIRPS avant intégration dans la table globale

# Objectif : long --> wide

################################################################################

matricule <- "X822385"

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF", sep = "")
} 
if (matricule == "X822385"){
  path_USER <- "C:/Users/X822385/Desktop/DS/Projet"
}

# 0 | chargement des libraries  -----

setwd(paste0(path_USER,"/pg_propre"))

source("_before_chemins.R")
source("_before_libraries.R")

source("X1_MeF_CHIRPS.R")
source("X2_MeF_AGRFIN.R")
source("X5_like_recipees.R")
source("X6_assemblage_table.R")


# 0 | Chargement de paramètres 
dt_placement_futur <-  as.Date("2024-12-31")       # date à laquelle on se place
interval_month <- 11                               # pour calculer le top_defaillance
annee_nb <- 6                                      # combien d'année on va extraire
nb_model <- 2                                     # nb de modèles que nous allons estimer (surtout important pour les modèles qui comportent une part de random)
forme_dt_ls <- c("simple","add_succ_surplus","add_succ_chocs","poly") # forme des données
TOP_production_dataframe_futur <- TRUE            # production de la table
forme_dt  <- paste(forme_dt_ls, collapse = "_")    # variables calculées




if (TOP_production_dataframe_futur == TRUE) {
  
  print("=> chargement de la table AGR_FIN")
  agrfin_data_futur <- X2_creationSIREN_db(para_dt_placement = dt_placement_futur, 
                                           para_interval = interval_month)
  
  print("=> chargement de la table CHIRPS")
  chirps_data_futur <- X1_creationCHIRPS_db(para_dt_fin = dt_placement_futur,      # date à laquelle on se place
                                            para_interval_month = interval_month,  # top_defaillance est calculé sur cette table
                                            para_nbYear_scope = annee_nb)          # combien d'année 
  
  print("=> rajout des variables géo")
  region_departement <- read_excel(paste0(path_data_,"/region_departement.xlsx", sep =""))
  names(region_departement) <- c("department","departement_name", "region")
  
  print("=> merge et final db")
  DB_futur_preparation <- agrfin_data_futur %>% 
    rename(code_insee = adr_depcom, 
           Besoin_en_FDR_moy=b001_moy,
           FDR_moy=b002_moy,
           Total_actif_immobilise_moy=b102_moy,
           Total_actif_circulant_moy=b103_moy,
           Emprunts_et_dettes_assim_moy=b330_moy,
           Dettes_four_et_comptes_ratt_moy=b342_moy,
           Autres_dettes_moy=b348_moy,
           Total_dettes_moy=b500_moy,
           EBE_moy=r005_moy,
           Capacite_autofin_moy=r008_moy,
           Resultat_comptable_moy=r100_moy,
           CA_net_en_France_moy=r420_moy) %>%
    left_join(chirps_data_futur, by = c("code_insee", "dt")) %>%
    mutate(department=as.factor(substr(code_insee,1,2))) %>%
    select(-c(date_min,top_defaillance, code_insee)) %>%
    rename(top_defaillance=top_defaillance2) %>% 
    left_join(region_departement %>% 
                select(department, region), by = "department") %>% 
    mutate(ent_age = as.numeric((dt - date_creation)/365),
           across(Total_dettes_moy, ~replace_na(., median(., na.rm=TRUE)))) %>% 
    select(-date_creation)

  
  print("=> Sauvegarde des données")
  saveRDS(DB_futur_preparation, file = paste0(path_data_vf,"/",dt_placement_futur,"_DB_.RDS"))
  
}

DB_ori <- readRDS(paste0(path_data_vf,"/",dt_placement_futur,"_DB_.RDS"))
sapply(DB_futur_preparation, function(x) sum(is.na(x)))

### 1 - Mettre en page les variables qualitatives 

########## Variable nj

DB_ret <- fc_transform_quali(para_DB = DB_ori, 
                             para_dt_placement = dt_placement_futur) # voir fonctions dans X5

### 2 - Créer les polygones sur les données meteo

DB_poly <- MiseAuCarre(para_DB = DB_ori, 
                       para_str_start ="rf_M",
                       para_dt_placement = dt_placement_futur)

### 3 - Travail sur les interactions 

DB_surplus <- Creation_new_var(para_DB=DB_ori, 
                               para_str_start = "rf_",
                               para_niv = 0, 
                               para_newNameVar = "top_surplus", 
                               para_abs = FALSE, 
                               para_dt_placement = dt_placement_futur)

DB_extreme_chocs <- Creation_new_var(para_DB=DB_ori, 
                                     para_str_start = "rf_",
                                     para_niv = 1.5, 
                                     para_newNameVar = "top_chocs", 
                                     para_abs = TRUE, 
                                     para_dt_placement = dt_placement_futur)

#### 4 - Des variables cumulatives 


DB_surplus_succ <- add_cumul_function(para_db =DB_surplus, 
                                      prefix = "top_surplus_rf_M", 
                                      para_interval = 3, 
                                      new_nom = "surplus", 
                                      para_dt_placement = dt_placement_futur)

DB_surplus_chocs <- add_cumul_function(para_db =DB_extreme_chocs, 
                                       prefix = "top_chocs_rf_M", 
                                       para_interval = 3, 
                                       new_nom = "chocs", 
                                       para_dt_placement = dt_placement_futur)
###################################################################################

# Base choisie (JMI : exactement la même base que celle utilisée dans le programme 4)
DB_futur <- X6_construction_base(forme_dt_ls,
                                 para_succ_nb_periode = 3, 
                                 para_dt_placement = dt_placement_futur)

# Nettoyage final de la table
DB_futur <- DB_futur %>% 
  select(-c(siren,dt,ea_ul)) %>% # je retire "ea_ul" qui n'a plus d'utilité (var quali : region, nj_ret, ape_ret, ent_age_ret)
  rename(Y = top_defaillance) %>%
  mutate(Y= factor(Y))

saveRDS(DB_futur, file = paste0(path_data_vf,"/",dt_placement_futur,"_DB_ret.RDS"))

DB_futur_pred <- DB_futur %>% select(Y)

for (pho in 1:nb_model){
  
  print(paste0("tour de boucle : prediction sur données futures",pho))
  
  print("=> Lecture xgboost")
  xgb_fit <- readRDS(paste0(path_pg_models_save,"/",forme_dt,"_vf_xgb_n",pho,".RDS"))
  DB_futur_pred[,paste("xgb_mod_",pho, sep = "")] <- predict(xgb_fit, DB_futur, type = "prob")[2]
  
  if (pho < 2){
    print("=> Lecture logit")
    logit_fit <- readRDS(paste0(path_pg_models_save,"/",forme_dt,"_vf_logit_n",pho,".RDS"))
    DB_futur_pred[,paste("logit","mod",pho, sep = "_")] <- predict(logit_fit, DB_futur, type = "prob")[2]
  }
  
  print("=> Lecture random forest")
  rf_mod_fit <- readRDS(paste0(path_pg_models_save,"/",forme_dt,"_vf_random_forest_n",pho,".RDS"))
  DB_futur_pred[,paste("rf_mod",pho, sep = "_")] <- predict(rf_mod_fit, DB_futur)$predictions[,2]
  
}

saveRDS(DB_futur_pred, file = paste0(path_data_vf,"/",dt_placement_futur,"_prediction_demain_defaillance.RDS"))

names(DB)
names(DB_futur)
names(DB_futur_preparation)



