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


# 0 | Chargement de paramètres 
dt_placement <-  as.Date("2024-12-31")       # date à laquelle on se place
interval_month <- 11                         # pour calculer le top_defaillance
annee_nb <- 5 

TOP_RECONSTITUTION <- FALSE

# production de la table CHIRPS

if (TOP_RECONSTITUTION == TRUE){
  # Programme pour générer la table chirps depuis les fichiers sur GitHub https://github.com/greg78998/bdfprojet_data
  source("temp_CHIRPScreation.R")
}





# chargement de la table AGR_FIN

agrfin_data <- X2_creationSIREN_db(para_dt_placement = dt_placement, 
                                   para_interval = interval_month)
  
# chargement de la table CHIRPS 

chirps_data <- X1_creationCHIRPS_db(para_dt_fin = dt_placement,            # date à laquelle on se place
                                    para_interval_month = interval_month,  # top_defaillance est calculé sur cette table
                                    para_nbYear_scope = annee_nb)          # combien d'année 


# Merger les tables 
### (dans un second temps, les données de marché seront utilisées)

# On rajoute les régions sur la base des département
region_departement <- read_excel(paste0(path_data_,"/region_departement.xlsx", sep =""))
names(region_departement) <- c("department","departement_name", "region")



DB <- agrfin_data %>% 
  rename(code_insee = adr_depcom) %>%
  left_join(chirps_data, by = c("code_insee", "dt")) %>%
  mutate(department=as.factor(substr(code_insee,1,2))) %>%
  select(-c(date_min,top_defaillance, code_insee,b500_moy)) %>%
  rename(top_defaillance=top_defaillance2) %>% 
  left_join(region_departement %>% 
              select(department, region), by = "department") %>% 
  mutate(ent_age = as.numeric((dt - date_creation)/365)) %>% 
  select(-date_creation)


# Sauvegarde du modèle

sapply(DB, function(x) sum(is.na(x)))
saveRDS(DB, file = paste0(path_data_vf,"/",dt_placement,"_DB_.RDS"))


# Pour estimer sur une nouvelle base 
#saveRDS(DB, file = paste0(path_data_vf,"/","base_",dt_placement,".RDS"))


# Application du data engineering #################################################
source(paste0(path_USER,"/pg_propre/","X5_like_recipees.R"))
DB_ori <- readRDS(paste0(path_data_vf,"/",dt_placement,"_DB_.RDS"), )
### 1 - Mettre en page les variables qualitatives 

########## Variable nj

DB_ret <- fc_transform_quali(para_DB = DB_ori) # voir fonctions dans X5

### 2 - Créer les polygones sur les données meteo

DB_poly <- MiseAuCarre(para_DB = DB_ori, para_str_start ="rf_M")

### 3 - Travail sur les interactions 

DB_surplus <- Creation_new_var(para_DB=DB_ori, 
                               para_str_start = "rf_",
                               para_niv = 0, 
                               para_newNameVar = "top_surplus", 
                               para_abs = FALSE)

DB_extreme_chocs <- Creation_new_var(para_DB=DB_ori, 
                                     para_str_start = "rf_",
                                     para_niv = 1.5, 
                                     para_newNameVar = "top_chocs", 
                                     para_abs = TRUE)

#### 4 - Des variables cumulatives 


DB_surplus_succ <- add_cumul_function(para_db =DB_surplus, 
                                      prefix = "top_surplus_rf_M", 
                                      para_interval = 3, 
                                      new_nom = "surplus")

DB_surplus_succ <- add_cumul_function(para_db =DB_extreme_chocs, 
                                      prefix = "top_chocs_rf_M", 
                                      para_interval = 3, 
                                      new_nom = "chocs")
###################################################################################

# Construction de DB en fonction de la forme choisie
source(paste0(path_USER,"/pg_propre/","X6_assemblage_table.R"))
forme_dt_ls <- c("simple","add_succ_surplus","add_succ_chocs","poly") 
DB_futur <- X6_construction_base(forme_dt_ls,para_succ_nb_periode = 3)
forme_dt  <- paste(forme_dt_ls, collapse = "_")

# Nettoyage final de la table
DB_futur <- DB_futur %>% 
  select(-c(siren,dt,ea_ul)) %>% # je retire "ea_ul" qui n'a plus d'utilité (var quali : region, nj_ret, ape_ret, ent_age_ret)
  rename(Y = top_defaillance)# %>%
DB_futur$Y <- factor(DB_futur$Y)

DB_futur<- DB_futur %>%  filter(Y == 0)

saveRDS(DB_futur, file = paste0(path_data_vf,"/",dt_placement,"_DB_.RDS"))


