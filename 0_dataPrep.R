################################################################################
# Date : 05/04/2024
# Titre : construction de la table CHIRPS avant intégration dans la table globale

# Objectif : long --> wide 

################################################################################

matricule <- "N818398"

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF", sep = "")
} 
if (matricule == ""){
  path_USER <- ""
}

# 0 | chargement des libraries  -----

setwd(paste0(path_USER,"/pg_propre"))

source("_before_chemins.R")
source("_before_libraries.R")

source("X1_MeF_CHIRPS.R")
source("X2_MeF_AGRFIN.R")


# 0 | Chargement de paramètres 
dt_placement <-  as.Date("2023-12-31")       # date à laquelle on se place
interval_month <- 12                         # pour calculer le top_defaillance
annee_nb <- 3 

saveRDS(dt_placement, file = paste0(path_data_vf,"/","para_dt_placement.RDS"))
saveRDS(interval_month, file = paste0(path_data_vf,"/","para_interval_month.RDS"))
saveRDS(annee_nb, file = paste0(path_data_vf,"/","para_annee_nb.RDS"))


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
region_departement <- read_excel("C:/Users/N818398/Desktop/projetBdF/_data/region_departement.xlsx")
names(region_departement) <- c("department","departement_name", "region")



DB <- agrfin_data %>% 
  rename(code_insee = adr_depcom) %>%
  left_join(chirps_data, by = c("code_insee", "dt")) %>%
  mutate(department=as.factor(substr(code_insee,1,2))) %>%
  select(-c(date_min,top_defaillance, code_insee)) %>%
  rename(top_defaillance=top_defaillance2) %>% 
  left_join(region_departement %>% 
              select(department, region), by = "department")



# Sélectionner des variables


sapply(chirps_data, function(x) sum(is.na(x)))



saveRDS(DB, file = paste0(path_data_vf,"/","base.RDS"))

