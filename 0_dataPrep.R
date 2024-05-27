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
dt_placement <-  as.Date("2023-12-31")       # date à laquelle on se place
interval_month <- 11                         # pour calculer le top_defaillance
annee_nb <- 5

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


# On rajoute les régions sur la base des département
region_departement <- read_excel(paste0(path_data_,"/region_departement.xlsx", sep =""))
names(region_departement) <- c("department","departement_name", "region")

DB <- agrfin_data %>% 
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
  left_join(chirps_data, by = c("code_insee", "dt")) %>%
  mutate(department=as.factor(substr(code_insee,1,2))) %>%
  select(-c(date_min,top_defaillance, code_insee)) %>%
  rename(top_defaillance=top_defaillance2) %>% 
  left_join(region_departement %>% 
              select(department, region), by = "department") %>% 
  mutate(ent_age = as.numeric((dt - date_creation)/365), 
         across(Total_dettes_moy, ~replace_na(., median(., na.rm=TRUE)))) %>% 
  select(-date_creation)

saveRDS(DB, file = paste0(path_data_vf,"/",dt_placement,"_DB_avt_stats.RDS"))

# cf. programme 1_statistics.R, suppression de variables fi (car trop corrélées) puis d'outliers 
DB <- DB %>% select(-c(FDR_moy, Total_actif_immobilise_moy, Emprunts_et_dettes_assim_moy, Autres_dettes_moy, EBE_moy, Capacite_autofin_moy, CA_net_en_France_moy))

DB <- DB %>% filter(effectifs_moy_2014_2021<350000 & Besoin_en_FDR_moy>-80000 & Besoin_en_FDR_moy<60000 & Total_actif_circulant_moy<150000 &
                        Dettes_four_et_comptes_ratt_moy<10000 & Total_dettes_moy<60000 & Resultat_comptable_moy<75000 & Resultat_comptable_moy>-20000)

#DB_2023 <- DB
#DB_2022 <- DB
#DB_2019 <- DB
#DB <- rbind(DB_2019, DB_2022, DB_2023)

print("contrôle des valeurs manquants")
print("=> on ne veut que des colonnes complètes")
sapply(DB, function(x) sum(is.na(x)))


saveRDS(DB, file = paste0(path_data_vf,"/",dt_placement,"_DB_.RDS"))


print(paste0("La table de référence est la suivante : ",dt_placement,"_DB_.RDS") )
print(paste0("se trouve à l'adresse suivante : ",path_data_vf))
