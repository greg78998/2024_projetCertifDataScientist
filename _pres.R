## Avant le lancement de l'application 

matricule <- "N818398"

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF", sep = "")
} 
if (matricule == ""){
  path_USER <- ""
}

# 0 | chargement des libraries  -----

source(paste0(path_USER,"/pg_propre/","_before_chemins.R"))
source(paste0(path_USER,"/pg_propre/","_before_libraries.R"))

# 1 | chargement de la table + chargement des paramètres


db_defaillance <- haven::read_sas(data = paste0(path_data_vf,"/clean_donnees2.sas7bdat"), NULL) %>%
  filter(top_defaillance == "Y") %>% 
  mutate(date2 = as.Date(date_min)) %>% 
  select(date2) %>%
  mutate(annee = year(date2),
         mois = month(date2)) %>%
  group_by(annee, mois) %>%
  summarize(nb_defaillance = n())

DB <- readRDS(paste0(path_data_vf,"/","base_postRET.RDS"))


DB_defaillance <- DB %>% filter(top_defaillance ==1 )
