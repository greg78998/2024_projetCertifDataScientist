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

# 0bis | chargement des fonctions 
source(paste0(path_USER,"/pg_propre/","X3_MeF_predictionModels.R"))
source(paste0(path_USER,"/pg_propre/","X_creation_matrix_confusion.R"))


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

# On vient chercher la table de prévision
n<- 100000
pourTravaux <- data.frame(Y = as.numeric(sample(c(0:10), size = n, replace=TRUE)>9))

for (ii in 1:10){
  pourTravaux[,paste0("rf_",ii)] <- rnorm(n = n, mean = 0.1,sd = 0.05)
  pourTravaux[,paste0("xgb_",ii)] <- rnorm(n = n, mean = 0.1,sd = 0.05)
  pourTravaux[,paste0("logit_",ii)] <- rnorm(n = n, mean = 0.1,sd = 0.05)
}

DF_entrainement <- pourTravaux[1:n*0.8+1,]
DF_test <- pourTravaux[n*0.8+1:dim(pourTravaux)[1],]

#DF_entrainement <- readRDS(file = paste0(path_data_vf,"/","basesPREVISION_train",".RDS") )
#DF_test <- readRDS(file = paste0(path_data_vf,"/","basesPREVISION_test",".RDS") )

ls_label <- c("Non en difficulté", "En difficulté")

choice_A <- "Choix_Majoritaire"
choice_B <- "Un_modèle_suffit"
choice_C <- "Sur_moyenne_probabilités"


# Pour les predictions de demain

region_departement <- read_excel(paste0(path_data_,"/region_departement.xlsx", sep =""))
names(region_departement) <- c("department","departement_name", "region")

demain <- haven::read_sas(data = paste0(path_data_vf,"/clean_donnees2.sas7bdat"), NULL) %>% 
  mutate(department = substr(adr_depcom,1,2)) %>% 
  left_join(region_departement, by = "department") %>%
  select(siren, nj, ape,adr_depcom, region)




demain_nj <- demain %>% 
  select(nj) %>% 
  distinct() %>% 
  arrange(nj)
demain_ape <- demain %>% 
  select(ape) %>% 
  distinct() %>% 
  arrange(ape)




  
yy <- dim(demain)[1]
predictionDemain <- data.frame(Y = as.numeric(sample(c(0:10), size = yy, replace=TRUE)>9))

for (ii in 1:10){
  predictionDemain[,paste0("rf_",ii)] <- rnorm(n = yy, mean = 0.1,sd = 0.05)
  predictionDemain[,paste0("xgb_",ii)] <- rnorm(n = yy, mean = 0.1,sd = 0.05)
  predictionDemain[,paste0("logit_",ii)] <- rnorm(n = yy, mean = 0.1,sd = 0.05)
}


