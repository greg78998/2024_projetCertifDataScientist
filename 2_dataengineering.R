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
source(paste0(path_USER,"/pg_propre/","X5_like_recipees.R"))

DB_ori <- readRDS(paste0(path_data_vf,"/","base.rds"))


dt_placement <- readRDS(file = paste0(path_data_vf,"/","para_dt_placement.RDS"))
interval_month <- readRDS( file = paste0(path_data_vf,"/","para_interval_month.RDS"))
annee_nb <- readRDS( file = paste0(path_data_vf,"/","para_annee_nb.RDS"))

### 1 - Mettre en page les variables qualitatives 

########## Variable nj

DB <- fc_transform_quali(para_DB = DB_ori) # voir fonctions dans X5

saveRDS(DB, file = paste0(path_data_vf,"/","base_postRET.RDS"))

### 2 - Créer les polygones sur les données meteo

MiseAuCarre <- function(para_DB, para_dt_placement=dt_placement){
  
  XXmeteo_0 <- para_DB %>% select(starts_with("rf_M"))
  XXmeteo <- XXmeteo_0^2
  colnames(XXmeteo) <- paste(colnames(XXmeteo_0),"_sq",sep="")
  
  saveRDS(XXmeteo, file = paste0(path_data_vf,"/",para_dt_placement,"_sq_precipitation.RDS"))
  
}

MiseAuCarre(para_DB = DB)

### 3 - Travail sur les interactions 



sapply(DB, function(x) sum(is.na(x)))

