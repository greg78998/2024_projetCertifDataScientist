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

dt_placement <- readRDS(file = paste0(path_data_vf,"/","para_dt_placement.RDS"))
interval_month <- readRDS( file = paste0(path_data_vf,"/","para_interval_month.RDS"))
annee_nb <- readRDS( file = paste0(path_data_vf,"/","para_annee_nb.RDS"))

DB_ori <- readRDS(paste0(path_data_vf,"/",dt_placement,"_DB_.RDS"))

### 1 - Mettre en page les variables qualitatives 

########## Variable nj

DB_ret <- fc_transform_quali(para_DB = DB_ori) # voir fonctions dans X5

saveRDS(DB_ret, file = paste0(path_data_vf,"/",dt_placement,"_DB_postRET.RDS"))

### 2 - Créer les polygones sur les données meteo

MiseAuCarre(para_DB = DB, para_str_start ="rf_M")

### 3 - Travail sur les interactions 

DB_surplus <- Creation_new_var(para_DB=DB, 
                               para_str_start = "rf_",
                               para_niv = 0, 
                               para_newNameVar = "top_surplus", 
                               para_abs = FALSE)
DB_extreme_chocs <- Creation_new_var(para_DB=DB, 
                                     para_str_start = "rf_",
                                     para_niv = 1.5, 
                                     para_newNameVar = "top_chocs", 
                                     para_abs = TRUE)



