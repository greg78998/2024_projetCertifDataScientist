matricule <- "N818398"

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF", sep = "")
} 
if (matricule == "X822385"){
  path_USER <- "C:/Users/X822385/Desktop/DS/Projet"
}

# 0 | chargement des libraries  -----

source(paste0(path_USER,"/pg_propre/","_before_chemins.R"))
source(paste0(path_USER,"/pg_propre/","_before_libraries.R"))
source(paste0(path_USER,"/pg_propre/","X5_like_recipees.R"))

dt_placement <- readRDS(file = paste0(path_data_vf,"/","para_dt_placement.RDS"))
interval_month <- readRDS( file = paste0(path_data_vf,"/","para_interval_month.RDS"))
annee_nb <- readRDS( file = paste0(path_data_vf,"/","para_annee_nb.RDS"))

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

