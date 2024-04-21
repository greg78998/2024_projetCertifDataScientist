# Pour s'assurer de la cohérence de la table DB 

if (any(grepl("add", forme_dt_ls))){
  DB_surplus <- readRDS(paste0(path_data_vf,"/",dt_placement,"_top_surplus.RDS"))
  DB_chocs <- readRDS(paste0(path_data_vf,"/",dt_placement,"_top_chocs.RDS"))
  DB <- DB %>% cbind(DB_surplus, DB_chocs)
  print("La table contient désormais les variables relatives aux précipitations additionnelles")
  print("-->top_surplus : la valeur est positive")
  print("-->top_chocs : la valeur dépasse un seuil en valeur absolu")
}
if (any(grepl("poly", forme_dt_ls))){
  DB_poly <- readRDS(paste0(path_data_vf,"/",dt_placement,"_sq_rf_M.RDS"))
  DB <- DB %>% cbind(DB_poly)
  print("La table contient désormais les variables relatives aux précipitations au carré")
  
}
