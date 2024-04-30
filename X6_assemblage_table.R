# Pour s'assurer de la cohérence de la table DB 

X6_construction_base <- function(forme_dt_ls,para_succ_nb_periode){
  
  if (any(grepl("simple", forme_dt_ls))){
    DB <- readRDS(paste0(path_data_vf,"/",dt_placement,"_DB_postRET.RDS"))
    print("La table contient les variables simples")
  }

  if (any(grepl("add_surplus", forme_dt_ls))){
    DB_surplus <- readRDS(paste0(path_data_vf,"/",dt_placement,"_top_surplus.RDS"))
    DB <- DB %>% cbind(DB_surplus)
    print("La table contient désormais les variables relatives aux précipitations additionnelles")
    print("-->top_chocs : la valeur dépasse un seuil en valeur absolu")
  }
  if (any(grepl("add_chocs", forme_dt_ls))){
    DB_chocs <- readRDS(paste0(path_data_vf,"/",dt_placement,"_top_chocs.RDS"))
    DB <- DB %>% cbind(DB_chocs)
    print("La table contient désormais les variables relatives aux précipitations additionnelles")
    print("-->top_chocs : la valeur dépasse un seuil en valeur absolu")
  }

  if (any(grepl("add_succ_surplus", forme_dt_ls))){
    DB_succ_surplus <- readRDS(paste0(path_data_vf,"/",dt_placement,"_succ_",para_succ_nb_periode,"_surplus.RDS"))
    DB <- DB %>% cbind(DB_succ_surplus)
    print("La table contient désormais les variables relatives aux succ ")
    print("-->top_succ_chocs : succession de surplus d'eau")
  }
  if (any(grepl("add_succ_chocs", forme_dt_ls))){
    DB_succ_chocs <- readRDS(paste0(path_data_vf,"/",dt_placement,"_succ_",para_succ_nb_periode,"_chocs.RDS"))
    DB <- DB %>% cbind(DB_succ_chocs)
    print("La table contient désormais les variables relatives aux succ ")
    print("-->top_succ_chocs : succession de chocs")
  }
  
  if (any(grepl("poly", forme_dt_ls))){
    DB_poly <- readRDS(paste0(path_data_vf,"/",dt_placement,"_sq_rf_M.RDS"))
    DB <- DB %>% cbind(DB_poly)
    print("La table contient désormais les variables relatives aux précipitations au carré")
    
  }
  
  return(DB)
}

