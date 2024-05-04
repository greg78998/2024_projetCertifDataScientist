fc_transform_col <- function(para_DB, para_col, para_seuil){
  
  # Fonction qui transforme une variable qualitative avec plusieurs modalités
  #
  # @ para_DB : DataFrame sur lequel on travaille
  # @ para_col : Nom de la colonne que l'on va traiter (comme chaîne de caractères)
  # @ para_seuil : Seuil de pourcentage pour garder les modalités fréquentes
  
  # Table de pourcentages des valeurs uniques de la colonne
  tab2 <- data.frame(
    Var1 = names(table(para_DB[[para_col]])), 
    Freq = round(table(para_DB[[para_col]]) / nrow(para_DB) * 100, 2)
  )
  
  # Filtrer les valeurs qui dépassent le seuil
  tab3 <- tab2 %>% dplyr::filter(Freq.Freq > para_seuil*100)
  
  # Créer une nouvelle colonne en catégorisant les valeurs
  para_DB[[paste0(para_col, "_ret")]] <- ifelse(
    para_DB[[para_col]] %in% tab3$Var1,
    para_DB[[para_col]],
    "Autres"
  )
  
  # Supprimer la colonne originale
  para_DB <- para_DB[, !(names(para_DB) %in% para_col)]
  
  return(para_DB)
}

fc_transform_col_delete <- function(para_DB, para_col){
  
  # fonction qui supprime la liste de variables
  #
  # @ para_DB : dataframe sur lequel on travaille 
  # @ para_col : liste des colonnes que nous allons supprimer  

  para_DB <- para_DB[, !names(para_DB) %in% para_col]
  return(para_DB)
}

fc_transform_quali <- function(para_DB){
  
  # fonction qui applique l'ensemble des transformation 
  # @ para_DB : le dataframe sur lequel on travaille en input 
  
  # Création d'une cat sur la variable nj
  print("Transformation de la variable nj")
  transform_1 <- fc_transform_col(para_DB = para_DB, para_col = "nj", para_seuil = 0.1)
  
  # Création sur les ape
  print("Transformation de la variable ape")
  transform_2 <- fc_transform_col(para_DB = transform_1, para_col = "ape", para_seuil = 0.08)
  
  # Découper la variable age_ent
  print("Découpage de la variable ent_age")
  transform_2[,paste0("ent_age","_ret", sep = "")] <- cut(transform_2$ent_age, breaks = c(0,3,10,20,40,200))
  transform_2 <- transform_2[, !names(transform_2) %in% 'ent_age']
  
  
  # Transformer en facteur 
  print("Mise en facteur de la variable region")
  transform_2$region <- factor(transform_2$region)
    
  # Retirer des colonnes 
  print("Suppression de la variable department")
  transform_5 <- fc_transform_col_delete(transform_2, para_col = c("department"))
  
  # sauvegarder la table
  saveRDS(transform_5, file = paste0(path_data_vf,"/",dt_placement,"_DB_postRET.RDS"))
  
  return(transform_5)
}



MiseAuCarre <- function(para_DB, 
                        para_dt_placement=dt_placement,
                        para_str_start){
  
  #
  # --> Mise au carré des variable qui commence par le string
  #
  # @ para_DB : le dataframe qui sert d'input
  # @ para_dt_placement : la chaîne de caractère étudidée <- permet de stocker la base sous un autre nom
  # @ para_str_start : chaine de caractère utilisé
  
  XXmeteo_0 <- para_DB %>% select(starts_with(para_str_start))
  XXmeteo <- XXmeteo_0^2
  colnames(XXmeteo) <- paste(colnames(XXmeteo_0),"_sq",sep="")
  
  saveRDS(XXmeteo, file = paste0(path_data_vf,"/",para_dt_placement,"_sq_",para_str_start,".RDS"))
  
  return(XXmeteo)
}



Creation_new_var <- function(para_DB, para_dt_placement=dt_placement, para_str_start, para_niv, para_newNameVar, para_abs){
  #
  # --> Fonction qui permet de créer de nouvelles variables engineering 
  #
  # @ para_DB : le dataframe qui sert d'input
  # @ para_dt_placement : permet de produire une table avec le bon prefix
  # @ para_str_start : la chaîne de caractère étudidée
  # @ para_niv : le seuil
  # @ para_newNameVar : le nouveau nom de la variable
  # @ para_abs : le seuil est-il en valeur absolue ou pas
  
  var_names <- grep(paste0("^",para_str_start, sep = ""), names(para_DB), value = TRUE)
  for (var_name in var_names) {
    if (para_abs==FALSE){
      para_DB[[paste0(para_newNameVar,"_", var_name, sep = "")]] <- ifelse(para_DB[[var_name]] > para_niv, 1, 0)
    } else {
      para_DB[[paste0(para_newNameVar,"_", var_name, sep = "")]] <- ifelse(abs(para_DB[[var_name]]) > para_niv, 1, 0)
      
    }
  }
  para_DB_2 <- para_DB %>% select(starts_with(para_newNameVar))
  
  saveRDS(para_DB_2, file = paste0(path_data_vf,"/",para_dt_placement,"_",para_newNameVar,".RDS"))
  
  return(para_DB_2)
  
}

add_cumul_function <- function(para_db, para_interval, prefix,new_nom){
  
  for (ii in seq(interval_month+1,interval_month+dim(para_db)[2]-(para_interval+1))){
    
    s <- seq(ii+1,ii+para_interval) 
    s_cal <- paste0(prefix, "_",s)
    
    para_db[,paste("top_succ",para_interval,new_nom, ii, sep = "_")] = round(rowSums(para_db[, s_cal])/para_interval,0)
  }
  
  ls_var_cons <- grep(paste("top_succ",para_interval, sep = "_"), names(para_db), value = TRUE)
  
  para_db_vf <- para_db %>% select(all_of(ls_var_cons))
  
  print(paste0("La table :",dt_placement,"_succ_",para_interval,"_",new_nom , " est sauvegardée."))
  saveRDS(para_db_vf, file = paste0(path_data_vf,"/",dt_placement,"_succ_",para_interval,"_",new_nom,".RDS"))
  
  return(para_db_vf)
}

