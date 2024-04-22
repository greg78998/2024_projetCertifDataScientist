
fc_transform_col <- function(para_DB,para_col, para_seuil){
  
  # fonction qui transforme une variable qualitative avec plusieurs de modalités 
  #
  # @ para_DB : dataframe sur lequel on travaille 
  # @ para_col : la colonne que l'on va traiter 
  # @ seuil : fixé en pourcentage
  
  tab2 <- data.frame(perc = round(table(para_DB[,para_col]) / dim(para_DB)[1]*100,2))
  tab3 <- tab2 %>% filter(perc.Freq > para_seuil*100)
  
  tab4 <- para_DB
  tab4[,paste0(para_col,"_ret", sep = "")] <- as.factor(
    ifelse(tab4[,para_col] %in% tab3$perc.Var1,
           tab4[,para_col],
           "Autres"))
  
  return(tab4)
}

fc_transform_col_factor <- function(para_DB,para_col){
  
  # fonction qui transforme une variable en facteur
  #
  # @ para_DB : dataframe sur lequel on travaille 
  # @ para_col : la colonne que l'on va traiter 

  para_DB[,para_col] <- as.factor(para_DB[,para_col])
  
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
  transform_1 <- fc_transform_col(para_DB = para_DB, para_col = "nj", para_seuil = 0.1)
  
  # Création sur les ape
  transform_2 <- fc_transform_col(para_DB = transform_1, para_col = "ape", para_seuil = 0.08)
  
  # Découper la variable age_ent
  transform_2[,paste0("ent_age","_ret", sep = "")] <- cut(transform_2$ent_age, breaks = c(0,3,10,20,40,200))
  
  # Transformer en facteur 
  transform_4 <- fc_transform_col_factor(transform_2,para_col = "region")
  
  # Retirer des colonnes 
  transform_5 <- fc_transform_col_delete(transform_4, para_col = c("department"))
  
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

