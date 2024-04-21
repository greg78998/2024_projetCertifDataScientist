
fc_transform_col <- function(para_DB,para_col, para_seuil){
  
  seuil_nj <- 0.1
  
  tab2 <- data.frame(perc = round(table(para_DB[,para_col]) / dim(para_DB)[1]*100,2))
  tab3 <- tab2 %>% filter(perc.Freq > para_seuil*100)
  
  tab4 <- para_DB
  tab4[,paste0(para_col,"_ret", sep = "")] <- as.factor(
    ifelse(tab4[,para_col] %in% tab3$perc.Var1,
           tab4[,para_col],
           "Autres"))
  
  return(tab4)
}

fc_transform_col_quanti <- function(para_DB,para_col, para_breaks){
  
  para_DB[,paste0(para_col,"_ret", sep = "")] <- cut(para_DB[,para_col], breaks = para_breaks)
  
  return(para_DB)
}
fc_transform_col_factor <- function(para_DB,para_col){
  
  para_DB[,para_col] <- as.factor(para_DB[,para_col])
  
  return(para_DB)
}
fc_transform_col_delete <- function(para_DB, para_col){
  para_DB <- para_DB[, !names(para_DB) %in% para_col]
  return(para_DB)
}

fc_transform_quali <- function(para_DB){
  
  # Création d'une cat sur la variable nj
  transform_1 <- fc_transform_col(para_DB = para_DB, para_col = "nj", para_seuil = 0.1)
  
  # Création sur les ape
  transform_2 <- fc_transform_col(para_DB = transform_1_0, para_col = "ape", para_seuil = 0.08)
  
  # Découper la variable age_ent
  transform_2[,paste0("ent_age","_ret", sep = "")] <- cut(transform_2$ent_age, breaks = c(0,3,10,20,40,200))
  
  # Transformer en facteur 
  transform_4 <- fc_transform_col_factor(transform_2,para_col = "region")
  
  # Retirer des colonnes 
  transform_5 <- fc_transform_col_delete(transform_4, para_col = c("department"))
  
  return(para_DB)
}
