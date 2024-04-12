


X1_creationCHIRPS_db <- function(para_dt_fin, para_interval_month,para_nbYear_scope) {
  
  ### para_dt_fin         <- date à laquelle on se place 
  ### para_interval_month <- intervale construit sur la base du mois
  ### para_dt_scope       <- combien de période nous voulons dans M_...
  
  # TOP_Defaillance va être calculer par cette variable
  c_dt_deb <- seq(para_dt_fin, by = "-1 month", length.out=para_interval_month)[para_interval_month]
  # Date qui va nous permettre de travailler moins de données
  c_dt_scope <- seq(para_dt_fin, by = "-1 month", 
                    length.out=para_nbYear_scope * 12 + 2)[para_nbYear_scope * 12 + 2]
  
  # 1 | Importation des tables 
  
  chirps_ori <- data.table::fread(paste0(path_data_vf,"/data_inputCHIRPS.csv"),dec=",")
  
  ls_code_insee <- haven::read_sas(data_file = paste0(path_data_vf,
                                                     "/clean_donnees2.sas7bdat", sep = ""), NULL) %>% 
    select(adr_depcom) %>% 
    distinct() %>% 
    arrange() %>% 
    rename(code_insee = adr_depcom)
  
  chirps_1 <- chirps_ori %>% 
    dplyr::filter(date > c_dt_scope & date <= para_dt_fin) %>%
    dplyr::select(date, code_insee, rf_value_aug) %>% 
    dplyr::group_by(date,code_insee) %>%
    dplyr::summarise(rf_value_aug = mean(rf_value_aug)) %>% 
    dplyr::ungroup(date,code_insee) %>%
    dplyr::arrange(code_insee, desc(date))
  
  ls_nb <- chirps_1 %>% select(date) %>% distinct()
  ls_nb$seq <- seq(0, dim(chirps_1 %>% select(date) %>% distinct())[1]-1)
  
  # 2 | Transposition de la table 
  
  chirps_2 <- chirps_1 %>% 
    dplyr::left_join(ls_nb, by = "date") %>% 
    dplyr::mutate(seq_date = paste("M",seq, sep ="_")) %>% 
    pivot_wider(id_cols = code_insee, 
                       names_from = seq_date, 
                       names_prefix = "rf_",
                       values_from = rf_value_aug)
  
  chirps_3 <- ls_code_insee %>% left_join(chirps_2, by = "code_insee")
  chirps_3[is.na(chirps_3)] <- rnorm(n = 1, mean = 0, sd = 2)
  chirps_3$dt <- para_dt_fin
  
  
  print(paste0("Chargement de la table CHIRPS réussi : (",c_dt_scope , ")-(", para_dt_fin,")"))
  print("---------------------------------------------")
  return(chirps_3)
  
}
