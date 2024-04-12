

X2_creationSIREN_db <- function(para_dt_placement,
                                para_interval) {
  
  ## para_dt_placement : date à laquelle on se place
  ## para_interval : intervalle en mois considéré 

  
  c_dt_deb <- seq(para_dt_placement, by = "-1 month", length.out=para_interval)[para_interval]
  
  
  # chargement des données relatives aux SIREN 
  agr_fin_data <- haven::read_sas(data_file = paste0(path_data_vf,
                                                     "/clean_donnees2.sas7bdat", sep = ""), NULL)
  
  # Nom des variables en minuscules
  names(agr_fin_data) <- tolower(names(agr_fin_data))
  
  # Définition du TOPDEFAILLANCE
  print(paste0("Est considéré comme défaillant si Y entre le (",c_dt_deb,") et le (",dt_placement,")"))

  agr_fin_data$top_defaillance2 <- ifelse(agr_fin_data$top_defaillance == "Y" & 
                                            agr_fin_data$date_min <= dt_placement &
                                            agr_fin_data$date_min >= c_dt_deb,1,0) 
  
  # Suppression des établissements déjà défaillant le passé
  agr_fin_data2 <-  agr_fin_data %>% 
    filter(!(top_defaillance=="Y" & 
               (date_min > dt_placement | date_min < c_dt_deb) ), 
           ent_age > 0) %>% 
    mutate(dt = para_dt_placement)
  
  print("Chargement de la table AGRFIN réussi") 
  print("---------------------------------------------")
  return(agr_fin_data2)
  
}