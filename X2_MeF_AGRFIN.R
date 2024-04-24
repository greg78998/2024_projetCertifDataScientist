

X2_creationSIREN_db <- function(para_dt_placement,
                                para_interval) {
  
  ## para_dt_placement : date à laquelle on se place
  ## para_interval : intervalle en mois considéré 
  ## para_dt_placement <- as.Date("2023-12-31")
  ## para_interval <- 11
  
  # on veut obtenir 2023-01-01 (et pas 2023-01-31) pour para_dt_placement = 2023-12-31, d'où 4 étapes
  c_dt_deb <- floor_date(para_dt_placement, "month") - months(para_interval)
  c_dt_deb <- as.Date(c_dt_deb)
  
  
  # chargement des données relatives aux SIREN 
  agr_fin_data <- haven::read_sas(data_file = paste0(path_data_vf,
                                                     "/clean_donnees2.sas7bdat", sep = ""), NULL)
  
  # Nom des variables en minuscules
  names(agr_fin_data) <- tolower(names(agr_fin_data))

  
  # Ajout des variables financières
  
        #### on impute le mode (pour un siren donné) aux valeurs manquantes dans chaque variable ; on veut minimiser les variations lorsqu'il ya des données manquantes pour un siren donné
        agr_fin_data_0 <- agr_fin_data
        
        calculer_mode_sans_na <- function(row) { row_sans_na <- na.omit(row) 
        if (length(row_sans_na) == 0) { return(NA) # Retourner NA si toutes les valeurs sont manquantes 
        } 
        mode <- names(sort(table(row_sans_na), decreasing = TRUE))[1] # Trouver la valeur la plus fréquente 
        return(mode)}
        
        e001_a_travailler <- c("e001_m_2014", "e001_m_2015", "e001_m_2016", "e001_m_2017", "e001_m_2018", "e001_m_2019", "e001_m_2020", "e001_m_2021")

        agr_fin_data_0$e001_mode <- apply(agr_fin_data_0[,e001_a_travailler], 1,calculer_mode_sans_na)
        agr_fin_data_0$e001_mode <- as.numeric(agr_fin_data_0$e001_mode)

        # summary(agr_fin_data_0)
  
        agr_fin_data_0$e001_moy <- rowMeans(agr_fin_data_0[,c("e001_m_2014", "e001_m_2015", "e001_m_2016", "e001_m_2017", "e001_m_2018", "e001_m_2019", "e001_m_2020", "e001_m_2021")], na.rm=TRUE)
        agr_fin_data_0$r310_moy <- rowMeans(agr_fin_data_0[,c("r310_m_2014", "r310_m_2015", "r310_m_2016", "r310_m_2017", "r310_m_2018", "r310_m_2019", "r310_m_2020", "r310_m_2021")], na.rm=TRUE)
  
        agr_fin_data_1 <- agr_fin_data_0
        agr_fin_data_1[e001_a_travailler] <- lapply(agr_fin_data_0[e001_a_travailler], function(col) ifelse(is.na(col), agr_fin_data_0$e001_mode, col))
        # summary(agr_fin_data_1)
  
        #### on supprime les lignes où les var fi sont toutes nulles (possible quand on a que des données 2012/2013/2022/2023)
        # summary(subset(agr_fin_data_1, top_defaillance=="Y")) # on perd 119 lignes pour les sociétés défaillantes
        # summary(subset(agr_fin_data_1, top_defaillance=="N")) # on perd 5345 lignes pour les sociétés défaillantes
        agr_fin_data_2 <- agr_fin_data_1[complete.cases(agr_fin_data_1$e001_mode),]
        

        #################### E001 (UTILISATION DU MODE POUR CORRIGER DONNEES ABERRANTES) ######################
        calculer_ecarts_au_mode <- function(dataframe, mode_variable, variables_a_comparer_au_mode) {
          for (colonne in variables_a_comparer_au_mode) {
            nom_ecart <- paste0("ecart_", colonne, "_au_mode") 
            dataframe[[nom_ecart]] <- dataframe[[colonne]] - dataframe[[mode_variable]] 
          } 
          return(dataframe)
          }
        agr_fin_data_2 <- calculer_ecarts_au_mode(agr_fin_data_2, "e001_mode", e001_a_travailler)

        agr_fin_data_2_e001 <- agr_fin_data_2[,c("siren", "date_creation", "date_min", "e001_m_2014", "e001_m_2015", "e001_m_2016",
                                                 "e001_m_2017", "e001_m_2018", "e001_m_2019", "e001_m_2020", "e001_m_2021", "e001_moy", "e001_mode",
                                                 "ecart_e001_m_2014_au_mode", "ecart_e001_m_2015_au_mode", "ecart_e001_m_2016_au_mode",
                                                 "ecart_e001_m_2017_au_mode", "ecart_e001_m_2018_au_mode", "ecart_e001_m_2019_au_mode",
                                                 "ecart_e001_m_2020_au_mode", "ecart_e001_m_2021_au_mode")]
        
        agr_fin_data_2_e001$ecart_e001_m_2014_abs <- abs(agr_fin_data_2_e001$ecart_e001_m_2014_au_mode)
        agr_fin_data_2_e001$ecart_e001_m_2015_abs <- abs(agr_fin_data_2_e001$ecart_e001_m_2015_au_mode)
        agr_fin_data_2_e001$ecart_e001_m_2016_abs <- abs(agr_fin_data_2_e001$ecart_e001_m_2016_au_mode)
        agr_fin_data_2_e001$ecart_e001_m_2017_abs <- abs(agr_fin_data_2_e001$ecart_e001_m_2017_au_mode)
        agr_fin_data_2_e001$ecart_e001_m_2018_abs <- abs(agr_fin_data_2_e001$ecart_e001_m_2018_au_mode)
        agr_fin_data_2_e001$ecart_e001_m_2019_abs <- abs(agr_fin_data_2_e001$ecart_e001_m_2019_au_mode)
        agr_fin_data_2_e001$ecart_e001_m_2020_abs <- abs(agr_fin_data_2_e001$ecart_e001_m_2020_au_mode)
        agr_fin_data_2_e001$ecart_e001_m_2021_abs <- abs(agr_fin_data_2_e001$ecart_e001_m_2021_au_mode)
        
        agr_fin_data_2_e001b <- agr_fin_data_2_e001
        agr_fin_data_2_e001b$e001_m_2014 <- ifelse(agr_fin_data_2_e001b$ecart_e001_m_2014_abs > 0.25*agr_fin_data_2_e001b$e001_moy, agr_fin_data_2_e001b$e001_mode, agr_fin_data_2_e001b$e001_m_2014)
        agr_fin_data_2_e001b$e001_m_2015 <- ifelse(agr_fin_data_2_e001b$ecart_e001_m_2015_abs > 0.25*agr_fin_data_2_e001b$e001_moy, agr_fin_data_2_e001b$e001_mode, agr_fin_data_2_e001b$e001_m_2015)
        agr_fin_data_2_e001b$e001_m_2016 <- ifelse(agr_fin_data_2_e001b$ecart_e001_m_2016_abs > 0.25*agr_fin_data_2_e001b$e001_moy, agr_fin_data_2_e001b$e001_mode, agr_fin_data_2_e001b$e001_m_2016)
        agr_fin_data_2_e001b$e001_m_2017 <- ifelse(agr_fin_data_2_e001b$ecart_e001_m_2017_abs > 0.25*agr_fin_data_2_e001b$e001_moy, agr_fin_data_2_e001b$e001_mode, agr_fin_data_2_e001b$e001_m_2017)
        agr_fin_data_2_e001b$e001_m_2018 <- ifelse(agr_fin_data_2_e001b$ecart_e001_m_2018_abs > 0.25*agr_fin_data_2_e001b$e001_moy, agr_fin_data_2_e001b$e001_mode, agr_fin_data_2_e001b$e001_m_2018)
        agr_fin_data_2_e001b$e001_m_2019 <- ifelse(agr_fin_data_2_e001b$ecart_e001_m_2019_abs > 0.25*agr_fin_data_2_e001b$e001_moy, agr_fin_data_2_e001b$e001_mode, agr_fin_data_2_e001b$e001_m_2019)
        agr_fin_data_2_e001b$e001_m_2020 <- ifelse(agr_fin_data_2_e001b$ecart_e001_m_2020_abs > 0.25*agr_fin_data_2_e001b$e001_moy, agr_fin_data_2_e001b$e001_mode, agr_fin_data_2_e001b$e001_m_2020)
        agr_fin_data_2_e001b$e001_m_2021 <- ifelse(agr_fin_data_2_e001b$ecart_e001_m_2021_abs > 0.25*agr_fin_data_2_e001b$e001_moy, agr_fin_data_2_e001b$e001_mode, agr_fin_data_2_e001b$e001_m_2021)
        agr_fin_data_2_e001b <- calculer_ecarts_au_mode(agr_fin_data_2_e001b, "e001_mode", e001_a_travailler)
        agr_fin_data_2_e001_vf <- agr_fin_data_2_e001b[,1:11]
        agr_fin_data_2_e001_vf$e001_moy <- rowMeans(agr_fin_data_2_e001_vf[,c("e001_m_2014", "e001_m_2015", "e001_m_2016", "e001_m_2017", "e001_m_2018", "e001_m_2019", "e001_m_2020", "e001_m_2021")])
        agr_fin_data_2_e001_vf <- agr_fin_data_2_e001_vf[,-c(4:11)]
        rm (agr_fin_data_0, agr_fin_data_1, agr_fin_data_2_e001,agr_fin_data_2_e001b)
        
        
        #################### R310 (ON NE CORRIGE PAS DONNEES ABERRANTES ; MOYENNE SANS NA) - déjà calculé plus haut  ######################

        
        #################### FUSION APRES RETRAITEMENTS DONNEES ABERRANTES ##################
        agr_fin_data_3 <- merge(agr_fin_data_2[,-c(9:26,28:ncol(agr_fin_data_2))],agr_fin_data_2_e001_vf[,c(1,4)], by="siren")
        agr_fin_data_3 <- rename(agr_fin_data_3, ca_moy_2014_2021=r310_moy, effectifs_moy_2014_2021=e001_moy)

  
  # Définition du TOPDEFAILLANCE
  print(paste0("Est considéré comme défaillant si Y entre le (",c_dt_deb,") et le (",dt_placement,")"))

  agr_fin_data_3$top_defaillance2 <- ifelse(agr_fin_data_3$top_defaillance == "Y" & 
                                            agr_fin_data_3$date_min <= dt_placement &
                                            agr_fin_data_3$date_min >= c_dt_deb,1,0) 
  
  # Suppression des établissements déjà défaillant avant phase d'analyse (on ne garde pas si défaillance Y ET défaillance avant 2023-01-31)
  agr_fin_data_vf <-  agr_fin_data_3 %>% 
    filter(!(top_defaillance=="Y" & 
               (date_min < c_dt_deb) )) %>% 
    mutate(dt = para_dt_placement)
  
  # Suppression des établissements créés pendant ou après la période d'analyse de la défaillance (fonction de la dt_placement) 
  agr_fin_data_vf <-  agr_fin_data_vf %>% 
    filter(!(as.Date(agr_fin_data_vf$date_creation)>=c_dt_deb))
  
  # summary(agr_fin_data_vf)
  
  print("Chargement de la table AGRFIN réussi") 
  print("---------------------------------------------")
  return(agr_fin_data_vf)
  
}