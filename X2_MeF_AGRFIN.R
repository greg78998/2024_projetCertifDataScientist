

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
  summary(agr_fin_data)
  
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
        agr_fin_data_0$b001_moy <- rowMeans(agr_fin_data_0[,c("b001_m_2014", "b001_m_2015", "b001_m_2016", "b001_m_2017", "b001_m_2018", "b001_m_2019", "b001_m_2020", "b001_m_2021")], na.rm=TRUE)
        agr_fin_data_0$b002_moy <- rowMeans(agr_fin_data_0[,c("b002_m_2014", "b002_m_2015", "b002_m_2016", "b002_m_2017", "b002_m_2018", "b002_m_2019", "b002_m_2020", "b002_m_2021")], na.rm=TRUE)
        agr_fin_data_0$b102_moy <- rowMeans(agr_fin_data_0[,c("b102_m_2014", "b102_m_2015", "b102_m_2016", "b102_m_2017", "b102_m_2018", "b102_m_2019", "b102_m_2020", "b102_m_2021")], na.rm=TRUE)
        agr_fin_data_0$b103_moy <- rowMeans(agr_fin_data_0[,c("b103_m_2014", "b103_m_2015", "b103_m_2016", "b103_m_2017", "b103_m_2018", "b103_m_2019", "b103_m_2020", "b103_m_2021")], na.rm=TRUE)
        agr_fin_data_0$b330_moy <- rowMeans(agr_fin_data_0[,c("b330_m_2014", "b330_m_2015", "b330_m_2016", "b330_m_2017", "b330_m_2018", "b330_m_2019", "b330_m_2020", "b330_m_2021")], na.rm=TRUE)
        agr_fin_data_0$b342_moy <- rowMeans(agr_fin_data_0[,c("b342_m_2014", "b342_m_2015", "b342_m_2016", "b342_m_2017", "b342_m_2018", "b342_m_2019", "b342_m_2020", "b342_m_2021")], na.rm=TRUE)
        agr_fin_data_0$b348_moy <- rowMeans(agr_fin_data_0[,c("b348_m_2014", "b348_m_2015", "b348_m_2016", "b348_m_2017", "b348_m_2018", "b348_m_2019", "b348_m_2020", "b348_m_2021")], na.rm=TRUE)
        agr_fin_data_0$b500_moy <- rowMeans(agr_fin_data_0[,c("b500_m_2014", "b500_m_2015", "b500_m_2016", "b500_m_2017", "b500_m_2018", "b500_m_2019", "b500_m_2020", "b500_m_2021")], na.rm=TRUE)
        agr_fin_data_0$r005_moy <- rowMeans(agr_fin_data_0[,c("r005_m_2014", "r005_m_2015", "r005_m_2016", "r005_m_2017", "r005_m_2018", "r005_m_2019", "r005_m_2020", "r005_m_2021")], na.rm=TRUE)
        agr_fin_data_0$r008_moy <- rowMeans(agr_fin_data_0[,c("r008_m_2014", "r008_m_2015", "r008_m_2016", "r008_m_2017", "r008_m_2018", "r008_m_2019", "r008_m_2020", "r008_m_2021")], na.rm=TRUE)
        agr_fin_data_0$r100_moy <- rowMeans(agr_fin_data_0[,c("r100_m_2014", "r100_m_2015", "r100_m_2016", "r100_m_2017", "r100_m_2018", "r100_m_2019", "r100_m_2020", "r100_m_2021")], na.rm=TRUE)
        agr_fin_data_0$r310_moy <- rowMeans(agr_fin_data_0[,c("r310_m_2014", "r310_m_2015", "r310_m_2016", "r310_m_2017", "r310_m_2018", "r310_m_2019", "r310_m_2020", "r310_m_2021")], na.rm=TRUE)
        agr_fin_data_0$r420_moy <- rowMeans(agr_fin_data_0[,c("r420_m_2014", "r420_m_2015", "r420_m_2016", "r420_m_2017", "r420_m_2018", "r420_m_2019", "r420_m_2020", "r420_m_2021")], na.rm=TRUE)
        
        
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
        agr_fin_data_3 <- merge(agr_fin_data_2[,-c(9:122,136:ncol(agr_fin_data_2))],agr_fin_data_2_e001_vf[,c(1,4)], by="siren")
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