# STASTISTIQUES (on fait des stats sur DB avant ajustements sur les variables financières : DB_stats, ainsi on n'interfère pas avec DB)

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

# 1 | chargement de la table + chargement des paramètres

DB_stats <- readRDS(paste0(path_data_vf,"/",dt_placement,"_DB_avt_stats.RDS")) # on cherche DB avant retraitements stats
DB <- readRDS(paste0(path_data_vf,"/",dt_placement,"_DB_.RDS")) # pour le avant/après pour les matrices de corr et les boxplots


dt_placement <- readRDS(file = paste0(path_data_vf,"/","para_dt_placement.RDS"))
interval_month <- readRDS( file = paste0(path_data_vf,"/","para_interval_month.RDS"))
annee_nb <- readRDS( file = paste0(path_data_vf,"/","para_annee_nb.RDS"))

# 2 | gestion des valeurs manquants

sapply(DB_stats, function(x) sum(is.na(x)))

# 3 | grapher les données 

graph_chirps <- DB_stats %>% 
  select(siren, region, top_defaillance, starts_with("rf_")) %>%
  pivot_longer(cols = starts_with("rf_M"), 
               names_to = "per_ref") %>%
  mutate(per_ref2 = as.integer(substr(per_ref,6,7))) %>%   # pour que les valeurs soient comparables
  filter(per_ref2 >= interval_month)                     # pour retirer l'interval avant défaillance

tab1 <- graph_chirps %>% filter(top_defaillance == 0) %>% select(siren) %>% distinct()
tab1$sample <- sample(rep(1:100,length=nrow(tab1)))
tab2 <- tab1 %>% filter(sample == 25)

draw_graph <- graph_chirps %>% 
  filter(top_defaillance == 1) %>%
  bind_rows(graph_chirps %>% 
              filter(top_defaillance == 0, siren %in% tab2$siren))
ggplot() + 
  geom_line(data=draw_graph,aes(x = desc(per_ref2), y = value)) +
  xlab("Lag") + 
  ylab("Rainfalls standardized") + 
  geom_hline(yintercept=0, linetype="dashed", 
            color = "red", size=1) + 
  geom_vline(xintercept=-interval_month, linetype="dashed", color = "red", size =1) +
  facet_grid(~top_defaillance) + 
  labs(title = "Analyse des séries de précipitation.", 
       subtitle = paste0("Etre en difficultés signifie le déclenchement d'une procédure collective dans les ",
                         interval_month, " derniers mois."),
       caption = "Un point représente une donnée climatique relative à un SIREN (source CHIRPS) et données géographiques")


# 4 | grapher les données financières (boxplot avant et après retrait de certains points atypiques)

# matrice de corrélation entre les variables financières
cor_matrix_avt <- DB_stats %>%
  select(effectifs_moy_2014_2021, ca_moy_2014_2021, Besoin_en_FDR_moy, FDR_moy, Total_actif_immobilise_moy, Total_actif_circulant_moy,
         Emprunts_et_dettes_assim_moy, Dettes_four_et_comptes_ratt_moy, Autres_dettes_moy, Total_dettes_moy, EBE_moy, Capacite_autofin_moy,
         Resultat_comptable_moy, CA_net_en_France_moy) %>% cor()

corrplot(cor_matrix_avt, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7, 
         col = colorRampPalette(c("red", "white", "blue"))(200))
saveRDS(cor_matrix_avt, file = paste0(path_data_vf,"/","cor_matrix_avt.RDS"))

# on déduit de cor_matrix_avt qu'on peut supprimer des variables très corrélées
#DB2 <- DB_stats %>% select(-c(FDR_moy, Total_actif_immobilise_moy, Emprunts_et_dettes_assim_moy, Autres_dettes_moy, EBE_moy, Capacite_autofin_moy, CA_net_en_France_moy))

cor_matrix_aps <- DB %>%
  select(effectifs_moy_2014_2021, ca_moy_2014_2021, Besoin_en_FDR_moy, Total_actif_circulant_moy,
         Dettes_four_et_comptes_ratt_moy, Total_dettes_moy, Resultat_comptable_moy) %>% cor()
saveRDS(cor_matrix_aps, file = paste0(path_data_vf,"/","cor_matrix_aps.RDS"))


corrplot(cor_matrix_aps, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", number.cex = 0.7, 
         col = colorRampPalette(c("red", "white", "blue"))(200))

# on supprime les observations qui sont vraiment atypiques (très faible densité de points) quand le Y est 0 vu que la base est très déséquilibrée : juste 108 obs en moins
#DB3 <- DB2 %>% filter(effectifs_moy_2014_2021<350000 & Besoin_en_FDR_moy>-80000 & Besoin_en_FDR_moy<60000 & Total_actif_circulant_moy<150000 &
#                             Dettes_four_et_comptes_ratt_moy<10000 & Total_dettes_moy<60000 & Resultat_comptable_moy<75000 & Resultat_comptable_moy>-20000)

ggplot(DB_stats, aes(x = "", y = ca_moy_2014_2021)) + geom_boxplot() + labs(title = "Boxplot de ca_moy_2014_2021", y = "ca_moy_2014_2021") + theme(axis.text.x = element_blank())

ggplot(DB_stats, aes(x = "", y = effectifs_moy_2014_2021)) + 
  geom_boxplot() + 
  labs(title = "Boxplot de effectifs_moy_2014_2021 avant retraitement", 
       y = "effectifs_moy_2014_2021") + 
  theme(axis.text.x = element_blank())
ggplot(DB, aes(x = "", y = effectifs_moy_2014_2021)) + geom_boxplot() + labs(title = "Boxplot de effectifs_moy_2014_2021 après retraitement", y = "effectifs_moy_2014_2021") + theme(axis.text.x = element_blank())

ggplot(DB_stats, aes(x = "", y = Besoin_en_FDR_moy)) + geom_boxplot() + labs(title = "Boxplot de Besoin en fond de roulement (DFDR) moyen avant retraitement", y = "Besoin_en_FDR_moy") + theme(axis.text.x = element_blank())
ggplot(DB, aes(x = "", y = Besoin_en_FDR_moy)) + geom_boxplot() + labs(title = "Boxplot de Besoin en fond de roulement (DFDR) moyen après retraitement", y = "Besoin_en_FDR_moy") + theme(axis.text.x = element_blank())

ggplot(DB_stats, aes(x = "", y = Total_actif_circulant_moy)) + geom_boxplot() + labs(title = "Boxplot de  Total de l'actif circulant moyen avant retraitement", y = "Total_actif_circulant_moy") + theme(axis.text.x = element_blank())
ggplot(DB, aes(x = "", y = Total_actif_circulant_moy)) + geom_boxplot() + labs(title = "Boxplot de  Total de l'actif circulant moyen après retraitement", y = "Total_actif_circulant_moy") + theme(axis.text.x = element_blank())

ggplot(DB_stats, aes(x = "", y = Dettes_four_et_comptes_ratt_moy)) + geom_boxplot() + labs(title = "Boxplot de  Dettes fournisseurs et comptes rattachées moyen avant retraitement", y = "Dettes_four_et_comptes_ratt_moy") + theme(axis.text.x = element_blank())
ggplot(DB, aes(x = "", y = Dettes_four_et_comptes_ratt_moy)) + geom_boxplot() + labs(title = "Boxplot de  Dettes fournisseurs et comptes rattachées moyen après retraitement", y = "Dettes_four_et_comptes_ratt_moy") + theme(axis.text.x = element_blank())

ggplot(DB_stats, aes(x = "", y = Total_dettes_moy)) + geom_boxplot() + labs(title = "Boxplot de Total des dettes avant retraitement", y = "Total_dettes_moy") + theme(axis.text.x = element_blank())
ggplot(DB, aes(x = "", y = Total_dettes_moy)) + geom_boxplot() + labs(title = "Boxplot de Total des dettes après retraitement", y = "Total_dettes_moy") + theme(axis.text.x = element_blank())

ggplot(DB_stats, aes(x = "", y = Resultat_comptable_moy)) + geom_boxplot() + labs(title = "Boxplot de Résultat comptable (bénéfice ou perte) avant retraitement", y = "Resultat_comptable_moy") + theme(axis.text.x = element_blank())
ggplot(DB, aes(x = "", y = Resultat_comptable_moy)) + geom_boxplot() + labs(title = "Boxplot de Résultat comptable (bénéfice ou perte) après retraitement", y = "Resultat_comptable_moy") + theme(axis.text.x = element_blank())


# 5 | Comparaison des défaillances entre le fichier du SIETE et Webstat 
compar_siete_webstat <- data.frame(
  Annee=c(2016,2017,2018,2019,2020,2021,2022,2023),
  Defaillances_SIETE=c(246,235,282,418,359,553,619,617),
  Defaillances_WEBSTAT=c(1444,1528,1419,1414,940,1061,1206,1285))


compar_siete_webstat_2 <- pivot_longer(compar_siete_webstat, cols = starts_with("Defaillances"), 
                          names_to = "Source", values_to = "Defaillances")

ggplot(data = compar_siete_webstat_2, aes(x = Annee, y = Defaillances, color = Source)) +
  geom_line(size = 1.2) + 
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Évolution des Défaillances par Année",
       x = "Année",
       y = "Nombre de Défaillances",
       color = "Source") +
  scale_color_manual(values = c("Defaillances_SIETE" = "steelblue", "Defaillances_WEBSTAT" = "darkorange")) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

