
  
  
  Graph_fi_B1 <- ggplot(DB_stats, aes(x = "", y = effectifs_moy_2014_2021)) + geom_boxplot() + labs(title = "effectifs_moy_2014_2021 avant retraitement", y = "effectifs_moy_2014_2021") + theme(axis.text.x = element_blank())
  Graph_fi_B2 <-ggplot(DB, aes(x = "", y = effectifs_moy_2014_2021)) + geom_boxplot() + labs(title = "effectifs_moy_2014_2021 après retraitement", y = "effectifs_moy_2014_2021") + theme(axis.text.x = element_blank())
  
  Graph_fi_C1 <-ggplot(DB_stats, aes(x = "", y = Besoin_en_FDR_moy)) + geom_boxplot() + labs(title = "Besoin en fond de roulement (DFDR) moyen avant retraitement", y = "Besoin_en_FDR_moy") + theme(axis.text.x = element_blank())
  Graph_fi_C2 <-ggplot(DB, aes(x = "", y = Besoin_en_FDR_moy)) + geom_boxplot() + labs(title = "Besoin en fond de roulement (DFDR) moyen après retraitement", y = "Besoin_en_FDR_moy") + theme(axis.text.x = element_blank())
  
  Graph_fi_D1 <- ggplot(DB_stats, aes(x = "", y = Total_actif_circulant_moy)) + geom_boxplot() + labs(title = "Total de l'actif circulant moyen avant retraitement", y = "Total_actif_circulant_moy") + theme(axis.text.x = element_blank())
  Graph_fi_D2 <-ggplot(DB, aes(x = "", y = Total_actif_circulant_moy)) + geom_boxplot() + labs(title = "Total de l'actif circulant moyen après retraitement", y = "Total_actif_circulant_moy") + theme(axis.text.x = element_blank())
  
  Graph_fi_E1 <-ggplot(DB_stats, aes(x = "", y = Dettes_four_et_comptes_ratt_moy)) + geom_boxplot() + labs(title = "Dettes fournisseurs et comptes rattachées moyen avant retraitement", y = "Dettes_four_et_comptes_ratt_moy") + theme(axis.text.x = element_blank())
  Graph_fi_E2 <- ggplot(DB, aes(x = "", y = Dettes_four_et_comptes_ratt_moy)) + geom_boxplot() + labs(title = "Dettes fournisseurs et comptes rattachées moyen après retraitement", y = "Dettes_four_et_comptes_ratt_moy") + theme(axis.text.x = element_blank())
  
  Graph_fi_F1 <- ggplot(DB_stats, aes(x = "", y = Total_dettes_moy)) + geom_boxplot() + labs(title = "Total des dettes avant retraitement", y = "Total_dettes_moy") + theme(axis.text.x = element_blank())
  Graph_fi_F2 <- ggplot(DB, aes(x = "", y = Total_dettes_moy)) + geom_boxplot() + labs(title = "Total des dettes après retraitement", y = "Total_dettes_moy") + theme(axis.text.x = element_blank())
  
  Graph_fi_H1 <-ggplot(DB_stats, aes(x = "", y = Resultat_comptable_moy)) + geom_boxplot() + labs(title = "Résultat comptable (bénéfice ou perte) avant retraitement", y = "Resultat_comptable_moy") + theme(axis.text.x = element_blank())
  Graph_fi_H2 <- ggplot(DB, aes(x = "", y = Resultat_comptable_moy)) + geom_boxplot() + labs(title = "Résultat comptable (bénéfice ou perte) après retraitement", y = "Resultat_comptable_moy") + theme(axis.text.x = element_blank())
  

