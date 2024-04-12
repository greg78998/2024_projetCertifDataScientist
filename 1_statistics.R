# STASTISTIQUES 

matricule <- "N818398"

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF", sep = "")
} 
if (matricule == ""){
  path_USER <- ""
}

# 0 | chargement des libraries  -----

source(paste0(path_USER,"/pg_propre/","_before_chemins.R"))
source(paste0(path_USER,"/pg_propre/","_before_libraries.R"))

# 1 | chargement de la table + chargement des paramètres

DB <- readRDS(paste0(path_data_vf,"/","base.rds"))

dt_placement <- readRDS(file = paste0(path_data_vf,"/","para_dt_placement.RDS"))
interval_month <- readRDS( file = paste0(path_data_vf,"/","para_interval_month.RDS"))
annee_nb <- readRDS( file = paste0(path_data_vf,"/","para_annee_nb.RDS"))

# 2 | gestion des valeurs manquants

sapply(DB, function(x) sum(is.na(x)))

# 3 | grapher les données 

graph_chirps <- DB %>% 
  select(siren, top_defaillance, starts_with("rf_")) %>%
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
  geom_jitter(data=draw_graph,aes(x = desc(per_ref2), y = value)) +
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

