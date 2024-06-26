## Avant le lancement de l'application

matricule <- "N818398"

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF", sep = "")
} 
if (matricule == "X822385"){
  path_USER <- "C:/Users/X822385/Desktop/DS/Projet"
}

source(paste0(path_USER,"/pg_propre/","_before_chemins.R"))
source(paste0(path_USER,"/pg_propre/","_before_libraries.R"))


dt_placement <- readRDS(file = paste0(path_data_vf,"/","para_dt_placement.RDS"))
dt_placement_futur <- as.Date("2024-12-31")
forme_dt_ls <- c("simple","add_succ_surplus","add_succ_chocs","poly")
forme_dt  <- paste(forme_dt_ls, collapse = "_")  



ls_label <- c("Non en difficulté", "En difficulté")

choice_A <- "Choix_Majoritaire"
choice_B <- "Un_modèle_suffit"
choice_C <- "Sur_moyenne_probabilités"

# 0 | chargement des libraries  -----

# 0bis | chargement des fonctions 
source(paste0(path_USER,"/pg_propre/","X3_MeF_predictionModels.R"))
source(paste0(path_USER,"/pg_propre/","X_creation_matrix_confusion.R"))


# 1 | chargement de la table + chargement des paramètres

db_defaillance <- haven::read_sas(data = paste0(path_data_vf,"/clean_donnees2.sas7bdat"), NULL) %>%
  filter(top_defaillance == "Y") %>% 
  mutate(date2 = as.Date(date_min)) %>% 
  select(date2) %>%
  mutate(annee = year(date2),
         mois = month(date2)) %>%
  group_by(annee, mois) %>%
  summarize(nb_defaillance = n())

DB_forGraph <- readRDS(paste0(path_data_vf,"/",dt_placement,"_DB_postRET.RDS"))

# Données pour l'onglet Statistiques
DB_defaillance <- DB_forGraph %>% filter(top_defaillance ==1 )

# Comparaison des défaillances entre le fichier du SIETE et Webstat 
compar_siete_webstat <- data.frame(
  Annee=c(2016,2017,2018,2019,2020,2021,2022,2023),
  Defaillances_SIETE=c(246,235,282,418,359,553,619,617),
  Defaillances_WEBSTAT=c(1444,1528,1419,1414,940,1061,1206,1285))

compar_siete_webstat_2 <- pivot_longer(compar_siete_webstat, cols = starts_with("Defaillances"), 
                                       names_to = "Source", values_to = "Defaillances")


# Matrices de corrélation avant et après suppression de certaines var fi 
cor_matrix_avt <- readRDS(file = paste0(path_data_vf,"/","cor_matrix_avt.RDS"))
cor_matrix_aps <- readRDS(file = paste0(path_data_vf,"/","cor_matrix_aps.RDS"))


# Données pour Boxplots des var fi avant et après retraitements des obs très atypiques
DB_stats <- readRDS(paste0(path_data_vf,"/",dt_placement,"_DB_avt_stats.RDS")) # on cherche DB avant retraitements stats


# Import des table de prediction

DF_entrainement <- readRDS(paste(paste0(path_data_vf,"/",dt_placement),forme_dt,"basesPREVISION","train.RDS", sep = "_")) %>%
  mutate(Y = as.integer(Y)-1)
DF_test <- readRDS(paste(paste0(path_data_vf,"/",dt_placement),forme_dt,"basesPREVISION","test.RDS", sep = "_")) %>%
  mutate(Y = as.integer(Y)-1)

DF_demain_pred <- readRDS(paste(paste0(path_data_vf,"/",dt_placement_futur),"prediction_demain_defaillance.RDS", sep = "_")) 


metriques_pour_Shiny <- readRDS(file = paste0(path_data_vf,"/","metriques_pour_Shiny.RDS") )

lr_rate_mapping <- readRDS(file = paste0(path_data_vf,"/","lr_rate_mapping.RDS") ) 


model_results_pour_Shiny <- readRDS(file = paste0(path_data_vf,"/","model_results_pour_Shiny.RDS") )

DF_test_S <- readRDS(paste(paste0(path_data_vf,"/",dt_placement),forme_dt,"basesPREVISION","test_S.RDS", sep = "_"))

# Import du modèle XGB ajusté aux données d'apprentissage pour rechercher l'importance des variables
xgb_fit <- readRDS(file = paste0(path_pg_models_save,"/",forme_dt,"_vf_xgb_n",1,".RDS"))
model <- extract_fit_parsnip(xgb_fit)
importance_plot <- vip(model, num_features=20, geom="col", horiz=TRUE, aesthetics = list(fill="steelblue"))


# Pour les predictions de demain

region_departement <- read_excel(paste0(path_data_,"/region_departement.xlsx", sep =""))
names(region_departement) <- c("department","departement_name", "region")
region_departement$dep_num_name = paste(region_departement$department, region_departement$departement_name, sep = "-")

# On importe le dataframe
DF_demain_brut <- readRDS(paste(paste0(path_data_vf,"/",dt_placement_futur),"DB_.RDS", sep = "_")) %>%  
  left_join(region_departement %>% select(department,departement_name), by = "department") %>%
  mutate(dep_num_name = paste(department, departement_name, sep = "-"), 
         emails = paste0("email",row_number(),"@adresse.fr")) %>% 
  select(siren, nj, ape,department, dep_num_name, region, ent_age, emails) 


# Pour construire la liste des menus

demain_nj <- DF_demain_brut %>% 
  select(nj) %>% 
  distinct() %>% 
  arrange(nj)
demain_ape <- DF_demain_brut %>% 
  select(ape) %>% 
  distinct() %>% 
  arrange(ape)

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF", sep = "")
} 
if (matricule == "X822385"){
  path_USER <- "C:/Users/X822385/Desktop/DS/Projet"
}

# SIMPLE : Pas de feature engineering (sauf retraitement)
# ADD : Ajout de variables
# POLY :  polygon sur les variables météo
# Interaction : on en fait déjà implicitement avec les add, la rec_inter fonctionne mal avec ridge/lasso (à creuser mais rien de significatif à attendre)

# 0 | chargement des libraries  -----

source(paste0(path_USER,"/pg_propre/","_before_chemins.R"))
source(paste0(path_USER,"/pg_propre/","_before_libraries.R"))

source(paste0(path_USER,"/pg_propre/","X6_assemblage_table.R"))

# 1 | chargement de la table + chargement des paramètres

dt_placement <- readRDS(file = paste0(path_data_vf,"/","para_dt_placement.RDS"))
interval_month <- readRDS( file = paste0(path_data_vf,"/","para_interval_month.RDS"))
annee_nb <- readRDS( file = paste0(path_data_vf,"/","para_annee_nb.RDS"))

#Lorsqu'on veut concaténer les DB sur plusieurs années
#DB_2021 <- DB
#DB <- rbind(DB_2021,DB_2022,DB_2023)


DB <- X6_construction_base(forme_dt_ls,para_succ_nb_periode = 3) %>% 
  rename(Y = top_defaillance) # output du programme 2, comme ça pas besoin de le relancer

# 2 | Mise en place du training + test
need <- TRUE    # Pour assurer la reproductivité 
if (need){
  set.seed(1234)
  
  data_split <- initial_split(DB, strata = Y, prop = 0.8)
  
  training <- training(data_split) # data frame qui permet de faire le premier découpage
  test_set <- testing(data_split) # extraire le test set
  
  training_split <- initial_split(training, strata = Y, prop = 0.8)
  
  train_set <- training(training_split)
  eval_set <- testing(training_split)
}

source(paste0(path_USER,"/pg_propre/","prog_graph.R"))

