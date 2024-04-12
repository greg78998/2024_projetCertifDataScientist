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

DB_ori <- readRDS(paste0(path_data_vf,"/","base.rds"))
DB <- DB_ori 



### 1 - Mettre en page les variables qualitatives 

########## Variable nj

seuil_nj <- 0.1

id_nj <- data.frame(perc = round(table(DB$nj) / dim(DB)[1]*100,2))
id_nj <- id_nj %>% filter(perc.Freq > seuil_nj*100)

DB <- DB %>% 
  mutate(nj_ret = ifelse(nj %in% id_nj$perc.Var1,as.character(nj),"Autres")) %>% 
  select (-nj) %>% 
  mutate(nj_ret = as.factor(nj_ret))


########## Variable ape

seuil_ape <- 0.08

id_ape <- data.frame(perc = round(table(DB$ape) / dim(DB)[1]*100,2))
id_ape <- id_ape %>% filter(perc.Freq > seuil_ape*100)

DB <- DB %>% 
  mutate(ape_ret = ifelse(ape %in% id_ape$perc.Var1,ape,"Autres")) %>% 
  select (-ape) %>% 
  mutate(ape_ret = as.factor(ape_ret))

######## Variable age_ent

DB$ent_age_cat <- cut(DB$ent_age, breaks = c(0,3,10,20,40,max(DB$ent_age)) )
DB <- DB %>% select(-ent_age) 

####### variable region 

DB <- DB %>% select(-department) %>% mutate(region = as.factor(region))


saveRDS(DB, file = paste0(path_data_vf,"/","base_postRET.RDS"))

### 2 - Créer les polygones sur les données meteo

XXmeteo_0 <- DB %>% select(starts_with("rf_M"))
XXmeteo <- XXmeteo_0^2
colnames(XXmeteo) <- paste(colnames(XXmeteo_0),"_sq",sep="")

saveRDS(XXmeteo, file = paste0(path_data_vf,"/","base_precipitationcarre.RDS"))

### 3 - Travail sur les interactions 


sapply(DB, function(x) sum(is.na(x)))

