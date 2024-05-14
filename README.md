# 2024_projetCertifDataScientist
Haniquaut & Migeon 

Dans le cadre de la certification DataScientist, nous proposons un projet qui cherche à prédire les défaillances des entreprises agricoles en intégrant des données météo (précipitations). 

## Base de données.
- Redressement et liquidation judiciaire (source tribunaux de commerce)
- Données agricoles (source INSEE)
- Données chirps

  
## Description des programmes :

### Programmes récurrents
- _before_libraries : différentes librairies
- _before_chemins : différents chemins utilisés

### Data engineering et modélisation
- Programme 0 : création et construction de la première table
- Programme 1 : statistiques exploratoires
- Programme 2 : data engineering (retraitement, création de base complémentaires)
- Programme 3 : validation croisée
- Programme 4 : estimation des modèles finaux
- Programme 5 : prédiction pour demain
  
### Description des différentes fonctions : 
- **X_creation_matrix_confusion :**  construction d'une matrice de confusion + mise en dessin de la matrice

Utilisée dans l'application
- **X1_MeF_CHIRPS :** mise en forme des données de précipitation chirps

Utilisée dans le programme 0 et le programme 5
- **X2_MeF_AGRFIN :** mise en forme des données agricoles + défaillances

Utilisée dans le programme 0 et le programme 5
- **X3_MeF_predictionModels :** un fonction que l'on utilise dans l'application pour créer des variables supplémentaires (top_vote_majo, top_vote_positif, top_moyenne_prob)

Utilisé dans l'application via les fonctions de **X_creation_matrix_confusion**
- **X5_like_recipees :** des traitements de data engineering - creation d'une catégorie "autres", découpage de variables, mise en facteur

Utilisée dans les programmes 3,4,5
- **X6_assemblage_table :** une fonction qui permet de choisir en fonction des différentes options, la forme que va prendre les données. Plusieurs possibilités : simple (ou les données brutes modulo des petits retraitements), poly (ou les carrés des données de précipitations), add_surplus (il est tombé plus d'eau sur les derniers trois mois), add_extreme (cumul de chocs extrêmes)

Utilisée dans les programmes 3,4,5

### Application rshiny 
- _pres : programme qui récupère les outputs 
- xxx
- ui : user interface
- serveur : permet les calculs

### Restants à faire 
- Statistiques :
- --> rajouter un deux graphiques avec les principales variables JM ggplot() + facetwrap (données financières, ape, catégorie juridique)
- --> nombre de défaillances
- --> exploitation des données météo
- --> 
- Choix de modèles
- --> GH : faire tourner la validation croisée
- --> JM : rajout d'une colonne avec la forme des données (simple, simple + poly...)
  Calibrage :
  --> Rien à faire
  Résultats :
  --> GH  : Rajout un onglet pour explorer les faux négatifs (train / test)
  --> Courbe de densité
  En difficultés demain :
  --> rajouter les libellés des cj, ape
