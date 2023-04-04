# s4_pca.R


# Un fichier dédié à la réalisation de l'analyse en composantes principales
# dans un but de sélection de variables.


# Université Laval
# Session d'hiver 2023
# Cours: STT-7335 - Méthodes d'analyse de données
# Équipe 4:
#  - Lepage, Alexandre
#  - Helaiem, Amélie
#  - Hangsin, Andrea
#  - Ouedraogo, Angelo
#  - Pellerin, Jocelyn

# Développeur: Amélie Helaiem (AMHEL3@ulaval.ca) et Andréa Hangsin 
# (andrea.hangsin.1@ulaval.ca)


# Fonctions --------------------------------------------------------------------


source("./R/utils.R")


# Librairies -------------------------------------------------------------------


libs <- c("FactoMineR")

inst_load_packages(libs)


# Chemins d'accès --------------------------------------------------------------


input_path <- "Data/s5_donnees_rf_var_sel.rds"
output_path <- "Data/s6_donnees_orthogonalisees.rds"
output_path_obj <- "inst/s6_pca.rds"


# Charger les données ----------------------------------------------------------


river_dt <- as.data.table(base::readRDS(input_path))


# Préparation du jeu de données ------------------------------------------------

# Pour éviter les modifications par référence
ini_dt <- copy(river_dt)

# On retire les variables catégoriques, les variables d'ID's et la variable 
# réponse
cat_var <- c("glc_cl_cmj", 
             "wet_cl_cmj", 
             "fmh_cl_cmj", 
             "lit_cl_cmj")

id_var <- c("HYRIV_ID",
            "NEXT_DOWN",
            "MAIN_RIV",
            "LENGTH_KM")

river_dt[, c(cat_var, id_var)] <- list(NULL)

# On normalise la variable de débit
river_dt$dis_m3_pyr <- 
  (river_dt$dis_m3_pyr - mean(river_dt$dis_m3_pyr)) / sd(river_dt$dis_m3_pyr)


# PCA --------------------------------------------------------------------------

w <- which(names(river_dt) == "dis_m3_pyr")

# Réalisation du PCA
pca <- PCA(X = river_dt, 
           quanti.sup = w)


# Sauvegarder l'objet résultant ------------------------------------------------


saveRDS(pca, output_path_obj)


# Sauvegarder les données résultantes ------------------------------------------


saveRDS(river_dt, output_path, compress = "xz")
