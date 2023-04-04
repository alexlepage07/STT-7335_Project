# s5_random_forest.R


# Un fichier dédié à sélectionner les variables importantes à partir d'un forêt
# aléatoire.


# Université Laval
# Session d'hiver 2023
# Cours: STT-7335 - Méthodes d'analyse de données
# Équipe 4:
#  - Lepage, Alexandre
#  - Helaiem, Amélie
#  - Hangsin, Andrea
#  - Ouedraogo, Angelo
#  - Pellerin, Jocelyn

# Développeur: Andréa Hangsin (andrea.hangsin.1@ulaval.ca)

# Fonctions --------------------------------------------------------------------


source("./R/utils.R")


# Librairies -------------------------------------------------------------------


libs <- c("ggplot2")

inst_load_packages(libs)


# Chemins d'accès --------------------------------------------------------------


input_path <- "Data/s3_donnees_standardisees.rds"
output_path <- "Data/s4_donnees_sans_multicol.rds"


# Charger le jeu de données ----------------------------------------------------


river_dt <- readRDS(input_path)
stopifnot("data.table" %in% class(river_dt))


# Établir la matrice de corrélation --------------------------------------------


cor_mat <- calc_matrix_cor(river_dt[, -c("HYRIV_ID",
                                         "NEXT_DOWN",
                                         "MAIN_RIV",
                                         "LENGTH_KM",
                                         "dis_m3_pyr")], 
                           remove_dupl = TRUE)

cor_mat[value > 0.95 & Var1 != Var2]


# Gérer les variables très corrélées -------------------------------------------


# Retrait de certaines variables fortement corrélées
river_dt <- river_dt[, -c("riv_tc_usu",
                          "pre_mm_uyr",
                          "pre_mm_c05",
                          "pre_mm_c04",
                          "snw_pc_uyr",
                          "prm_pc_use",
                          "swc_pc_uyr")]


# Sauvegarder les données résultantes ------------------------------------------


saveRDS(river_dt, output_path, compress = "xz")






