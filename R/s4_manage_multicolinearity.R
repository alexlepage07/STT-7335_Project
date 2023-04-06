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


libs <- c("ggplot2",
          "olsrr")

inst_load_packages(libs)


# Chemins d'accès --------------------------------------------------------------


input_path <- "Data/s3_donnees_standardisees.rds"
output_path <- "Data/s4_donnees_sans_multicol.rds"
output_path_vif <- "inst/s4_vif.rds"
output_path_ind_cond <- "inst/s4_ind_cond.rds"


# Charger le jeu de données ----------------------------------------------------


river_dt <- readRDS(input_path)
ini_dt <- copy(river_dt)
stopifnot("data.table" %in% class(river_dt))


# Retrait des variables nominales ----------------------------------------------

cat_var <- sapply(river_dt, function(x) !is.numeric(x))
cat_var <- names(cat_var)[which(cat_var)]
river_dt <- river_dt[, -cat_var, with = FALSE]

# Retrait de variable parfaitement corrélées -----------------------------------


cor_mat <- calc_matrix_cor(river_dt[, -c("HYRIV_ID",
                                         "NEXT_DOWN",
                                         "MAIN_RIV",
                                         "LENGTH_KM",
                                         "dis_m3_pyr")], 
                           remove_dupl = TRUE)

cor_mat[value > 0.97 & Var1 != Var2]

removed_vars <- c("riv_tc_usu",
                  "pre_mm_uyr",
                  "pre_spring_max_csu",
                  "snw_pc_uyr",
                  "prm_pc_use",
                  "swc_pc_uyr")

river_dt <- river_dt[, -removed_vars, with = FALSE]


# Diagnostique de multicolinéarité ---------------------------------------------

# Première itération
mod <- lm(
   formula = dis_m3_pyr ~ . - HYRIV_ID - NEXT_DOWN - MAIN_RIV - LENGTH_KM, 
   data = river_dt
)

vif_1 <- setDT(ols_vif_tol(mod))
table_vif(vif_1)

ind_cond_1 <- setDT(ols_eigen_cindex(mod))
table_ind_cond(ind_cond_1)

removed_vars <- c(removed_vars,
                  "ria_ha_csu")

river_dt <- 
   river_dt[, -names(river_dt)[removed_vars %in% names(river_dt)], with = FALSE] 

# Deuxième itération
mod <- lm(
   formula = dis_m3_pyr ~ . - HYRIV_ID - NEXT_DOWN - MAIN_RIV - LENGTH_KM, 
   data = river_dt
)

vif_2 <- setDT(ols_vif_tol(mod))
table_vif(vif_2)

ind_cond_2 <- setDT(ols_eigen_cindex(mod))
table_ind_cond(ind_cond_2)

river_dt <- river_dt[, -c("riv_cross_area_csu",
                          "riv_larg_csu")]

# Troisième itération
mod <- lm(
   formula = dis_m3_pyr ~ . - HYRIV_ID - NEXT_DOWN - MAIN_RIV - LENGTH_KM, 
   data = river_dt
)

vif_3 <- setDT(ols_vif_tol(mod))
table_vif(vif_3)

ind_cond_3  <- setDT(ols_eigen_cindex(mod))
table_ind_cond(ind_cond_3)

river_dt <- river_dt[, -c("pre_mm_cyr")]

# Quatrième itération
mod <- lm(
   formula = dis_m3_pyr ~ . - HYRIV_ID - NEXT_DOWN - MAIN_RIV - LENGTH_KM, 
   data = river_dt
)

vif_4 <- setDT(ols_vif_tol(mod))
table_vif(vif_4)

ind_cond_4  <- setDT(ols_eigen_cindex(mod))
table_ind_cond(ind_cond_4)

river_dt <- river_dt[, -c("pre_mm_cyr")]

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


saveRDS(vif, output_path_obj, compress = "xz")


# Sauvegarder les données résultantes ------------------------------------------


saveRDS(river_dt, output_path, compress = "xz")






