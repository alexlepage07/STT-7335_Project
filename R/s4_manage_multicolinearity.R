# s4_manage_multicolinearity.R


# Un fichier dédié à faire un diagnostique de multicolinéarité


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


libs <- "olsrr"

inst_load_packages(libs)


# Chemins d'accès --------------------------------------------------------------


input_path <- "Data/s3_donnees_standardisees.rds"
output_path <- "Data/s4_donnees_sans_multicol.rds"
output_path_ind_cond <- "inst/s4_ind_cond.rds"


# Charger le jeu de données ----------------------------------------------------


river_dt <- readRDS(input_path)
ini_dt <- copy(river_dt)
stopifnot("data.table" %in% class(river_dt))


# Retrait des variables nominales ----------------------------------------------

cat_var <- sapply(river_dt, function(x) !is.numeric(x))
cat_var <- names(cat_var)[which(cat_var)]
river_dt <- river_dt[, -cat_var, with = FALSE]


# Diagnostique de multicolinéarité ---------------------------------------------


# Première itération
mod <- lm(
   formula = dis_m3_pyr ~ . - HYRIV_ID - NEXT_DOWN - MAIN_RIV - LENGTH_KM, 
   data = river_dt
)

ind_cond_1 <- setDT(ols_eigen_cindex(mod))
table_ind_cond(ind_cond_1)

river_dt <- 
   river_dt[, -"ria_ha_usu", with = FALSE]

removed_vars <- "ria_ha_usu"

# Deuxième itération
mod <- lm(
   formula = dis_m3_pyr ~ . - HYRIV_ID - NEXT_DOWN - MAIN_RIV - LENGTH_KM, 
   data = river_dt
)

ind_cond_2 <- setDT(ols_eigen_cindex(mod))
table_ind_cond(ind_cond_2)

river_dt <- 
   river_dt[, -"pre_mm_cyr", with = FALSE]

removed_vars <- c("pre_mm_cyr", removed_vars)

# Troisième itération
mod <- lm(
   formula = dis_m3_pyr ~ . - HYRIV_ID - NEXT_DOWN - MAIN_RIV - LENGTH_KM, 
   data = river_dt
)

ind_cond_3 <- setDT(ols_eigen_cindex(mod))
table_ind_cond(ind_cond_3)

river_dt <- 
   river_dt[, -"riv_tc_csu", with = FALSE]

removed_vars <- c("riv_tc_csu", removed_vars)

# Quatrième itération
mod <- lm(
   formula = dis_m3_pyr ~ . - HYRIV_ID - NEXT_DOWN - MAIN_RIV - LENGTH_KM, 
   data = river_dt
)

ind_cond_4 <- setDT(ols_eigen_cindex(mod))
table_ind_cond(ind_cond_4)

river_dt <- 
   river_dt[, -"prm_pc_use", with = FALSE]

removed_vars <- c("prm_pc_use", removed_vars)

# Cinquième itération
mod <- lm(
   formula = dis_m3_pyr ~ . - HYRIV_ID - NEXT_DOWN - MAIN_RIV - LENGTH_KM, 
   data = river_dt
)

ind_cond_5 <- setDT(ols_eigen_cindex(mod))
table_ind_cond(ind_cond_5)

# Modification des données initiales -------------------------------------------


ini_dt <- ini_dt[, -removed_vars, with = FALSE]


# Sauvegarder les objets résultants --------------------------------------------


ind_cond <- list(
   ind_cond_1 = ind_cond_1, 
   ind_cond_2 = ind_cond_2, 
   ind_cond_3 = ind_cond_3, 
   ind_cond_4 = ind_cond_4, 
   ind_cond_5 = ind_cond_5, 
   removed_vars = removed_vars
)

saveRDS(ind_cond, output_path_ind_cond, compress = "xz")


# Sauvegarder les données résultantes ------------------------------------------


saveRDS(ini_dt, output_path, compress = "xz")






