# s2_data_standardisation.R


# Un fichier dédié à la standardisation de certaines données


# Université Laval
# Session d'hiver 2023
# Cours: STT-7335 - Méthodes d'analyse de données
# Équipe 4:
#  - Lepage, Alexandre
#  - Helaiem, Amélie
#  - Hangsin, Andrea
#  - Ouedraogo, Angelo
#  - Jocelyn Pellerin

# Développeur: Alexandre Lepage (alexandre.lepage.3@ulaval.ca)


# Librairies -------------------------------------------------------------------


library(assertthat)
library(data.table)
library(tidyverse)


# Chemins d'accès --------------------------------------------------------------


input_path <- "Data/s1_donnees_filtrees.rds"
output_path <- "Data/s2_donnees_normalisees.rds"


# Charger les données ----------------------------------------------------------


river_df <- readRDS(input_path)


# Standardiser les pourcentages ------------------------------------------------

var_pc <- names(river_df)[names(river_df) %like% "_pc_"]
summary(river_df[, sort(var_pc)])

river_dt <- as.data.table(river_df)
river_dt <- cbind(
   river_dt[, -var_pc, with = FALSE],
   river_dt[, lapply(.SD, as.double), .SDcols = var_pc]
)

river_dt[cly_pc_cav != -999, cly_pc_cav := cly_pc_cav / 100]
river_dt[cly_pc_uav != -999, cly_pc_uav := cly_pc_uav / 100]
river_dt[slt_pc_cav != -999, slt_pc_cav := slt_pc_cav / 100]
river_dt[slt_pc_uav != -999, slt_pc_uav := slt_pc_uav / 100]
river_dt[snd_pc_cav != -999, snd_pc_cav := snd_pc_cav / 100]
river_dt[snd_pc_uav != -999, snd_pc_uav := snd_pc_uav / 100]

river_dt[, dor_pc_pva := dor_pc_pva / max(dor_pc_pva)]
river_dt[, lka_pc_cse := lka_pc_cse / max(lka_pc_cse)]
river_dt[, lka_pc_use := lka_pc_use / max(lka_pc_use)]

river_dt[, for_pc_cse := for_pc_cse / 100]
river_dt[, for_pc_use := for_pc_use / 100]
river_dt[, ire_pc_cse := ire_pc_cse / 100]
river_dt[, ire_pc_use := ire_pc_use / 100]
river_dt[, kar_pc_cse := kar_pc_cse / 100]
river_dt[, kar_pc_use := kar_pc_use / 100]
river_dt[, prm_pc_cse := prm_pc_cse / 100]
river_dt[, prm_pc_use := prm_pc_use / 100]
river_dt[, snw_pc_cyr := snw_pc_cyr / 100]
river_dt[, snw_pc_uyr := snw_pc_uyr / 100]
river_dt[, swc_pc_cyr := swc_pc_cyr / 100]
river_dt[, swc_pc_uyr := swc_pc_uyr / 100]

wet_variables <- var_pc[var_pc %like% "wet_pc"]
river_dt_wet <- river_dt[
   , lapply(.SD, function(x) x / 100), .SDcols = wet_variables]

river_dt <- cbind(river_dt[,-wet_variables, with = FALSE], river_dt_wet)

summary(river_dt)


# Normaliser les grands nombres ------------------------------------------------


data_to_normalize <- river_dt %>%
   select(where(~ is.numeric(.x) && max(.x) > 1), - dis_m3_pyr)

var_to_normalize <- names(data_to_normalize)

river_dt_normalised <- data_to_normalize %>%
   mutate_all(function(x) (x - mean(x)) / sd(x))

summary(river_dt_normalised)

river_dt <- cbind(
   river_dt[, -var_to_normalize, with = FALSE],
   river_dt_normalised
)


# Vérifier qu'aucune colonne s'est perdu en chemin
assertthat::assert_that(all(dim(river_dt) == dim(river_df)))


# Sommaire post-transformations
summary(river_dt)


# Sauvegarder les données transformées -----------------------------------------

   
saveRDS(river_dt, output_path, compress = "xz")
