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


libs <- c("assertthat",
          "data.table",
          "tidymodels",
          "tidyverse")

inst_load_packages(libs)


# Chemins d'accès --------------------------------------------------------------


input_path <- "Data/s2_donnees_imputees.rds"
output_path <- "Data/s3_donnees_standardisees.rds"


# Charger les données ----------------------------------------------------------


river_dt <- readRDS(input_path)


# Gérer les valeurs aberrantes -------------------------------------------------


river_dt %>% summary()

# Voir la doc: https://www.diva-portal.org/smash/get/diva2:453424/FULLTEXT01.pdf
river_dt[, dor_cl_pva_over1000 := as.factor(dor_pc_pva > 1000)]
river_dt[, dor_pc_pva := pmin(dor_pc_pva, 100)]

# Voir la doc: https://www.nature.com/articles/ncomms13603.pdf
river_dt[,  lka_pc_cse := lka_pc_cse / 10]
river_dt[,  lka_pc_use := lka_pc_use / 10]


# Feature engineering ----------------------------------------------------------


# Combiner l'effet des barrages avec le volume hydrique
river_dt[, dam_effect := rev_mc_usu * (100 - dor_pc_pva) / 100]

# Calculer l'aire moyen (verticalement) du bassin
river_dt[, riv_cross_area_csu := riv_tc_csu / LENGTH_KM]

# Calculer la largeur du bassin
river_dt[, riv_larg_csu := ria_ha_csu / LENGTH_KM]

# Calculer les précipitations printanières (moyennes) maximum
river_dt[, pre_spring_max_csu := pmax(pre_mm_c03, pre_mm_c04, pre_mm_c05, pre_mm_c06)]

# Ajouter l'interaction entre les précipitations et la couverture de neige
river_dt[, snow_prec_csu := snw_pc_cyr * pre_spring_max_csu]


# Standardiser les variables numériques ----------------------------------------


no_boxcox <- c(  # Ces variables ne peuvent être normalisées avec une transformation Box-Cox.
   "dor_pc_pva", "wet_pc_c02", "wet_pc_c03", "wet_pc_c04", "wet_pc_u02", 
   "wet_pc_u03", "wet_pc_u04", "ire_pc_cse", "ire_pc_use", "swc_pc_cyr", 
   "swc_pc_uyr", "hdi_ix_cav", "dam_effect"
)

recette_standardisation <- 
   recipes::recipe(dis_m3_pyr ~ ., data = river_dt) %>%
   step_rm(where(is.factor)) %>%
   step_rm(c(HYRIV_ID, NEXT_DOWN, MAIN_RIV)) %>%
   step_mutate_at(where(~min(.x) < 0), fn = ~.x - min(.x)) %>%
   step_mutate_at(where(~min(.x) == 0), fn=~ .x + 1) %>%
   step_BoxCox(everything(), -!!no_boxcox, limits = c(-8, 5)) %>%
   step_normalize(everything(), -dis_m3_pyr) %>%
   step_rm(where(~var(.x) == 0)) %>%
   prep()

numeric_dt <-
   recette_standardisation %>%
   bake(river_dt) %>%
   as.data.table()

summary(numeric_dt)


# Combiner les données transformées --------------------------------------------


donnees_standardisees <- cbind(
   river_dt[, 1:3],
   river_dt %>% select(where(is.factor)),
   numeric_dt
)

assertthat::are_equal(ncol(river_dt), ncol(donnees_standardisees))


# Sommaire post-transformations
summary(donnees_standardisees)


# Sauvegarder les données transformées -----------------------------------------

   
saveRDS(donnees_standardisees, output_path, compress = "xz")
