# s8_mixed_effect.R


# Un fichier pour créer le modèle final


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

# Source : https://multilevelmod.tidymodels.org/articles/multilevelmod.html

# Fonctions --------------------------------------------------------------------


source("./R/utils.R")


# Librairies -------------------------------------------------------------------


libs <- c("data.table",
          "lme4",
          "tidymodels")

inst_load_packages(libs)


# Chemins d'accès --------------------------------------------------------------


input_path <- "Data/s7_donnees_lasso_var_sel.rds"
output_path_obj <- "inst/s7_mixed_effect.rds"
lasso_model_path <- "inst/s7_lasso.rds"


# Charger le jeu de données ----------------------------------------------------


river_dt <- readRDS(input_path)
lasso_model <- readRDS(lasso_model_path)
stopifnot("data.table" %in% class(river_dt))


# Séparer le jeu ---------------------------------------------------------------


set.seed(7335)
split_dt <- initial_split(river_dt, prop = 0.85)
train_dt <- training(split_dt) 
split_train_dt <- initial_split(train_dt, prop = 1 - 0.15/0.85)
train_dt <- training(split_train_dt)
val_dt  <- testing(split_train_dt)
test_dt  <- testing(split_dt)


# Modèle -----------------------------------------------------------------------

## Modèle 1

# L'objectif est de déterminer entre les différentes groupes ayant la même 
# rivière en aval le débit est différent en moyenne ou/et si pour certaines 
# variables explicatives la relation entre le débit et une de celles-ci la 
# relation est différente.

res_0 <- lapply(
   names(train_dt), 
   function(x) {
      graph_res_vs_var(train_dt, lasso_model$res, x)
   }
)

# Modèle avec une ordonnée à l'origine différent par MAIN_RIVER

lmm_1 <- lmer(
   dis_m3_pyr ~ 
      lka_pc_use +
      ria_ha_csu + 
      riv_tc_usu + 
      gwt_cm_cav + 
      slp_dg_cav + 
      swc_pc_uyr + 
      kar_pc_cse + 
      riv_larg_csu + 
      pre_spring_max_csu + 
      wet_pc_u01 + 
      cly_pc_uav +
      glc_cl_cmj + 
      wet_cl_cmj + 
      lit_cl_cmj + 
      I(sgr_dk_rav)^2 +
      I(for_pc_use)^2 +
      I(slp_dg_uav)^2 +
      riv_tc_usu:ria_ha_csu  + 
      (1 | MAIN_RIV),
   data = train_dt,
   REML = TRUE
)

## Modèle 2

res_1 <- lapply(
   names(train_dt), 
   function(x) {
      graph_res_vs_var(train_dt, residuals(lmm_1), x)
   }
)

names(res_1) <- names(train_dt)

# Modèle 1 + une pente différente pour la variable ria_ha_csu

lmm_2 <- lmer(
   dis_m3_pyr ~ 
      lka_pc_use +
      ria_ha_csu + 
      riv_tc_usu + 
      gwt_cm_cav + 
      slp_dg_cav + 
      swc_pc_uyr + 
      kar_pc_cse + 
      riv_larg_csu + 
      pre_spring_max_csu + 
      wet_pc_u01 + 
      cly_pc_uav +
      glc_cl_cmj + 
      wet_cl_cmj + 
      lit_cl_cmj + 
      I(sgr_dk_rav)^2 +
      I(for_pc_use)^2 +
      I(slp_dg_uav)^2 +
      riv_tc_usu:ria_ha_csu  + 
      (ria_ha_csu | MAIN_RIV),
   data = train_dt,
   REML = TRUE
)

## Modèle 3

# Modèle 1 + une pente différente pour la variable ria_ha_csu (pente et ordonnée
# non-corrélées)

lmm_3 <- lmer(
   dis_m3_pyr ~ 
      lka_pc_use +
      ria_ha_csu + 
      riv_tc_usu + 
      gwt_cm_cav + 
      slp_dg_cav + 
      swc_pc_uyr + 
      kar_pc_cse + 
      riv_larg_csu + 
      pre_spring_max_csu + 
      wet_pc_u01 + 
      cly_pc_uav +
      glc_cl_cmj + 
      wet_cl_cmj + 
      lit_cl_cmj + 
      I(sgr_dk_rav)^2 +
      I(for_pc_use)^2 +
      I(slp_dg_uav)^2 +
      riv_tc_usu:ria_ha_csu  + 
      (ria_ha_csu || MAIN_RIV),
   data = train_dt,
   REML = TRUE
)

## Modèle 4

res_2 <- lapply(
   names(train_dt), 
   function(x) {
      graph_res_vs_var(train_dt, residuals(lmm_2), x)
   }
)

names(res_2) <- names(train_dt)

# Modèle 2 + une pente différente pour la variable lka_pc_use

lmm_4 <- lmer(
   dis_m3_pyr ~ 
      lka_pc_use +
      ria_ha_csu + 
      riv_tc_usu + 
      gwt_cm_cav + 
      slp_dg_cav + 
      swc_pc_uyr + 
      kar_pc_cse + 
      riv_larg_csu + 
      pre_spring_max_csu + 
      wet_pc_u01 + 
      cly_pc_uav +
      glc_cl_cmj + 
      wet_cl_cmj + 
      lit_cl_cmj + 
      I(sgr_dk_rav)^2 +
      I(for_pc_use)^2 +
      I(slp_dg_uav)^2 +
      riv_tc_usu:ria_ha_csu  + 
      (ria_ha_csu + lka_pc_use | MAIN_RIV),
   data = train_dt,
   REML = TRUE
)

## Modèle 4

# Modèle 2 + une pente différente pour la variable lka_pc_use (pente et ordonnée
# non-corrélées)

lmm_5 <- lmer(
   dis_m3_pyr ~ 
      lka_pc_use +
      ria_ha_csu + 
      riv_tc_usu + 
      gwt_cm_cav + 
      slp_dg_cav + 
      swc_pc_uyr + 
      kar_pc_cse + 
      riv_larg_csu + 
      pre_spring_max_csu + 
      wet_pc_u01 + 
      cly_pc_uav +
      glc_cl_cmj + 
      wet_cl_cmj + 
      lit_cl_cmj + 
      I(sgr_dk_rav)^2 +
      I(for_pc_use)^2 +
      I(slp_dg_uav)^2 +
      riv_tc_usu:ria_ha_csu  + 
      (ria_ha_csu + lka_pc_use || MAIN_RIV),
   data = train_dt,
   REML = TRUE
)

