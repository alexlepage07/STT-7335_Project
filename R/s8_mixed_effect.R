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
          "lme4")

inst_load_packages(libs)


# Chemins d'accès --------------------------------------------------------------


input_path <- "Data/s7_donnees_lasso_var_sel.rds"
output_path_obj <- "inst/s7_mixed_effect.rds"


# Charger le jeu de données ----------------------------------------------------


river_dt <- readRDS(input_path)
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

lmm <- lmer(
   dis_m3_pyr ~ . + riv_tc_usu:ria_ha_csu + (1 | MAIN_RIV),
   data = train_dt[, -c("HYRIV_ID", 
                        "NEXT_DOWN",
                        "LENGTH_KM")],
   REML = TRUE
)

