# s5_random_forest.R


# Un fichier dédié à sélectionner les variables à partir d'un modèle Lasso


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


libs <- c("data.table",
          "tidymodels",
          "glmnet")

inst_load_packages(libs)


# Chemins d'accès --------------------------------------------------------------


input_path <- "Data/s6_donnees_orthogonalisees.rds"
output_path <- "Data/s7_donnees_lasso_var_sel.rds"
output_path_obj <- "inst/s7_lasso.rds"


# Charger le jeu de données ----------------------------------------------------


river_dt <- readRDS(input_path)
stopifnot("data.table" %in% class(river_dt))


# Séparer le jeu ---------------------------------------------------------------


set.seed(7335)
split_dt <- initial_split(river_dt, prop = 0.8)
train_dt <- training(split_dt)
test_dt  <- testing(split_dt)


# Sélection des hyperparamètres ------------------------------------------------




# Sauvegarder les données résultantes ------------------------------------------


saveRDS(river_dt, output_path, compress = "xz")


# Sauvegarder le modèle résultant ----------------------------------------------


saveRDS(final_model, output_path_model, compress = "xz")




