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

libs <- c("data.table",
          "ranger",
          "tidymodels")

inst_load_packages(libs)

# Chemins d'accès --------------------------------------------------------------

input_path <- "Data/s2_donnees_imputees.rds"
output_path <- "Data/s5_random_forest.rds"

# Charger le jeu de données ----------------------------------------------------

river_dt <- readRDS(input_path)
stopifnot("data.table" %in% class(river_dt))

# Séparer le jeu ---------------------------------------------------------------

split_dt <- initial_split(river_dt, prop = 0.8)
train_dt <- training(split_dt)
test_dt  <- testing(split_dt)

# Sélection des hyperparamètres ------------------------------------------------

# Initaliser le germe
set.seed(7335)

# Initialiser notre modèle
tune_spec <- rand_forest(
   mtry = tune(),
   trees = tune(),
   min_n = tune()
) %>% 
   set_engine(
      engine = "ranger"
   ) %>% 
   set_mode(
      mode = "regression"
   )

# Établir les combinaisons d'hyperparamètres à tester 
forest_grid <- grid_regular(
   mtry(range = c(1L, 15L)),
   trees(range = c(100L, 500L)),
   min_n(range = c(100L, 1000L))
)

# Séparer notre jeu de données en plis pour la validation croisée
train_folds <- vfold_cv(train_dt, v = 5)

# Créer le workflow
forest_wf <- workflow() %>%
   add_model(
      spec = tune_spec
   ) %>%
   add_formula(
      formula = dis_m3_pyr ~ . - HYRIV_ID - NEXT_DOWN - MAIN_RIV - LENGTH_KM
   )

# Faire la validation croisée
forest_res <-   
   forest_wf %>% 
   tune_grid(
      resamples = train_folds,
      grid = forest_grid
   )

# Modèle final -----------------------------------------------------------------

# Est-ce qu'on priorise le EQM? Ou l'EQA? Est-ce qu'on a beaucoup de valeurs 
# extrêmes que l'on voudrait éviter qu'ils aillent trop d'influence?

# Sélectionner le meilleur modèle basé sur la mesure de performance choisie
best_forest <- forest_res %>%
   select_best(metric = "rmse")
