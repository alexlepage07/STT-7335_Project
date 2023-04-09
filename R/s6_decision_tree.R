# s6_decision_tree.R


# Un fichier dédié à trouver des interactions possibles


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

# Source : https://juliasilge.com/blog/lasso-the-office/

# Fonctions --------------------------------------------------------------------


source("./R/utils.R")


# Librairies -------------------------------------------------------------------


libs <- c("data.table",
          "tidymodels",
          "rpart",
          "rpart.plot")

inst_load_packages(libs)


# Chemins d'accès --------------------------------------------------------------


input_path <- "Data/s5_donnees_rf_var_sel.rds"
output_path_obj <- "inst/s6_decision_tree.rds"


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


# Sélection des hyperparamètres ------------------------------------------------


# Initialiser notre modèle
tune_spec <- decision_tree(
   # Nous fixons à 5 pour que ce soit relativement interprétable et possible
   # de sélectionner des interactions possibles.
   tree_depth = 5,
   cost_complexity = tune(),
   min_n = tune()
) %>% 
   set_engine(
      engine = "rpart"
   ) %>% 
   set_mode(
      mode = "regression"
   )

# Établir les valeurs de pénalités à tester
tree_grid <- grid_regular(
   cost_complexity(),
   min_n(range = c(100L, 1000L))
)

# Séparer notre jeu de données en plis pour la validation croisée
train_folds <- vfold_cv(train_dt, v = 3)

# Créer le workflow
tree_wf <- workflow() %>%
   add_model(
      spec = tune_spec
   ) %>%
   add_formula(
      formula = dis_m3_pyr ~ . - HYRIV_ID - NEXT_DOWN - MAIN_RIV - LENGTH_KM
   )

# Faire la validation croisée
tree_res <-   
   tree_wf %>% 
   tune_grid(
      resamples = train_folds,
      grid = tree_grid
   )


# Modèle final -----------------------------------------------------------------

# Sélectionner le meilleur modèle basé sur la mesure de performance choisie
best_tree <- tree_res %>%
   select_best(metric = "rmse")

# Mettre à jour le workflow avec les hyperparamètres venant maximiser la 
# métrique de performance visée
final_wf <- tree_wf %>% 
   finalize_workflow(
      best_tree
   )

# Faire le dernier entraînement
final_fit <- 
   final_wf %>%
   last_fit(split_train_dt)

# Obtenir l'importance des variables
final_model <- 
   final_fit %>%
   extract_fit_parsnip()

# Obtenir le rmse sur le jeu d'entraînement
rmse_train <- final_model %>% 
   augment(training(split_train_dt)) %>% 
   rmse(dis_m3_pyr, .pred)

