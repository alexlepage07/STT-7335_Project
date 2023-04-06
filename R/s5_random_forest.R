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
          "tidymodels",
          "vip",
          "doParallel")

inst_load_packages(libs)


# Chemins d'accès --------------------------------------------------------------


input_path <- "Data/s4_donnees_sans_multicol.rds"
output_path <- "Data/s5_donnees_rf_var_sel.rds"
output_path_obj <- "inst/s5_random_forest.rds"


# Charger le jeu de données ----------------------------------------------------


river_dt <- readRDS(input_path)
stopifnot("data.table" %in% class(river_dt))


# Séparer le jeu ---------------------------------------------------------------


set.seed(7335)
split_dt <- initial_split(river_dt, prop = 0.8)
train_dt <- training(split_dt)
test_dt  <- testing(split_dt)


# Sélection des hyperparamètres ------------------------------------------------


# Initialiser notre modèle
tune_spec <- rand_forest(
   mtry = tune(),
   trees = tune(),
   min_n = tune()
) %>% 
   set_engine(
      engine = "ranger",
      importance = "permutation",
      scale.permutation.importance = TRUE
   ) %>% 
   set_mode(
      mode = "regression"
   )

# Établir les combinaisons d'hyperparamètres à tester 
forest_grid <- grid_regular(
   mtry(range = c(5L)),
   trees(range = c(250L)),
   min_n(range = c(500L))
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
registerDoParallel()

forest_res <-   
   forest_wf %>% 
   tune_grid(
      resamples = train_folds,
      grid = forest_grid
   )

# Modèle final -----------------------------------------------------------------

# Sélectionner le meilleur modèle basé sur la mesure de performance choisie
best_forest <- forest_res %>%
   select_best(metric = "rmse")

# Mettre à jour le workflow avec les hyperparamètres venant maximiser la 
# métrique de performance visée
final_wf <- forest_wf %>% 
   finalize_workflow(
      best_forest
   )

# Faire le dernier entraînement
final_fit <- 
   final_wf %>%
   fit(train_dt)

# Obtenir l'importance des variables
final_model <- 
   final_fit %>%
   extract_fit_parsnip()

# Retrait de variables ---------------------------------------------------------

nb <- 20

vars <- names(sort(final_model$fit$variable.importance, decreasing = TRUE)[1:nb])

river_dt <- river_dt[, c(vars,
                         "HYRIV_ID",
                         "NEXT_DOWN",
                         "MAIN_RIV",
                         "LENGTH_KM",
                         "dis_m3_pyr"), with = FALSE]


# Sauvegarder les données résultantes ------------------------------------------


saveRDS(river_dt, output_path, compress = "xz")


# Sauvegarder le modèle résultant ----------------------------------------------


saveRDS(final_model, output_path_model, compress = "xz")




