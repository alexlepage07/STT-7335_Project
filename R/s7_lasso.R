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

# Source : https://juliasilge.com/blog/lasso-the-office/

# Fonctions --------------------------------------------------------------------


source("./R/utils.R")


# Librairies -------------------------------------------------------------------


libs <- c("data.table",
          "tidymodels",
          "glmnet",
          "moments",
          "MASS",
          "doParallel")

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


# Choix de la distribution -----------------------------------------------------


# Loi normale?
shapiro.test(
   x = river_dt$dis_m3_pyr[sample(
      x = 1:length(river_dt$dis_m3_pyr), 
      size = 5000
      )]
)

skewness(river_dt$dis_m3_pyr)
kurtosis(river_dt$dis_m3_pyr)

graph_qqplot(river_dt$dis_m3_pyr)
graph_density(numeric_dt, "dis_m3_pyr")


# Sélection des hyperparamètres ------------------------------------------------


# Initialiser notre modèle
tune_spec <- linear_reg(
   penalty = tune(),
   # Lasso
   mixture = 1
) %>% 
   set_engine(
      engine = "glmnet"
   ) %>% 
   set_mode(
      mode = "regression"
   )

# Établir les valeurs de pénalités à tester
lasso_grid <- grid_regular(
   penalty(),
   levels = 25
)

# Séparer notre jeu de données en plis pour la validation croisée
train_folds <- vfold_cv(train_dt, v = 5)

# Créer le workflow
lasso_wf <- workflow() %>%
   add_model(
      spec = tune_spec
   ) %>%
   add_formula(
      formula = dis_m3_pyr ~ . - HYRIV_ID - NEXT_DOWN - MAIN_RIV - LENGTH_KM
   )

# Faire la validation croisée
registerDoParallel()

lasso_res <-   
   lasso_wf %>% 
   tune_grid(
      resamples = train_folds,
      grid = lasso_grid
   )


# Modèle final -----------------------------------------------------------------

# Sélectionner le meilleur modèle basé sur la mesure de performance choisie
best_lasso <- lasso_res %>%
   select_best(metric = "rmse")

# Mettre à jour le workflow avec les hyperparamètres venant maximiser la 
# métrique de performance visée
final_wf <- lasso_wf %>% 
   finalize_workflow(
      best_lasso
   )

# Faire le dernier entraînement
final_fit <- 
   final_wf %>%
   fit(train_dt)

lasso_res %>%
   collect_metrics() %>%
   ggplot(aes(penalty, mean, color = .metric)) +
   geom_errorbar(aes(
      ymin = mean - std_err,
      ymax = mean + std_err
   ),
   alpha = 0.5
   ) +
   geom_line(size = 1.5) +
   facet_wrap(~.metric, scales = "free", nrow = 2) +
   scale_x_log10() +
   theme(legend.position = "none")

# Sauvegarder les données résultantes ------------------------------------------


saveRDS(river_dt, output_path, compress = "xz")


# Sauvegarder le modèle résultant ----------------------------------------------


saveRDS(final_model, output_path_model, compress = "xz")




