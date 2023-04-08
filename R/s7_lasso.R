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
          "datapasta")

inst_load_packages(libs)


# Chemins d'accès --------------------------------------------------------------


input_path <- "Data/s5_donnees_rf_var_sel.rds"
output_path <- "Data/s7_donnees_lasso_var_sel.rds"
output_path_obj <- "inst/s7_lasso.rds"


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

# graph_qqplot(river_dt$dis_m3_pyr)
# graph_density(river_dt, "dis_m3_pyr")


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
train_folds <- vfold_cv(train_dt, v = 3)

# Créer une recipe
rec <- recipe(
   dis_m3_pyr ~ ., 
   data = train_dt
) %>% 
   step_rm(
      "HYRIV_ID",
      "NEXT_DOWN",
      "MAIN_RIV",
      "LENGTH_KM"
   ) %>% 
   step_dummy(
      all_nominal()
   ) 

# Créer le workflow
lasso_wf <- workflow() %>%
   add_recipe(rec) %>% 
   add_model(
      spec = tune_spec
   )

# Faire la validation croisée
lasso_res <-   
   lasso_wf %>% 
   tune_grid(
      resamples = train_folds,
      grid = lasso_grid
   )


# Modèle final -----------------------------------------------------------------


# Sélectionner le meilleur modèle basé sur la mesure de performance choisie
best_lasso <- lasso_res %>%
   estimate_tune_results() %>%
   filter(.metric == "rmse") %>%
   filter(mean <= min(mean) + 0.05) %>% 
   arrange(desc(penalty)) %>% 
   filter(row_number() == 1)

# Mettre à jour le workflow avec les hyperparamètres venant maximiser la 
# métrique de performance visée
final_wf <- lasso_wf %>% 
   finalize_workflow(
      best_lasso
   )

# Faire le dernier entraînement
final_fit <- 
   final_wf %>%
   last_fit(split_train_dt)

# Obtenir le dernier modèle
final_model <- 
   final_fit %>%
   extract_fit_parsnip()

# Obtenir le rmse sur le jeu d'entraînement
rmse_train <- 
   final_wf %>%
   fit(train_dt) %>% 
   predict(train_dt) %>% 
   rmse(train_dt$dis_m3_pyr, .pred)

# Graphique de la validation croisée
g <- lasso_res %>%
   collect_metrics() %>%
   filter(.metric == "rmse") %>%
   ggplot() +
   geom_point((
      mapping = aes(penalty, mean)
   )) +
   geom_vline(
      xintercept = best_lasso$penalty
   ) + 
   scale_x_log10() +
   labs(
      x = "\U03BB",
      y = "EQM^0.5",
      title = "Racine carrée de l'EQM obtenu en validation croisée en fonction de \U03BB"
   )  + 
   theme(
      text = element_text(family = "Times New Roman")
   )

# Retirer les variables --------------------------------------------------------

coefs <- coef(final_model$fit, s = final_model$spec$args$penalty)

vars <- coefs@Dimnames[[1]][-(coefs@i + 1)][-1]

# vector_paste_vertical(vars)

river_dt <- river_dt[, c("lka_pc_use",
                         "riv_tc_usu",
                         "gwt_cm_cav",
                         "slp_dg_uav",
                         "sgr_dk_rav",
                         "snw_pc_cyr",
                         "wet_pc_u01",
                         "cly_pc_uav",
                         "swc_pc_uyr",
                         "kar_pc_cse",
                         "riv_larg_csu",
                         "pre_spring_max_csu",
                         "glc_cl_cmj",
                         "wet_cl_cmj",
                         "lit_cl_cmj",
                         "HYRIV_ID",
                         "NEXT_DOWN",
                         "MAIN_RIV",
                         "LENGTH_KM",
                         "dis_m3_pyr"), with = FALSE]


# Comparaison prédit/observés --------------------------------------------------


graph_pred_by_var(
   pred = final_fit$.predictions[[1]]$.pred, 
   obs = final_fit$.predictions[[1]]$dis_m3_pyr,
   var = val_dt$pre_mm_uyr,
   nm_var = "for_pc_use"
)

# Sauvegarder les données résultantes ------------------------------------------


saveRDS(river_dt, output_path, compress = "xz")


# Sauvegarder le modèle résultant ----------------------------------------------

info_ls <- list(
   rmse_val = final_fit$.metrics,
   rmse_train = rmse_train,
   model = final_model,
   graph_cv = g
)

saveRDS(info_ls, output_path_obj, compress = "xz")




