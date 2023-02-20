# s3_imput_missings_soc_th.R

# Un script permettant de faire de l'imputation sur la variable de quantité de
# carbone dans le sol (soc_th_cav, soc_th_uav).

# Université Laval
# Session d'hiver 2023
# Cours: STT-7335 - Méthodes d'analyse de données
# Équipe 4:
#  - Lepage, Alexandre
#  - Helaiem, Amélie
#  - Hangsin, Andrea
#  - Ouedraogo, Angelo
#  - Pellerin, Jocelyn

# Développeur: Alexandre Lepage (alexandre.lepage.3@ulaval.ca)


# Librairies -------------------------------------------------------------------


library(data.table)
library(doParallel)
library(lme4)
library(multilevelmod)
library(naniar)
library(tidymodels)

require(Hmisc)
require(glmer)
require(glmnet)
require(stringr)


# Chemins d'accès --------------------------------------------------------------


input_path <- "Data/s2_donnees_sol_pc_imputees.rds"
output_path <- "Data/s3_donnees_manquantes_imputees.rds"


# Charger le jeu de données ----------------------------------------------------


river_dt <- base::readRDS(input_path) %>% as.data.table()
variables <- names(river_dt)
navigation_var <- variables[1:3]


# Analyse des variables -------------------------------------------------------


# Regarder le format des données
glimpse(river_dt)
glimpse(river_dt %>% select(contains("_cl_")))  # Tous des facteurs. Ok !
glimpse(river_dt %>% select(contains("_pc_")))  # Tous numériques. Ok !
glimpse(river_dt %>% select(!matches("_(pc|cl)_")))  # Tous numériques.
river_dt %>% summary()
# Les variables dor_pc_pva, lka_pc_cse et lka_pc_use contiennent des valeurs
# aberrantes


# Uniformiser les valeurs manquantes 
river_dt[soc_th_uav == -999]$soc_th_uav <- NA
river_dt[soc_th_cav == -999]$soc_th_cav <- NA

river_dt[dor_pc_pva > 1000]$dor_pc_pva <- NA
river_dt[dor_pc_pva > 100, dor_pc_pva := min(dor_pc_pva, 100)]

river_dt[lka_pc_cse > 100]$lka_pc_cse <- NA
river_dt[lka_pc_use > 100]$lka_pc_use <- NA


# Visualiser les variables où il reste des valeurs manquantes ------------------


# Calculer le nombre de données manquantes par variable
var_missing_data <- sapply(
   river_dt, 
   function(x) sum(x == -999, na.rm= TRUE) + sum(is.na(x))
) 
var_missing_data[var_missing_data != 0]


# Traitement de la non-réponse pour la variable wet_cl_cmj ---------------------


# wet_cl_cmj est manquante dans le cas où il n'y a pas de milieu humide
# On va donc simplement créer une classe pour supplémentaire pour cette variable
river_dt[,wet_cl_cmj] %>% summary()  # Il y a 12 classes au total

# On va en créer une pour les valeurs manquantes
river_dt[, wet_cl_cmj := as.integer(wet_cl_cmj)]
river_dt[wet_cl_cmj == -999]$wet_cl_cmj <- 0
river_dt[, wet_cl_cmj := factor(wet_cl_cmj)]


# Analyse des autres variables manquantes --------------------------------------


# Test de Little
river_dt %>% 
   dplyr::select(-c("HYRIV_ID", "NEXT_DOWN", "MAIN_RIV", "dis_m3_pyr", "LENGTH_KM")) %>%
   naniar::mcar_test()
# On rejette l'hypothèse nulle que les valeurs manquantes sont complètement 
# aléatoires (MCAR).
Hmisc::na.pattern(river_dt)


cor(river_dt[, .(soc_th_uav, soc_th_cav)], use="complete.obs")
# La corrélation linéaire entre ces deux variables est très grande. 
# Nous pourrions en retirer une des deux. 
# Comme il y moins de valeurs manquantes avec soc_th_uav, c'est elle que l'on va
# conserver.
river_dt[, soc_th_cav := NULL]


river_dt[,. (dor_pc_pva, lka_pc_cse, lka_pc_use)] %>% summary(lka_pc_use)
# La moitié des observations ont des valeurs nulles avec ces variables.
# A priori, aucune autre variable ne peut servir à imputer les variables lka_pc_
# puisqu'elles représentent le pourcentage du bassin qui est recouvert par des lacs.
# Elles sont donc MNAR et nous allons les supprimer du jeu de données
river_dt[, lka_pc_cse := NULL]
river_dt[, lka_pc_use := NULL]


# Inférence sur la variable soc_th_uav -----------------------------------------


data_train <- river_dt %>% subset(
      !is.na(soc_th_uav),
      !names(river_dt) %in% c("HYRIV_ID", "NEXT_DOWN", "dis_m3_pyr", "LENGTH_KM"),
      with = FALSE
)


# Premièrement, on va entraîner un modèle LASSO pour sélectionner les variables
# d'intérêt.

recipe <- 
   recipes::recipe(soc_th_uav ~ ., data_train) %>%
   recipes::step_rm(MAIN_RIV, dor_pc_pva) %>%
   recipes::step_log(where(~ is.numeric(.x) && var(.x, na.rm=TRUE) > 1e+4), offset = 1) %>%
   recipes::step_dummy(all_nominal()) %>%
   prep()

bake(recipe, data_train) %>% summary()

lasso_spec <-  
   linear_reg(mixture = 1, penalty = tune()) %>%  # Mixture = 1: Régularisation Lasso
   set_mode("regression") %>%
   set_engine("glmnet")

lasso_workflow <- 
   workflows::workflow() %>%
   add_model(lasso_spec) %>%
   add_recipe(recipe)

set.seed(12345)
doParallel::registerDoParallel(3L)
lasso_results <- tune::tune_grid(
   lasso_workflow,
   resamples = rsample::vfold_cv(data_train, v = 3L),
   grid = 10,
   metrics = metric_set(rmse)
)
doParallel::stopImplicitCluster()

lasso_results$.notes[[1]]

lasso_final <- 
   lasso_workflow %>% 
   finalize_workflow(select_best(lasso_results)) %>% 
   fit(data_train)

predictions <- predict(lasso_final, data_train) %>% unlist()
rmse(data_train, soc_th_uav, predictions)


predictors <- lasso_final %>% tidy() %>% as.data.table()
predictors <- predictors[estimate != 0]$term[-1]
predictors <- stringr::str_remove_all(predictors, "_X[0-9]+") %>% unique()


# Maintenant, on va entraîner un modèle mixte avec ces prédicteurs -------------


mix_model_data <- river_dt[, c("soc_th_uav", "MAIN_RIV", predictors), with = FALSE]
data_train <- mix_model_data[!is.na(soc_th_uav)]


recipe <- 
   recipes::recipe(soc_th_uav ~ ., data_train) %>%
   recipes::step_mutate(MAIN_RIV = factor(MAIN_RIV)) %>%
   recipes::step_log(where(~ is.numeric(.x) && var(.x, na.rm=TRUE) > 1e+4), offset = 1) %>%
   recipes::step_normalize(all_numeric()) %>%
   recipes::step_pca(all_numeric(), num_comp = 3) %>%
   recipes::step_dummy(contains("_cl_")) %>%
   prep()

bake(recipe, data_train) %>% summary()

lmer_spec <-  
   linear_reg() %>%
   set_mode("regression") %>%
   set_engine("lmer")

lmer_workflow <- 
   workflows::workflow() %>%
   add_recipe(recipe) %>%
   add_model(lmer_spec, formula = soc_th_uav ~ .- MAIN_RIV + (1|MAIN_RIV)) 


doParallel::registerDoParallel(3L)
lmer_results <- fit_resamples(
   lmer_workflow, 
   resamples = rsample::vfold_cv(data_train, v = 3L)
)
doParallel::stopImplicitCluster()

lmer_results$.notes[[1]]

glmer_final <- 
   lmer_workflow %>% 
   finalize_workflow(select_best(lmer_results)) %>% 
   fit(data_train)
