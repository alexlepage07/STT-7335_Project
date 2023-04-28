# s9_error_vizualisation.R

# Un fichier dédié à la vizualisation des erreurs de prédiction


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


source("R/utils.R")

libs <- c("terra", "sf", "leaflet", "tidymodels")
inst_load_packages(libs)


# Intrants ---------------------------------------------------------------------


# Coordonnées GPS entourant le Québec
EXTENT <- terra::ext(c(-79.85, -55.53, 45.04, 62.80))


# Chemins d'accès --------------------------------------------------------------


data_path <- "Data/RiverATLAS_Data_v10_shp/RiverATLAS_v10_shp"
orig_covariates_path <- "Data/s4_donnees_sans_multicol.rds"
final_covariates_path <- "Data/s7_donnees_lasso_var_sel.rds"
model_output_path <- "inst/s7_mixed_effect.rds"


# Charger les données ----------------------------------------------------------


# Chargement des données sous la forme d'un SpatVector (Vecteur spatial)
river_atlas <- terra::vect(
   file.path(data_path, "RiverATLAS_v10_na.shp"),
   extent = EXTENT
)

original_dt <- readRDS(orig_covariates_path)
final_dt <- readRDS(final_covariates_path)
assertthat::assert_that(all(original_dt$MAIN_RIV == final_dt$MAIN_RIV))
river_dt <- cbind(HYRIV_ID=original_dt$HYRIV_ID, final_dt)

model_output <- readRDS(model_output_path)
final_model <- model_output$model


# Séparer le jeu ---------------------------------------------------------------


set.seed(7335)
split_dt <- initial_split(river_dt, prop = 0.85)
train_dt <- training(split_dt) 
split_train_dt <- initial_split(train_dt, prop = 1 - 0.15/0.85)
train_dt <- training(split_train_dt)
val_dt  <- testing(split_train_dt)
test_dt  <- testing(split_dt)

# On n'a plus besoin du jeu de validation finalement puisque le ratio des 
# vraisemblances est utilisé pour sélectionner le meilleur sous-modèle et cette
# méthode utilise le jeu d'entraînement.
test_dt <- rbind(val_dt, test_dt)

test_poly <- terra::merge(river_atlas[,"HYRIV_ID"], test_dt)

results_test_poly <- cbind(
   test_poly[, c("HYRIV_ID", "dis_m3_pyr", "MAIN_RIV")], 
   data.frame(prediction = predict(final_model, test_dt, allow.new.levels = TRUE))
) |> sf::st_as_sf()

errors <- (results_test_poly$prediction - results_test_poly$dis_m3_pyr)^2
pal <- colorNumeric(palette = c('green', 'red'), 
                    domain = errors)   

leaflet() |>
   addTiles() |>
   addPolylines(stroke = pal)
   

