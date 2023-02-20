# s2_imput_missing_soils_pc.R


# Un fichier dédié à l'imputation des données manquantes pour les proportions
# de constitution du sol.


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
library(leaflet)
library(Hmisc)
library(naniar)
library(sf)
library(terra)

source("R/utils.R")


# Chemins d'accès --------------------------------------------------------------


input_path <- "Data/s1_donnees_filtrees.rds"
output_path <- "Data/s2_donnees_imputees.rds"

shape_file_path <- "Data/RiverATLAS_Data_v10_shp/RiverATLAS_v10_shp"


# Charger le jeu de données ----------------------------------------------------


river_dt <- as.data.table(base::readRDS(input_path))
variables <- names(river_dt)
navigation_var <- variables[1:3]


# Sélectionner les variables contenant des valeurs manquantes ------------------


# Identifier les variables avec des valeurs manquantes
var_missing_data <- 
   river_dt |> lapply(function(x) {
   res <- sum(x == -999)
   if(res != 0) {res} else {NULL}
}) |> unlist()
var_missing_data


# Ajouter une classe pour la variable catégorielle wet_cl_cmj ------------------


mod_missing_wet_cl_cmj(river_dt)


# Regarder les patrons de non-réponse ------------------------------------------


# Identifier les variables avec des valeurs manquantes
col_w_na <- get_col_w_na(river_dt, na_pattern = -999)


# Remplacer les codes -999 par des NA
river_dt[river_dt == -999] <- NA


missings_dt <- river_dt[, col_w_na, with = FALSE]
Hmisc::na.pattern(missings_dt)
col_w_na
# Il y a deux patrons possibles:
#  - Toutes les variables cav sont manquantes 
#  - Toutes les variables sont manquantes
# 
# Note: Il y a plus de variables manquantes avec les variables cav que uav


Similarity_matrix <- varclus(as.matrix(missings_dt))
Similarity_matrix
plot(Similarity_matrix)
# Il semble y avoir une corrélation très forte entre les variables uav et cav.


# Calculer les corrélation entre les mêmes variables, mais cav et uav
uav_vars <- grep("uav", col_w_na, value = TRUE) 

uav_vars |>
sapply(substr, 1, nchar(uav_vars) - 4) |>
unique() |>
sapply(function(nm)
   cor(river_dt[[paste0(nm, "_cav")]],
       river_dt[[paste0(nm, "_uav")]],
       use = "complete.obs")) |> round(2)
# Les variables uav et cav sont tellement corrélées qu'on peut en retirer une des
# deux


river_dt <- river_dt[, !grep("cav", col_w_na, value = TRUE), with = FALSE]


river_dt[, get_col_w_na(river_dt, na_pattern = NA), with = FALSE] |>
   Hmisc::na.pattern()
# Il ne reste maintenant qu'un patron de non réponse.


# Vérifions si le dernier patron est MCAR --------------------------------------


navigation_var <- c(
   "HYRIV_ID",   # Identifiant des segments de rivière
   "NEXT_DOWN",  # HYRIV_ID du prochain segment en aval 
   "MAIN_RIV"    # HYRIV_ID du segment de rivière le plus en aval de ce bassin
)
mcar_test(river_dt[, -navigation_var, with = FALSE])
# Selon le test de Little, le patron de non réponse n'est pas MCAR.
# Cependant, ce test est peu fiable.
# Voyons avec un test t:


river_dt[, patron := 0]
river_dt[is.na(snd_pc_uav), patron := 1]
t_test_res <- t_test_by_patron(river_dt, "patron")
setorder(t_test_res, p_value)
t_test_res[p_value < 0.05]
# On conclu que la moyenne des autres variables est significativement différentes
# en présence du patron de non-réponse


# On est maintenant confiant que le patron de non-réponse n'est pas MCAR.
river_dt[, patron := NULL]


# Analyse géographique du patron de non-réponse --------------------------------


# Dans la documentation du jeu de données, on apprend que le dernier patron de
# non réponse n'inclut que des variables de la base de données suivante:
# [SoilGrids1km; Hengl et al. 2014]
# 
# De plus, on y voit que les données de composition du sol sont manquantes
# puisqu'elles se trouvent dans des grands bassins d'eau tels que des lacs.
# Validons cette affirmation:

river_vec <- terra::vect(
   shape_file_path,
   extent = ext(c(-79.85, -55.53, 45.04, 62.80))
)
river_vec <- sf::st_as_sf(river_vec)
leaflet(river_vec[river_vec$slt_pc_uav == -999, ]) %>%
   addTiles() %>%
   addPolylines()

# C'est effectivement le cas.


# En conclusion, comme la non réponse pouvait être prédite selon la variable de
# pourcentage de couverture par des lacs dans le bassin (lka_pc_cse) et que
# celle-ci était associée à des valeurs significativement différentes pour les
# autres variables, on peut affirmer que le patron de non-réponse est MAR.

# Néanmoins, l'objectif de ce travail est de prédire le débit d'eau des rivières.
# Comme il n'y a pas de sens à calculer un débit d'eau dans un lac, nous allons 
# simplement retirer ces observations du jeu de données.

river_dt <- na.omit(river_dt)


# Sauvegarder les données résultantes ------------------------------------------


saveRDS(river_dt, output_path, compress = "xz")
