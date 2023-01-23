# s1_riverAtlas_preparation.R


# Un fichier dédié à l'exploration et la préparation des données de RiverAtlas


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


library(assertthat)
library(data.table)
library(terra)
library(tidyverse)


# Paramètres -------------------------------------------------------------------


# Coordonnées GPS entourant le Québec
EXTENT <- terra::ext(c(-79.85, -55.53, 45.04, 62.80))

# Variance minimal pour conserver une variable du jeu de données
MIN_VAR <- 1e-3


# Chemins d'accès --------------------------------------------------------------


data_path <- "Data/RiverATLAS_Data_v10_shp/RiverATLAS_v10_shp"
output_path <- "Data/s1_donnees_filtrees.rds"


# Charger les données pour la région Nord-Américaine  --------------------------


# Chargement des données sous la forme d'un SpatVector (Vecteur spatial)
river_atlas <- terra::vect(file.path(data_path, "RiverATLAS_v10_na.shp"))

# Visualisation de l'objet
river_atlas

# Visualisation des champs disponibles
names(river_atlas)
# Pour une description détaillée des champs, voir le fichier pdf
# RiverAtlas_Catalog_v10.pdf

# Restreindre l'étendue de la carte au Québec
river_atlas_croped <- terra::crop(river_atlas, EXTENT)
river_atlas_croped
plot(river_atlas_croped)


# Identifier les variables nous permettant de naviguer dans les données.
navigation_var <- c(
   "HYRIV_ID",   # Identifiant des segments de rivière
   "NEXT_DOWN",  # HYRIV_ID du prochain segment en aval 
   "MAIN_RIV"    # HYRIV_ID du segment de rivière le plus en aval de ce bassin
)
head(as.data.frame(river_atlas_croped[, navigation_var]), 20)
summary(river_atlas_croped[, navigation_var])


# Filtrer les champs qui nous intéressent
variables <- c(
   "dis_m3_pyr",  # Notre variable endogène: Le débit d'eau moyen
   "LENGTH_KM",   # Longueur du segment de la rivière
   "rev_mc_usu",  # Le volume du réservoir d'eau
   "dor_pc_pva",  # Index informant sur l'effet des barrages sur le débit d'eau
   "lka_pc_cse", "lka_pc_use",  # Pourcentage de terre qui correspond à des lacs
   "ria_ha_csu", "ria_ha_usu",  # L'aire de la rivière
   "riv_tc_csu", "riv_tc_usu",  # Le volume de la rivière
   "gwt_cm_cav",  # La profondeur des nappes phréatiques
   "slp_dg_cav", "slp_dg_uav",  # L'inclinaison du terrain
   "sgr_dk_rav",  # La pente hydrique
   "pre_mm_cyr", "pre_mm_uyr",  # Précipitations annuelles moyennes
   paste0("pre_mm_c0", 3:6),    # Les précipitations printanières moyennes
   "snw_pc_cyr", "snw_pc_uyr",  # La couverture de neige
   "glc_cl_cmj",  # Classification du terrain (forêt, ville, champs, etc.)
   "wet_cl_cmj",  # Classification des zones humides
   paste0("wet_pc_c0", 1:9), paste0("wet_pc_u0", 1:9),  # Proportion occupée par chacune des classes de terres humides
   "for_pc_cse", "for_pc_use",  # Proportion occupée par les forêts
   "ire_pc_cse", "ire_pc_use",  # Proportion de terre irriguée
   "gla_pc_cse", "gla_pc_use",  # Proportion de terre couverte par un glacier
   "prm_pc_cse", "prm_pc_use",  # Proportion de terre faisant partie du pergélisol (sol gelé en permanence)
   "fmh_cl_cmj",  # Classification hydrologique du bassin versant
   "lit_cl_cmj",  # Classes lithologiques (type de roches)
   "snd_pc_cav", "snd_pc_uav",  # Proportion de sable
   "cly_pc_cav", "cly_pc_uav",  # Proportion d'argile
   "slt_pc_cav", "slt_pc_uav",  # Proportion de limon (Un mélange de sable et d'argile)
   "soc_th_cav", "soc_th_uav",  # Quantité moyenne de carbone dans les 5 premiers centimètres de sol (tonnes/hectar)
   "swc_pc_cyr", "swc_pc_uyr",  # Taux de saturation du sol en eau
   "kar_pc_cse", "kar_pc_use",  # Proportion de Karst (une roche sédimentaire se déformant sous l'effet de l'érosion)
   "ero_kh_cav", "ero_kh_uav",  # L'érosion causé par les rivières en kg/hectare/année de roche
   "hdi_ix_cav"  # Indice de développement humain
)

data_names <- names(river_atlas_croped)
assertthat::assert_that(length(variables[!(variables %in% data_names)]) == 0) 

river_atlas_filtered <- river_atlas_croped[, c(navigation_var, variables)]


# Convertir le SpatVetor résultant en data.frame qui sera facile à travailler
river_df <- as.data.frame(river_atlas_filtered)

# Visualiser la classe de chacune des variables
dplyr::glimpse(river_df)

# Visualisation de quelques statistiques descriptives
summary(river_df)

# Observations:
# - Il y a des variables qui contiennent des valeurs manquantes notées par un 
#   -999
# - Certaines variables ne sont pas utiles puisqu'elles n'ont aucune variance.
# - Toutes les variables sont soit des entiers ou des nombres décimaux.
#   Les variables catégorielles devraient être des facteurs.


# Nettoyage des données --------------------------------------------------------


variances <- apply(river_df, 2, var)
river_df <- river_df[, names(variances)[variances > MIN_VAR]]

# Quelques statistiques descriptives
dim(river_df)
summary(river_df)
# On ne changera pas les valeurs manquantes pour le moment. 
# On y touchera plus tard


# Visualisation des données ----------------------------------------------------


river_df_var_sorted <- river_df[, sort(names(river_df))]
glimpse(river_df_var_sorted)
summary(river_df_var_sorted)


# Transformer les variables catégorielles en facteurs --------------------------


var_cl <- names(river_df)[names(river_df) %like% "_cl_"]
river_df <- river_df %>% mutate_at(var_cl, factor)

summary(river_df[, sort(var_cl)])


# Visualisation de l'effet des variables catégorielles sur la var endogène -----


var_cl

river_df %>% ggplot() + geom_boxplot(aes(x = glc_cl_cmj, y=dis_m3_pyr))
river_df %>% ggplot() + geom_boxplot(aes(x = wet_cl_cmj, y=dis_m3_pyr))
river_df %>% ggplot() + geom_boxplot(aes(x = fmh_cl_cmj, y=dis_m3_pyr))
river_df %>% ggplot() + geom_boxplot(aes(x = lit_cl_cmj, y=dis_m3_pyr))


# Sauvegarder les données résultantes ------------------------------------------


saveRDS(river_df, output_path, compress = "xz")
