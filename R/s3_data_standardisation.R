# s2_data_standardisation.R


# Un fichier dédié à la standardisation de certaines données


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
library(tidymodels)
library(tidyverse)


# Chemins d'accès --------------------------------------------------------------


input_path <- "Data/s2_donnees_imputees.rds"
output_path <- "Data/s3_donnees_standardisees.rds"


# Charger les données ----------------------------------------------------------


river_dt <- readRDS(input_path)


# Standardiser les pourcentages ------------------------------------------------


recette_standardisation_pourcentages <- 
   recipes::recipe(dis_m3_pyr ~ ., data = river_dt) %>%
   step_select(contains("_pc_")) %>%
   step_mutate_at(where(~max(.x) > 100), fn = ~ min(.x, 100)) %>%
   step_mutate_at(everything(), fn = ~ .x / 100) %>%
   step_rm(where(~var(.x) == 0)) %>%
   prep()

percentages_dt <- 
   recette_standardisation_pourcentages %>%
   bake(river_dt) %>% 
   as.data.table()

summary(percentages_dt)


# Standardiser les grandes valeurs ---------------------------------------------


recette_standardisation_grands_nombres <- 
   recipes::recipe(dis_m3_pyr ~ ., data = river_dt) %>%
   step_rm(where(is.factor), contains("_pc_")) %>%
   step_rm(1:3) %>%
   step_mutate_at(where(~ min(.x) == 0), fn=~ .x + 1) %>%
   step_BoxCox(everything(), -hdi_ix_cav, limits = c(-8, 5)) %>%
   step_normalize(everything(), -dis_m3_pyr) %>%
   prep()

grandes_valeurs_dt <- 
   recette_standardisation_grands_nombres %>%
   bake(river_dt) %>% 
   as.data.table()

summary(grandes_valeurs_dt)


# Combiner les données transformées --------------------------------------------


donnees_standardisees <- cbind(
   river_dt[, 1:3],
   river_dt %>% select(where(is.factor)),
   percentages_dt,
   grandes_valeurs_dt
)


# Sommaire post-transformations
summary(donnees_standardisees)


# Sauvegarder les données transformées -----------------------------------------

   
saveRDS(river_dt, output_path, compress = "xz")
