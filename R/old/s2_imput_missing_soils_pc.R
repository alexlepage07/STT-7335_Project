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
library(dplyr)
library(Hmisc)
library(lme4)
library(naniar)

require(MASS)


# Chemins d'accès --------------------------------------------------------------


input_path <- "Data/s1_donnees_filtrees.rds"
output_path <- "Data/s2_donnees_sol_pc_imputees.rds"


# Charger le jeu de données ----------------------------------------------------


river_dt <- as.data.table(base::readRDS(input_path))
variables <- names(river_dt)
navigation_var <- variables[1:3]


# Sélectionner les variables relatives à la composition du sol -----------------
# Ces variables sont la source principales de valeurs manquantes


soil_composition_vars <- variables[
   variables %like% "_pc_" & variables %like% "snd|cly|slt|soc"]

soil_comp_dt <- river_dt[, c(navigation_var, soil_composition_vars), with = FALSE]

# Remplacer les -999 par des NA
soil_comp_dt[soil_comp_dt == -999] <- NA


# Analyse de pattern -----------------------------------------------------------


soil_comp_dt[is.na(snd_pc_cav)] %>% summary()
soil_comp_dt[is.na(snd_pc_uav)] %>% summary()

na.pattern(soil_comp_dt)
# Clairement, les valeurs manquantes se produisent toujours selon le même pattern

par(mfrow = c(2,2))
naplot(naclus(soil_comp_dt[, ..soil_composition_vars]))
par(mfrow = c(1,1))


# Analyse des corrélations -----------------------------------------------------


cor(soil_comp_dt[!is.na(snd_pc_cav), .SD[,-c(1:3)]])
# Il y a une très forte dépendance linéaire entre les variables de composition du sol.

cor(soil_comp_dt[, .(snd_pc_cav, snd_pc_uav)], use="complete.obs")
cor(soil_comp_dt[, .(cly_pc_cav, cly_pc_uav)], use="complete.obs")
cor(soil_comp_dt[, .(slt_pc_cav, slt_pc_uav)], use="complete.obs")
# Cette dépendance est d'autant plus forte lorsque l'on compare les proportions,
# pour une même type de sol, entre les segments de rivières (cav) et le réseau en 
# amont qui le constitue (uav).
# À un tel point, qu'il serait totalement inutile de conserver les colonnes "_cav"
# et que l'on pourrait travailler uniquement avec les "_uav"

soil_composition_vars <- 
   soil_composition_vars[soil_composition_vars %like% "_uav"]
soil_comp_dt <- 
   soil_comp_dt[, c(navigation_var, soil_composition_vars), with = FALSE]


# Analyse des distributions conditionnelles ------------------------------------


# Analyse de statistiques sur la composition du sol, par affluent principal 
# (MAIN_RIV)
soil_comp_dt[
   , 
   lapply(.SD, mean, na.rm = TRUE), 
   by = MAIN_RIV, 
   .SDcols = soil_composition_vars
] %>% summary()  # Moyennes conditionnelles

soil_comp_dt[
   ,
   lapply(.SD, var, na.rm = TRUE),
   by = MAIN_RIV,
   .SDcols = soil_composition_vars
] %>% summary()  # Variances conditionnelles


# Clairement les distributions de ces variables sont impactées par l'affluent 
# de la rivière
soil_comp_dt[, .N, by = MAIN_RIV]$N %>% describe()
# Cependant, il y a beaucoup d'affluents principaux qui n'ont qu'un seul bassin


soil_comp_dt[, snd_pc_uav] %>% hist()
soil_comp_dt[, cly_pc_uav] %>% hist()
soil_comp_dt[, slt_pc_uav] %>% hist()


# Imputation des données manquantes --------------------------------------------


impute_soil_composition <- function(soil_comp_dt, n_epoch=30L, seed=1234) {
   
   
   # Préparer les données ------------------------------------------------------
   
   
   id_to_imput <- soil_comp_dt[is.na(slt_pc_uav), HYRIV_ID]
   imputation_dt <- copy(soil_comp_dt)
   
   soil_vars <- names(soil_comp_dt)[names(soil_comp_dt) %like% "_pc_uav"]
   imputation_dt <- imputation_dt %>% mutate_at(soil_vars, as.double)
   
   imputation_dt$key <- seq_len(nrow(imputation_dt))
   
   
   # Entraîner un modèle mixte pour chacune des variables ----------------------
   
   
   snd_lmm <- lme4::lmer(
      snd_pc_uav ~ cly_pc_uav + slt_pc_uav + (1 | MAIN_RIV),
      data = imputation_dt[!HYRIV_ID %in% id_to_imput]
   )
   
   cly_lmm <- lme4::lmer(
      cly_pc_uav ~ snd_pc_uav + slt_pc_uav + (1 | MAIN_RIV),
      data = imputation_dt[!HYRIV_ID %in% id_to_imput]
   )
   
   slt_lmm <- lme4::lmer(
      slt_pc_uav ~ snd_pc_uav + cly_pc_uav + (1 | MAIN_RIV),
      data = imputation_dt[!HYRIV_ID %in% id_to_imput]
   )
   

   # Définir des valeurs initiales ---------------------------------------------
   
   
   conditional_means <- imputation_dt[
      , 
      lapply(.SD, mean, na.rm = TRUE),
      .SDcols = soil_composition_vars, 
      by = "MAIN_RIV"
   ]
   
   for(id in id_to_imput) {
      
      main_riv <- imputation_dt[HYRIV_ID == id, MAIN_RIV]
      
      imputation_dt[
         HYRIV_ID == id, 
         snd_pc_uav := conditional_means[MAIN_RIV == main_riv, snd_pc_uav]
      ]
      imputation_dt[
         HYRIV_ID == id, 
         cly_pc_uav := conditional_means[MAIN_RIV == main_riv, cly_pc_uav]
      ]
      imputation_dt[
         HYRIV_ID == id, 
         slt_pc_uav := conditional_means[MAIN_RIV == main_riv, slt_pc_uav]
      ]
   }
   
   
   # Simuler des réalisations des trois variables de composition du sol --------
   
   
   impute_variable <- function(lmm_model, dt_to_predict, Z, K) {
      
      .sum <- summary(lmm_model)
      
      mu <- as.numeric(predict(lmm_model, dt_to_predict))
      Sigma <- compute_var_Y(
         D = c(.sum$varcor$MAIN_RIV) ^ 2 * diag(K),
         V = .sum$sigma ^2 * diag(nrow(dt_to_predict)),
         Z = Z
      ) 
      return(round(MASS::mvrnorm(1, mu, Sigma)))
   }
   
   
   compute_var_Y <- function(D, V, Z){
      return(Z %*% D %*% t(Z) + V)
   }
   
   
   set.seed(seed)
   dt_to_predict <- imputation_dt[HYRIV_ID %in% id_to_imput]
   
   
   # Extraire les caractéristiques sur l'effet aléatoire
   main_riv_to_imput <- dt_to_predict$MAIN_RIV
   
   Z <- sapply(unique(main_riv_to_imput), function(main_riv) {
      main_riv_to_imput == main_riv
   })
   
   K <- length(unique(main_riv_to_imput))
   
   # Imputer itérativement
   for(i in 1:n_epoch) {
      
      dt_to_predict[,
         snd_pc_uav := impute_variable(snd_lmm, dt_to_predict, Z, K)
      ]
      
      dt_to_predict[,
         cly_pc_uav := impute_variable(cly_lmm, dt_to_predict, Z, K)
      ]
      
      dt_to_predict[,
         slt_pc_uav := impute_variable(slt_lmm, dt_to_predict, Z, K)
      ]
   }
   
   dt_to_predict[, slt_pc_uav := min(slt_pc_uav, 100 - snd_pc_uav - cly_pc_uav)]
   
   output <- rbind(
      imputation_dt[!(HYRIV_ID %in% id_to_imput)],
      dt_to_predict
   )
   
   setorder(output, key)
   
   output <- output %>% dplyr::select(-"key")
   
   return(output)
}


imputed_soils <- impute_soil_composition(soil_comp_dt)

river_dt[, snd_pc_uav := imputed_soils[, snd_pc_uav]]
river_dt[, cly_pc_uav := imputed_soils[, cly_pc_uav]]
river_dt[, slt_pc_uav := imputed_soils[, slt_pc_uav]]

river_dt[, snd_pc_cav := NULL]
river_dt[, cly_pc_cav := NULL]
river_dt[, slt_pc_cav := NULL]


# Sauvegarder les données résultantes ------------------------------------------


saveRDS(river_dt, output_path, compress = "xz")
