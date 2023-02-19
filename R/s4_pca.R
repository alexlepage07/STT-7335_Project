# s4_pca.R


# Un fichier dédié à la réalisation de l'analyse en composantes principales
# dans un but de sélection de variables.


# Université Laval
# Session d'hiver 2023
# Cours: STT-7335 - Méthodes d'analyse de données
# Équipe 4:
#  - Lepage, Alexandre
#  - Helaiem, Amélie
#  - Hangsin, Andrea
#  - Ouedraogo, Angelo
#  - Pellerin, Jocelyn

# Développeur: Amélie Helaiem (AMHEL3@ulaval.ca)



# Chemins d'accès --------------------------------------------------------------

input_path <- "../Data/s2_donnees_sol_pc_imputees.rds"

# Charger les données ----------------------------------------------------------

river_dt <- as.data.table(base::readRDS(input_path))

# Préparation du jeu de données ------------------------------------------------


# On retire les variables qui ne sont pas numériques (on ne peut pas faire de PCA dessus)
# On pourrait aussi peut-être les transformer en variables numériques
col_cl <- c("glc_cl_cmj", "wet_cl_cmj", "fmh_cl_cmj", "lit_cl_cmj")

# On retire aussi les colonnes avec des valeurs manquantes (-999) car elles peuvent biaiser
# la sélection de variables
sapply(river_dt, function(x) sum(x == -999))
col_na = c("snd_pc_uav", "cly_pc_uav", "slt_pc_uav", "soc_th_cav", "soc_th_uav")

# Contient les variables retirées de façon temporaire
river_temp <- subset(river_dt, select = col_cl) 
river_temp2 <- subset(river_dt, select = col_na)

river_dt[,c(col_cl, col_na)] <- list(NULL)

# On retire la variable cible (dis_m3_pyr)
river_dt2 = subset(river_dt, select = -dis_m3_pyr)

# PCA --------------------------------------------------------------------------

# Réalisation du PCA
comp.pr <- prcomp(river_dt2, scale = T)
  

# Calcul de la variance expliquée par chaque composante principale
comp.pr.var <- comp.pr$sdev^2

# Calcul de la proportion de variance expliquée
prop.var <- comp.pr.var/sum(comp.pr.var)
prop.var


# Visualisation graphique ------------------------------------------------------

# Proportion de variance expliquée par chaque composante principale

plot (prop.var , xlab = " Composante principale ",
       ylab = " Proportion de variance expliquée", ylim = c(0 , 1) ,
       type = "b", col = "blue")

grid(nx = NA, ny = NULL,
     lty = 1, col = "gray", lwd = 1)

# Proportion de variance expliquée cumulative
plot(cumsum(prop.var), xlab = "Composante principale", ylab = " Proportion de variance expliquée (Cumulative)", col = "blue", type = "o")

grid(nx = NA, ny = NULL,
     lty = 1, col = "gray", lwd = 1)

# 90% de variance expliquée par les 19 premières composantes principales
abline(h = 0.9, col = "red", lty = 2)


# Sélection de variables -------------------------------------------------------

# On garde les premières composantes principales qui expliquent plus de 90% de la variabilité
nombre.comp = which(cumsum(prop.var) > 0.9)[1] 
river_pca = comp.pr$x[,1:nombre.comp]

# Rajout des variables retirées précédemment
river_pca = cbind(river_pca, river_temp, river_temp2)

# On rajoute la variable réponse
river_pca = cbind(river_pca, river_dt$dis_m3_pyr)
names(river_pca)[names(river_pca) == 'V2'] <- 'dis_m3_pyr'
