# pca_explo.R


# Un fichier dédié à la réalisation de l'analyse en composantes principales
# dans un but exploratoire.


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

# Librairies -------------------------------------------------------------------

library(FactoMineR)
library(knitr)
library(ggplot2)
library(DT)

# Chemins d'accès --------------------------------------------------------------

input_path <- "../Data/s3_donnees_standardisees.rds"

# Charger les données ----------------------------------------------------------

river_dt <- base::readRDS(input_path)

# Préparation du jeu de données ------------------------------------------------


# On retire les colonnes avec des valeurs manquantes car elles peuvent biaiser
# la sélection de variables
col_na = c("soc_th_cav", "soc_th_uav")

# Contient les variables retirées de façon temporaire
river_temp2 <- subset(river_dt, select = col_na)

river_dt[,col_na] <- list(NULL)

# On retire les variables d'identification
river_dt2 = subset(river_dt, select = -c(HYRIV_ID, NEXT_DOWN, MAIN_RIV, LENGTH_KM))

# On normalise dis_m3_pyr pour que ses coordonnées rentrent dans le cercle plus tard
river_dt2$dis_m3_pyr = (river_dt2$dis_m3_pyr - mean(river_dt2$dis_m3_pyr)) / sd(river_dt2$dis_m3_pyr)

# PCA --------------------------------------------------------------------------

# Nombre de composantes principales sélectionnées
n = 3

riv_pca <- PCA(river_dt2, ncp = n, quali.sup = c(2:5), 
               quanti.sup = c(1), scale.unit = F, graph = F)

# Calcul des poids -------------------------------------------------------------

# Extraction des poids
vec.poids <- riv_pca$svd$V
row.names(vec.poids) <- colnames(river_dt2[6:length(river_dt2)])
colnames(vec.poids) <- paste0('CP', c(1:n))

# Affichage des poids
datatable(round(vec.poids[,1:n],2))

# Représentation des observations ----------------------------------------------

plot.PCA(riv_pca, axes = c(1,2), choix = 'ind')


# Représentation des  variables ------------------------------------------------

plot.PCA(riv_pca, 
         axes = c(1,2), 
         choix = 'var')


# Enregistrement des coordonnées des variables
corr_dt <- as.data.frame(riv_pca$var$coord)
noms_var <- rownames(corr_dt)

# Traçage du cercle
tt <- seq(0,2*pi,length.out = 100)
cercle <- data.frame(x = cos(tt), y = sin(tt))

# Variables servant à faire l'ACP représentées sur un cercle
cercle_var <- ggplot() +
  geom_text(data = corr_dt, 
            aes(x = Dim.1, y = Dim.2, label = noms_var),  
            hjust = 'left', 
            vjust = 'middle') +
  xlim(c(-1.2,1.2)) +
  ylim(c(-1.2,1.2)) +
  geom_segment(data = corr_dt, 
               aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2),  
               arrow = arrow(length = unit(0.2,"cm"), type = 'closed')
  ) +
  geom_polygon(data = cercle, aes(x,y), alpha = 0.1) +
  theme_minimal()

cercle_var


# Coordonnées des variables quantitatives supplémentaires 
# (ici c'est seulement dis_m3_pyr)
coord_quanti_sup <- as.data.frame(riv_pca$quanti.sup$coord)

# Ajout de dis_m3_pyr sur le graphique des variables

cercle_var +
  geom_text(data = coord_quanti_sup, 
            aes(x = Dim.1, y = Dim.2, label = rownames(coord_quanti_sup), 
                color = 'Variables supplémentaires'),  
            hjust = 'right', 
            vjust = 'middle') + 
  geom_segment(data = coord_quanti_sup, 
               aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2, 
                   color = 'red'),  
               arrow = arrow(length = unit(0.2,"cm"), type = 'closed')
  ) +
  theme(legend.position="none")


# Sélection de composantes -----------------------------------------------------

kable(round(riv_pca$eig, 3),
      caption = 'Proportion de la variation expliquée par chaque composante principale')

# Variabilité de dis_m3_pyr expliquée par les composantes principales
riv_pca$quanti.sup$cos2

# On peut se contenter d'interpréter les 3-4 premières composantes principales