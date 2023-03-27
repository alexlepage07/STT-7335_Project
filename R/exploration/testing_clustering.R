# testing_clustering.R

# A script to test a clustering of the dataset


# Libraries --------------------------------------------------------------------


library(cluster)
library(data.table)
library(ggplot2)
library(parallel)
library(pbapply)
library(tidyverse)


# Chemins d'accès --------------------------------------------------------------


input_path <- "Data/s3_donnees_standardisees.rds"


# Chargement des données -------------------------------------------------------


river_dt <- readRDS(input_path)

y <- river_dt$dis_m3_pyr
x <- river_dt |> select(
   -c("HYRIV_ID", "NEXT_DOWN", "MAIN_RIV", "LENGTH_KM", "dis_m3_pyr"))


# Tester la mesure de dissimilarité de Gower -----------------------------------


# Calculer des poids pour chacune des variables
abs_corr <-
   x |> select(where(is.numeric)) |> cor(y, method = "spearman") |> abs() |> t()

fact_variables <- x |> select(where(is.factor))
w_fact <- rep(mean(abs_corr), ncol(fact_variables)) |> t() |> as.data.table()
colnames(w_fact) <- names(fact_variables)

weights <- cbind(abs_corr, w_fact) |> setcolorder(names(x))
assertthat::assert_that(all(names(weights) == names(x)))


# Identifier les variables asymétriques
x |> select(where(is.factor)) |> summary()


# Calculer la matrice de dissimilarité
dissimilarities <- daisy(
   as.data.frame(x), 
   metric = "gower",
   type = list(asymm = "dor_cl_pva_over1000"),
   weights = as.numeric(weights)
)
#' La taille du jeu de données est trop grand pour pouvoir calculer une matrice
#' de dissimilarité.


# Tester un K-medoids ----------------------------------------------------------


K.max <- 20L


k_medoids_clustering <- function(k, data) {
   clara_obj <- 
      cluster::clara(data, k, cluster.only = FALSE, samples = 15, rngR = TRUE)
   gc(verbose = FALSE)
   return(clara_obj$objective)
}


set.seed(1234)
cl <- parallel::makeCluster(5L)
clusterEvalQ(cl, library(cluster))
wss <- pbsapply(1:K.max, k_medoids_clustering, select(x, where(is.numeric)))
stopCluster(cl)

plot(1:K.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="K-medoids objective function's output")

# Avec la méthode du coude, on trouve que 3 groupes serait un nombre suffisant
# Mais pour s'amuser, on va en définir plus... disons 14

K <- 14L

set.seed(1234)
river_dt$cluster <- cluster::clara(
   select(x, where(is.numeric)),
   K,
   cluster.only = TRUE, 
   samples = 15L, 
   rngR = TRUE
)

ordered_clusters <- 
   river_dt[, .(avg_discharge = mean(dis_m3_pyr)), by = cluster] |> 
   setorder(avg_discharge) |>
   select(cluster) |>
   unlist() |>
   as.character()

river_dt <-
   river_dt |> 
   mutate(cluster = factor(cluster, levels = ordered_clusters, ordered = TRUE))


# Visualiser l'effet du regroupement des données sur la variable endogène

river_dt |> ggplot() +
   geom_boxplot(aes(x = cluster, y = dis_m3_pyr))


# Remarque importante ----------------------------------------------------------


#' Il a été démontré que l'effet du PCA était très similaire à celui du K-moyennes 
#' à l'exception qu'il n'y a pas de contrainte de "classification"; 
#' c.-a-d. avec le PCA, les variables en sortie peuvent demeurer numériques. 
#' Voir: https://ranger.uta.edu/~chqding/papers/KmeansPCA1.pdf
#' 
#' Comme le K-medoids est très apparentée au K-moyennes, il n'y a aucune raison 
#' d'appliquer un groupement par K-medoids ET un PCA.
