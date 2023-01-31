# utils.R


# Un fichier avec des fonctions utiles pour éviter le code redondant


# Université Laval
# Session d'hiver 2023
# Cours: STT-7335 - Méthodes d'analyse de données
# Équipe 4:
#  - Lepage, Alexandre
#  - Helaiem, Amélie
#  - Hangsin, Andrea
#  - Ouedraogo, Angelo
#  - Jocelyn Pellerin


# Développeur: Andréa Hangsin (andrea.hangsib.1@ulaval.ca)


# Librairies -------------------------------------------------------------------


lib <- c("data.table",
         "ggplot2",
         "tidyverse",
         "utils")

l <- lapply(
   lib,
   function(x) {do.call("library", list(x))}
)


# Fonctions --------------------------------------------------------------------


# Fonction servant à créer un graphique exposant la distribution d'une 
# variable `var` dans un jeu de données `data`


graph_density <- 
   function(data, var, nb_bins = 10, quantile_min = 0.05, quantile_max = 0.95) {
      
      # Donner la classe data.table à l'objet data
      setDT(data)
      
      # Créer les variables basées sur les arguments
      q_min <- quantile(data[[var]], quantile_min)
      q_max <- quantile(data[[var]], quantile_max)
      
      if (q_min == q_max) {
         q_min <- min(data[[var]])
         q_max <- max(data[[var]])
      }
      
      dist <- (q_max - q_min) / nb_bins
      puissance <- 10 ^ floor(log10(dist))
      bin_width <- round(dist / puissance) * puissance
      
      # Créer les intervalles basées sur les variables précédemment créées
      data$bins_var <- cut(
         x = data[[var]], 
         breaks = unique(
            c(-Inf,
              0,
              seq(from = round(q_min / bin_width) * bin_width,
                  to = round(q_max / bin_width) * bin_width,
                  by =  bin_width),
              Inf)
         ), 
         dig.lab = 10
      )
      
      # Calculer le % dans chacun des intervalles
      res <- data[, .(pourc = .N / nrow(data)), by = bins_var]
      
      # Valider nos résultats
      stopifnot(sum(res$pourc) == 1)
      
      # Créer le graphique
      p <- ggplot(
         data = res, 
      ) + 
         geom_bar(
            mapping = aes(y = pourc,
                          x = bins_var),
            stat = "identity"
         ) +
         theme(
            axis.text.x = element_text(angle = 35,
                                       size = 5)
         ) + 
         labs(title = paste0("Distribution - ", translate_var(var)),
              x = translate_var(var), 
              y = translate_var("dis_m3_pyr")
         )
      
      return(p)
   }


# Fonction qui "traduit" les variables en noms compréhensibles (créée pour les 
# noms des axes des graphiques)


translate_var <- function(vars) {
   
   # À compléter
   map <- list(
      dis_m3_pyr = "Débit d'eau moyen (en m\U00B3/s)",
      rev_mc_usu = "Volume du réservoir d'eau (en million m\U00B3)",
      dor_pc_pva = "Degré de régulation du réservoir (en %)",
      lka_pc_cse = "Pourcentage de terre qui correspond à des lacs (en %) (cse)",
      lka_pc_use = "Pourcentage de terre qui correspond à des lacs (en %) (use)",
      
      snd_pc_cav = "Proportion de sable (en %) (cav)",
      snd_pc_uav = "Proportion de sable (en %) (uav)",
      cly_pc_cav = "Proportion d'argile (en %) (cav)",
      cly_pc_uav = "Proportion d'argile (en %) (uav)",
      slt_pc_cav = "Proportion de limon (en %) (cav)",
      slt_pc_uav = "Proportion de limon (en %) (uav)",
      soc_th_cav = "Quantité moyenne de carbone dans les 5 premiers centimètres de sol (en tonnes/hectar) (cav)",
      soc_th_uav = "Quantité moyenne de carbone dans les 5 premiers centimètres de sol (en tonnes/hectar) (uav)",
      
      
      wet_cl_cmj = "Classification des zones humides"
   )
   
   translation <- vapply(
      vars, 
      function(x) {
         res <- map[[x]]
         if (is.null(res)) {
            res <- x
         }
         res
      },
      character(1L)
   ) %>% unname()
   
   return(translation)
   
}


# Fonction pour installer, au besoin, une ou plusieurs librairies, pour ensuite
# les télécharger


inst_load_packages <- function(libs) {
   
   # Installer les librairies si elles ne sont pas dans les librairies installées
   s <- sapply(
      libs, 
      function(x) {
         if (!(x %in% utils::installed.packages())) 
            utils::install.librairies(x)
      }
   )
   
   # Télécharger les librairies
   s <- sapply(libs, function(x) do.call(library, list(x)))
}



