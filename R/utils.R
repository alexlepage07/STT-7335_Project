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
         "terra",
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
   function(data, var, nb_bins = 10, quantile_min = 0.05, quantile_max = 0.95, color = NULL) {
      
      # Donner la classe data.table à l'objet data
      setDT(data)
      
      # Créer un variable var_color
      set(
         x = data, 
         j = "var_color", 
         value = if (is.null(color)) 1 else as.factor(data[[color]])
      )
      
      # Calculer les moyennes des groupes
      if (!is.null(color)) {
         mean_by_color <- 
            data[, round(mean(get(var, inherits = FALSE), na.rm = TRUE), 2), 
                 by = var_color]
      }
      
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
      data[, tot_value := .N, by = var_color]
      res <- data[, .(pourc = .N / unique(tot_value)), by = .(bins_var, var_color)]
      
      # Valider nos résultats
      stopifnot(round(sum(res[var_color == res$var_color[1]]$pourc) * 100) == 100)
      
      # Créer le graphique
      p <- ggplot(
         data = res, 
      ) + 
         geom_bar(
            mapping = aes(y = pourc,
                          x = bins_var,
                          fill = var_color),
            stat = "identity",
            color = "#000000",
            position = "dodge"
         ) +
         theme(
            axis.text.x = element_text(
               angle = 35,
               size = 5
            ),
            text = element_text(
               family = "serif"
            )
         ) + 
         labs(
            title = paste0("Distribution - ", translate_var(var)),
            x = translate_var(var), 
            y = "Proportion (en %)",
            fill = color
         )
      
      if (!is.null(color)) {
         label <- 
            paste(
               paste(
                  "Moy. ", 
                  color, 
                  " = ", 
                  mean_by_color$var_color, 
                  " : ", 
                  mean_by_color$V1
               ), 
               collapse = "\n"
            )
         
         p <- p + 
            annotate(
               geom = "text",
               x = ggplot_build(p)$layout$panel_params[[1]]$x.range[2] - 3, 
               y =  ggplot_build(p)$layout$panel_params[[1]]$y.range[2], 
               label = label,
               size = 4,
               family = "serif"
            )
         
      }
      
      return(p) 
   }


# Fonction qui "traduit" les variables en noms compréhensibles (créée pour les 
# noms des axes des graphiques)


translate_var <- function(vars) {
   
   # À compléter
   map <- list(
      dis_m3 = "Débit d'eau moyen (en m\U00B3/s)",
      rev_mc = "Volume du réservoir d'eau (en million m\U00B3)",
      dor_pc = "Degré de régulation du réservoir (en %)",
      lka_pc = "Pourcentage de terre qui correspond à des lacs (en %) (cse)",
      snd_pc = "Proportion de sable (en %)",
      snd_pc = "Proportion de sable (en %)",
      cly_pc = "Proportion d'argile (en %)",
      slt_pc = "Proportion de limon (en %)",
      soc_th = "Quantité moyenne de carbone dans les 5 premiers centimètres de sol (en tonnes/hectar)",
      for_pc = "Proportion occupée par les forêts",
      ero_kh = "Érosion causé par les rivières en kg/hectare/année de roche",
      wet_cl = "Classification des zones humides",
      riv_tc = "Volume de la rivière (en millier m\U00B3)",
      ria_ha = "Aire de la rivière (en hectares)",
      wet_cl = "Classification des zones humides",
      pre_mm = "Précipitations printanières moyennes (en mm) : ",
      wet_pc = "Proportion occupée par chacune la classe de terres humides : ",
      prm_pc = "Proportion de terre faisant partie du pergélisol (sol gelé en permanence)",
      gwt_cm = "Profondeur des nappes phréatiques (en cm)",
      slp_dg = "Inclinaison du terrain (en degrés)",
      lit_cl = "Classes lithologiques (type de roches)"
   )
   
   translation <- vapply(
      vars, 
      function(x) {
         res <- map[[substr(x, 1, 6)]]
         if (is.null(res)) {
            res <- x
         }
         res
      },
      character(1L)
   ) %>% unname()
   
   translation <- fifelse(
      substr(vars, 1, 6) %in% c("wet_pc", "pre_mm"),
      paste0(translation, substr(vars, nchar(vars) - 1, nchar(vars))),
      translation
   )
   
   translation
   
}


# Fonction pour installer, au besoin, une ou plusieurs librairies, pour ensuite
# les télécharger


inst_load_packages <- function(libs) {
   
   # Installer les librairies si elles ne sont pas dans les librairies installées
   s <- sapply(
      libs, 
      function(x) {
         if (!(x %in% utils::installed.packages())) 
            install.packages(x)
      }
   )
   
   # Télécharger les librairies
   s <- sapply(libs, function(x) do.call(library, list(x)))
}


# Fonction pour faire des tests de comparaison de moyenne par patron

t_test_by_patron <- function(dt, patron_nm) {
   
   # Créer une colonne contenant l'indicateur de si l'observation fait partie
   # du patron ou non
   set(x = dt, j = "patron_nm", value = dt[[patron_nm]])
   
   # Sélection des variables numériques et qui ne sont pas en majuscule
   n_num <- grep("patron", names(dt)[sapply(dt, is.numeric)], value = TRUE, invert = TRUE)
   n_num <- n_num[toupper(n_num) != n_num]
   n_num <- n_num[!(n_num %in% get_col_w_na(dt))]
   
   
   # Faire tous les test-t's
   t_test_res <- data.table(
      names = translate_var(n_num),
      p_value = sapply(
         n_num,
         function(x) {
            t.test(
               dt[patron_nm == 0][[x]],
               dt[patron_nm == 1][[x]]
            )[["p.value"]]
         }
      )
   )
   
   # Retirer la colonne créée
   set(x = dt, j = "patron_nm", value = NULL)
   
   # Retour de la fonction
   return(t_test_res)
}


# Fonction qui crée des NA's basé sur le type de données initial


get_na_by_type <- function(x) {
   res <- switch(
      typeof(x),
      integer = NA_integer_,
      double = NA_real_
   )
   
   if (is.factor(x)) {
      res <- factor(
         x = res, 
         levels = levels(x), 
         exclude = NA
      )
   }
   
   return(res)
}



# Fonction qui permet d'obtenir les noms de colonnes qui ont des NA's


get_col_w_na <- function(dt, na_pattern = NA) {
   
   names(unlist(lapply(
      X = dt, 
      FUN = function(x) {
         res <- if (is.na(na_pattern)) {sum(is.na(x))} else {sum(x == na_pattern)}
         if(res != 0) {res} else {NULL}
      }
   )))
   
}

# Fonction qui modifie la classe -999 de la variable 


mod_missing_wet_cl_cmj <- function(dt) {
   
   dt[, wet_cl_cmj := factor(wet_cl_cmj, unique(c(0, levels(dt$wet_cl_cmj))))]
   
   dt[wet_cl_cmj == -999, wet_cl_cmj := "0"]
   
}


# Fonction pour afficher l'importance des variables 

show_imp_var <- function(mod_obj, nb_var = 10) {
   
   stopifnot("variable.importance" %in% names(mod_obj))
   
   var_imp <- sort(mod_obj$variable.importance, decreasing = TRUE)[1:nb_var]
   
   ggplot() +
      geom_bar(
         mapping = aes(
            x = var_imp,
            y = factor(
               translate_var(names(var_imp)), 
               levels = unique(translate_var(names(var_imp)))
            ), 
         ),
         stat = "identity",
         orientation = "y"
      ) +
      labs(
         x = "Importance de la variable",
         y = "Variables"
      ) + 
      theme(
         text = element_text(family = "Times New Roman")
      )
   
   
}

