# Projet STT-7335

## Estimation du débit d'eau moyen pour un bassin versant avec des méthodes d'ensemble

Université Laval

Équipe 4:

 - Hangsin, Andrea
 - Helaiem, Amélie
 - Jocelyn Pellerin
 - Lepage, Alexandre
 - Ouedraogo, Angelo

Session d'Hiver 2023

----

### Description du projet:

Nous utilisons les données de l'HydroAtlas, provenant du site de [HydroSheds](https://www.hydrosheds.org/hydroatlas)
pour tenter de prédire le débit moyen des rivières pour un bassin versant donné.
L'objectif étant de tester les méthodes apprises dans le cadre du cours STT-7335
sur un véritable jeu de données.

Nous focalisons nos efforts dans la province du Québec en prenant les données 
nord-américaine et en rognant les fichiers de formes (shapefiles) autour de la 
province.
Le jeu de données ainsi créé contient $162\,507$ observations et $295$ variables 
disponibles.
Chaque observations du jeu de données correspond à un bassin versant et les 
variables représentent des données utiles pour effectuer des études 
hydrologiques, telles que les précipitations, le volume d'eau en amont, 
la pente hydrique, etc.
La variable d'intérêt est la suivante: `dis_m3_pyr`. Celle-ci représente la 
moyenne annuelle des débits d'eau en $m^3$/sec. 
Le débit minimal observé est de $0.075 m^3/s$ et il peut atteindre 
des valeurs aussi élevées que $12\,343 m^3/s$, avec un moyenne de $25 m^3/s.$

Les variables explicatives d'intérêt ont été sélectionnées manuellement en 
considérant les variables nécessaires pour calculer
l'équation de [Manning](https://fr.wikipedia.org/wiki/Formule_de_Manning-Strickler#:~:text=La%20formule%20de%20Manning%20est,sont%20gouvern%C3%A9s%20par%20la%20gravit%C3%A9).
et quelques autres supplémentaires qui suivent une logique similaire.
$66$ variables ont ainsi été sélectionnées.
Par la suite, les variables offrant peu ou pas d'information (variance nulle) 
ont été retirées, nous laissant avec $56$ variables.

Pour en apprendre d'avantage sur les données utilisées, consultez le site internet
d'HydoSheds à l'adresse suivante:
https://data.hydrosheds.org/file/technical-documentation/HydroATLAS_TechDoc_v10_1.pdf

Les variables constituant les données sont décrites à cette adresse:
https://data.hydrosheds.org/file/technical-documentation/RiverATLAS_Catalog_v10.pdf


----


### Contenu du répertoire

- `Data`: Dossier destiné à l'entreposage des données;
- `Doc`: Où la documentation du projet est entreposée, y compris le rapport 
final;
- `Out`: Dossier destiné à aux sorties de modèle;
- `R`: Tout le code R est enregistré ici;
- `.gitignore`: Un fichier utilisé pour spécifier quels types de documents ne 
seront pas enregistrés sur le serveur en ligne;
- `.Rhistory`: Un fichier cache pour vos sessions R;
- `Projet_STT7335.Rproj`: Fichier .Rproj --> C'est le projet R qui lie tous les 
éléments du répertoire ensemble. 
(**On devrait toujours ouvrir ce fichier avant de travailler en R!**)
