# simCFA — L'analyse factorielle confirmatoire

**Spécifier un modèle de mesure, l'estimer avec lavaan, puis juger son ajustement, sa fiabilité et sa validité.**

simCFA est une application pédagogique **R/Shiny**, en français, qui guide pas à pas une analyse factorielle confirmatoire (AFC) : choisir les données, définir le modèle, estimer, interpréter. On peut simuler des données à partir d'une structure connue, choisir un jeu de données classique ou importer son propre fichier.

- [Utiliser l'application en ligne](https://shiny.dcstats.org/simCFA/)
- [Consulter le manuel pédagogique](https://dcstats.org/simCFA/)

## Objectifs pédagogiques

- distinguer l'AFC de l'analyse factorielle exploratoire : un modèle posé *a priori*;
- spécifier saturations, covariances entre facteurs et entre erreurs, contraintes et paramètres fixés;
- vérifier les données : descriptives, corrélations, normalité univariée et multivariée;
- juger l'ajustement global et local, et comparer des modèles concurrents;
- évaluer la fiabilité (alpha, oméga, AVE) et la validité convergente et discriminante (Fornell-Larcker, HTMT, HTMT2).

## Fonctionnalités

| Volet | Contenu |
| --- | --- |
| Introduction | Concepts de base, équations, séquence d'analyse. |
| Données | Simulation (facteurs, indicateurs, n, données continues ou ordinales), jeux classiques ou fichier importé (CSV, Excel, SPSS, Stata…); statistiques descriptives et normalité. |
| Modèle | Spécification, contraintes, covariances entre erreurs, paramètres fixes, diagramme du modèle. |
| Estimation | Estimateurs de lavaan (ML, robustes, WLSMV pour les items ordinaux), FIML pour les données manquantes, bootstrap. |
| Résultats | Ajustement, paramètres, résidus et indices de modification, comparaisons de modèles, fiabilité et validité, scores factoriels. |
| Rapport et code | Rapport téléchargeable et code R/lavaan reproductible. |

## Exécuter l'application localement

```r
install.packages(c("shiny", "bslib", "shinyjs", "colourpicker", "DT", "dplyr", "purrr", "ggplot2",
                   "lavaan", "semPlot", "nortest", "haven", "rio", "openxlsx"))
shiny::runApp("simCFA")
```

## Licence

GPL-3.0. Voir le fichier `LICENSE`.

## Auteur

Daniel Coulombe, Ph.D. — <https://dcstats.org>
