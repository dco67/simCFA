library(shiny)
library(bslib)
library(DT)
library(rio)
library(lavaan)
library(semPlot)
library(ggplot2)
library(colourpicker)
library(nortest)

library(dplyr)
library(purrr)

library(shinyjs)

library(haven)     
library(openxlsx) 

# JavaScript pour capturer le chemin complet
jscode <- "
function handleFileSelect(input) {
  if (input.files && input.files[0]) {
    var file = input.files[0];
    // Pour Chrome et Firefox
    if (file.webkitRelativePath) {
      input.setAttribute('data-full-path', file.webkitRelativePath);
    }
    // Pour IE et Edge
    else if (file.name) {
      input.setAttribute('data-full-path', file.name);
    }
    Shiny.setInputValue('fullPath', file.name);
  }
}
"

# Configuration globale pour les tableaux DT
dt_options <- list(
  autoWidth = FALSE,
  pageLength = 10,
  scrollX = TRUE,
  dom = 'tip',
  processing = TRUE,
  searchHighlight = TRUE,
  deferRender = TRUE
)

# CSS pour standardiser les contrôles et leur alignement
css_standardization <- "
  /* Standardisation des entrées de formulaire */
  .form-control, .selectize-input, .btn {
    min-height: 38px;  /* Hauteur standard pour les entrées */
    font-size: 15px;  /* Taille de police uniforme */
  }
  
  /* Standardisation des labels */
  label {
    font-size: 15px;
    font-weight: 500;
    margin-bottom: 6px;
    display: block;
  }
  
  /* Standardisation des sections */
  .card-header {
    padding: 12px 16px;
    font-size: 16px;
    font-weight: 500;
  }
  
  /* Standardisation des espaces entre contrôles */
  .shiny-input-container {
    margin-bottom: 15px;
  }
  
  /* Taille uniforme pour les boutons d'action */
  .action-button, .btn {
    min-width: 0;
  }
  
  /* Alignement des checkboxes */
  .checkbox-inline, .radio-inline {
    margin-right: 15px;
    padding-top: 0;
  }
  
  /* Meilleur alignement pour les contrôles en ligne */
  .checkbox-inline input, .radio-inline input {
    margin-right: 5px;
  }
  
  /* Style cohérent pour les onglets */
  .nav-tabs .nav-link {
    font-size: 15px;
    padding: 8px 16px;
  }
  
  /* Standardisation des marges pour les sections */
  .card {
    margin-bottom: 20px;
  }
  
  /* Meilleur espacement dans les groupes d'options */
  .checkbox-group, .radio-group {
    margin-bottom: 15px;
  }
  
  /* Standardisation des sliders */
  .irs {
    height: 50px;
  }
  
  /* Taille cohérente pour les icônes */
  .fa {
    font-size: 15px;
  }
  
  /* Alignement des contrôles dans les colonnes */
  .col-sm-6, .col-md-6, .col-lg-6 {
    padding: 0 10px;
  }
  
  /* Améliorations pour l'affichage mobile */
  @media (max-width: 768px) {
    .form-control, .selectize-input, .btn {
      height: 42px;  /* Légèrement plus grand sur mobile */
    }
    
    label {
      font-size: 15px;
    }
  }
"

# Fonction pour générer des loadings aléatoires dans une étendue donnée
generate_random_loadings <- function(n_loadings, min_loading = 0.3, max_loading = 0.9) {
  runif(n_loadings, min = min_loading, max = max_loading)
}

source("cfa_core.R")

# Fonction pour obtenir le chemin du fichier de stockage
save_recent_files <- function(files) {
  storage_path <- get_storage_path()
  tryCatch({
    if (!is.data.frame(files)) {
      files <- as.data.frame(files, stringsAsFactors = FALSE)
    }
    writeLines(jsonlite::toJSON(files, pretty = TRUE), storage_path)
  }, error = function(e) {
    warning("Impossible de sauvegarder la liste des fichiers récents:", e$message)
  })
}

load_recent_files <- function() {
  storage_path <- get_storage_path()
  
  if (file.exists(storage_path)) {
    tryCatch({
      recent <- jsonlite::fromJSON(storage_path)
      
      if (is.data.frame(recent) && nrow(recent) > 0) {
        existing_files <- file.exists(recent$orig_path)
        recent <- recent[existing_files, ]
        return(recent)
      }
      return(data.frame(
        name = character(),
        path = character(),
        orig_path = character(),
        date = character(),
        stringsAsFactors = FALSE
      ))
    }, error = function(e) {
      return(data.frame(
        name = character(),
        path = character(),
        orig_path = character(),
        date = character(),
        stringsAsFactors = FALSE
      ))
    })
  } else {
    return(data.frame(
      name = character(),
      path = character(),
      orig_path = character(),
      date = character(),
      stringsAsFactors = FALSE
    ))
  }
}


safe_skewness <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  if (n < 3) return(NA)
  m <- mean(x)
  s <- sd(x)
  sum((x - m)^3)/(n * s^3)
}

safe_kurtosis <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  if (n < 3) return(NA)
  m <- mean(x)
  s <- sd(x)
  (sum((x - m)^4)/(n * s^4)) - 3
}

# Fonction utilitaire pour reconstruire une matrice symétrique complète
make_symmetric_matrix <- function(lower_tri) {
  if(is.matrix(lower_tri)) return(lower_tri)  # Si déjà une matrice complète
  
  # Extraire les noms de variables
  var_names <- attr(lower_tri, "Dimnames")[[1]]
  n <- length(var_names)
  
  # Créer une matrice vide
  full_matrix <- matrix(0, n, n)
  colnames(full_matrix) <- rownames(full_matrix) <- var_names
  
  # Remplir la matrice
  for(i in 1:n) {
    for(j in 1:i) {
      full_matrix[i,j] <- full_matrix[j,i] <- lower_tri[i,j]
    }
  }
  return(full_matrix)
}


# Fonction pour calculer le ratio HTMT manuellement
source("cfa_validity.R")

# ── Traduction lavaan → Stata sem/gsem ───────────────────
lavaan_to_stata <- function(syntax, estimator="ml", ordinal_vars=NULL,
                             missing="listwise", std_lv=FALSE,
                             n_factors=1, source_info=list()) {
  lines <- strsplit(trimws(syntax), "\n")[[1]]
  lines <- lines[nzchar(trimws(lines))]

  has_ordinal <- length(ordinal_vars) > 0
  cmd         <- if (has_ordinal) "gsem" else "sem"

  meas_lines <- character()
  cov_lines  <- character()
  err_lines  <- character()

  for (l in lines) {
    l <- trimws(l)
    if (grepl("=~", l)) {
      parts  <- strsplit(l, "=~")[[1]]
      factor <- trimws(parts[1])
      items  <- trimws(strsplit(trimws(parts[2]), "\\+")[[1]])
      items  <- gsub("^[0-9a-zA-Z_.]+\\*", "", items)
      if (has_ordinal) {
        items_ord  <- items[items %in% ordinal_vars]
        items_cont <- items[!items %in% ordinal_vars]
        if (length(items_cont) > 0)
          meas_lines <- c(meas_lines,
            sprintf("(%s -> %s)", factor, paste(items_cont, collapse=" ")))
        if (length(items_ord) > 0)
          meas_lines <- c(meas_lines,
            sprintf("(%s -> %s, family(ordinal) link(logit))",
                    factor, paste(items_ord, collapse=" ")))
      } else {
        meas_lines <- c(meas_lines,
          sprintf("(%s -> %s)", factor, paste(items, collapse=" ")))
      }
    } else if (grepl("~~", l)) {
      parts <- strsplit(l, "~~")[[1]]
      lhs   <- trimws(gsub("[0-9a-zA-Z_.]+\\*", "", parts[1]))
      rhs   <- trimws(gsub("[0-9a-zA-Z_.]+\\*", "", parts[2]))
      if (lhs == rhs) next
      if (grepl("^F[0-9]+$", lhs) && grepl("^F[0-9]+$", rhs)) {
        cov_lines <- c(cov_lines, sprintf("cov(%s*%s)", lhs, rhs))
      } else {
        err_lines <- c(err_lines, sprintf("cov(e.%s*e.%s)", lhs, rhs))
      }
    }
  }

  all_paths   <- c(meas_lines, err_lines)
  n_paths     <- length(all_paths)
  stata_lines <- character()
  for (i in seq_along(all_paths)) {
    sep <- if (i < n_paths || length(cov_lines) > 0) " ///" else ","
    stata_lines <- c(stata_lines, paste0("    ", all_paths[i], sep))
  }

  method_stata <- switch(tolower(estimator),
    ml="ml", mlb="ml", mlr="mlr", dwls="wls", uls="uls", wls="wls", "ml")

  opts <- c(sprintf("method(%s)", method_stata))
  if (std_lv)          opts <- c(opts, "standardize")
  if (missing == "fiml") opts <- c(opts, "missing(ml)")

  factor_names <- unique(gsub(".*\\(([A-Za-z0-9_]+) ->.*", "\\1", meas_lines))
  opts <- c(opts, sprintf("latent(%s)", paste(factor_names, collapse=" ")))

  # ── Initialiser out ───────────────────────────────────
  out <- c(
    "* ── Analyse Factorielle Confirmatoire — Stata ──────────",
    "* Généré par simCFA",
    "",
    "* ── Chargement des données ──────────────────────────"
  )

  # Chargement selon la source
  if (!is.null(source_info$method) && nchar(source_info$method) > 0) {
    if (source_info$method == "file") {
      out <- c(out,
        sprintf("* use \"%s\", clear", source_info$nom),
        "* (adaptez le chemin complet)")
    } else if (source_info$method == "real") {
      out <- c(out,
        sprintf("* Source : %s (package R %s)", source_info$nom, source_info$pkg),
        "* Exportez depuis R : haven::write_dta(data, \"donnees.dta\")",
        sprintf("* use \"donnees.dta\", clear  // données %s", source_info$nom))
    } else {
      out <- c(out,
        "* Données simulées — exportez depuis R :",
        "* haven::write_dta(data, \"donnees_simulees.dta\")",
        "* use \"donnees_simulees.dta\", clear")
    }
  } else {
    out <- c(out,
      "* use \"chemin/vers/vos_donnees.dta\", clear",
      "*",
      "* Depuis un fichier CSV :",
      "* import delimited \"chemin/vers/donnees.csv\", clear",
      "*",
      "* Depuis un fichier Excel :",
      "* import excel \"chemin/vers/donnees.xlsx\", firstrow clear",
      "*",
      "* Depuis SPSS (.sav) :",
      "* import spss \"chemin/vers/donnees.sav\", clear")
  }

  out <- c(out, "")

  if (has_ordinal) {
    out <- c(out,
      "* Variables ordinales détectées → commande gsem",
      sprintf("* Variables ordinales : %s", paste(ordinal_vars, collapse=", ")),
      "")
  }

  if (tolower(estimator) == "mlb") {
    out <- c(out,
      "* Note : Bootstrap ML (lavaan mlb) → utiliser bootstrap en Stata :",
      sprintf("%s [commande ci-dessous], puis : bootstrap, reps(1000): sem ...", cmd),
      "")
  }

  out <- c(out, paste0(cmd, " ///"))
  out <- c(out, stata_lines)

  if (length(cov_lines) > 0) {
    for (i in seq_along(cov_lines)) {
      sep <- if (i < length(cov_lines)) " ///" else ","
      out <- c(out, paste0("    ", cov_lines[i], sep))
    }
  }

  out <- c(out, paste0("    ", paste(opts, collapse=" ")))
  out <- c(out, "")
  out <- c(out, "* Résultats standardisés")
  out <- c(out, "estat stdize")
  out <- c(out, "")
  out <- c(out, "* Indices d'ajustement")
  out <- c(out, "estat gof, stats(all)")
  out <- c(out, "")
  out <- c(out, "* Scores factoriels")
  out <- c(out, sprintf("predict %s, latent",
                        paste(tolower(factor_names), collapse=" ")))
  out <- c(out, "")
  out <- c(out, "* ── Diagramme du modèle ─────────────────────────────")

  if (has_ordinal) {
    out <- c(out,
      "* Note : gsem ne supporte pas semplot directement.",
      "* Utilisez plutôt le Builder graphique : Statistics > SEM > Builder",
      "* ou exportez le diagramme manuellement depuis le SEM Builder.")
  } else {
    out <- c(out,
      "* Diagramme de base (post-estimation)",
      "semplot",
      "",
      "* Avec coefficients standardisés",
      "semplot, standardized",
      "",
      "* Sans les variances (plus lisible)",
      "semplot, novariances",
      "",
      "* Avec les intervalles de confiance",
      "semplot, ci")
  }

  if (length(ordinal_vars) > 0) {
    out <- c(out, "",
      "* ── Déclaration des variables ordinales ─────────────",
      "* encode ou recode selon vos catégories, puis :",
      sprintf("* foreach v of varlist %s {", paste(ordinal_vars, collapse=" ")),
      "*     label values `v' `v'_lbl",
      "* }")
  }

  paste(out, collapse="\n")
}

# ── Exemple d'utilisation ────────────────────────────────
# library(lavaan)
# model <- '
#   F1 =~ x1 + x2 + x3
#   F2 =~ x4 + x5 + x6
#   F3 =~ x7 + x8 + x9
# '
# fit <- cfa(model, data=HolzingerSwineford1939)
# res <- cfa_validity(fit)
# print(res)               # tableau complet
# print(res, type="fornell")  # matrice seule
# summary(res)             # synthèse avec évaluation
# plot(res, type="indices")   # graphique des indices
# plot(res, type="fornell")   # heatmap Fornell-Larcker
# as.data.frame(res)       # pour export


source("cfa_learning.R")
source("cfa_datasets.R")
