# Catalogue des copies locales : aucune connexion ni installation de psychTools
# n'est nécessaire au chargement. Préparation et références : data/exemples/.
cfa_example_catalog <- function() {
  catalog <- list(
    HolzingerSwineford1939 = list(
      label="Holzinger–Swineford — 301 élèves, 9 tests cognitifs",
      desc="Neuf tests cognitifs administrés à 301 élèves de deux écoles; données classiques de Holzinger et Swineford (1939).",
      pkg="lavaan", estimator="ml", ordered=character(),
      factors=list(Visuel=paste0("x",1:3),Texte=paste0("x",4:6),Vitesse=paste0("x",7:9)),
      question="Trois aptitudes corrélées représentent-elles les neuf tests cognitifs ?",
      dictionary=c("x1 : perception visuelle; x2 : cubes; x3 : losanges",
        "x4 : compréhension de paragraphes; x5 : phrases; x6 : sens des mots",
        "x7 : additions; x8 : comptage de points; x9 : discrimination de lettres"),
      caution="Indicateurs traités comme continus. Le modèle est une hypothèse à évaluer : examinez les résidus et les saturations, même si certains indices paraissent satisfaisants. Le fichier conserve aussi les variables descriptives d’origine, qui ne sont pas des indicateurs du modèle.",
      preparation="Copie complète du fichier de lavaan; aucun recodage ni retrait de ligne.",
      reference="Holzinger, K. J. et Swineford, F. (1939). A study in factor analysis: The stability of a bi-factor solution. Supplementary Educational Monographs, 48.",
      url="https://search.r-project.org/CRAN/refmans/lavaan/html/HolzingerSwineford1939.html"),
    PoliticalDemocracy = list(
      label="Démocratie politique — 75 pays, 11 indicateurs",
      desc="Indicateurs d’industrialisation et de démocratie en 1960 et 1965; exemple de Bollen (1989), 75 pays.",
      pkg="lavaan", estimator="ml", ordered=character(),
      factors=list(Democratie60=paste0("y",1:4),Democratie65=paste0("y",5:8),Industrialisation=paste0("x",1:3)),
      question="Les indicateurs représentent-ils trois dimensions distinctes mais corrélées ?",
      dictionary=c("y1–y4 : liberté de la presse, opposition politique, équité des élections et efficacité du parlement en 1960",
        "y5–y8 : mêmes dimensions en 1965", "x1–x3 : industrialisation en 1960"),
      caution="AFC à trois facteurs corrélés, sans les régressions ni les covariances résiduelles du modèle structurel complet de Bollen. Les mesures répétées peuvent partager une spécificité. Petit échantillon et indicateurs traités comme continus : discutez ces choix.",
      preparation="Copie intégrale de lavaan, sans transformation.",
      reference="Bollen, K. A. (1989). Structural Equations with Latent Variables. Wiley.",
      url="https://search.r-project.org/CRAN/refmans/lavaan/html/PoliticalDemocracy.html"),
    lsat6 = list(
      label="LSAT 6 — 1 000 réponses, 5 items binaires",
      desc="Section 6 (classification de figures) du LSAT, 1 000 observations; exemple classique de Bock et Lieberman (1970).",
      pkg="psych", estimator="wlsmv", ordered=paste0("Q",1:5),
      factors=list(Aptitude=paste0("Q",1:5)),
      question="Un seul facteur suffit-il à représenter les cinq réponses binaires ?",
      dictionary=c("Q1–Q5 : cinq items de la section 6", "0 = réponse incorrecte; 1 = réponse correcte"),
      caution="Le fichier individuel est développé à partir des effectifs des profils de réponses publiés : son ordre ne désigne pas les personnes d’origine. Les items sont déclarés ordinaux et WLSMV est proposé. Comparez ensuite avec la section 7, analysée séparément.",
      preparation="Copie de psych::lsat6, sans modification des réponses; fichier à 1 000 lignes dérivé de la table des profils.",
      reference="Bock, R. D. et Lieberman, M. (1970). Fitting a response model for n dichotomously scored items. Psychometrika, 35, 179–197.",
      url="https://search.r-project.org/CRAN/refmans/psych/html/bock.table.html"),
    lsat7 = list(
      label="LSAT 7 — 1 000 réponses, 5 items binaires",
      desc="Section 7 (débat) du LSAT, 1 000 observations; second exemple de Bock et Lieberman (1970).",
      pkg="psych", estimator="wlsmv", ordered=paste0("Q",1:5),
      factors=list(Aptitude=paste0("Q",1:5)),
      question="Le modèle unidimensionnel décrit-il aussi bien cette autre série de cinq items ?",
      dictionary=c("Q1–Q5 : cinq items de la section 7", "0 = réponse incorrecte; 1 = réponse correcte"),
      caution="Les lignes proviennent du développement d’une table de profils; aucun identifiant individuel d’origine n’est disponible. Ne fusionnez pas les lignes des sections 6 et 7 comme si elles appartenaient aux mêmes personnes. Items ordinaux, estimation WLSMV.",
      preparation="Copie de psych::lsat7, sans modification des réponses; fichier à 1 000 lignes dérivé de la table des profils.",
      reference="Bock, R. D. et Lieberman, M. (1970). Fitting a response model for n dichotomously scored items. Psychometrika, 35, 179–197.",
      url="https://search.r-project.org/CRAN/refmans/psych/html/bock.table.html"),
    ability = list(
      label="SAPA / ability — 1 525 personnes, 16 items cognitifs",
      desc="Seize items cognitifs binaires du projet SAPA : raisonnement, séries de lettres, matrices et rotations spatiales (collecte d’août 2012).",
      pkg="psychTools", estimator="wlsmv",
      ordered=c(paste0("reason.",c(4,16,17,19)),paste0("letter.",c(7,33,34,58)),paste0("matrix.",c(45,46,47,55)),paste0("rotate.",c(3,4,6,8))),
      factors=list(Raisonnement=paste0("reason.",c(4,16,17,19)),Lettres=paste0("letter.",c(7,33,34,58)),Matrices=paste0("matrix.",c(45,46,47,55)),Rotation=paste0("rotate.",c(3,4,6,8))),
      question="Quatre aptitudes corrélées rendent-elles compte des réponses aux seize items ?",
      dictionary=c("reason.* : raisonnement verbal; letter.* : séries de lettres",
        "matrix.* : raisonnement matriciel; rotate.* : rotation spatiale", "0 = incorrect; 1 = correct; cellules vides = données manquantes"),
      caution="La structure à quatre facteurs corrélés est proposée comme point de départ; aucun facteur général de second ordre n’est ajouté. WLSMV et cas complets : des lignes sont exclues, sans imputation des réponses manquantes. La documentation signale des particularités dans certains items de matrices.",
      preparation="Copie des 1 525 × 16 réponses de psychTools::ability, sans imputation ni recodage.",
      reference="Condon, D. M. et Revelle, W. (2014). The International Cognitive Ability Resource: Development and initial validation of a public-domain measure. Intelligence, 43, 52–64. Voir aussi la notice ability pour l’échantillon SAPA.",
      url="https://search.r-project.org/CRAN/refmans/psychTools/html/ability.html"),
    msq_affect = list(
      label="MSQ — 3 032 personnes, 20 items d’affect",
      desc="Première passation du MSQ pour 3 032 participants : vingt adjectifs associés à l’affect positif et négatif dans la documentation du jeu.",
      pkg="psychTools", estimator="wlsmv",
      ordered=c("active","alert","attentive","determined","enthusiastic","excited","inspired","interested","proud","strong","afraid","ashamed","distressed","guilty","hostile","irritable","jittery","nervous","scared","upset"),
      factors=list(AffectPositif=c("active","alert","attentive","determined","enthusiastic","excited","inspired","interested","proud","strong"),AffectNegatif=c("afraid","ashamed","distressed","guilty","hostile","irritable","jittery","nervous","scared","upset")),
      question="Deux facteurs corrélés d’affect positif et négatif représentent-ils les réponses à ces adjectifs ?",
      dictionary=c("AffectPositif : actif, alerte, attentif, déterminé, enthousiaste, excité, inspiré, intéressé, fier, fort (traductions indicatives)",
        "AffectNegatif : apeuré, honteux, en détresse, coupable, hostile, irritable, agité, nerveux, effrayé, bouleversé (traductions indicatives)",
        "Réponses à propos du moment présent : 0 = pas du tout; 1 = un peu; 2 = modérément; 3 = beaucoup"),
      caution="Sous-ensemble du MSQ, selon les clés PA/NA documentées; il ne s’agit pas d’une nouvelle passation indépendante du PANAS. Seule la première occasion est conservée pour ne pas traiter les mesures répétées comme des personnes supplémentaires. Plusieurs études et conditions sont réunies; la structure simple reste à examiner.",
      preparation="Sélection exacte msqR$time == 1, puis des vingt colonnes PA/NA. 3 032 lignes conservées, ordre d’origine; aucune imputation, inversion ni standardisation.",
      reference="Revelle, W. et Anderson, K. J. (1998). Personality, motivation and cognitive performance. Données du Personality, Motivation and Cognition Laboratory, Northwestern University. Clés PA/NA documentées dans msqR; Watson, Clark et Tellegen (1988).",
      url="https://search.r-project.org/CRAN/refmans/psychTools/html/msqR.html"),
    bfi = list(
      label="BFI / SAPA — 2 800 personnes, 25 items de personnalité",
      desc="Vingt-cinq items de personnalité du projet SAPA, 2 800 répondants, fichier bfi du package psych.",
      pkg="psych", estimator="wlsmv", ordered=paste0(rep(c("A","C","E","N","O"),each=5),1:5),
      factors=list(Agreabilite=paste0("A",1:5),Conscience=paste0("C",1:5),Extraversion=paste0("E",1:5),Neuroticisme=paste0("N",1:5),Ouverture=paste0("O",1:5)),
      question="Les vingt-cinq items présentent-ils la structure attendue des cinq dimensions de personnalité ?",
      dictionary=c("A1–A5 : agréabilité; C1–C5 : conscience; E1–E5 : extraversion; N1–N5 : neuroticisme; O1–O5 : ouverture",
        "Réponses de 1 (très inexact) à 6 (très exact); cellules vides = données manquantes",
        "Items orientés en sens inverse selon les clés : A1, C4, C5, E1, E2, O2, O5; aucun recodage effectué"),
      caution="Items déclarés ordinaux, WLSMV et cas complets proposés. Les items inversés sont conservés dans leur codage d’origine : des saturations négatives peuvent être attendues. Avant de former un score, vérifiez les clés. Ces items SAPA ne constituent pas le Big Five Inventory d’Oliver John.",
      preparation="Vingt-cinq colonnes A1–O5 de psych::bfi, toutes les 2 800 lignes; variables démographiques retirées; valeurs manquantes conservées.",
      reference="Revelle, W., Wilt, J. et Rosenthal, A. (2010). Personality and cognition: The personality-cognition link. Items issus de l’International Personality Item Pool via SAPA; voir la notice bfi.",
      url="https://search.r-project.org/CRAN/refmans/psych/html/bfi.html")
  )
  for(id in names(catalog)) {
    m <- catalog[[id]]
    m$syntax <- paste(vapply(names(m$factors), function(f)
      paste(f,"=~",paste(m$factors[[f]],collapse=" + ")), character(1)),collapse="\n")
    m$file <- file.path("data","exemples",paste0(id,".csv"))
    catalog[[id]] <- m
  }
  catalog
}

cfa_example_choices <- function() {
  m <- cfa_example_catalog()
  setNames(names(m),vapply(m,`[[`,character(1),"label"))
}

cfa_load_example <- function(id) {
  meta <- cfa_example_catalog()[[id]]
  if(is.null(meta)) stop("Jeu de données inconnu.")
  if(!file.exists(meta$file)) stop("Fichier d’exemple absent. Copiez aussi le dossier data/exemples de l’archive.")
  read.csv(meta$file,check.names=FALSE,na.strings="",stringsAsFactors=FALSE)
}

cfa_example_notice <- function(id) {
  m <- cfa_example_catalog()[[id]]
  paste(c(m$label,m$desc,"",m$question,"",m$dictionary,"",m$caution,"",
    paste("Préparation :",m$preparation),paste("Source :",m$pkg),m$reference,m$url,"",
    paste("Estimateur proposé :",toupper(m$estimator)),"Modèle de départ :",m$syntax),collapse="\n")
}
