# _common.R — chargé au début de chaque chapitre.
# Tous les nombres du manuel sont calculés ici avec le moteur de l'application
# (copie dans moteur/) : aucun résultat n'est recopié à la main.
# Les analyses de référence sont mises en cache dans _reference.rds; le cache est
# recalculé dès qu'un fichier du moteur, un fichier de données, la version de
# lavaan ou REF_VERSION (plus bas) change.

suppressPackageStartupMessages({
  library(lavaan)
  library(knitr)
  library(ggplot2)
})

knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.align = "center", dev = "ragg_png", dpi = 200,
                      fig.width = 7, fig.height = 4.2, out.width = "88%")
options(knitr.kable.NA = "—", OutDec = ".")

RACINE <- normalizePath(".")
MOTEUR_DIR <- file.path(RACINE, "moteur")
DATA_DIR <- file.path(RACINE, "data")

# ── Moteur de l'application ──────────────────────────────────────────────
if (!exists("moteur", inherits = FALSE) || !is.environment(moteur)) {
  moteur <- new.env(parent = globalenv())
  local({
    owd <- setwd(MOTEUR_DIR); on.exit(setwd(owd))
    for (f in c("cfa_core.R", "cfa_validity.R", "cfa_learning.R", "cfa_datasets.R"))
      sys.source(f, envir = moteur, keep.source = FALSE)
    moteur$cfa_cfadiag()          # charge cfadiag (vendor/) pendant que le dossier est actif
  })
  moteur$cfa_load_example <- function(id) {
    read.csv(file.path(DATA_DIR, paste0(id, ".csv")), check.names = FALSE,
             na.strings = "", stringsAsFactors = FALSE)
  }
}
CATALOGUE <- moteur$cfa_example_catalog()

# ── Mise en forme à la française ─────────────────────────────────────────
NBSP <- " "   # espace fine insécable
v <- function(x, d = 3) {
  out <- ifelse(is.na(x), "—",
    formatC(round(x, d), format = "f", digits = d, decimal.mark = ",", big.mark = NBSP))
  sub("^-", "−", out)
}
vm <- function(x, d = 3) gsub(",", "{,}", gsub("−", "-", v(x, d)))   # dans $...$
ent <- function(x) formatC(x, format = "d", big.mark = NBSP)
pct <- function(x, d = 0) paste0(v(100 * x, d), " %")
vp <- function(p) ifelse(is.na(p), "—", ifelse(p < .001, "< 0,001", v(p, 3)))
tab <- function(df, digits = 3, ...) {
  al <- ifelse(vapply(df, is.numeric, logical(1)), "r", "l")
  df[] <- lapply(df, function(x) if (is.numeric(x) && !is.integer(x)) v(x, digits) else
    if (is.integer(x)) ent(x) else x)
  knitr::kable(df, align = al, row.names = FALSE, ...)
}

# ── Estimation comme dans l'application ─────────────────────────────────
options_app <- function(estimator = "ml", missing = "listwise", std.lv = FALSE,
                        means = FALSE, ordered = character()) {
  o <- list(estimator = estimator, missing = missing, std.lv = std.lv,
            meanstructure = missing == "fiml" || means)
  if (length(ordered)) o$ordered <- ordered
  o
}
ajuster <- function(syntax, data, opts) moteur$cfa_fit(syntax, data, opts)$fit
ajuster_exemple <- function(id, estimator = NULL, missing = "listwise", std.lv = FALSE,
                            data = NULL, syntax = NULL) {
  m <- CATALOGUE[[id]]
  if (is.null(data)) data <- moteur$cfa_load_example(id)
  vars <- unique(unlist(m$factors))
  est <- if (is.null(estimator)) m$estimator else estimator
  opts <- options_app(est, missing, std.lv, ordered = if (est %in% c("ml", "mlr")) character() else m$ordered)
  ajuster(if (is.null(syntax)) m$syntax else syntax, data[vars], opts)
}
indice <- function(tab_fit, nom) tab_fit$valeur[tab_fit$indice == nom]
version <- function(tab_fit, nom) tab_fit$version[tab_fit$indice == nom]
saturations <- function(fit) {
  s <- lavaan::standardizedSolution(fit); s <- s[s$op == "=~", ]
  data.frame(Facteur = s$lhs, Item = s$rhs, Std = s$est.std, stringsAsFactors = FALSE)
}
versions_indices <- function(fit) {
  fm <- lavaan::fitMeasures(fit)
  g <- function(n) if (n %in% names(fm)) unname(fm[[n]]) else NA_real_
  data.frame(Version = c("Standard", "Scaled", "Robust"),
             CFI = c(g("cfi"), g("cfi.scaled"), g("cfi.robust")),
             RMSEA = c(g("rmsea"), g("rmsea.scaled"), g("rmsea.robust")))
}
snapshot <- function(fit) {
  pt <- lavaan::parTable(fit); pt <- pt[pt$op == "=~", ]
  list(fit = fit, factor_vars = split(pt$rhs, factor(pt$lhs, levels = unique(pt$lhs))))
}

# ── Analyses de référence (cache) ────────────────────────────────────────
calculer_reference <- function() {
  R <- list()
  ## Holzinger–Swineford, modèle de référence
  hs <- ajuster_exemple("HolzingerSwineford1939")
  bund <- moteur$cfa_diagnostic_bundle(hs)
  R$hs <- list(
    n = lavaan::lavInspect(hs, "nobs"), npar = lavaan::lavInspect(hs, "npar"),
    fit = bund$fit, signaux = bund$signals, htmt2 = bund$htmt2,
    sat = saturations(hs), cor = lavaan::lavInspect(hs, "cor.lv"),
    val = moteur$cfa_validity(hs),
    mi = { mi <- lavaan::modindices(hs, sort. = TRUE, minimum.value = 10)
           mi[, c("lhs", "op", "rhs", "mi", "epc", "sepc.all")] },
    resid = lavaan::lavResiduals(hs, type = "cor.bentler")$cov,
    resid_z = lavaan::lavResiduals(hs)$cov.z,
    pe = lavaan::parameterEstimates(hs, standardized = TRUE),
    desc = { X <- moteur$cfa_load_example("HolzingerSwineford1939")[paste0("x", 1:9)]
             data.frame(Item = names(X), Moyenne = sapply(X, mean), ET = sapply(X, sd),
                        Min = sapply(X, min), Max = sapply(X, max)) })
  ## ML robuste (MLR) sur les mêmes données
  hm <- ajuster_exemple("HolzingerSwineford1939", estimator = "mlr")
  R$hs_mlr <- list(fit = moteur$cfa_diagnostic_bundle(hm)$fit, versions = versions_indices(hm),
                   ecart_sat = max(abs(saturations(hm)$Std - R$hs$sat$Std)))
  ## Diagnostics univariés et observations éloignées (mêmes règles que l'application)
  Xhs <- moteur$cfa_load_example("HolzingerSwineford1939")[paste0("x", 1:9)]
  R$hs_uni <- data.frame(Item = names(Xhs),
    Asymetrie = sapply(Xhs, function(x) { m <- mean(x); s <- sd(x); sum((x - m)^3) / (length(x) * s^3) }),
    Aplatissement = sapply(Xhs, function(x) { m <- mean(x); s <- sd(x); sum((x - m)^4) / (length(x) * s^4) - 3 }),
    SW_p = sapply(Xhs, function(x) shapiro.test(x)$p.value))
  md <- stats::mahalanobis(Xhs, colMeans(Xhs), stats::cov(Xhs))
  rob <- moteur$with_cfa_seed(123, MASS::cov.rob(as.matrix(Xhs), method = "mcd"))
  mdr <- stats::mahalanobis(Xhs, rob$center, rob$cov)
  R$hs_out <- list(n = nrow(Xhs), seuil = stats::qchisq(1 - .001, ncol(Xhs)),
                   classique = sum(stats::pchisq(md, ncol(Xhs), lower.tail = FALSE) < .001),
                   robuste = sum(stats::pchisq(mdr, ncol(Xhs), lower.tail = FALSE) < .001),
                   max_md = max(md))
  ## Changement d'identification
  hs1 <- ajuster_exemple("HolzingerSwineford1939", std.lv = TRUE)
  R$hs_stdlv <- list(fit = moteur$cfa_diagnostic_bundle(hs1)$fit, sat = saturations(hs1),
                     ecart_max = max(abs(saturations(hs1)$Std - R$hs$sat$Std)),
                     pe = lavaan::parameterEstimates(hs1))
  ## Un seul facteur
  un <- ajuster_exemple("HolzingerSwineford1939",
                        syntax = paste("General =~", paste0("x", 1:9, collapse = " + ")))
  R$hs_un <- list(fit = moteur$cfa_diagnostic_bundle(un)$fit, sat = saturations(un))
  ## Tests d'égalité (Comparaisons)
  X <- moteur$cfa_load_example("HolzingerSwineford1939")[paste0("x", 1:9)]
  opts <- options_app()
  R$hs_tests <- lapply(c(tau = "tau", homo = "homo", parallel = "parallel"), function(k) {
    r <- moteur$cfa_restrict(hs, k)
    cf <- moteur$cfa_fit(r$table, X, opts)$fit
    t <- lavaan::lavTestLRT(hs, cf)
    list(df0 = lavaan::fitMeasures(hs, "df"), df1 = lavaan::fitMeasures(cf, "df"),
         dchi = t[2, "Chisq diff"], ddf = t[2, "Df diff"], p = t[2, "Pr(>Chisq)"])
  })
  ## Covariance résiduelle x7 ~~ x8
  hs78 <- ajuster(paste(CATALOGUE$HolzingerSwineford1939$syntax, "\nx7 ~~ x8"), X, opts)
  R$hs78 <- list(fit = moteur$cfa_diagnostic_bundle(hs78)$fit, val = moteur$cfa_validity(hs78),
                 sig = moteur$cfa_diagnostic_bundle(hs78)$signals,
                 lrt = lavaan::lavTestLRT(hs, hs78))
  ## Bootstrap HTMT (réglages du manuel : 1 000 réplications, graine 123)
  R$hs_boot <- moteur$cfa_htmt_bootstrap(snapshot(hs), R = 1000, seed = 123, type = "htmt")
  ## Scores factoriels
  sc <- moteur$cfa_score_table(snapshot(hs), "regression")
  R$hs_scores <- list(tete = head(sc, 5), cor = cor(sc[-1]), n = nrow(sc))
  ## Normalité multivariée (si MVN est disponible)
  R$hs_mvn <- tryCatch(moteur$cfa_mvn_all(X), error = function(e) NULL)
  ## Absences pédagogiques
  A <- read.csv(file.path(DATA_DIR, "HS_absences_pedagogiques.csv"), na.strings = "")
  syn <- CATALOGUE$HolzingerSwineford1939$syntax
  lw <- ajuster(syn, A[paste0("x", 1:9)], options_app())
  fi <- ajuster(syn, A[paste0("x", 1:9)], options_app(missing = "fiml"))
  R$abs <- list(na = colSums(is.na(A[paste0("x", 1:9)])), lignes = nrow(A),
    fits = list(complet = R$hs$fit, lw = moteur$cfa_diagnostic_bundle(lw)$fit,
                fiml = moteur$cfa_diagnostic_bundle(fi)$fit),
    n = c(complet = R$hs$n, lw = lavaan::lavInspect(lw, "nobs"), fiml = lavaan::lavInspect(fi, "nobs")),
    sat = data.frame(Item = paste0("x", 1:9), complet = R$hs$sat$Std,
                     lw = saturations(lw)$Std, fiml = saturations(fi)$Std),
    val_fiml = moteur$cfa_validity(fi))
  ## Autres jeux réels
  for (id in c("PoliticalDemocracy", "lsat6", "lsat7", "ability", "msq_affect", "bfi")) {
    f <- ajuster_exemple(id)
    b <- moteur$cfa_diagnostic_bundle(f)
    d <- moteur$cfa_load_example(id)
    R[[id]] <- list(n = lavaan::lavInspect(f, "nobs"), lignes = nrow(d),
      fit = b$fit, signaux = b$signals, versions = versions_indices(f),
      srmr = unname(lavaan::fitMeasures(f, "srmr")),
      sat = saturations(f), cor = lavaan::lavInspect(f, "cor.lv"),
      val = tryCatch(moteur$cfa_validity(f), error = function(e) NULL),
      seuils = { p <- lavaan::parameterEstimates(f); p[p$op == "|", c("lhs", "rhs", "est")] })
  }
  ## Simulation : un facteur, quatre indicateurs, saturations 0,70, graine 123
  sim1 <- function(n, l = .70, seed = 123) {
    s <- moteur$cfa_simulate(1, 4, n, c(l, l), c(.1, .7), seed = seed)
    f <- ajuster(paste("F1 =~", paste0("x", 1:4, collapse = " + ")), s$data, options_app())
    val <- moteur$cfa_validity(f)$results
    list(omega = val$Omega, ave = val$AVE, sat = saturations(f)$Std,
         se = { p <- lavaan::parameterEstimates(f); p$se[p$op == "=~" & p$se > 0] })
  }
  R$sim <- list(n100 = sim1(100), n1000 = sim1(1000),
                force = lapply(c(.4, .7, .9), function(l) sim1(1000, l)))
  ## Distribution d'échantillonnage de l'oméga (200 échantillons par taille)
  R$sim_mc <- do.call(rbind, lapply(c(100, 1000), function(n)
    data.frame(n = n, omega = vapply(1:200, function(k) sim1(n, seed = 1000 + k)$omega, numeric(1)))))
  R$versions <- list(lavaan = as.character(packageVersion("lavaan")),
                     R = R.version.string, date = format(Sys.Date()))
  R
}

# Incrémenter REF_VERSION quand calculer_reference() change.
REF_VERSION <- "2"
cle_reference <- function() {
  f <- c(list.files(MOTEUR_DIR, recursive = TRUE, full.names = TRUE),
         list.files(DATA_DIR, pattern = "[.]csv$", full.names = TRUE))
  paste(c(REF_VERSION, unname(tools::md5sum(sort(f))), as.character(packageVersion("lavaan"))), collapse = "|")
}
if (!exists("REF", inherits = FALSE)) {
  cache <- file.path(RACINE, "_reference.rds")
  cle <- cle_reference()
  REF <- if (file.exists(cache)) readRDS(cache) else NULL
  if (is.null(REF) || !identical(attr(REF, "cle"), cle)) {
    REF <- calculer_reference(); attr(REF, "cle") <- cle; saveRDS(REF, cache)
  }
}

# Raccourcis fréquents
HSF <- REF$hs$fit
ix <- function(fit_tab, nom, d = 3) v(indice(fit_tab, nom), d)

# ── Figures : palette commune ───────────────────────────────────────────
PAL <- c(bleu = "#24485b", clair = "#7fa7bf", orange = "#c2410c", vert = "#2f7d5b", gris = "#8a949c")
theme_manuel <- function(base = 12) theme_minimal(base_size = base) +
  theme(panel.grid.minor = element_blank(), plot.title.position = "plot",
        legend.position = "bottom", strip.text = element_text(face = "bold"))
