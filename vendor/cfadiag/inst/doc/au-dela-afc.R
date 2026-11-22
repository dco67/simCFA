## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", message = FALSE, warning = FALSE)
options(width = 80)
v <- function(x, d = 3) sub("^-", "−", formatC(round(x, d), format = "f", digits = d, decimal.mark = ","))

## ----donnees------------------------------------------------------------------
library(lavaan)
library(cfadiag)
HS <- HolzingerSwineford1939
modele <- "visuel  =~ x1 + x2 + x3
           verbal  =~ x4 + x5 + x6
           vitesse =~ x7 + x8 + x9"
fit <- cfa(modele, data = HS)
d   <- cfadiag(fit)

## ----sem-sature---------------------------------------------------------------
fit_sem <- sem(paste(modele, "\n verbal ~ visuel\n vitesse ~ visuel + verbal"), data = HS)
compute_validity(as.cfaModel(fit_sem))$table
d$validite$table

## ----sem-contraint------------------------------------------------------------
fit_c <- sem(paste(modele, "\n verbal ~ visuel\n vitesse ~ verbal"), data = HS)
round(lavInspect(fit_c, "cor.lv"), 3)
compute_validity(as.cfaModel(fit_c))$table
lavTestLRT(fit, fit_c)[, c("Chisq", "Df", "Pr(>Chisq)")]

## ----valeurs-contraint, include = FALSE---------------------------------------
r_c <- lavInspect(fit_c, "cor.lv")["vitesse", "visuel"]
r_a <- lavInspect(fit, "cor.lv")["vitesse", "visuel"]

## ----second-ordre-------------------------------------------------------------
fit_2o <- cfa(paste(modele, "\n g =~ visuel + verbal + vitesse"), data = HS)
cfadiag(fit_2o)$fiabilite[, c("facteur", "omega", "H")]

## ----multigroupe--------------------------------------------------------------
fit_g <- cfa(modele, data = HS, group = "school")
d_g <- cfadiag(fit_g)
names(d_g)
d_g[["Pasteur"]]

## ----comparaison-groupes------------------------------------------------------
compar <- function(dd) data.frame(
  groupe = names(dd),
  charge_x2 = sapply(dd, function(g) g$charges["x2", "visuel"]),
  omega_visuel = sapply(dd, function(g) g$fiabilite$omega[1]),
  H_visuel = sapply(dd, function(g) g$fiabilite$H[1]),
  omega_vitesse = sapply(dd, function(g) g$fiabilite$omega[3]), row.names = NULL)
compar(d_g)

## ----invariance---------------------------------------------------------------
fit_gl <- cfa(modele, data = HS, group = "school", group.equal = "loadings")
lavTestLRT(fit_g, fit_gl)[, c("Chisq diff", "Df diff", "Pr(>Chisq)")]
compar(cfadiag(fit_gl))

## ----valeurs-inv, include = FALSE---------------------------------------------
lrt_inv <- lavTestLRT(fit_g, fit_gl)

## ----score-cible--------------------------------------------------------------
fit_x9 <- cfa(paste(modele, "\n visuel =~ x9"), data = HS)
Lambda <- lavInspect(fit_x9, "est")$lambda
Psi    <- lavInspect(fit_x9, "cov.lv")
Sigma  <- lavInspect(fit_x9, "cov.ov")
w <- setNames(as.numeric(rownames(Lambda) %in% c("x7", "x8", "x9")), rownames(Lambda))
var_somme <- drop(t(w) %*% Sigma %*% w)
cible     <- drop((t(w) %*% Lambda[, "vitesse"])^2 * Psi["vitesse", "vitesse"]) / var_somme
commune   <- drop(t(w) %*% Lambda %*% Psi %*% t(Lambda) %*% w) / var_somme
c(vitesse_seule = cible, toute_variance_commune = commune)

## ----effet-direct-------------------------------------------------------------
fit_d <- sem(paste(modele, "\n x4 ~ x1"), data = HS)
d_d <- cfadiag(fit_d)
d_d$signaux
d_d$fiabilite[, c("facteur", "omega", "alpha", "H")]
writeLines(strwrap(d_d$fiabilite$statut[2], 76))

## ----effet-direct-charges-----------------------------------------------------
round(d_d$charges[c("x1", "x4"), ], 3)

## ----peu----------------------------------------------------------------------
fit_2 <- cfa("visuel =~ x1 + x2 + x3\n verbal =~ x4 + x5", data = HS)
cfadiag(fit_2)$signaux

## ----bifactoriel--------------------------------------------------------------
fit_b <- cfa("g  =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9
              s1 =~ x1 + x2 + x3
              s2 =~ x4 + x5 + x6
              s3 =~ x7 + x8 + x9", data = HS, orthogonal = TRUE, std.lv = TRUE)
d_b <- cfadiag(fit_b)
table(d_b$signaux$signal)
head(d_b$signaux, 2)

