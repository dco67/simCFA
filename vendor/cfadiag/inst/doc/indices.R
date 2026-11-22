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
m <- as.cfaModel(fit)

# Un facteur de population, construit à partir de charges standardisées
un_facteur <- function(charges) {
  L <- matrix(charges, ncol = 1, dimnames = list(paste0("y", seq_along(charges)), "F"))
  Sigma <- L %*% t(L); diag(Sigma) <- 1
  cfaModel(loadings = L, cov_lv = matrix(1, 1, 1, dimnames = list("F", "F")),
           implied = Sigma, sample = Sigma)
}

## ----egales-------------------------------------------------------------------
compute_reliability(un_facteur(c(.7, .7, .7, .7)))[, c("omega", "CR", "alpha", "H")]

## ----inegales-----------------------------------------------------------------
f_ineg <- compute_reliability(un_facteur(c(.9, .8, .5, .3)))
f_ineg[, c("omega", "CR", "alpha", "H")]

## ----hs-fiab------------------------------------------------------------------
compute_reliability(m)[, c("facteur", "omega", "CR", "alpha", "H")]

## ----variance-----------------------------------------------------------------
imp <- compute_reliability(m)$omega
obs <- compute_reliability(m, variance = "observee")$omega
data.frame(facteur = colnames(m$loadings), impliquee = imp, observee = obs)

## ----residuelle---------------------------------------------------------------
fit_r <- cfa(paste(modele, "\n x7 ~~ x8"), data = HS)
m_r <- as.cfaModel(fit_r)
avant <- compute_reliability(m)[3, c("omega", "alpha", "H")]
apres <- compute_reliability(m_r)[3, c("omega", "alpha", "H")]
rbind(sans_covariance = avant, avec_covariance = apres)
compute_diagnostics(m_r)

## ----ave----------------------------------------------------------------------
f3 <- un_facteur(c(.7, .7, .7))
compute_validity(f3)$table[, c("facteur", "AVE")]
compute_reliability(f3)$omega

## ----fl-----------------------------------------------------------------------
val <- compute_validity(m)
val$table
round(val$fornell_larcker, 3)

## ----htmt---------------------------------------------------------------------
H  <- compute_htmt(m)
H2 <- compute_htmt(m, type = "htmt2")
paires <- which(upper.tri(H), arr.ind = TRUE)
data.frame(paire  = paste(rownames(H)[paires[, 1]], colnames(H)[paires[, 2]], sep = " – "),
           r_facteurs = m$cor_lv[upper.tri(H)],
           HTMT  = H[upper.tri(H)],
           HTMT2 = H2[upper.tri(H)])

## ----htmt-population----------------------------------------------------------
L <- matrix(0, 6, 2, dimnames = list(paste0("y", 1:6), c("A", "B")))
L[1:3, "A"] <- c(.9, .6, .4)
L[4:6, "B"] <- c(.8, .7, .3)
Phi <- matrix(c(1, .5, .5, 1), 2, dimnames = list(c("A", "B"), c("A", "B")))
Sigma <- L %*% Phi %*% t(L); diag(Sigma) <- 1
pop <- cfaModel(loadings = L, cov_lv = Phi, implied = Sigma, sample = Sigma)
c(HTMT = compute_htmt(pop)[1, 2], HTMT2 = compute_htmt(pop, type = "htmt2")[1, 2])

## ----boot---------------------------------------------------------------------
b <- boot_htmt(m, R = 500, seed = 1)
b$table[, c("facteur1", "facteur2", "htmt", "borne_inf", "borne_sup")]

## ----ajustement---------------------------------------------------------------
fit_mlr <- cfa(modele, data = HS, estimator = "MLR")
ind <- compute_fit_indices(fit_mlr)
ind$valeur <- round(ind$valeur, 3)
ind[, c("indice", "valeur", "version", "atteint")]

## ----semtools-test, include = FALSE-------------------------------------------
a_semtools <- requireNamespace("semTools", quietly = TRUE)

## ----semtools, eval = a_semtools----------------------------------------------
semTools::compRelSEM(fit)                          # oméga, variance observée
compute_reliability(m, variance = "observee")$omega
semTools::compRelSEM(fit, tau.eq = TRUE)           # alpha
compute_reliability(m)$alpha
semTools::htmt(modele, HS)                         # HTMT

