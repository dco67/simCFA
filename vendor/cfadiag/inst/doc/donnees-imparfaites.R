## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", message = FALSE, warning = FALSE)
options(width = 80)
v <- function(x, d = 3) sub("^-", "−", formatC(round(x, d), format = "f", digits = d, decimal.mark = ","))

## ----donnees------------------------------------------------------------------
library(lavaan)
library(cfadiag)
HS <- HolzingerSwineford1939
X  <- HS[paste0("x", 1:9)]
modele <- "visuel  =~ x1 + x2 + x3
           verbal  =~ x4 + x5 + x6
           vitesse =~ x7 + x8 + x9"
reference <- cfadiag(cfa(modele, data = HS))   # données complètes, pour comparer

## ----matrice------------------------------------------------------------------
R   <- round(cor(X), 2)
et  <- round(sapply(X, sd), 2)
# getCov() lit le triangle inférieur ligne par ligne, diagonale comprise
S   <- getCov(R[upper.tri(R, diag = TRUE)], sds = et, names = colnames(R))
fit_S <- cfa(modele, sample.cov = S, sample.nobs = 301)
d_S <- cfadiag(fit_S)
cbind(complet = reference$fiabilite$omega, matrice = d_S$fiabilite$omega)
round(d_S$htmt, 3)

## ----matrice-boot-------------------------------------------------------------
try(boot_htmt(as.cfaModel(fit_S), R = 100))

## ----correlations-------------------------------------------------------------
fit_R <- cfa(modele, sample.cov = R, sample.nobs = 301)
cfadiag(fit_R)$fiabilite[, c("facteur", "omega", "CR", "alpha")]

## ----charges-publiees---------------------------------------------------------
L <- matrix(0, 6, 2, dimnames = list(paste0("e", 1:6), c("Engagement", "Fatigue")))
L[1:3, "Engagement"] <- c(.78, .71, .66)
L[4:6, "Fatigue"]    <- c(.82, .74, .59)
Phi <- matrix(c(1, -.42, -.42, 1), 2, dimnames = list(colnames(L), colnames(L)))
Sigma <- L %*% Phi %*% t(L); diag(Sigma) <- 1
publie <- cfaModel(loadings = L, cov_lv = Phi, implied = Sigma)
compute_reliability(publie)[, c("facteur", "omega", "CR", "alpha", "H")]
compute_validity(publie)$table

## ----manquantes---------------------------------------------------------------
set.seed(2)
H2 <- HS
H2$x2[runif(301) < plogis(-2 + 1.5 * scale(H2$x1))] <- NA
H2$x5[runif(301) < plogis(-2 + 1.5 * scale(H2$x4))] <- NA
colSums(is.na(H2[c("x2", "x5")]))

fit_lw   <- cfa(modele, data = H2)                     # cas complets
fit_fiml <- cfa(modele, data = H2, missing = "fiml")   # FIML
c(cas_complets = nobs(fit_lw), fiml = nobs(fit_fiml))

## ----comparaison-manquantes---------------------------------------------------
d_lw   <- cfadiag(fit_lw)
d_fiml <- cfadiag(fit_fiml)
data.frame(facteur  = reference$fiabilite$facteur,
           complet  = reference$fiabilite$omega,
           cas_complets = d_lw$fiabilite$omega,
           fiml     = d_fiml$fiabilite$omega)
haut <- function(H) H[upper.tri(H)]
data.frame(paire = c("visuel – verbal", "visuel – vitesse", "verbal – vitesse"),
           complet = haut(reference$htmt), cas_complets = haut(d_lw$htmt), fiml = haut(d_fiml$htmt))

## ----valeurs-manquantes, include = FALSE--------------------------------------
perdus <- 301 - nobs(fit_lw)

## ----boot-fiml----------------------------------------------------------------
b <- boot_htmt(as.cfaModel(fit_fiml), R = 200, seed = 1)
b$table[, c("facteur1", "facteur2", "htmt", "borne_inf", "borne_sup")]

## ----ordinaux-----------------------------------------------------------------
decouper <- function(k) as.data.frame(lapply(X, function(z)
  cut(z, quantile(z, 0:k / k), include.lowest = TRUE, labels = FALSE)))
res <- do.call(rbind, lapply(c(2, 3, 5), function(k) {
  Hk <- decouper(k)
  o <- cfadiag(cfa(modele, data = Hk, ordered = names(Hk)))$fiabilite$omega
  cc <- cfadiag(cfa(modele, data = Hk))$fiabilite$omega
  data.frame(categories = k, facteur = c("visuel", "verbal", "vitesse"),
             omega_ordinal = o, omega_codes = cc)
}))
res

## ----ordinal-rapport----------------------------------------------------------
H4 <- decouper(4)
d_o <- cfadiag(cfa(modele, data = H4, ordered = names(H4)))
ind <- d_o$ajustement
ind$valeur <- round(ind$valeur, 3)
ind[ind$indice %in% c("chisq", "cfi", "tli", "rmsea"), c("indice", "valeur", "version")]

