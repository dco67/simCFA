## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", message = FALSE, warning = FALSE)
options(width = 80)
v <- function(x, d = 3) sub("^-", "−", formatC(round(x, d), format = "f", digits = d, decimal.mark = ","))

## ----modele-------------------------------------------------------------------
library(lavaan)
library(cfadiag)
modele <- "visuel  =~ x1 + x2 + x3
           verbal  =~ x4 + x5 + x6
           vitesse =~ x7 + x8 + x9"
fit <- cfa(modele, data = HolzingerSwineford1939)

## ----rapport------------------------------------------------------------------
d <- cfadiag(fit, htmt_boot = TRUE, R = 500, seed = 2026)
d

## ----valeurs, include = FALSE-------------------------------------------------
r  <- d$fiabilite
va <- d$validite$table
H  <- d$htmt
af <- function(k) d$ajustement$valeur[d$ajustement$indice == k]

## ----graphiques, fig.width = 9, fig.height = 7--------------------------------
par(mfrow = c(2, 2))
for (t in c("charges", "fiabilite", "validite", "htmt")) plot(d, t)

## ----extraction---------------------------------------------------------------
d$fiabilite[, c("facteur", "omega", "alpha", "H")]
d$validite$table
d$ajustement[d$ajustement$indice %in% c("cfi", "rmsea", "srmr"), c("indice", "valeur", "atteint")]

## ----modification-------------------------------------------------------------
mi <- modindices(fit, sort. = TRUE, maximum.number = 3)
mi[, c("lhs", "op", "rhs", "mi", "epc")]
fit2 <- cfa(paste(modele, "\n visuel =~ x9"), data = HolzingerSwineford1939)
lrt <- lavTestLRT(fit, fit2)
d2 <- cfadiag(fit2)

## ----consequences-------------------------------------------------------------
d2$signaux
d2$fiabilite[, c("facteur", "omega", "alpha", "H")]
st <- with(d2$fiabilite, paste0(facteur, " : ", statut))
writeLines(strwrap(st, width = 76, exdent = 2))
round(d2$htmt, 3)

## ----arguments, eval = FALSE--------------------------------------------------
#  cfadiag(fit, variance = "observee")          # oméga avec la variance observée (comme semTools)
#  cfadiag(fit, reperes = list(cfi = .90, tli = .90, rmsea = .08, srmr = .10))
#  cfadiag(fit, charge_faible = .50, htmt_alerte = .90)   # seuils des signaux
#  cfadiag(fit, htmt_boot = TRUE, R = 2000, seed = 1)     # bootstrap plus précis

