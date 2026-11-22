## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", message = FALSE, warning = FALSE)
options(width = 80)
v <- function(x, d = 3) sub("^-", "−", formatC(round(x, d), format = "f", digits = d, decimal.mark = ","))

## ----simulate-----------------------------------------------------------------
library(lavaan)
library(cfadiag)
s <- simulate_cfa(n = 500, n_factors = 3, items_per_factor = 4,
                  loading_range = c(.5, .8), factor_correlation = .4, seed = 1)
names(s)
round(s$loadings, 2)
cat(s$model)

## ----population---------------------------------------------------------------
population <- function(s) {
  Sigma <- s$loadings %*% s$Phi %*% t(s$loadings) + diag(s$theta)
  cfaModel(loadings = s$loadings, cov_lv = s$Phi, implied = Sigma, sample = Sigma)
}
pop <- population(s)
compute_reliability(pop)[, c("facteur", "omega", "alpha", "H")]
compute_validity(pop)$table[, c("facteur", "AVE", "MSV")]

## ----verite-------------------------------------------------------------------
d <- cfadiag(cfa(s$model, data = s$data))
data.frame(facteur = d$fiabilite$facteur,
           omega_vrai = compute_reliability(pop)$omega, omega_estime = d$fiabilite$omega,
           AVE_vraie = compute_validity(pop)$table$AVE, AVE_estimee = d$validite$table$AVE)
round(compute_htmt(pop), 3)
round(d$htmt, 3)

## ----verite-valeurs, include = FALSE------------------------------------------
ecart_omega <- max(abs(d$fiabilite$omega - compute_reliability(pop)$omega))
Hp <- compute_htmt(pop)
ecart_htmt <- max(abs(d$htmt[upper.tri(Hp)] - Hp[upper.tri(Hp)]))

## ----faibles------------------------------------------------------------------
s2 <- simulate_cfa(n = 300, n_factors = 2, items_per_factor = 4,
                   loading_range = c(.3, .5), factor_correlation = .3, seed = 11)
d2 <- cfadiag(cfa(s2$model, data = s2$data))
d2$signaux
d2$fiabilite[, c("facteur", "omega", "alpha", "H")]
compute_reliability(population(s2))$omega

## ----faibles-ajustement-------------------------------------------------------
round(fitMeasures(cfa(s2$model, data = s2$data), c("cfi", "rmsea", "srmr")), 3)

## ----proches------------------------------------------------------------------
grille <- do.call(rbind, lapply(c(.3, .5, .7, .85, .95), function(r) {
  s3 <- simulate_cfa(n = 2000, n_factors = 2, items_per_factor = 4,
                     loading_range = c(.5, .7), factor_correlation = r, seed = 100)
  d3 <- cfadiag(cfa(s3$model, data = s3$data))
  data.frame(r_vraie = r,
             r_estimee = d3$validite$fornell_larcker[1, 2],
             HTMT = d3$htmt[1, 2],
             AVE_min = min(d3$validite$table$AVE),
             MSV = d3$validite$table$MSV[1],
             AVE_sup_MSV = all(d3$validite$table$AVE_superieure_MSV),
             signal_HTMT = any(d3$signaux$signal == "HTMT élevé"))
}))
grille

## ----proches-valeurs, include = FALSE-----------------------------------------
premier_echec <- grille$r_vraie[which(!grille$AVE_sup_MSV)[1]]
premier_htmt  <- grille$r_vraie[which(grille$signal_HTMT)[1]]

## ----cachee-------------------------------------------------------------------
s4 <- simulate_cfa(n = 400, n_factors = 2, items_per_factor = 4,
                   loading_range = c(.6, .75), factor_correlation = .3, seed = 5)
pop4 <- paste(s4$population_model, "\n F1 =~ 0.5*x5")
dat4 <- simulateData(pop4, sample.nobs = 400, seed = 5)
fit4 <- cfa(s4$model, data = dat4)
round(fitMeasures(fit4, c("cfi", "rmsea", "srmr")), 3)
cfadiag(fit4)$signaux
modindices(fit4, sort. = TRUE, maximum.number = 3)[, c("lhs", "op", "rhs", "mi")]

## ----cachee-corrigee----------------------------------------------------------
fit4b <- cfa(paste(s4$model, "\n F1 =~ x5"), data = dat4)
d4b <- cfadiag(fit4b)
round(d4b$charges["x5", ], 3)
d4b$signaux

## ----montecarlo---------------------------------------------------------------
s5 <- simulate_cfa(n = 100, n_factors = 2, items_per_factor = 4,
                   loading_range = c(.5, .8), factor_correlation = .4, seed = 1)
vrai <- population(s5)
omega_vrai <- compute_reliability(vrai)$omega[1]
htmt_vrai  <- compute_htmt(vrai)[1, 2]

res <- t(sapply(1:100, function(i) {
  x <- as.cfaModel(cfa(s5$model, data = simulateData(s5$population_model,
                                                     sample.nobs = 100, seed = 1000 + i)))
  c(omega = compute_reliability(x)$omega[1], htmt = compute_htmt(x)[1, 2])
}))
summary(res)

## ----montecarlo-figure, fig.width = 8, fig.height = 3.8-----------------------
par(mfrow = c(1, 2), mar = c(4.5, 4, 2.5, 1))
hist(res[, "omega"], breaks = 15, col = "grey85", border = "white",
     main = "Oméga de F1", xlab = "estimation (n = 100)", ylab = "échantillons")
abline(v = omega_vrai, lwd = 2, col = "firebrick")
hist(res[, "htmt"], breaks = 15, col = "grey85", border = "white",
     main = "HTMT F1 – F2", xlab = "estimation (n = 100)", ylab = "échantillons")
abline(v = htmt_vrai, lwd = 2, col = "firebrick")

## ----montecarlo-valeurs, include = FALSE--------------------------------------
p_sous <- mean(res[, "omega"] < .70)
etendue_h <- range(res[, "htmt"])

