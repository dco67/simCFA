suppressPackageStartupMessages(library(lavaan))
HS <- HolzingerSwineford1939
m3 <- "visual =~ x1 + x2 + x3
       textual =~ x4 + x5 + x6
       speed =~ x7 + x8 + x9"
fit <- cfa(m3, data = HS)
cm <- as.cfaModel(fit)

test_that("le HTMT exclut la diagonale et suit la d\u00e9finition de Henseler", {
  R <- abs(cor(HS[paste0("x", 1:9)]))
  mono <- function(it) { r <- R[it, it]; mean(r[upper.tri(r)]) }
  attendu <- mean(R[1:3, 4:6]) / sqrt(mono(1:3) * mono(4:6))
  H <- compute_htmt(cm)
  expect_equal(unname(H["visual", "textual"]), attendu, tolerance = 1e-6)
  expect_true(isSymmetric(unname(H)))
  expect_equal(unname(diag(H)), rep(1, 3))
})

test_that("l'om\u00e9ga retrouve la fiabilit\u00e9 composite classique sans covariance r\u00e9siduelle", {
  s <- standardizedSolution(fit); l <- s$est.std[s$op == "=~" & s$lhs == "visual"]
  cr <- sum(l)^2 / (sum(l)^2 + sum(1 - l^2))
  expect_equal(compute_reliability(cm)$CR[1], cr, tolerance = 1e-6)
  # oméga de la somme brute : (somme des charges brutes)^2 / variance impliquée de la somme
  e <- lavInspect(fit, "est"); it <- c("x1", "x2", "x3")
  expect_equal(compute_reliability(cm)$omega[1], sum(e$lambda[it, "visual"])^2 * e$psi["visual", "visual"] / sum(fitted(fit)$cov[it, it]), tolerance = 1e-6)
  # même résultat avec la variance des facteurs fixée à 1
  cm2 <- as.cfaModel(cfa(m3, data = HS, std.lv = TRUE))
  expect_equal(compute_reliability(cm2)$omega, compute_reliability(cm)$omega, tolerance = 1e-6)
})

test_that("l'om\u00e9ga int\u00e8gre une covariance r\u00e9siduelle", {
  f2 <- cfa(paste(m3, "\nx7 ~~ x8"), data = HS)
  c2 <- as.cfaModel(f2)
  it <- c("x7", "x8", "x9"); est <- lavInspect(f2, "est")
  num <- sum(est$lambda[it, "speed"])^2 * lavInspect(f2, "cov.lv")["speed", "speed"]
  expect_equal(compute_reliability(c2)$omega[3], num / sum(fitted(f2)$cov[it, it]), tolerance = 1e-6)
  expect_true(any(compute_diagnostics(c2)$signal == "Covariance r\u00e9siduelle"))
})

test_that("H est au moins \u00e9gal \u00e0 l'om\u00e9ga et l'AVE est la moyenne des charges au carr\u00e9", {
  r <- compute_reliability(cm); expect_true(all(r$H >= r$omega - 1e-10))
  s <- standardizedSolution(fit); l <- s$est.std[s$op == "=~" & s$lhs == "speed"]
  expect_equal(compute_validity(cm)$table$AVE[3], mean(l^2), tolerance = 1e-6)
})

test_that("une entr\u00e9e par matrice donne les m\u00eames indices que les donn\u00e9es brutes", {
  S <- cov(HS[paste0("x", 1:9)])
  fs <- cfa(m3, sample.cov = S, sample.nobs = nrow(HS))
  cs <- as.cfaModel(fs)
  expect_null(cs$data)
  expect_equal(compute_htmt(cs), compute_htmt(cm), tolerance = 1e-6)
  expect_equal(compute_reliability(cs)$omega, compute_reliability(cm)$omega, tolerance = 1e-4)
  expect_error(boot_htmt(cs, R = 10), "donn\u00e9es brutes")
})

test_that("dans un SEM, les corr\u00e9lations entre facteurs sont les corr\u00e9lations impliqu\u00e9es", {
  sem_fit <- sem(paste(m3, "\ntextual ~ visual\nspeed ~ visual + textual"), data = HS)
  cs <- as.cfaModel(sem_fit)
  expect_equal(unclass(cs$cor_lv), unclass(lavInspect(fit, "cor.lv")), tolerance = 1e-4, ignore_attr = TRUE)
  expect_equal(compute_validity(cs)$table$MSV, compute_validity(cm)$table$MSV, tolerance = 1e-4)
})

test_that("un mod\u00e8le multigroupe produit un rapport par groupe", {
  fg <- cfa(m3, data = HS, group = "school")
  d <- cfadiag(fg)
  expect_s3_class(d, "cfadiag_multigroupe")
  expect_equal(names(d), lavInspect(fg, "group.label"))
  expect_s3_class(as.cfaModel(fg, group = "Pasteur"), "cfaModel")
})

test_that("FIML et indicateurs ordinaux sont pris en charge", {
  set.seed(1); H2 <- HS; H2$x2[sample(nrow(H2), 40)] <- NA
  cf <- as.cfaModel(cfa(m3, data = H2, missing = "fiml"))
  expect_false(anyNA(compute_htmt(cf)))
  b <- boot_htmt(cf, R = 50, seed = 1)
  expect_true(all(b$table$n_valides == 50))
  Ho <- as.data.frame(lapply(HS[paste0("x", 1:9)], function(z) cut(z, quantile(z, 0:4 / 4), include.lowest = TRUE, labels = FALSE)))
  fo <- cfa(m3, data = Ho, ordered = names(Ho))
  co <- as.cfaModel(fo)
  expect_equal(length(co$ordered), 9)
  expect_equal(diag(co$sample), setNames(rep(1, 9), names(Ho)), tolerance = 1e-8)
  expect_error(boot_htmt(co, R = 10), "ordinaux")
  expect_true(all(compute_fit_indices(co)$version[compute_fit_indices(co)$indice %in% c("cfi", "rmsea")] != "standard"))
})

test_that("le bootstrap r\u00e9\u00e9chantillonne une fois par r\u00e9plication et est reproductible", {
  b1 <- boot_htmt(cm, R = 200, seed = 42); b2 <- boot_htmt(cm, R = 200, seed = 42)
  expect_equal(b1$table, b2$table)
  rep1 <- b1$replications[1, , ]
  expect_true(isSymmetric(unname(rep1)))
  expect_true(all(b1$table$borne_inf <= b1$table$htmt & b1$table$htmt <= b1$table$borne_sup))
})

test_that("les indices d'ajustement portent des noms simples et des rep\u00e8res", {
  f <- compute_fit_indices(cm)
  expect_equal(f$valeur[f$indice == "cfi"], as.numeric(fitMeasures(fit, "cfi")))
  expect_true(is.logical(f$atteint))
  fr <- compute_fit_indices(cfa(m3, data = HS, estimator = "MLR"))
  expect_equal(fr$version[fr$indice == "cfi"], "robust")
})

test_that("simulate_cfa produit des indicateurs de variance 1", {
  s <- simulate_cfa(n = 20000, seed = 3)
  expect_true(all(s$theta > 0))
  expect_equal(unname(apply(s$data, 2, var)), rep(1, 12), tolerance = .05)
  d <- cfadiag(cfa(s$model, data = s$data))
  expect_s3_class(d, "cfadiag")
  expect_output(print(d), "HTMT")
})

test_that("les résultats concordent avec semTools", {
  skip_if_not_installed("semTools")
  expect_equal(compute_reliability(cm, "observee")$omega, as.numeric(semTools::compRelSEM(fit)), tolerance = 1e-6)
  h <- semTools::htmt(m3, HS, htmt2 = FALSE)
  expect_equal(unname(compute_htmt(cm)[2, 1]), unname(h[2, 1]), tolerance = 1e-6)
  h2 <- semTools::htmt(m3, HS, htmt2 = TRUE)
  expect_equal(unname(compute_htmt(cm, type = "htmt2")[3, 1]), unname(h2[3, 1]), tolerance = 1e-6)
})

test_that("l'argument variance choisit le dénominateur de l'oméga", {
  e <- lavInspect(fit, "est"); S <- lavInspect(fit, "sampstat")$cov; it <- c("x1", "x2", "x3")
  attendu <- sum(e$lambda[it, "visual"])^2 * e$psi["visual", "visual"] / sum(S[it, it])
  expect_equal(compute_reliability(cm, "observee")$omega[1], attendu, tolerance = 1e-8)
  expect_equal(cfadiag(fit, variance = "observee")$fiabilite$omega, compute_reliability(cm, "observee")$omega)
  expect_error(compute_reliability(cm, "autre"))
})
