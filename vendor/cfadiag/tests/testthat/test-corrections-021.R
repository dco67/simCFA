suppressPackageStartupMessages(library(lavaan))
HS<-HolzingerSwineford1939
m3<-"visual =~ x1+x2+x3\ntextual =~ x4+x5+x6\nspeed =~ x7+x8+x9"
fit<-cfa(m3,data=HS)
cm<-as.cfaModel(fit)

test_that("H utilise toute la matrice r\u00e9siduelle et est invariant \u00e0 l'identification", {
  L<-matrix(.7,3,1,dimnames=list(paste0('x',1:3),'F1'))
  T<-matrix(-.2,3,3,dimnames=list(rownames(L),rownames(L)));diag(T)<-.51
  P<-matrix(1,1,1,dimnames=list('F1','F1'));S<-L%*%P%*%t(L)+T
  model<-cfaModel(loadings=L,cov_lv=P,theta=T,implied=S,sample=S)
  r<-compute_reliability(model)
  expect_equal(r$H,4.41/(4.41+.33),tolerance=1e-10)
  expect_gte(r$H,r$omega-1e-10)
  P[,]<-2
  r2<-compute_reliability(cfaModel(loadings=L,cov_lv=P,theta=T,implied=S,sample=S))
  expect_equal(r2$omega,r$omega,tolerance=1e-10)
  expect_equal(r2$H,r$H,tolerance=1e-10)
})

test_that("les formes complexes ne re\u00e7oivent pas de fiabilit\u00e9 cible artificielle", {
  fx<-cfa(paste(m3,'textual =~ x3',sep='\n'),data=HS)
  cx<-as.cfaModel(fx)
  rr<-compute_reliability(cx)
  expect_true(all(is.na(rr$omega[rr$facteur %in% c('visual','textual')])))
  expect_true(is.finite(rr$omega[rr$facteur=='speed']))
  expect_true(all(is.na(compute_validity(cx)$table$AVE[1:2])))
  expect_true(is.na(compute_htmt(cx)['visual','textual']))
  expect_true(any(compute_diagnostics(cx)$signal=='Charge crois\u00e9e'))
})

test_that("les cas non applicables et corr\u00e9lations manquantes restent explicites", {
  one<-as.cfaModel(cfa('F1 =~ x1+x2+x3+x4',data=HS))
  expect_true(all(is.na(compute_htmt(one))))
  expect_true(is.na(compute_validity(one)$table$MSV))
  expect_equal(nrow(boot_htmt(one,R=5,seed=4)$table),0)
  R<-cov2cor(cm$sample);R['x1','x4']<-R['x4','x1']<-NA_real_
  expect_true(is.na(compute_htmt(cm,R=R)[1,2]))
  R<-cov2cor(cm$sample);R['x1','x2']<-R['x2','x1']<-0
  expect_true(is.na(compute_htmt(cm,R=R,type='htmt2')[1,2]))
  expect_error(compute_htmt(cm,R=matrix(1,2,2)),"incompatible")
})

test_that("le bootstrap FIML conserve la covariance EM pour le point et les r\u00e9plications", {
  set.seed(31);d<-HS;d$x2[sample(nrow(d),70)]<-NA
  cf<-as.cfaModel(cfa(m3,data=d,missing='fiml'))
  before<-.Random.seed;b<-boot_htmt(cf,R=30,seed=53)
  expect_identical(.Random.seed,before)
  expect_identical(b$missing,'fiml')
  expect_equal(b$table$htmt,unname(compute_htmt(cf)[lower.tri(compute_htmt(cf))]),tolerance=1e-7)
  set.seed(53);X<-as.matrix(cf$data);Xb<-X[sample.int(nrow(X),replace=TRUE),,drop=FALSE]
  Rb<-lavCor(as.data.frame(Xb),missing='fiml',output='cor',se='none',test='none')
  expected<-compute_htmt(cf,R=Rb)
  expect_equal(b$replications[1,,],expected,tolerance=1e-10)
  expect_true(all(b$table$n_valides==30))
})

test_that("les validations et r\u00e9plications bootstrap d\u00e9g\u00e9n\u00e9r\u00e9es ne plantent pas", {
  expect_error(boot_htmt(cm,R=0),"entier")
  expect_error(boot_htmt(cm,R=5,level=1),"level")
  expect_error(boot_htmt(cm,R=5,seed=-1),"graine")
  bad<-cm;bad$data[,]<-1
  b<-suppressWarnings(boot_htmt(bad,R=5,seed=4))
  expect_true(all(b$table$n_valides==0))
  expect_true(all(is.na(b$table$borne_inf)))
  expect_true(all(is.na(b$table$et_boot)))
  missing_data<-cm;missing_data$data[1,1]<-NA;missing_data$meta$missing<-NULL
  expect_error(boot_htmt(missing_data,R=5),"Pr\u00e9cisez")
})

test_that("les simulations valident une vraie matrice de corr\u00e9lations", {
  expect_error(simulate_cfa(n=1),"entiers")
  expect_error(simulate_cfa(n_factors=2.5),"entiers")
  expect_error(simulate_cfa(loading_range=c(.8,.4)),"bornes")
  expect_error(simulate_cfa(n_factors=2,factor_correlation=matrix(c(1,.2,.7,1),2)),"sym\u00e9trique")
  expect_error(simulate_cfa(n_factors=2,factor_correlation=diag(c(2,1))),"diagonale 1")
  expect_error(simulate_cfa(n_factors=2,factor_correlation=matrix(1,2,2)),"positive")
  expect_error(simulate_cfa(n_factors=3,factor_correlation=diag(2)),"Dimensions")
  set.seed(5);before<-.Random.seed;s1<-simulate_cfa(n_factors=1,seed=7)
  expect_identical(.Random.seed,before)
  expect_identical(s1,simulate_cfa(n_factors=1,seed=7))
  expect_match(s1$population_model,'~~ 1\\*F1')
})

test_that("le conteneur v\u00e9rifie les noms et conserve une saturation estim\u00e9e nulle", {
  expect_error(cfaModel(loadings=matrix(.7,3,1)),"noms")
  expect_error(cfaModel(loadings=cm$loadings,implied=diag(2)),"Dimensions")
  expect_error(as.cfaModel(fit,group=2),"Groupe")
  x<-cm;x$loadings['x1','visual']<-0
  expect_true(any(compute_diagnostics(x)$signal=='Charge faible'))
})

test_that("le diagnostic r\u00e9siduel invalide ne produit pas de sqrt n\u00e9gatif", {
  x<-cm;x$theta[1,1]<- -.1;x$theta[1,2]<-x$theta[2,1]<-.05
  expect_silent(signals<-compute_diagnostics(x))
  expect_true(any(signals$signal=='Cas de Heywood'))
  expect_true(any(signals$signal=='Matrice r\u00e9siduelle inadmissible'))
  expect_true(is.na(compute_reliability(x)$H[1]))
})

test_that("les mod\u00e8les non converg\u00e9s et juste identifi\u00e9s ne sont pas accept\u00e9s par seuils", {
  fj<-cfa('F1 =~ x1+x2+x3',data=HS)
  expect_true(all(is.na(compute_fit_indices(fj)$atteint)))
  nf<-cfa(m3,data=HS,do.fit=FALSE)
  expect_warning(nm<-as.cfaModel(nf),"converg\u00e9")
  expect_true(all(is.na(compute_reliability(nm)$omega)))
  expect_true(all(is.na(compute_fit_indices(nm)$valeur)))
})

test_that("les graphiques traitent un seul facteur et HTMT au-del\u00e0 de un", {
  file<-tempfile(fileext='.pdf');grDevices::pdf(file)
  on.exit({grDevices::dev.off();unlink(file)})
  one<-cfadiag(cfa('F1 =~ x1+x2+x3+x4',data=HS))
  expect_silent(plot(one,'charges'))
  expect_silent(plot(one,'validite'))
  object<-cfadiag(fit);object$htmt[1,2]<-object$htmt[2,1]<-1.2
  expect_silent(plot(object,'htmt'))
})

test_that("une matrice r\u00e9siduelle singuli\u00e8re n'annule pas l'om\u00e9ga de la somme", {
  L<-matrix(1,3,1,dimnames=list(paste0('x',1:3),'F1'))
  S<-matrix(1,3,3,dimnames=list(rownames(L),rownames(L)))
  T<-matrix(0,3,3,dimnames=dimnames(S))
  r<-compute_reliability(cfaModel(loadings=L,theta=T,implied=S,sample=S))
  expect_equal(r$omega,1);expect_equal(r$CR,1);expect_true(is.na(r$H))
  expect_match(r$statut,"singuli")
})

test_that("0.2.3 : charges lues dans la table des parametres avec un effet direct sur un indicateur", {
  skip_if_not_installed("lavaan")
  HS <- lavaan::HolzingerSwineford1939
  m <- "visuel =~ x1 + x2 + x3\n verbal =~ x4 + x5 + x6\n vitesse =~ x7 + x8 + x9"
  fit0 <- lavaan::cfa(m, data = HS)
  fit <- lavaan::sem(paste(m, "\n x4 ~ x1"), data = HS)
  x <- as.cfaModel(fit)
  ss <- lavaan::standardizedSolution(fit)
  expect_equal(unname(x$loadings["x1", "visuel"]), ss$est.std[ss$lhs == "visuel" & ss$op == "=~" & ss$rhs == "x1"], tolerance = 1e-8)
  expect_true(all(x$loadings[x$loading_pattern] > .2))
  sig <- compute_diagnostics(x)
  expect_false(any(sig$signal == "Charge faible" & grepl("= 0,000", sig$detail)))
  expect_true(any(sig$signal == "Effet direct sur indicateur"))
  rel <- compute_reliability(x)
  expect_true(grepl("Effet direct", rel$statut[rel$facteur == "verbal"]))
  expect_true(is.na(rel$omega[rel$facteur == "verbal"]))
  expect_equal(rel$omega[rel$facteur != "verbal"], compute_reliability(as.cfaModel(fit0))$omega[-2], tolerance = .02)
  expect_true(is.na(compute_validity(x)$table$AVE[2]))
  expect_true(any(grepl("x4", sig$detail[sig$signal == "Effet direct sur indicateur"])))
  # Sans effet direct, l'extraction redonne les charges standardisees de lavaan
  x0 <- as.cfaModel(fit0)
  expect_equal(unname(x0$loadings[x0$loading_pattern]), unname(lavaan::lavInspect(fit0, "std")$lambda[x0$loading_pattern]), tolerance = 1e-8)
  expect_equal(unname(x0$loadings_unstd[x0$loading_pattern]), unname(lavaan::lavInspect(fit0, "est")$lambda[x0$loading_pattern]), tolerance = 1e-8)
})

test_that("0.2.3 : affichage multigroupe, modele juste identifie et virgule decimale des signaux", {
  skip_if_not_installed("lavaan")
  HS <- lavaan::HolzingerSwineford1939
  m <- "visuel =~ x1 + x2 + x3\n verbal =~ x4 + x5 + x6\n vitesse =~ x7 + x8 + x9"
  out <- capture.output(print(cfadiag(lavaan::cfa(m, data = HS, group = "school"))))
  expect_true(any(grepl("tous groupes confondus", out)))
  out1 <- capture.output(print(cfadiag(lavaan::cfa("verbal =~ x4 + x5 + x6", data = HS))))
  expect_false(any(grepl("indisponible", out1)))
  sig <- compute_diagnostics(as.cfaModel(lavaan::cfa(paste(m, "\n x7 ~~ x8"), data = HS)))
  expect_true(any(grepl("0,471", sig$detail, fixed = TRUE)))
})

test_that("0.2.3 : variance residuelle d'un indicateur predicteur reconstituee", {
  skip_if_not_installed("lavaan")
  HS <- lavaan::HolzingerSwineford1939
  m <- "visuel =~ x1 + x2 + x3\n verbal =~ x4 + x5 + x6\n vitesse =~ x7 + x8 + x9"
  x <- as.cfaModel(lavaan::sem(paste(m, "\n x4 ~ x1"), data = HS))
  expect_equal(unname(x$theta["x1", "x1"]), 1 - unname(x$loadings["x1", "visuel"])^2, tolerance = 1e-8)
  rel <- compute_reliability(x)
  expect_false(is.na(rel$H[rel$facteur == "visuel"]))
  expect_gte(rel$H[1], rel$omega[1])
})
