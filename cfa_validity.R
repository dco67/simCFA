# Moteur unique de fiabilité/validité utilisé à l'écran et dans le rapport.
# Les seuils sont des repères descriptifs, sans verdict automatique de validité.
cfa_validity <- function(fit, digits=3, seuil_cr=.70, seuil_ave=.50, seuil_h=.70) {
  if (!inherits(fit,"lavaan")) stop("Un modèle lavaan est requis.")
  if (lavaan::lavInspect(fit,"ngroups")!=1L) stop("Ces indices sont réservés ici aux modèles à un groupe.")
  if (!isTRUE(lavaan::lavInspect(fit,"converged")) || !isTRUE(suppressWarnings(lavaan::lavInspect(fit,"post.check")))) stop("Solution non convergée ou inadmissible : indices non interprétables.")
  p <- lavaan::parameterEstimates(fit); load <- p[p$op=="=~",]; factors <- unique(load$lhs)
  if (!length(factors)) stop("Aucun facteur de mesure.")
  if (anyDuplicated(load$rhs) || any(load$rhs %in% factors) || any(p$op=="~"))
    stop("Indices non calculés : structure complexe (saturations croisées, second ordre ou régressions). Analyse spécifique du score requise.")
  items <- setNames(lapply(factors,function(f) load$rhs[load$lhs==f]),factors)
  matrices <- lavaan::lavInspect(fit,"est")
  L <- matrices$lambda; P <- matrices$psi; T <- matrices$theta
  Sigma <- L %*% P %*% t(L)+T
  COR <- lavaan::lavInspect(fit,"cor.lv")[factors,factors,drop=FALSE]
  # sampstat = statistiques des données (polychorique si ordered), jamais cor.ov.
  S <- lavaan::lavInspect(fit,"sampstat")$cov
  R <- cov2cor(S)
  raw <- tryCatch(as.data.frame(lavaan::lavInspect(fit,"data")),error=function(e) NULL)
  rows <- lapply(factors,function(f) {
    vv <- items[[f]]; lambda <- L[vv,f]; theta <- T[vv,vv,drop=FALSE]; phi <- P[f,f]
    common <- phi*sum(lambda)^2; total <- sum(Sigma[vv,vv,drop=FALSE])
    omega <- if (is.finite(total) && total>0) common/total else NA_real_
    ave <- mean(lambda^2*phi/diag(Sigma[vv,vv,drop=FALSE]))
    h <- tryCatch({ a <- phi*as.numeric(t(lambda)%*%solve(theta,lambda)); a/(1+a) },error=function(e) NA_real_)
    alpha <- NA_real_; n_complete <- NA_integer_
    if (!is.null(raw) && all(vv %in% names(raw))) {
      x <- raw[stats::complete.cases(raw[vv]),vv,drop=FALSE]; n_complete <- nrow(x)
      if (ncol(x)>1 && nrow(x)>1) { sx<-stats::cov(x); den<-sum(sx); if (den>0) alpha<-ncol(x)/(ncol(x)-1)*(1-sum(diag(sx))/den) }
    }
    other <- setdiff(factors,f)
    data.frame(Facteur=f,Alpha=alpha,Omega=omega,CR=omega,AVE=ave,H=h,
       MSV=if(length(other)) max(COR[f,other]^2) else NA_real_,
       ASV=if(length(other)) mean(COR[f,other]^2) else NA_real_,N_items=length(vv),N_alpha=n_complete)
  })
  RES <- do.call(rbind,rows); RES$flag_disc <- !is.na(RES$MSV) & RES$AVE<RES$MSV
  HTMT <- matrix(NA_real_,length(factors),length(factors),dimnames=list(factors,factors))
  if (length(factors)>1) for (pair in combn(seq_along(factors),2,simplify=FALSE)) {
    a<-pair[1]; b<-pair[2]; va<-items[[a]]; vb<-items[[b]]
    aa<-abs(R[va,va,drop=FALSE]); bb<-abs(R[vb,vb,drop=FALSE])
    den<-sqrt(mean(aa[lower.tri(aa)])*mean(bb[lower.tri(bb)]))
    value<-if (is.finite(den) && den>0) mean(abs(R[va,vb,drop=FALSE]))/den else NA_real_
    HTMT[a,b]<-HTMT[b,a]<-value
  }
  FL<-COR; diag(FL)<-sqrt(RES$AVE); FL[upper.tri(FL)]<-NA_real_
  ordinal<-length(lavaan::lavNames(fit,"ov.ord"))>0
  notes<-c("Oméga = fiabilité de la somme des items dans leur unité d'origine; les covariances résiduelles sont incluses au dénominateur. CR désigne ici la même quantité et n'est pas une preuve indépendante.",
    "Les items de sens opposé ne sont jamais inversés automatiquement. Recoder les items si le score visé l'exige, puis réestimer.",
    "Alpha : somme des scores observés, cas complets par facteur (N_alpha). H : combinaison à poids optimaux selon le modèle, différente de la somme simple.",
    "HTMT arithmétique sur les corrélations des données utilisées par lavaan (statistiques d'échantillon; gestion des absences de l'estimation). Ce tableau présente des estimations ponctuelles. Des intervalles bootstrap peuvent être demandés séparément dans la synthèse pour les données compatibles.",
    if (ordinal) "Indicateurs ordinaux : oméga, CR, H et AVE portent sur les réponses latentes continues; HTMT emploie les corrélations polychoriques/polyseriales. Ce ne sont pas la fiabilité de la somme des catégories observées." else "Indicateurs continus : HTMT emploie les corrélations issues des covariances d'échantillon.",
    if(length(factors)==1) "Un seul facteur : HTMT, MSV, ASV et validité discriminante ne sont pas applicables." else "Les repères .70 (fiabilité), .50 (AVE), .85/.90 (HTMT) ne démontrent pas à eux seuls la validité d'un construit.")
  structure(list(results=RES,FL=FL,COR=COR,HTMT=HTMT,Lambda=setNames(lapply(factors,function(f)L[items[[f]],f]),factors),items=items,
      notes=notes,ordinal=ordinal,seuils=list(cr=seuil_cr,ave=seuil_ave,h=seuil_h),digits=digits,call=match.call()),class="cfa_validity")
}
print.cfa_validity <- function(x,type="all",...) {
  if(type %in% c("all","indices")) print(x$results,digits=x$digits,row.names=FALSE)
  if(type %in% c("all","fornell")) print(round(x$FL,x$digits))
  if(type %in% c("all","htmt")) print(round(x$HTMT,x$digits))
  cat(paste(x$notes,collapse="\n\n"),"\n"); invisible(x)
}
summary.cfa_validity <- function(object,...) print(object,...)
as.data.frame.cfa_validity <- function(x,...) x$results
calculate_htmt_manual <- function(fit,data=NULL) cfa_validity(fit)$HTMT

plot.cfa_validity <- function(x,type=c("indices","fornell"),...) {
  type<-match.arg(type)
  if(type=="indices") {
    values<-t(as.matrix(x$results[,c("Omega","AVE","H")]))
    colnames(values)<-x$results$Facteur
    graphics::barplot(values,beside=TRUE,ylim=c(0,max(1,values,na.rm=TRUE)),
      col=c("#2C3E50","#3498DB","#18BC9C"),legend.text=rownames(values),
      ylab="Indice (estimation ponctuelle)",...)
  } else {
    z<-x$FL;z[upper.tri(z)]<-t(z)[upper.tri(z)]
    graphics::image(seq_len(nrow(z)),seq_len(ncol(z)),z,zlim=c(-1,1),
      axes=FALSE,xlab="",ylab="",col=grDevices::colorRampPalette(c("#2166ac","white","#b2182b"))(60),...)
    graphics::axis(1,seq_len(nrow(z)),rownames(z));graphics::axis(2,seq_len(ncol(z)),colnames(z))
    for(i in seq_len(nrow(z))) for(j in seq_len(ncol(z))) graphics::text(i,j,format(round(z[i,j],x$digits)))
  }
  invisible(x)
}
