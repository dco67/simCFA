#' Construire un objet cfaModel
#'
#' @param fit Ajustement lavaan facultatif.
#' @param loadings Matrice des saturations standardisées (indicateurs par facteurs).
#' @param loadings_unstd Matrice non standardisée. Si omise, elle est reconstruite
#'   avec les variances de `implied` et `cov_lv` lorsqu'elles sont disponibles.
#' @param cov_lv,cor_lv Covariances et corrélations latentes impliquées.
#' @param theta Matrice standardisée des variances et covariances résiduelles.
#' @param implied,sample Matrices impliquée et observée des indicateurs.
#' @param data Données brutes facultatives.
#' @param ordered Indicateurs ordinaux.
#' @param group Étiquette du groupe.
#' @param meta Métadonnées, notamment le traitement des absences.
#' @param loading_pattern Matrice logique des relations de mesure spécifiées.
#'   Elle distingue une saturation estimée nulle d'une absence de relation.
#' @return Un objet de classe `cfaModel`.
#' @examples
#' # Construire un objet à la main, à partir de charges publiées
#' L <- matrix(c(.80, .75, .70, .65), ncol = 1,
#'             dimnames = list(paste0("E", 1:4), "Engagement"))
#' Sigma <- L %*% t(L); diag(Sigma) <- 1     # covariances impliquées
#' m <- cfaModel(loadings = L, cov_lv = matrix(1, 1, 1, dimnames = list("Engagement", "Engagement")),
#'               implied = Sigma, sample = Sigma)
#' m
#' compute_reliability(m)   # oméga de population : environ 0,817
#' @export
cfaModel <- function(fit=NULL,loadings,loadings_unstd=loadings,cov_lv=NULL,
                     cor_lv=NULL,theta=NULL,implied=NULL,sample=NULL,data=NULL,
                     ordered=character(),group=NULL,meta=list(),loading_pattern=NULL) {
  if(missing(loadings) || is.null(loadings)) stop("Les saturations sont requises.",call.=FALSE)
  L<-as.matrix(loadings)
  if(!is.numeric(L) || !nrow(L) || !ncol(L) || is.null(rownames(L)) || is.null(colnames(L)) ||
     anyDuplicated(rownames(L)) || anyDuplicated(colnames(L)))
    stop("La matrice des saturations doit \u00eatre num\u00e9rique, non vide et porter des noms uniques.",call.=FALSE)
  items<-rownames(L);fs<-colnames(L)
  align<-function(x,names,what) {
    if(is.null(x)) return(NULL)
    x<-as.matrix(x)
    if(!is.numeric(x) || !identical(dim(x),c(length(names),length(names)))) stop(paste("Dimensions invalides :",what),call.=FALSE)
    if(is.null(rownames(x)) && is.null(colnames(x))) dimnames(x)<-list(names,names)
    if(!setequal(rownames(x),names) || !setequal(colnames(x),names)) stop(paste("Noms incompatibles :",what),call.=FALSE)
    x<-x[names,names,drop=FALSE]
    if(!isTRUE(all.equal(unname(x),unname(t(x)),tolerance=1e-8))) stop(paste("Matrice non sym\u00e9trique :",what),call.=FALSE)
    x
  }
  if(is.null(cov_lv)) cov_lv<-if(!is.null(cor_lv)) cor_lv else diag(length(fs))
  cov_lv<-align(cov_lv,fs,"covariances latentes")
  if(is.null(cor_lv)) cor_lv<-suppressWarnings(stats::cov2cor(cov_lv))
  cor_lv<-align(cor_lv,fs,"corr\u00e9lations latentes")
  implied<-align(implied,items,"covariances impliqu\u00e9es")
  sample<-align(sample,items,"covariances observ\u00e9es")
  if(is.null(theta) && !is.null(implied) && all(is.finite(implied)) && all(diag(implied)>0))
    theta<-stats::cov2cor(implied)-L%*%cor_lv%*%t(L)
  theta<-align(theta,items,"r\u00e9sidus standardis\u00e9s")
  if(missing(loadings_unstd) && !is.null(implied) && all(diag(cov_lv)>0) && all(diag(implied)>0))
    loadings_unstd<-sweep(sweep(L,1,sqrt(diag(implied)),"*"),2,sqrt(diag(cov_lv)),"/")
  U<-as.matrix(loadings_unstd)
  if(!is.numeric(U) || !identical(dim(U),dim(L))) stop("Dimensions incompatibles des saturations non standardis\u00e9es.",call.=FALSE)
  if(is.null(rownames(U)) && is.null(colnames(U))) dimnames(U)<-dimnames(L)
  if(!setequal(rownames(U),items) || !setequal(colnames(U),fs)) stop("Noms incompatibles des saturations non standardis\u00e9es.",call.=FALSE)
  U<-U[items,fs,drop=FALSE]
  if(is.null(loading_pattern)) loading_pattern<-is.finite(L) & L!=0
  loading_pattern<-as.matrix(loading_pattern)
  if(!identical(dim(loading_pattern),dim(L)) || anyNA(loading_pattern)) stop("Structure de mesure incompatible.",call.=FALSE)
  storage.mode(loading_pattern)<-"logical";dimnames(loading_pattern)<-dimnames(L)
  if(!is.null(data)) {
    data<-as.data.frame(data)
    if(!all(items %in% names(data))) stop("Indicateurs absents des donn\u00e9es brutes.",call.=FALSE)
    data<-data[,items,drop=FALSE]
    if(!all(vapply(data,is.numeric,logical(1)))) stop("Les donn\u00e9es brutes des indicateurs doivent \u00eatre num\u00e9riques.",call.=FALSE)
  }
  if(!all(ordered %in% items)) stop("Indicateur ordinal absent du mod\u00e8le.",call.=FALSE)
  structure(list(fit=fit,loadings=L,loadings_unstd=U,cov_lv=cov_lv,cor_lv=cor_lv,
    theta=theta,implied=implied,sample=sample,data=data,ordered=ordered,group=group,
    meta=meta,loading_pattern=loading_pattern),class="cfaModel")
}

#' Convertir un ajustement en objet cfaModel
#' @param x Ajustement lavaan à convertir.
#' @param ... Arguments supplémentaires.
#' @return Un objet cfaModel, ou une liste par groupe.
#' @examples
#' library(lavaan)
#' modele <- "visuel =~ x1 + x2 + x3
#'            verbal =~ x4 + x5 + x6
#'            vitesse =~ x7 + x8 + x9"
#' fit <- cfa(modele, data = HolzingerSwineford1939)
#' m <- as.cfaModel(fit)
#' m
#' m$loadings          # charges standardisées
#' m$cor_lv            # corrélations impliquées entre facteurs
#'
#' # Modèle multigroupe : un objet par groupe, ou un seul groupe
#' fit_g <- cfa(modele, data = HolzingerSwineford1939, group = "school")
#' names(as.cfaModel(fit_g))
#' as.cfaModel(fit_g, group = "Pasteur")
#'
#' # Estimation à partir d'une matrice de covariances
#' S <- cov(HolzingerSwineford1939[paste0("x", 1:9)])
#' fit_s <- cfa(modele, sample.cov = S, sample.nobs = 301)
#' as.cfaModel(fit_s)  # pas de données brutes : HTMT oui, bootstrap non
#' @export
as.cfaModel <- function(x,...) UseMethod("as.cfaModel")

#' @rdname as.cfaModel
#' @param group Étiquette ou numéro de groupe; NULL extrait tous les groupes.
#' @details Les covariances latentes sont les covariances impliquées, y compris
#'   dans un SEM. La covariance observée vient des statistiques d'échantillon :
#'   covariance EM avec FIML et corrélations polychoriques avec indicateurs ordinaux.
#' @export
as.cfaModel.lavaan <- function(x,group=NULL,...) {
  if(!isTRUE(lavaan::lavInspect(x,"converged"))) warning("Le mod\u00e8le n'a pas converg\u00e9; les indices d\u00e9pendant des param\u00e8tres ne sont pas interpr\u00e9tables.",call.=FALSE)
  ng<-lavaan::lavInspect(x,"ngroups");labels<-lavaan::lavInspect(x,"group.label")
  if(!length(labels)) labels<-as.character(seq_len(ng))
  idx<-if(is.null(group)) seq_len(ng) else if(is.numeric(group)) group else match(group,labels)
  if(!length(idx) || anyNA(idx) || any(!is.finite(idx)) || any(idx!=as.integer(idx)) || any(idx<1 | idx>ng)) stop("Groupe introuvable dans le mod\u00e8le.",call.=FALSE)
  out<-lapply(idx,function(g) extraire_groupe(x,g,if(ng>1) labels[g] else NULL));names(out)<-labels[idx]
  if(length(out)==1) out[[1]] else out
}

#' @rdname as.cfaModel
#' @export
as.cfaModel.default <- function(x,...) stop("Fournissez un ajustement lavaan ou un objet cfaModel.",call.=FALSE)
par_groupe <- function(obj,g,ng) if(ng>1) obj[[g]] else obj

extraire_groupe <- function(x,g,label) {
  ng<-lavaan::lavInspect(x,"ngroups")
  std<-par_groupe(lavaan::lavInspect(x,"std"),g,ng)
  ov<-lavaan::lavNames(x,"ov",group=g)
  pt<-lavaan::parTable(x);pt<-pt[pt$group==g,,drop=FALSE]
  measurement<-pt[pt$op=="=~" & pt$rhs %in% ov & (pt$free>0 | is.na(pt$est) | pt$est!=0),,drop=FALSE]
  lv<-lavaan::lavNames(x,"lv",group=g)
  fs<-intersect(lv,unique(measurement$lhs))
  if(!length(fs)) stop("Aucun facteur mesur\u00e9 par des indicateurs observ\u00e9s.",call.=FALSE)
  items<-intersect(ov,unique(measurement$rhs))
  # Charges lues dans la table des param\u00e8tres : la matrice lambda de lavaan
  # ne les contient pas lorsqu'un indicateur est aussi r\u00e9gress\u00e9 (variables fant\u00f4mes).
  ss<-lavaan::standardizedSolution(x,type="std.all",se=FALSE,zstat=FALSE,pvalue=FALSE,ci=FALSE,remove.def=TRUE)
  if(!"group" %in% names(ss)) ss$group<-1L
  ss<-ss[ss$group==g & ss$op=="=~",,drop=FALSE]
  lu<-ls<-matrix(0,length(items),length(fs),dimnames=list(items,fs))
  for(i in seq_len(nrow(measurement))) if(measurement$lhs[i] %in% fs && measurement$rhs[i] %in% items) {
    lu[measurement$rhs[i],measurement$lhs[i]]<-measurement$est[i]
    k<-which(ss$lhs==measurement$lhs[i] & ss$rhs==measurement$rhs[i])
    if(length(k)) ls[measurement$rhs[i],measurement$lhs[i]]<-ss$est.std[k[1]]
  }
  pattern<-matrix(FALSE,length(items),length(fs),dimnames=list(items,fs))
  for(i in seq_len(nrow(measurement))) if(measurement$lhs[i] %in% fs && measurement$rhs[i] %in% items) pattern[measurement$rhs[i],measurement$lhs[i]]<-TRUE
  P<-par_groupe(lavaan::lavInspect(x,"cov.lv"),g,ng)[fs,fs,drop=FALSE]
  S<-tryCatch(par_groupe(lavaan::lavInspect(x,"sampstat"),g,ng)$cov[items,items,drop=FALSE],error=function(e) NULL)
  data<-tryCatch({ d<-par_groupe(lavaan::lavInspect(x,"data"),g,ng);if(is.null(d) || !length(d)) NULL else as.data.frame(d)[,items,drop=FALSE] },error=function(e) NULL)
  options<-lavaan::lavInspect(x,"options")
  impl<-par_groupe(lavaan::lavInspect(x,"cov.ov"),g,ng)[items,items,drop=FALSE]
  th<-std$theta[items,items,drop=FALSE]
  # Indicateur repr\u00e9sent\u00e9 par une variable fant\u00f4me (indicateur r\u00e9gress\u00e9 ou pr\u00e9dicteur) :
  # sa variance r\u00e9siduelle est dans psi; on la reconstitue \u00e0 partir de la matrice impliqu\u00e9e.
  fant<-intersect(items,colnames(std$lambda))
  if(length(fant)) {
    Pc<-suppressWarnings(stats::cov2cor(P))
    commun<-diag(ls%*%Pc%*%t(ls));names(commun)<-items
    for(n in fant) th[n,n]<-1-commun[n]
  }
  cfaModel(fit=x,loadings=ls,loadings_unstd=lu,
    cov_lv=P,cor_lv=suppressWarnings(stats::cov2cor(P)),theta=th,
    implied=impl,sample=S,data=data,
    ordered=intersect(lavaan::lavNames(x,"ov.ord",group=g),items),group=label,loading_pattern=pattern,
    meta=list(estimator=options$estimator,missing=options$missing,
      nobs=par_groupe(lavaan::lavInspect(x,"nobs"),g,ng),
      converged=isTRUE(lavaan::lavInspect(x,"converged")),
      admissible=isTRUE(lavaan::lavInspect(x,"converged")) && isTRUE(suppressWarnings(lavaan::lavInspect(x,"post.check"))),
      direct_indicator_effects=any(pt$op=="~" & pt$lhs %in% items),
      direct_effect_items=unique(pt$lhs[pt$op=="~" & pt$lhs %in% items])))
}
effet_direct <- function(model,it) {
  if(!isTRUE(model$meta$direct_indicator_effects)) return(FALSE)
  cible<-model$meta$direct_effect_items
  if(is.null(cible)) TRUE else any(it %in% cible)
}
items_du_facteur <- function(model,f) {
  pattern<-model$loading_pattern
  if(is.null(pattern)) pattern<-is.finite(model$loadings) & model$loadings!=0
  rownames(model$loadings)[pattern[,f]]
}

#' @export
print.cfaModel <- function(x,...) {
  cat("\nObjet cfaModel",if(!is.null(x$group)) paste0(" (groupe ",x$group,")"),"\n")
  cat("Facteurs :",paste(colnames(x$loadings),collapse=", "),"\nIndicateurs :",nrow(x$loadings),"\n")
  cat("Donn\u00e9es brutes disponibles :",if(is.null(x$data)) "non" else "oui","\n")
  if(length(x$ordered)) cat("Indicateurs ordinaux :",length(x$ordered),"\n")
  invisible(x)
}
