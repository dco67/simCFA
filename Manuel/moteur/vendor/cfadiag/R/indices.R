#' Fiabilité des scores composés
#'
#' Calcule la fiabilité de la somme brute (omega), de la somme des scores
#' standardisés (CR), l'alpha issu de la covariance observée et le coefficient H.
#' H utilise la matrice résiduelle complète : a = lambda' Theta^-1 lambda,
#' puis H = a/(1+a), en métrique standardisée. Les covariances résiduelles sont
#' conservées. Les signes des items ne sont pas inversés automatiquement.
#'
#' @param model Objet cfaModel.
#' @param variance Variance de la somme au dénominateur de l'oméga et de la
#'   CR : `"impliquee"` (défaut, variance reproduite par le modèle) ou
#'   `"observee"` (variance observée, comme `semTools::compRelSEM()` par
#'   défaut). Les deux coïncident quand le modèle reproduit exactement les
#'   covariances des indicateurs du facteur.
#' @details Les facteurs avec saturations croisées, effets directs sur leurs
#'   indicateurs ou solution inadmissible ne reçoivent pas de fiabilité cible
#'   automatique. Le champ statut explique les valeurs non calculées. Avec des
#'   indicateurs ordinaux, ces indices portent sur les réponses latentes continues.
#'   L'alpha utilise la covariance observée fournie (EM avec FIML ou polychorique
#'   avec items ordinaux); ce n'est pas nécessairement l'alpha brut sur cas complets.
#' @return Un data.frame : facteur, n_items, omega, CR, alpha, H et statut.
#' @examples
#' library(lavaan)
#' modele <- "visuel =~ x1 + x2 + x3
#'            verbal =~ x4 + x5 + x6
#'            vitesse =~ x7 + x8 + x9"
#' fit <- cfa(modele, data = HolzingerSwineford1939)
#' compute_reliability(as.cfaModel(fit))
#' compute_reliability(as.cfaModel(fit), variance = "observee")
#'
#' # Une covariance résiduelle entre x7 et x8 modifie l'oméga de « vitesse »
#' fit2 <- cfa(paste(modele, "\n x7 ~~ x8"), data = HolzingerSwineford1939)
#' compute_reliability(as.cfaModel(fit2))
#'
#' # Une charge croisée : pas de fiabilité cible sans définition du score
#' fit3 <- cfa(paste(modele, "\n visuel =~ x9"), data = HolzingerSwineford1939)
#' compute_reliability(as.cfaModel(fit3))[, c("facteur", "omega", "alpha", "statut")]
#' @export
compute_reliability <- function(model,variance=c("impliquee","observee")) {
  verifier(model);variance<-match.arg(variance);fs<-colnames(model$loadings)
  pattern<-model$loading_pattern
  if(is.null(pattern)) pattern<-is.finite(model$loadings) & model$loadings!=0
  out<-lapply(fs,function(f) {
    it<-items_du_facteur(model,f);k<-length(it)
    omega<-cr<-alpha<-h<-NA_real_;status<-"Calcul\u00e9 selon le mod\u00e8le; pr\u00e9ciser le score et ses unit\u00e9s."
    if(!is.null(model$sample) && k>=2) {
      S<-model$sample[it,it,drop=FALSE];den<-sum(S)
      if(all(is.finite(S)) && den>0) alpha<-k/(k-1)*(1-sum(diag(S))/den)
    }
    supported<-k>=2 && !any(rowSums(pattern[it,,drop=FALSE])>1) &&
      !effet_direct(model,it) && !identical(model$meta$admissible,FALSE)
    if(supported) {
      ls<-model$loadings[it,f];lu<-model$loadings_unstd[it,f];phi<-model$cov_lv[f,f]
      Theta<-if(is.null(model$theta)) diag(1-ls^2,nrow=k) else model$theta[it,it,drop=FALSE]
      if(all(is.finite(c(ls,lu,phi,Theta))) && phi>0 && min(eigen(Theta,symmetric=TRUE,only.values=TRUE)$values)>= -1e-10) {
        if(min(eigen(Theta,symmetric=TRUE,only.values=TRUE)$values)>1e-10) {
          a<-as.numeric(crossprod(ls,solve(Theta,ls)));if(a>=0) h<-a/(1+a)
        } else status<-"H non calcul\u00e9 : la matrice r\u00e9siduelle est singuli\u00e8re; om\u00e9ga et CR restent \u00e9valuables."
        V<-if(variance=="observee") model$sample else model$implied
        if(!is.null(V) && !is.null(model$implied)) {
          sigma<-model$implied[it,it,drop=FALSE];vv<-V[it,it,drop=FALSE];den<-sum(vv)
          if(all(is.finite(c(sigma,vv))) && all(diag(sigma)>0) && den>0) {
            omega<-sum(lu)^2*phi/den
            sdi<-sqrt(diag(sigma))
            cr<-sum(ls)^2/sum(vv/outer(sdi,sdi))
          }
        } else if(variance=="observee") status<-"Matrice observ\u00e9e indisponible : om\u00e9ga et CR non calcul\u00e9s avec la variance observ\u00e9e."
      } else status<-"Solution ou matrice r\u00e9siduelle inadmissible : fiabilit\u00e9 cible non calcul\u00e9e."
    } else status<-if(k<2) "Au moins deux indicateurs sont requis ici." else if(identical(model$meta$admissible,FALSE)) "Solution non converg\u00e9e ou inadmissible." else if(effet_direct(model,it)) "Effet direct sur un indicateur : d\u00e9finir un score et sa variance cible avant d'\u00e9valuer sa fiabilit\u00e9." else "Structure complexe : d\u00e9finir un score et sa variance cible avant d'\u00e9valuer sa fiabilit\u00e9."
    data.frame(facteur=f,n_items=k,omega=omega,CR=cr,alpha=alpha,H=h,statut=status)
  })
  out<-do.call(rbind,out);rownames(out)<-NULL;out
}

#' Validité convergente et discriminante
#' @param model Objet cfaModel.
#' @details L'AVE est la moyenne des saturations standardisées au carré pour
#'   une structure simple. Une structure complexe ou inadmissible produit NA.
#'   Avec un seul facteur, MSV, ASV et la comparaison AVE > MSV sont non applicables.
#' @return Liste table et fornell_larcker. Les corrélations factorielles sont
#'   les corrélations impliquées; leur valeur absolue est utilisée pour la comparaison.
#' @examples
#' library(lavaan)
#' modele <- "visuel =~ x1 + x2 + x3
#'            verbal =~ x4 + x5 + x6
#'            vitesse =~ x7 + x8 + x9"
#' fit <- cfa(modele, data = HolzingerSwineford1939)
#' v <- compute_validity(as.cfaModel(fit))
#' v$table              # AVE, MSV, ASV
#' round(v$fornell_larcker, 3)
#'
#' # Dans un SEM, les corrélations entre facteurs restent les corrélations impliquées
#' fit_sem <- sem(paste(modele, "\n verbal ~ visuel\n vitesse ~ visuel + verbal"),
#'                data = HolzingerSwineford1939)
#' compute_validity(as.cfaModel(fit_sem))$table
#' @export
compute_validity <- function(model) {
  verifier(model);fs<-colnames(model$loadings)
  pattern<-model$loading_pattern;if(is.null(pattern)) pattern<-is.finite(model$loadings) & model$loadings!=0
  ave<-vapply(fs,function(f) {
    it<-items_du_facteur(model,f)
    if(!length(it) || identical(model$meta$admissible,FALSE) || effet_direct(model,it) || any(rowSums(pattern[it,,drop=FALSE])>1)) return(NA_real_)
    mean(model$loadings[it,f]^2)
  },numeric(1))
  R<-model$cor_lv[fs,fs,drop=FALSE]
  msv<-asv<-stats::setNames(rep(NA_real_,length(fs)),fs)
  if(length(fs)>1) for(f in fs) {
    values<-R[f,setdiff(fs,f)]^2
    if(all(is.finite(values))) {msv[f]<-max(values);asv[f]<-mean(values)}
  }
  FL<-R;diag(FL)<-sqrt(ave)
  list(table=data.frame(facteur=fs,AVE=unname(ave),MSV=unname(msv),ASV=unname(asv),AVE_superieure_MSV=unname(ave>msv),row.names=NULL),fornell_larcker=FL)
}

#' Rapport hétérotrait-monotrait (HTMT)
#' @param model Objet cfaModel.
#' @param R Matrice de corrélations; par défaut celle issue de la covariance observée.
#' @param type htmt (moyennes arithmétiques) ou htmt2 (géométriques).
#' @param absolute Utiliser les valeurs absolues des corrélations.
#' @details Aucune paire n'est calculée pour des blocs partageant un indicateur
#'   ou contenant moins de deux indicateurs. Une moyenne monotrait non positive,
#'   des corrélations non calculables ou un zéro avec HTMT2 produisent NA.
#' @return Matrice symétrique; non applicable pour un seul facteur.
#' @examples
#' library(lavaan)
#' modele <- "visuel =~ x1 + x2 + x3
#'            verbal =~ x4 + x5 + x6
#'            vitesse =~ x7 + x8 + x9"
#' fit <- cfa(modele, data = HolzingerSwineford1939)
#' m <- as.cfaModel(fit)
#' round(compute_htmt(m), 3)
#' round(compute_htmt(m, type = "htmt2"), 3)
#'
#' # Indicateurs ordinaux : le HTMT porte sur les corrélations polychoriques
#' HS4 <- as.data.frame(lapply(HolzingerSwineford1939[paste0("x", 1:9)],
#'   function(z) cut(z, quantile(z, 0:4 / 4), include.lowest = TRUE, labels = FALSE)))
#' fit_o <- cfa(modele, data = HS4, ordered = names(HS4))
#' round(compute_htmt(as.cfaModel(fit_o)), 3)
#' @export
compute_htmt <- function(model,R=NULL,type=c("htmt","htmt2"),absolute=TRUE) {
  verifier(model);type<-match.arg(type)
  if(is.null(R)) {
    if(is.null(model$sample)) stop("Aucune matrice observ\u00e9e disponible pour le HTMT.",call.=FALSE)
    R<-suppressWarnings(stats::cov2cor(model$sample))
  }
  fs<-colnames(model$loadings)
  htmt_matrice(R,stats::setNames(lapply(fs,function(f) items_du_facteur(model,f)),fs),type,absolute)
}

htmt_matrice <- function(R,blocs,type="htmt",absolute=TRUE) {
  R<-as.matrix(R)
  if(!is.numeric(R) || nrow(R)!=ncol(R) || is.null(rownames(R)) || is.null(colnames(R)) ||
     !setequal(rownames(R),colnames(R)) || !all(unique(unlist(blocs)) %in% rownames(R))) stop("Matrice de corr\u00e9lations incompatible avec les indicateurs.",call.=FALSE)
  R<-R[rownames(R),rownames(R),drop=FALSE]
  if(!isTRUE(all.equal(unname(R),unname(t(R)),tolerance=1e-8)) || any(abs(R[is.finite(R)])>1+1e-8)) stop("La matrice doit \u00eatre sym\u00e9trique et ses corr\u00e9lations comprises entre -1 et 1.",call.=FALSE)
  if(length(absolute)!=1 || is.na(absolute) || !is.logical(absolute)) stop("absolute doit \u00eatre TRUE ou FALSE.",call.=FALSE)
  if(absolute) R<-abs(R)
  average<-function(z) {
    if(!length(z) || any(!is.finite(z))) return(NA_real_)
    if(type=="htmt") mean(z) else if(any(z<=0)) NA_real_ else exp(mean(log(z)))
  }
  mono<-vapply(blocs,function(it) {
    if(length(it)<2) return(NA_real_)
    r<-R[it,it,drop=FALSE];average(r[upper.tri(r)])
  },numeric(1))
  fs<-names(blocs);H<-matrix(NA_real_,length(fs),length(fs),dimnames=list(fs,fs))
  if(length(fs)<2) return(H)
  diag(H)<-ifelse(is.finite(mono) & mono>0,1,NA_real_)
  for(i in seq_along(fs)) for(j in seq_along(fs)) if(i<j) {
    if(length(intersect(blocs[[i]],blocs[[j]])) || !is.finite(mono[i]) || !is.finite(mono[j]) || mono[i]<=0 || mono[j]<=0) next
    H[i,j]<-H[j,i]<-average(R[blocs[[i]],blocs[[j]],drop=FALSE])/sqrt(mono[i]*mono[j])
  }
  H
}

#' Intervalles bootstrap du HTMT
#' @param model Objet cfaModel avec données brutes.
#' @param R Nombre entier de réplications (au moins deux).
#' @param seed Graine entière facultative; lorsqu'elle est fournie, l'état
#'   aléatoire global est restauré après calcul.
#' @param type,absolute Voir compute_htmt.
#' @param level Niveau de l'intervalle percentile, strictement entre 0 et 1.
#' @param missing Traitement des absences : auto, complete.obs,
#'   pairwise.complete.obs ou fiml. auto reprend les métadonnées du modèle.
#' @details Le point estimé et chaque réplication emploient exactement le même
#'   traitement des absences. Avec FIML, lavCor réestime la covariance EM à chaque
#'   réplication. Aucune imputation ni suppression silencieuse n'est effectuée.
#'   Le bootstrap ordinal reste non proposé. Une paire sans réplication valide
#'   reçoit NA et n_valides=0. Une seule réplication valide ne suffit pas pour un IC.
#' @return Liste replications, table, level, type et missing.
#' @examples
#' library(lavaan)
#' modele <- "visuel =~ x1 + x2 + x3
#'            verbal =~ x4 + x5 + x6
#'            vitesse =~ x7 + x8 + x9"
#' fit <- cfa(modele, data = HolzingerSwineford1939)
#' b <- boot_htmt(as.cfaModel(fit), R = 200, seed = 1)
#' b$table
#'
#' # Données manquantes : la matrice EM est réestimée à chaque réplication
#' H2 <- HolzingerSwineford1939
#' set.seed(1); H2$x2[sample(301, 60)] <- NA
#' fit_m <- cfa(modele, data = H2, missing = "fiml")
#' boot_htmt(as.cfaModel(fit_m), R = 100, seed = 1)$table
#' \donttest{
#' # Pour une étude : au moins 1 000 réplications
#' boot_htmt(as.cfaModel(fit), R = 2000, seed = 1)$table
#' }
#' @export
boot_htmt <- function(model,R=1000,seed=NULL,type=c("htmt","htmt2"),absolute=TRUE,level=.95,
                      missing=c("auto","complete.obs","pairwise.complete.obs","fiml")) {
  verifier(model);type<-match.arg(type);missing<-match.arg(missing)
  if(!is.numeric(R) || length(R)!=1 || !is.finite(R) || R<2 || R>.Machine$integer.max || R!=as.integer(R)) stop("R doit \u00eatre un entier d'au moins 2.",call.=FALSE)
  if(!is.numeric(level) || length(level)!=1 || !is.finite(level) || level<=0 || level>=1) stop("level doit \u00eatre strictement entre 0 et 1.",call.=FALSE)
  X<-model$data
  if(is.null(X)) stop("Le bootstrap du HTMT exige les donn\u00e9es brutes.",call.=FALSE)
  if(length(model$ordered)) stop("Bootstrap non propos\u00e9 pour des indicateurs ordinaux.",call.=FALSE)
  fs<-colnames(model$loadings);blocs<-stats::setNames(lapply(fs,function(f) items_du_facteur(model,f)),fs)
  it<-unique(unlist(blocs));X<-as.matrix(X[,it,drop=FALSE])
  if(missing=="auto") {
    missing<-if(!anyNA(X)) "complete.obs" else if(isTRUE(model$meta$missing %in% c("ml","fiml","ml.x","fiml.x"))) "fiml" else if(identical(model$meta$missing,"pairwise")) "pairwise.complete.obs" else if(identical(model$meta$missing,"listwise")) "complete.obs" else stop("Pr\u00e9cisez le traitement des valeurs manquantes avec missing.",call.=FALSE)
  }
  if(missing=="complete.obs") X<-X[stats::complete.cases(X),,drop=FALSE]
  if(nrow(X)<3) stop("Au moins trois observations sont requises pour ce bootstrap.",call.=FALSE)
  corr<-function(data) {
    if(missing=="fiml" && anyNA(data)) {
      as.matrix(lavaan::lavCor(as.data.frame(data),missing="fiml",output="cor",se="none",test="none"))
    } else stats::cor(data,use=if(missing=="pairwise.complete.obs") "pairwise.complete.obs" else "everything")
  }
  cfadiag_with_seed(seed,{
    reps<-array(NA_real_,c(R,length(fs),length(fs)),dimnames=list(NULL,fs,fs))
    H<-compute_htmt(model,R=suppressWarnings(corr(X)),type=type,absolute=absolute)
    for(b in seq_len(R)) {
      Xb<-X[sample.int(nrow(X),replace=TRUE),,drop=FALSE]
      reps[b,,]<-tryCatch(suppressWarnings(htmt_matrice(corr(Xb),blocs,type,absolute)),error=function(e) NA_real_)
    }
    a<-(1-level)/2;rows<-list()
    for(i in seq_along(fs)) for(j in seq_along(fs)) if(i<j) {
      z<-reps[,i,j];z<-z[is.finite(z)]
      bounds<-if(length(z)>=2) stats::quantile(z,c(a,1-a),names=FALSE) else c(NA_real_,NA_real_)
      rows[[length(rows)+1]]<-data.frame(facteur1=fs[i],facteur2=fs[j],htmt=H[i,j],et_boot=if(length(z)>=2) stats::sd(z) else NA_real_,borne_inf=bounds[1],borne_sup=bounds[2],n_valides=length(z))
    }
    table<-if(length(rows)) do.call(rbind,rows) else data.frame(facteur1=character(),facteur2=character(),htmt=numeric(),et_boot=numeric(),borne_inf=numeric(),borne_sup=numeric(),n_valides=integer())
    list(replications=reps,table=table,level=level,type=type,missing=missing)
  })
}

cfadiag_with_seed <- function(seed,expr) {
  if(is.null(seed)) return(force(expr))
  if(!is.numeric(seed) || length(seed)!=1 || !is.finite(seed) || seed<0 || seed>.Machine$integer.max || seed!=as.integer(seed)) stop("La graine doit \u00eatre un entier entre 0 et 2147483647.",call.=FALSE)
  had<-exists(".Random.seed",.GlobalEnv,inherits=FALSE);if(had) old<-get(".Random.seed",.GlobalEnv)
  on.exit(if(had) assign(".Random.seed",old,.GlobalEnv) else if(exists(".Random.seed",.GlobalEnv,inherits=FALSE)) rm(".Random.seed",envir=.GlobalEnv))
  set.seed(seed);force(expr)
}

#' Indices d'ajustement et repères conventionnels
#' @param model Objet cfaModel ou ajustement lavaan.
#' @param reperes Liste des repères cfi, tli, rmsea et srmr.
#' @details Les versions robustes puis corrigées sont prioritaires. L'intervalle
#'   RMSEA conserve la même version que son estimation. Les repères ne constituent
#'   pas des décisions de validité; ils ne sont pas évalués si df <= 0.
#'   En multigroupe, les indices décrivent le modèle global, pas chaque groupe séparément.
#' @return Data.frame indice, valeur, version, repere et atteint.
#' @examples
#' library(lavaan)
#' modele <- "visuel =~ x1 + x2 + x3
#'            verbal =~ x4 + x5 + x6
#'            vitesse =~ x7 + x8 + x9"
#' fit <- cfa(modele, data = HolzingerSwineford1939)
#' compute_fit_indices(fit)
#'
#' # Repères plus souples, et versions robustes avec MLR
#' compute_fit_indices(fit, reperes = list(cfi = .90, tli = .90, rmsea = .08, srmr = .10))
#' fit_r <- cfa(modele, data = HolzingerSwineford1939, estimator = "MLR")
#' compute_fit_indices(fit_r)[, c("indice", "valeur", "version")]
#' @export
compute_fit_indices <- function(model,reperes=list(cfi=.95,tli=.95,rmsea=.06,srmr=.08)) {
  fit<-if(inherits(model,"cfaModel")) model$fit else model
  if(is.null(fit) || !inherits(fit,"lavaan")) stop("Aucun ajustement lavaan disponible.",call.=FALSE)
  fm<-if(isTRUE(lavaan::lavInspect(fit,"converged"))) tryCatch(lavaan::fitMeasures(fit),error=function(e) numeric()) else numeric()
  take<-function(base,suffix) {
    for(s in suffix) {n<-paste0(base,s);if(n %in% names(fm) && is.finite(fm[[n]])) return(list(value=fm[[n]],suffix=s))}
    list(value=NA_real_,suffix=NA_character_)
  }
  specs<-list(chisq=c(".scaled",""),df=c(".scaled",""),pvalue=c(".scaled",""),cfi=c(".robust",".scaled",""),tli=c(".robust",".scaled",""),rmsea=c(".robust",".scaled",""),rmsea.ci.lower="",rmsea.ci.upper="",srmr="")
  values<-lapply(names(specs),function(n) take(n,specs[[n]]));names(values)<-names(specs)
  for(n in c("rmsea.ci.lower","rmsea.ci.upper")) {
    s<-values$rmsea$suffix
    value<-if(is.na(s)) NA_real_ else fm[paste0(n,s)]
    if(!length(value) || !is.finite(value)) value<-NA_real_
    values[[n]]<-list(value=unname(value),suffix=s)
  }
  out<-data.frame(indice=names(values),valeur=vapply(values,function(v) unname(v$value),numeric(1)),version=vapply(values,function(v) if(is.na(v$suffix)) "indisponible" else if(v$suffix=="") "standard" else substring(v$suffix,2),character(1)),repere=NA_real_,atteint=NA)
  for(n in names(reperes)) {
    if(!n %in% c("cfi","tli","rmsea","srmr") || length(reperes[[n]])!=1 || !is.finite(reperes[[n]])) stop("Rep\u00e8re d'ajustement invalide.",call.=FALSE)
    i<-out$indice==n;out$repere[i]<-reperes[[n]]
    if(is.finite(out$valeur[out$indice=="df"]) && out$valeur[out$indice=="df"]>0) out$atteint[i]<-if(n %in% c("cfi","tli")) out$valeur[i]>=reperes[[n]] else out$valeur[i]<=reperes[[n]]
  }
  rownames(out)<-NULL;out
}

#' Signaux à examiner avant d'interpréter un modèle
#' @param model Objet cfaModel.
#' @param charge_faible Repère descriptif pour les faibles saturations.
#' @param htmt_alerte Repère descriptif du HTMT.
#' @return Data.frame signal et detail; un signal ne prescrit aucune modification.
#' @examples
#' library(lavaan)
#' modele <- "visuel =~ x1 + x2 + x3
#'            verbal =~ x4 + x5 + x6
#'            vitesse =~ x7 + x8 + x9"
#' fit <- cfa(modele, data = HolzingerSwineford1939)
#' compute_diagnostics(as.cfaModel(fit))   # aucun signal
#'
#' # Seuil de charge plus exigeant, puis une covariance résiduelle ajoutée
#' compute_diagnostics(as.cfaModel(fit), charge_faible = .60)
#' fit2 <- cfa(paste(modele, "\n x7 ~~ x8"), data = HolzingerSwineford1939)
#' compute_diagnostics(as.cfaModel(fit2))
#' @export
compute_diagnostics <- function(model,charge_faible=.40,htmt_alerte=.85) {
  verifier(model)
  if(length(charge_faible)!=1 || !is.finite(charge_faible) || charge_faible<0 || charge_faible>1 || length(htmt_alerte)!=1 || !is.finite(htmt_alerte) || htmt_alerte<=0) stop("Rep\u00e8res de diagnostic invalides.",call.=FALSE)
  s<-list();add<-function(type,detail) s[[length(s)+1]]<<-data.frame(signal=type,detail=detail)
  L<-model$loadings;pattern<-model$loading_pattern;if(is.null(pattern)) pattern<-is.finite(L) & L!=0
  if(!is.null(model$fit) && !isTRUE(lavaan::lavInspect(model$fit,"converged"))) add("Convergence","Le mod\u00e8le n'a pas converg\u00e9; ne pas interpr\u00e9ter ses param\u00e8tres.")
  if(!is.null(model$theta)) {
    T<-model$theta
    for(n in names(which(diag(T)<0))) add("Cas de Heywood",paste0(n," : variance r\u00e9siduelle standardis\u00e9e = ",f3(T[n,n])))
    if(all(is.finite(T)) && min(eigen(T,symmetric=TRUE,only.values=TRUE)$values)< -1e-10) add("Matrice r\u00e9siduelle inadmissible","La matrice des r\u00e9sidus n'est pas d\u00e9finie positive.")
    rc<-which(upper.tri(T) & is.finite(T) & T!=0,arr.ind=TRUE)
    if(nrow(rc)) for(k in seq_len(nrow(rc))) {
      i<-rc[k,1];j<-rc[k,2]
      value<-if(T[i,i]>0 && T[j,j]>0) f3(T[i,j]/sqrt(T[i,i]*T[j,j])) else "non calculable"
      add("Covariance r\u00e9siduelle",paste0(rownames(T)[i]," ~~ ",colnames(T)[j]," (corr\u00e9lation r\u00e9siduelle ",value,")"))
    }
  }
  bad<-which(pattern & !is.finite(L),arr.ind=TRUE)
  if(nrow(bad)) add("Saturation non calculable","Certaines saturations standardis\u00e9es ne sont pas finies.")
  categories<-list("Charge hors bornes"=is.finite(L) & abs(L)>1,"Charge faible"=pattern & is.finite(L) & abs(L)<charge_faible,"Charge n\u00e9gative"=pattern & is.finite(L) & L<0)
  for(type in names(categories)) {
    ii<-which(categories[[type]],arr.ind=TRUE)
    if(nrow(ii)) for(k in seq_len(nrow(ii))) add(type,paste0(colnames(L)[ii[k,2]]," \u2192 ",rownames(L)[ii[k,1]]," = ",f3(L[ii[k,1],ii[k,2]])))
  }
  R<-model$cor_lv;hr<-which(upper.tri(R) & is.finite(R) & abs(R)>=.999,arr.ind=TRUE)
  if(nrow(hr)) for(k in seq_len(nrow(hr))) add(if(abs(R[hr[k,1],hr[k,2]])>1) "Corr\u00e9lation factorielle hors bornes" else "Corr\u00e9lation factorielle \u2248 1",paste0(rownames(R)[hr[k,1]]," \u2194 ",colnames(R)[hr[k,2]]," : ",f3(R[hr[k,1],hr[k,2]])))
  for(n in rownames(L)[rowSums(pattern)>1]) add("Charge crois\u00e9e",paste(n,"mesure",paste(colnames(L)[pattern[n,]],collapse=" et ")))
  if(isTRUE(model$meta$direct_indicator_effects)) {
    cible<-model$meta$direct_effect_items
    if(is.null(cible)) add("Effet direct sur indicateur","La d\u00e9finition du score et de sa variance cible doit \u00eatre examin\u00e9e.") else
      for(n in intersect(cible,rownames(L))) add("Effet direct sur indicateur",paste0(n," est r\u00e9gress\u00e9 sur une autre variable ; score de ",paste(colnames(L)[pattern[n,]],collapse=" et ")," \u00e0 d\u00e9finir"))
  }
  for(f in colnames(L)) {k<-length(items_du_facteur(model,f));if(k<3) add("Peu d'indicateurs",paste0(f," : ",k," indicateur(s)"))}
  if(!is.null(model$sample) && ncol(L)>1) {
    H<-compute_htmt(model);hh<-which(upper.tri(H) & is.finite(H) & H>=htmt_alerte,arr.ind=TRUE)
    if(nrow(hh)) for(k in seq_len(nrow(hh))) add("HTMT \u00e9lev\u00e9",paste0(rownames(H)[hh[k,1]]," \u2013 ",colnames(H)[hh[k,2]]," = ",f3(H[hh[k,1],hh[k,2]])))
  }
  if(length(s)) do.call(rbind,s) else data.frame(signal=character(),detail=character())
}
verifier <- function(model) {
  if(!inherits(model,"cfaModel")) stop("L'objet doit \u00eatre de classe cfaModel (voir as.cfaModel()).",call.=FALSE)
  invisible(TRUE)
}
