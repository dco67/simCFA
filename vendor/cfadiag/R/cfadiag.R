#' Rapport de diagnostic d'une AFC
#'
#' Rassemble fiabilité, validité, HTMT, ajustement et signaux d'alerte pour un
#' modèle de mesure estimé avec lavaan.
#'
#' @param model Ajustement lavaan ou objet `cfaModel`. Un modèle multigroupe
#'   produit un rapport par groupe.
#' @param htmt_boot Calculer des intervalles bootstrap du HTMT (défaut `FALSE`).
#' @param R Nombre de réplications bootstrap.
#' @param seed Graine aléatoire du bootstrap.
#' @param variance Dénominateur de l'oméga et de la CR, voir [compute_reliability()].
#' @param reperes Repères d'ajustement, voir [compute_fit_indices()].
#' @param ... Arguments transmis à [compute_diagnostics()].
#' @return Un objet de classe `cfadiag`, ou une liste de tels objets (classe
#'   `cfadiag_multigroupe`) pour un modèle multigroupe.
#' @examples
#' library(lavaan)
#' modele <- "visuel =~ x1 + x2 + x3
#'            verbal =~ x4 + x5 + x6
#'            vitesse =~ x7 + x8 + x9"
#' fit <- cfa(modele, data = HolzingerSwineford1939)
#' d <- cfadiag(fit)
#' d
#'
#' # Avec intervalles bootstrap du HTMT
#' cfadiag(fit, htmt_boot = TRUE, R = 200, seed = 1)$htmt_boot
#'
#' # Oméga et CR avec la variance observée au dénominateur (comme semTools)
#' cfadiag(fit, variance = "observee")$fiabilite
#'
#' # Modèle multigroupe : un rapport par groupe
#' fit_g <- cfa(modele, data = HolzingerSwineford1939, group = "school")
#' cfadiag(fit_g)
#'
#' # Éléments du rapport, utilisables dans un document
#' d$fiabilite
#' d$validite$table
#' d$signaux
#' @export
cfadiag <- function(model, htmt_boot = FALSE, R = 1000, seed = NULL,
                    reperes = list(cfi = .95, tli = .95, rmsea = .06, srmr = .08),
                    variance = c("impliquee", "observee"), ...) {
  variance <- match.arg(variance)
  if (inherits(model, "lavaan")) model <- as.cfaModel(model)
  if (is.list(model) && !inherits(model, "cfaModel") && all(vapply(model, inherits, logical(1), "cfaModel"))) {
    out <- lapply(model, cfadiag, htmt_boot = htmt_boot, R = R, seed = seed, reperes = reperes, variance = variance, ...)
    return(structure(out, class = "cfadiag_multigroupe"))
  }
  verifier(model)
  fs <- colnames(model$loadings)
  out <- list(
    apercu = list(groupe = model$group, n_facteurs = length(fs), n_items = nrow(model$loadings),
                  n_obs = model$meta$nobs, estimateur = model$meta$estimator,
                  ordinal = length(model$ordered) > 0),
    charges = model$loadings,
    fiabilite = compute_reliability(model, variance),
    validite = compute_validity(model),
    htmt = if (!is.null(model$sample) && length(fs) > 1) compute_htmt(model) else NULL,
    ajustement = if (!is.null(model$fit)) compute_fit_indices(model, reperes) else NULL,
    signaux = compute_diagnostics(model, ...),
    htmt_boot = NULL)
  if (htmt_boot && length(fs) > 1) out$htmt_boot <- boot_htmt(model, R = R, seed = seed)$table
  structure(out, class = "cfadiag")
}

f3 <- function(x) ifelse(is.na(x), "\u2014", formatC(x, format = "f", digits = 3, decimal.mark = ","))

#' @export
print.cfadiag <- function(x, ...) {
  a <- x$apercu
  cat("\n================================================\n")
  cat(" Diagnostic du mod\u00e8le de mesure", if (!is.null(a$groupe)) paste0("\u2014 groupe ", a$groupe), "\n")
  cat("================================================\n")
  cat(sprintf(" %d facteurs, %d indicateurs, N = %s, estimateur %s%s\n", a$n_facteurs, a$n_items,
              paste(a$n_obs, collapse = "/"), a$estimateur, if (a$ordinal) " (indicateurs ordinaux)" else ""))
  if (!is.null(x$ajustement)) {
    cat(if (is.null(a$groupe)) "\nAjustement global\n" else "\nAjustement global (mod\u00e8le multigroupe, tous groupes confondus)\n")
    f <- x$ajustement; g <- function(n) f$valeur[f$indice == n]
    cat(sprintf("  Chi carr\u00e9 = %s, dl = %s, p %s\n", f3(g("chisq")), round(g("df")), if (isTRUE(g("pvalue") < .001)) "< 0,001" else paste("=", f3(g("pvalue")))))
    cat(sprintf("  RMSEA = %s [IC 90 %% : %s ; %s]\n", f3(g("rmsea")), f3(g("rmsea.ci.lower")), f3(g("rmsea.ci.upper"))))
    for (n in c("cfi", "tli", "rmsea", "srmr")) {
      i <- f$indice == n
      cat(sprintf("  %-6s %s   rep\u00e8re %s %s : %s\n", toupper(n), f3(f$valeur[i]),
                  if (n %in% c("cfi", "tli")) "\u2265" else "\u2264", f3(f$repere[i]),
                  ifelse(is.na(f$atteint[i]), "\u2014", ifelse(f$atteint[i], "atteint", "non atteint"))))
    }
    if (any(!f$version %in% c("standard", "indisponible"))) cat("  (versions corrig\u00e9es :", paste(unique(f$version[!f$version %in% c("standard", "indisponible")]), collapse = ", "), ")\n")
    cat("  Les rep\u00e8res sont des guides conventionnels, pas des seuils de d\u00e9cision.\n")
  }
  cat("\nFiabilit\u00e9 des scores compos\u00e9s\n")
  r <- x$fiabilite
  print(data.frame(Facteur = r$facteur, Items = r$n_items, Omega = f3(r$omega), CR = f3(r$CR), Alpha = f3(r$alpha), H = f3(r$H)), row.names = FALSE)
  if (a$ordinal) cat("  (indicateurs ordinaux : om\u00e9ga et alpha portent sur les r\u00e9ponses sous-jacentes)\n")
  cat("\nValidit\u00e9 convergente et discriminante\n")
  v <- x$validite$table
  print(data.frame(Facteur = v$facteur, AVE = f3(v$AVE), MSV = f3(v$MSV), ASV = f3(v$ASV),
                   `AVE > MSV` = ifelse(is.na(v$AVE_superieure_MSV), "non applicable", ifelse(v$AVE_superieure_MSV, "oui", "non")), check.names = FALSE), row.names = FALSE)
  cat("\nFornell\u2013Larcker (diagonale : racine de l'AVE ; hors diagonale : corr\u00e9lations)\n")
  print(noquote(matrix(f3(x$validite$fornell_larcker), nrow(x$validite$fornell_larcker),
                       dimnames = dimnames(x$validite$fornell_larcker))))
  if (!is.null(x$htmt)) {
    cat("\nHTMT\n")
    H <- x$htmt; H[lower.tri(H, diag = TRUE)] <- NA
    print(noquote(matrix(ifelse(is.na(H), "", f3(H)), nrow(H), dimnames = dimnames(H))))
  }
  if (!is.null(x$htmt_boot)) {
    b <- x$htmt_boot
    cat("\nHTMT : intervalles bootstrap percentile\n")
    print(data.frame(Paire = paste(b$facteur1, b$facteur2, sep = " \u2013 "), HTMT = f3(b$htmt),
                     `Borne inf.` = f3(b$borne_inf), `Borne sup.` = f3(b$borne_sup), R = b$n_valides,
                     check.names = FALSE) |> stats::setNames(c("Paire", "HTMT", "Borne inf.", "Borne sup.", "R\u00e9plications")), row.names = FALSE)
  }
  cat("\nSignaux \u00e0 examiner\n")
  if (nrow(x$signaux) == 0) cat("  Aucun selon les crit\u00e8res examin\u00e9s; cela ne d\u00e9montre pas la validit\u00e9 du mod\u00e8le.\n") else
    for (k in seq_len(nrow(x$signaux))) cat("  -", x$signaux$signal[k], ":", x$signaux$detail[k], "\n")
  cat("\n")
  invisible(x)
}

#' @export
print.cfadiag_multigroupe <- function(x, ...) {
  for (g in x) print(g)
  invisible(x)
}

#' Graphiques du diagnostic
#'
#' @param x Objet `cfadiag`.
#' @param type `"charges"`, `"fiabilite"`, `"validite"` ou `"htmt"`.
#' @param ... Inutilisé.
#' @examples
#' library(lavaan)
#' modele <- "visuel =~ x1 + x2 + x3
#'            verbal =~ x4 + x5 + x6
#'            vitesse =~ x7 + x8 + x9"
#' fit <- cfa(modele, data = HolzingerSwineford1939)
#' d <- cfadiag(fit)
#' plot(d, "charges")
#' plot(d, "fiabilite")
#' plot(d, "validite")
#' plot(d, "htmt")
#' @export
plot.cfadiag <- function(x, type = c("charges", "fiabilite", "validite", "htmt"), ...) {
  type <- match.arg(type)
  bleu <- "#123449"; vert <- "#08766d"; ocre <- "#ac592a"
  op <- graphics::par(las = 1, mar = c(4.5, 6, 3, 1)); on.exit(graphics::par(op))
  carte <- function(M, titre, zlim, pal) {
    graphics::image(seq_len(ncol(M)), seq_len(nrow(M)), t(M[nrow(M):1, , drop = FALSE]), zlim = zlim, col = pal,
                    axes = FALSE, xlab = "", ylab = "", main = titre)
    graphics::axis(1, seq_len(ncol(M)), colnames(M)); graphics::axis(2, seq_len(nrow(M)), rev(rownames(M)))
    for (i in seq_len(nrow(M))) for (j in seq_len(ncol(M))) if (!is.na(M[i, j]))
      graphics::text(j, nrow(M) - i + 1, f3(M[i, j]), cex = .8)
    graphics::box()
  }
  pal <- grDevices::colorRampPalette(c(ocre, "white", vert))(41)
  if (type == "charges") {
    L <- x$charges; L[L == 0] <- NA
    carte(L, "Charges standardis\u00e9es", range(c(-1,1,L),na.rm=TRUE), pal)
  } else if (type == "htmt") {
    if (is.null(x$htmt)) stop("Aucun HTMT disponible.", call. = FALSE)
    H <- x$htmt; diag(H) <- NA
    carte(H, "HTMT (alerte conventionnelle : 0,85)", range(c(0,1,H),na.rm=TRUE), grDevices::colorRampPalette(c("white", "#f3e3d6", ocre))(41))
  } else if (type == "fiabilite") {
    r <- x$fiabilite
    graphics::barplot(t(as.matrix(r[, c("omega", "alpha", "H")])), beside = TRUE, names.arg = r$facteur,
                      ylim = range(c(0,1.15,r$omega,r$alpha,r$H),na.rm=TRUE), col = c(vert, "#9cc9c1", bleu), border = NA, main = "Fiabilit\u00e9 des scores compos\u00e9s",
                      legend.text = c("Om\u00e9ga", "Alpha", "H"), args.legend = list(x = "top", bty = "n", horiz = TRUE))
  } else {
    v <- x$validite$table
    graphics::barplot(t(as.matrix(v[, c("AVE", "MSV")])), beside = TRUE, names.arg = v$facteur, ylim = c(0, 1.15),
                      col = c(bleu, "#c5d2dc"), border = NA, main = "AVE et MSV",
                      legend.text = c("AVE", "MSV"), args.legend = list(x = "top", bty = "n", horiz = TRUE))
  }
  invisible(x)
}

#' Simuler des données d'AFC
#'
#' Génère des indicateurs continus selon un modèle factoriel à structure
#' simple, avec des facteurs de variance 1 et des indicateurs de variance
#' totale 1.
#'
#' @param n Nombre d'observations.
#' @param n_factors Nombre de facteurs.
#' @param items_per_factor Nombre d'indicateurs par facteur.
#' @param loading_range Intervalle des charges tirées uniformément.
#' @param factor_correlation Corrélation commune entre facteurs, ou matrice complète.
#' @param seed Graine aléatoire, ou `NULL`.
#' @return Une liste : `data`, `loadings`, `Phi`, `theta` (variances
#'   résiduelles), `model` (modèle à estimer) et `population_model` (paramètres exacts).
#' @examples
#' s <- simulate_cfa(n = 400, n_factors = 2, items_per_factor = 4,
#'                   loading_range = c(.6, .8), factor_correlation = .5, seed = 1)
#' s$model                 # syntaxe lavaan prête à l'emploi
#' round(s$loadings, 2)    # charges génératrices
#' fit <- lavaan::cfa(s$model, data = s$data)
#' cfadiag(fit)            # comparer les estimations aux valeurs génératrices
#' @export
simulate_cfa <- function(n=500,n_factors=3,items_per_factor=4,loading_range=c(.5,.8),
                         factor_correlation=.3,seed=NULL) {
  integer_ok<-function(x,min) is.numeric(x) && length(x)==1 && is.finite(x) && x>=min && x<=.Machine$integer.max && x==as.integer(x)
  if(!integer_ok(n,2) || !integer_ok(n_factors,1) || !integer_ok(items_per_factor,1)) stop("n, n_factors et items_per_factor doivent \u00eatre des entiers positifs (n >= 2).",call.=FALSE)
  if(!is.numeric(loading_range) || length(loading_range)!=2 || any(!is.finite(loading_range)) || any(loading_range<=0 | loading_range>=1) || loading_range[1]>loading_range[2]) stop("loading_range doit contenir deux bornes ordonn\u00e9es strictement entre 0 et 1.",call.=FALSE)
  p<-n_factors*items_per_factor;it<-paste0("x",seq_len(p));fa<-paste0("F",seq_len(n_factors))
  if(is.matrix(factor_correlation)) {
    Phi<-factor_correlation
    if(!is.numeric(Phi) || !identical(dim(Phi),c(as.integer(n_factors),as.integer(n_factors)))) stop("Dimensions invalides de la matrice factorielle.",call.=FALSE)
    if(!is.null(rownames(Phi)) || !is.null(colnames(Phi))) {
      if(!setequal(rownames(Phi),fa) || !setequal(colnames(Phi),fa)) stop("Noms incompatibles de la matrice factorielle.",call.=FALSE)
      Phi<-Phi[fa,fa,drop=FALSE]
    }
  } else {
    if(!is.numeric(factor_correlation) || length(factor_correlation)!=1 || !is.finite(factor_correlation)) stop("Corr\u00e9lation factorielle scalaire invalide.",call.=FALSE)
    Phi<-matrix(factor_correlation,n_factors,n_factors);diag(Phi)<-1
  }
  if(any(!is.finite(Phi)) || !isTRUE(all.equal(unname(Phi),unname(t(Phi)),tolerance=1e-10)) || any(abs(diag(Phi)-1)>1e-10) || any(abs(Phi)>1+1e-10)) stop("La matrice factorielle doit \u00eatre une matrice de corr\u00e9lations sym\u00e9trique de diagonale 1.",call.=FALSE)
  dimnames(Phi)<-list(fa,fa)
  if(min(eigen(Phi,symmetric=TRUE,only.values=TRUE)$values)<=1e-10) stop("La matrice de corr\u00e9lations des facteurs n'est pas d\u00e9finie positive.",call.=FALSE)
  cfadiag_with_seed(seed,{
    L<-matrix(0,p,n_factors,dimnames=list(it,fa))
    for(f in seq_len(n_factors)) L[((f-1)*items_per_factor+1):(f*items_per_factor),f]<-stats::runif(items_per_factor,loading_range[1],loading_range[2])
    theta<-1-rowSums((L%*%Phi)*L)
    Fs<-matrix(stats::rnorm(n*n_factors),nrow=n)%*%chol(Phi)
    X<-Fs%*%t(L)+sweep(matrix(stats::rnorm(n*p),nrow=n),2,sqrt(theta),"*")
    X<-as.data.frame(X);names(X)<-it
    model<-paste(vapply(seq_len(n_factors),function(f) paste(fa[f],"=~",paste(it[L[,f]!=0],collapse=" + ")),character(1)),collapse="\n")
    pop<-character()
    for(f in seq_len(n_factors)) {
      ii<-which(L[,f]!=0)
      pop<-c(pop,paste(fa[f],"=~",paste(sprintf("%.17g*%s",L[ii,f],it[ii]),collapse=" + ")),paste0(fa[f]," ~~ 1*",fa[f]))
    }
    pop<-c(pop,sprintf("%s ~~ %.17g*%s",it,theta,it))
    if(n_factors>1) for(i in seq_len(n_factors-1)) for(j in seq.int(i+1,n_factors)) pop<-c(pop,sprintf("%s ~~ %.17g*%s",fa[i],Phi[i,j],fa[j]))
    list(data=X,loadings=L,Phi=Phi,theta=theta,model=model,population_model=paste(pop,collapse="\n"))
  })
}
