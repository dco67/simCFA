# Moteur commun : estimation, simulation et comparaisons. 20 novembre 2026.
cfa_status <- function(fit) {
  if (!inherits(fit, "lavaan")) return(list(ok=FALSE, message="Aucun ajustement lavaan."))
  converged <- isTRUE(lavaan::lavInspect(fit, "converged"))
  post <- if (converged) suppressWarnings(tryCatch(lavaan::lavInspect(fit,"post.check"), error=function(e) FALSE)) else FALSE
  se <- tryCatch(lavaan::parameterEstimates(fit,ci=FALSE)$se, error=function(e) NA_real_)
  ok <- converged && isTRUE(post) && all(is.finite(se))
  list(ok=ok, message=if (!converged) "L'estimation n'a pas convergé. Révisez le modèle et les données."
       else if (!isTRUE(post)) "Solution inadmissible : variance négative ou matrice non définie positive."
       else if (any(!is.finite(se))) "Incertitude non estimable : vérifiez l'identification du modèle."
       else "Estimation convergée et contrôles d'admissibilité satisfaits. Cela ne démontre pas la validité du modèle.")
}

cfa_fit <- function(syntax, data, options) {
  warnings <- character()
  fit <- withCallingHandlers(do.call("cfa", c(list(model=syntax, data=data), options), envir=asNamespace("lavaan")),
    warning=function(w) { warnings <<- unique(c(warnings, conditionMessage(w))); invokeRestart("muffleWarning") })
  status <- cfa_status(fit)
  if (!status$ok) stop(status$message, call.=FALSE)
  list(fit=fit, warnings=warnings, status=status)
}

# Chaque tirage est vérifié, sans réparer silencieusement les corrélations demandées.
generate_random_correlations <- function(n_factors, min_cor=.1, max_cor=.7) {
  if (n_factors < 2) return(numeric())
  if (min_cor > max_cor || min_cor <= -1 || max_cor >= 1) stop("Étendue de corrélations invalide.")
  pairs <- utils::combn(seq_len(n_factors),2)
  for (attempt in seq_len(10000)) {
    values <- runif(ncol(pairs),min_cor,max_cor)
    R <- diag(n_factors)
    R[t(pairs)] <- values; R <- R + t(R) - diag(n_factors)
    if (min(eigen(R,symmetric=TRUE,only.values=TRUE)$values)>1e-6) return(values)
  }
  stop("Aucune matrice définie positive trouvée dans cette étendue. Réduisez les corrélations ou le nombre de facteurs.")
}

with_cfa_seed <- function(seed, expr) {
  had <- exists(".Random.seed", .GlobalEnv, inherits=FALSE)
  if (had) old <- get(".Random.seed",.GlobalEnv)
  on.exit(if (had) assign(".Random.seed",old,.GlobalEnv) else if (exists(".Random.seed",.GlobalEnv,inherits=FALSE)) rm(".Random.seed",envir=.GlobalEnv))
  set.seed(seed); force(expr)
}

cfa_simulate <- function(n_factors, n_items, n, loading_range, cor_range,
                         seed=123, ordinal=FALSE, categories=5) {
  stopifnot(n_factors>=1, n_items>=2*n_factors, n>=2,
            all(loading_range>0), all(loading_range<1), length(loading_range)==2,
            is.finite(seed), seed>=0, seed<=.Machine$integer.max, categories>=2)
  with_cfa_seed(as.integer(seed), {
    lambda <- runif(n_items,loading_range[1],loading_range[2])
    correlations <- generate_random_correlations(n_factors,cor_range[1],cor_range[2])
    counts <- rep(n_items %/% n_factors,n_factors)
    if (n_items %% n_factors) counts[seq_len(n_items %% n_factors)] <- counts[seq_len(n_items %% n_factors)]+1L
    blocks <- split(paste0("x",seq_len(n_items)),rep(paste0("F",seq_len(n_factors)),counts))
    lines <- character(); offset <- 0L
    for (f in names(blocks)) {
      ii <- offset+seq_along(blocks[[f]]); offset <- max(ii)
      lines <- c(lines,paste(f,"=~",paste(sprintf("%.17g*%s",lambda[ii],blocks[[f]]),collapse=" + ")),
                 paste(f,"~~ 1*",f),sprintf("%s ~~ %.17g*%s",blocks[[f]],1-lambda[ii]^2,blocks[[f]]))
    }
    if (n_factors>1) {
      pp <- utils::combn(names(blocks),2)
      lines <- c(lines,sprintf("%s ~~ %.17g*%s",pp[1,],correlations,pp[2,]))
    }
    syntax <- paste(lines,collapse="\n")
    data <- lavaan::simulateData(syntax,sample.nobs=n, empirical=FALSE)
    if (ordinal) data[] <- lapply(data,function(x) as.integer(cut(x,c(-Inf,qnorm(seq_len(categories-1)/categories),Inf))))
    list(data=data, syntax=syntax, seed=as.integer(seed), factor_vars=blocks,
         ordinal=ordinal, categories=categories, loadings=lambda, correlations=correlations)
  })
}

# Contrainte ajoutée au tableau de paramètres du modèle ajusté : aucune autre
# covariance, moyenne ou restriction du modèle de base n'est reconstruite/perdue.
cfa_restrict <- function(fit, kind) {
  if (length(lavaan::lavNames(fit,"ov.ord"))) stop("Ces comparaisons d'égalité sont actuellement réservées aux indicateurs continus.")
  if (lavaan::lavInspect(fit,"ngroups")!=1L) stop("Comparaison réservée à une seule population.")
  pt <- lavaan::parTable(fit); factors <- lavaan::lavNames(fit,"lv")
  if (kind=="means") stop("Les moyennes factorielles sont fixées à zéro dans cette AFC à un groupe; ce test n'est pas identifiable tel quel.")
  if (kind=="unidim") stop("Le modèle à un facteur impose une limite de corrélation parfaite : le test usuel de différence de chi carré n'est pas appliqué.")
  if (any(pt$op=="~")) stop("Comparaison réservée aux modèles de mesure de premier ordre.")
  if (kind=="var_equal" && isTRUE(lavaan::lavInspect(fit,"options")$std.lv)) stop("Variances déjà fixées à 1 : aucune restriction supplémentaire.")
  constraints <- character()
  equalize <- function(ii) {
    if (length(ii)<2) return(invisible(NULL))
    targets <- character()
    for (i in ii) {
      if (pt$free[i]==0) targets <- c(targets,format(pt$est[i],digits=17,trim=TRUE)) else {
        if (!nzchar(pt$label[i])) pt$label[i] <<- paste0("cfaeq_",i)
        targets <- c(targets,pt$label[i])
      }
    }
    ref <- if (any(pt$free[ii]==0)) targets[which(pt$free[ii]==0)[1]] else targets[1]
    for (t in unique(targets)) if (t!=ref) {
      if (grepl("^[0-9.+-]",t)) { if (as.numeric(t)!=as.numeric(ref)) stop("Valeurs fixées incompatibles avec cette égalité.") }
      else constraints <<- c(constraints,paste(t,"==",ref))
    }
  }
  if (kind %in% c("tau","parallel")) for (f in factors) equalize(which(pt$op=="=~" & pt$lhs==f))
  if (kind %in% c("homo","parallel","intercepts")) for (f in factors) {
    items <- pt$rhs[pt$op=="=~" & pt$lhs==f]
    if (kind=="intercepts") equalize(which(pt$op=="~1" & pt$lhs %in% items))
    else equalize(which(pt$op=="~~" & pt$lhs==pt$rhs & pt$lhs %in% items))
  }
  if (kind=="var_equal") equalize(which(pt$op=="~~" & pt$lhs==pt$rhs & pt$lhs %in% factors))
  if (kind=="cov_equal") equalize(which(pt$op=="~~" & pt$lhs!=pt$rhs & pt$lhs %in% factors & pt$rhs %in% factors))
  if (!length(constraints)) stop("Aucune nouvelle égalité à tester (nombre de facteurs/indicateurs ou contraintes déjà présentes).")
  eq <- pt[rep(1L,length(constraints)),,drop=FALSE]
  for (nm in names(eq)) eq[[nm]] <- if (is.character(pt[[nm]])) "" else 0
  tokens <- strsplit(constraints," == ",fixed=TRUE)
  eq$lhs <- vapply(tokens,`[`,character(1),1); eq$rhs <- vapply(tokens,`[`,character(1),2)
  eq$op <- "=="; eq$id <- seq_len(nrow(eq))+max(pt$id)
  eq$free <- 0L; eq$ustart <- NA_real_; eq$user <- 1L
  list(table=rbind(pt,eq), description=paste(constraints,collapse="\n"))
}

# Export exact des données analysées et des options enregistrées, sans nouveau tirage.
cfa_r_script <- function(snapshot, embed_data=TRUE) {
  c("# Analyse exportée depuis simCFA : données et modèle de l'estimation affichée.",
    "library(lavaan)", "", "# Données incluses dans le script, dans l'ordre d'origine :",
    if(embed_data) paste0("data <- ",paste(capture.output(dput(snapshot$data,control=c("keepNA","keepInteger","niceNames","showAttributes","digits17"))),collapse="\n")) else sprintf("# Les %d lignes × %d indicateurs sont inclus dans le script téléchargé.",nrow(snapshot$data),ncol(snapshot$data)),
    if(!is.null(snapshot$analysis_seed)) paste0("set.seed(",snapshot$analysis_seed,") # Réplications bootstrap reproductibles"),
    paste0("model <- ",paste(capture.output(dput(snapshot$syntax)),collapse="\n")),
    paste0("options <- ",paste(capture.output(dput(snapshot$options)),collapse="\n")),
    "fit <- do.call(\"cfa\", c(list(model=model, data=data), options))",
    "summary(fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)")
}

# DT : largeur liée au contenu. Les grandes matrices restent défilables.
cfa_datatable <- function(data, options=list(), ..., width="auto", class="compact stripe hover") {
  defaults <- list(autoWidth=FALSE, scrollX=TRUE, pageLength=10, lengthChange=FALSE,
                   language=list(emptyTable="Aucune valeur disponible", search="Rechercher :",
                     info="Lignes _START_ à _END_ sur _TOTAL_", infoEmpty="Aucune ligne",
                     paginate=list(previous="Précédent","next"="Suivant")))
  DT::datatable(data, options=utils::modifyList(defaults,options), ...,width=width,class=class)
}

# Le chemin fourni par downloadHandler n'a pas nécessairement d'extension.
# Rendre sous un nom HTML, puis copier explicitement vers la destination demandée.
cfa_render_report <- function(destination, params, template="rapport_cfa.Rmd",
                              validity_file="cfa_validity.R") {
  tmp<-tempfile("rapport-cfa-");dir.create(tmp)
  on.exit(unlink(tmp,recursive=TRUE),add=TRUE)
  if(!file.copy(template,file.path(tmp,"rapport_cfa.Rmd")) ||
     !file.copy(validity_file,file.path(tmp,"cfa_validity.R"))) stop("Fichiers du rapport introuvables.")
  rendered<-rmarkdown::render(file.path(tmp,"rapport_cfa.Rmd"),output_file="rapport.html",
    output_dir=tmp,intermediates_dir=tmp,knit_root_dir=tmp,output_format="html_document",
    params=params,envir=new.env(parent=globalenv()),quiet=TRUE)
  if(!file.exists(rendered) || !file.copy(rendered,destination,overwrite=TRUE)) stop("Le rapport HTML n’a pas pu être enregistré.")
  invisible(destination)
}
