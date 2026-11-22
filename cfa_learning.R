# Accompagnement pédagogique et adaptateur du package cfadiag fourni par Daniel Coulombe.
# Les sources GPL-3 de cfadiag (version indiquée dans vendor/cfadiag/DESCRIPTION) sont conservées, sans modification, dans vendor/.
cfa_cfadiag <- local({
  engine<-NULL
  function() {
    if(is.null(engine)) {
      e<-new.env(parent=baseenv())
      for(path in list.files("vendor/cfadiag/R",pattern="[.]R$",full.names=TRUE)) sys.source(path,envir=e)
      if(!exists("compute_diagnostics",e,inherits=FALSE)) stop("Le moteur cfadiag inclus est introuvable.")
      engine<<-e
    }
    engine
  }
})

cfa_diagnostic_bundle <- function(fit) {
  engine<-cfa_cfadiag()
  model<-engine$as.cfaModel.lavaan(fit)
  signals<-engine$compute_diagnostics(model)
  advice<-c("Charge faible"="Examiner le contenu, l’incertitude et les résidus; ne pas supprimer automatiquement l’item.",
    "Charge négative"="Vérifier le sens de l’item et l’orientation du facteur avant tout recodage.",
    "Covariance résiduelle"="Justifier le lien résiduel par le contenu ou la méthode de mesure.",
    "Charge croisée"="Vérifier l’appartenance conceptuelle de l’item; la fiabilité requiert ici une analyse spécifique.",
    "Peu d’indicateurs"="Vérifier les conditions d’identification et la couverture du construit.",
    "HTMT élevé"="Examiner le chevauchement des construits et le contenu des items; un seuil seul ne conclut pas la validité.")
  signals$action<-unname(advice[signals$signal]);signals$action[is.na(signals$action)]<-"Examiner la spécification et les incertitudes avant de conclure."
  simple<-!any(rowSums(abs(model$loadings)>0)>1)
  htmt2<-if(simple && ncol(model$loadings)>1) engine$compute_htmt(model,type="htmt2") else NULL
  list(fit=engine$compute_fit_indices(model),signals=signals,htmt2=htmt2,
       engine=paste0("cfadiag ",tryCatch(read.dcf("vendor/cfadiag/DESCRIPTION",fields="Version")[1],error=function(e) "(version inconnue)")," (sources incluses)"),
       notes=c("Les signaux orientent l’examen du modèle; ils ne prescrivent ni suppression d’item ni ajout de covariance.",
         "HTMT2 : moyennes géométriques des corrélations observées en valeur absolue; une corrélation nulle rend certaines paires non calculables.",
         "Les formules de fiabilité et de H restent celles du moteur commun de l’application. Le calcul de H du package corrigé a été vérifié avec la matrice résiduelle complète."))
}

cfa_takeaways <- function(snapshot) {
  d<-snapshot$diagnostics; fm<-d$fit
  num<-function(n) { x<-fm$valeur[fm$indice==n];if(length(x) && is.finite(x)) formatC(x,digits=3,format="f",decimal.mark=",") else "non disponible" }
  signaux<-d$signals
  load<-lavaan::standardizedSolution(snapshot$fit);load<-load[load$op=="=~",]
  flags<-load[is.finite(load$est.std) & abs(load$est.std)<.40,]
  list(
    "Estimation"=paste("Estimation convergée et admissible.",lavaan::lavInspect(snapshot$fit,"nobs"),"observations utilisées. Cette vérification numérique ne démontre pas la validité du modèle."),
    "Ajustement"=if(isTRUE(fm$valeur[fm$indice=="df"]<=0)) "Modèle juste identifié ou sans degrés de liberté positifs : les indices d’ajustement global ne permettent pas d’évaluer la structure proposée. Examiner la théorie de mesure et des modèles testables." else paste0("CFI = ",num("cfi"),"; TLI = ",num("tli"),"; RMSEA = ",num("rmsea"),"; SRMR = ",num("srmr"),". Lire les versions des indices et l’intervalle du RMSEA dans le tableau; examiner les résidus en parallèle."),
    "Indicateurs"=if(nrow(flags)) paste(nrow(flags),"saturation(s) standardisée(s) en valeur absolue sous le repère descriptif .40. Examiner le contenu et l’incertitude des items signalés.") else "Aucune saturation sous le repère descriptif .40. Cela ne garantit ni une bonne couverture du construit ni l’absence d’erreurs de mesure.",
    "Distinction entre facteurs"=if(length(snapshot$factor_vars)<2) "Un seul facteur : la validité discriminante entre facteurs ne s’applique pas." else "Comparer les corrélations factorielles, les AVE, le HTMT et le contenu des construits. Aucun de ces repères ne démontre seul la validité discriminante.",
    "Pour conclure"=paste0("Formuler une conclusion sur la question de départ, en précisant l’échantillon, l’estimateur, les incertitudes et les limites. ",if(nrow(signaux)) paste(nrow(signaux),"signal(aux) à examiner dans le tableau ci-dessous.") else "Aucun signal selon les critères examinés; ce contrôle ne couvre pas toutes les sources de mauvaise spécification."))
}

cfa_score_table <- function(snapshot,method="regression") {
  scores<-as.data.frame(lavaan::lavPredict(snapshot$fit,method=method))
  names(scores)<-paste0("score_",names(scores))
  idx<-lavaan::lavInspect(snapshot$fit,"case.idx")
  if(is.list(idx)) stop("Export de scores réservé ici à un groupe.")
  if(length(idx)!=nrow(scores)) stop("Le nombre de scores ne correspond pas aux observations utilisées; export interrompu pour éviter une mauvaise association.")
  data.frame(ligne_source=as.integer(idx),scores,check.names=FALSE,row.names=NULL)
}

cfa_htmt_bootstrap <- function(snapshot,R=1000,seed=123,type=c("htmt","htmt2")) {
  type<-match.arg(type)
  if(length(snapshot$factor_vars)<2) stop("Le bootstrap HTMT ne s’applique pas à un seul facteur.")
  engine<-cfa_cfadiag(); model<-engine$as.cfaModel.lavaan(snapshot$fit)
  if(length(model$ordered)) stop("Bootstrap HTMT non proposé pour les indicateurs ordinaux : les corrélations polychoriques doivent être réestimées à chaque réplication.")
  if(is.null(model$data)) stop("Le bootstrap HTMT nécessite les données brutes de l’estimation.")
  if(any(rowSums(abs(model$loadings)>0)>1)) stop("Le bootstrap proposé ici requiert une structure simple sans saturations croisées.")
  if(length(R)!=1 || !is.finite(R) || R<200 || R>5000 || R!=as.integer(R)) stop("Choisir entre 200 et 5000 réplications entières.")
  if(length(seed)!=1 || !is.finite(seed) || seed<0 || seed>.Machine$integer.max || seed!=as.integer(seed)) stop("La graine doit être un entier entre 0 et 2147483647.")
  result<-tryCatch(with_cfa_seed(seed,engine$boot_htmt(model,R=as.integer(R),seed=seed,type=type)),error=function(e) stop("Le bootstrap n’a pas pu produire les intervalles : vérifier les corrélations, les items constants et les réplications non calculables.",call.=FALSE))
  if(is.null(result$table)) stop("Aucune paire de facteurs disponible.")
  result$seed<-seed
  result$note<-paste0(toupper(type)," (absences : ",result$missing,") : intervalle percentile bilatéral à 95 %, ",R," réplications demandées; graine ",seed,". Vérifier les réplications valides. Le modèle de mesure est conservé, sans nouvelle sélection d’items. L’intervalle ne prouve pas la validité d’un construit.")
  result
}

# Corrélations observées, ordonnées sans changer les signes ni les valeurs.
cfa_correlation_view <- function(data, fit=NULL, factors=list(),
                                 order=c("empirical","factors","original"),basis=c("fit","pearson")) {
  order<-match.arg(order);basis<-match.arg(basis)
  X<-data[vapply(data,is.numeric,logical(1))]
  if(!ncol(X)) stop("Aucun indicateur numérique disponible.")
  if(basis=="fit" && !is.null(fit)) {
    S<-lavaan::lavInspect(fit,"sampstat")$cov
    R<-stats::cov2cor(S);note<-if(length(lavaan::lavNames(fit,"ov.ord"))) "Corrélations polychoriques/polyseriales utilisées par l’estimation." else "Corrélations issues des statistiques d’échantillon de l’estimation (traitement des valeurs manquantes conservé)."
  } else {
    R<-suppressWarnings(stats::cor(X,use="pairwise.complete.obs"))
    note<-"Corrélations de Pearson entre scores observés, calculées par paires complètes."
  }
  R<-as.matrix(R);vars<-colnames(R)
  if(order=="empirical" && ncol(R)>1) {
    if(all(is.finite(R))) {
      distance<-sqrt(pmax(1-abs(R),0));dimnames(distance)<-dimnames(R)
      indices<-stats::hclust(stats::as.dist(distance),method="average")$order
      vars<-vars[indices]
      note<-paste(note,"Ordre empirique : classification hiérarchique des corrélations en valeur absolue. Le signe reste visible dans la carte; ce regroupement ne valide pas une structure factorielle.")
    } else note<-paste(note,"Certaines corrélations ne sont pas calculables; l’ordre d’origine est conservé.")
  } else if(order=="factors") {
    grouped<-unique(as.character(unlist(factors,use.names=FALSE)))
    vars<-c(intersect(grouped,vars),setdiff(vars,grouped))
    note<-paste(note,"Ordre des indicateurs dans les facteurs du modèle estimé; une saturation croisée ne duplique pas l’indicateur.")
  } else note<-paste(note,"Ordre d’origine des indicateurs.")
  list(matrix=R[vars,vars,drop=FALSE],note=note,variables=vars)
}

cfa_plot_correlations <- function(view) {
  R<-view$matrix;n<-nrow(R)
  cells<-expand.grid(row=seq_len(n),col=seq_len(n))
  cells$r<-R[cbind(cells$row,cells$col)];cells$y<-n+1-cells$row
  upper<-cells[cells$row<cells$col,,drop=FALSE]
  lower<-cells[cells$row>cells$col,,drop=FALSE]
  lower$label<-ifelse(is.finite(lower$r),formatC(lower$r,digits=2,format="f",decimal.mark=","),"—")
  diagonal<-cells[cells$row==cells$col,,drop=FALSE]
  ggplot2::ggplot(cells,ggplot2::aes(x=col,y=y))+
    ggplot2::geom_tile(fill="white",colour="#e5e7eb",linewidth=.3)+
    ggplot2::geom_tile(data=upper,ggplot2::aes(fill=r),colour="#e5e7eb",linewidth=.3)+
    ggplot2::geom_tile(data=diagonal,fill="#f0f2f4",colour="#e5e7eb",linewidth=.3)+
    ggplot2::geom_text(data=lower,ggplot2::aes(label=label),size=3.5,colour="#20252b")+
    ggplot2::geom_text(data=diagonal,label="1,00",size=3.2,colour="#626b73")+
    ggplot2::scale_fill_gradient2(low="#b45309",mid="white",high="#1d4f91",limits=c(-1,1),midpoint=0,na.value="#d1d5db",name="Corrélation")+
    ggplot2::scale_x_continuous(breaks=seq_len(n),labels=colnames(R),expand=c(0,0))+
    ggplot2::scale_y_continuous(breaks=seq_len(n),labels=rev(rownames(R)),expand=c(0,0))+
    ggplot2::coord_equal(clip="off")+
    ggplot2::labs(x=NULL,y=NULL)+ggplot2::theme_minimal(base_size=12)+
    ggplot2::theme(panel.grid=ggplot2::element_blank(),axis.text.x=ggplot2::element_text(angle=45,hjust=1),
      axis.text.y=ggplot2::element_text(colour="#20252b"),plot.margin=ggplot2::margin(10,20,10,10))
}

# Extraire uniquement le résultat du test MVN; ne jamais réimprimer l'objet complet.
cfa_mvn_table <- function(result) {
  for(key in c("multivariate_normality","multivariateNormality")) {
    table<-result[[key]]
    if(is.data.frame(table) || is.matrix(table)) return(as.data.frame(table))
  }
  stop("La version de MVN utilisée ne fournit pas de tableau de test reconnu; aucune donnée brute ni statistique descriptive n’a été réaffichée.")
}

cfa_preview_model <- function(syntax) semPlot::semPlotModel_lavaanModel(syntax)

# Les cinq diagnostics partagent le même échantillon de cas complets.
cfa_mvn_runner <- function(data,method) {
  if(!requireNamespace("MVN",quietly=TRUE)) stop("Le package facultatif MVN est requis pour ces diagnostics.")
  if(method %in% getNamespaceExports("MVN")) {
    fun<-getExportedValue("MVN",method);args<-list(data=data)
    if(method=="energy") {
      if("B" %in% names(formals(fun))) args$B<-1000L
      if("seed" %in% names(formals(fun))) args$seed<-123L
    }
    return(do.call(fun,args))
  }
  args<-list(data=data)
  modern<-"mvn_test" %in% names(formals(MVN::mvn))
  args[[if(modern) "mvn_test" else "mvnTest"]]<-if(!modern && method=="doornik_hansen") "dh" else method
  if("descriptives" %in% names(formals(MVN::mvn))) args$descriptives<-FALSE
  if(method=="energy" && "B" %in% names(formals(MVN::mvn))) args$B<-1000L
  cfa_mvn_table(do.call(MVN::mvn,args))
}

cfa_mvn_all <- function(data,ordered=character(),runner=cfa_mvn_runner) {
  methods<-c(hz="Henze–Zirkler",mardia="Mardia",royston="Royston",doornik_hansen="Doornik–Hansen",energy="Énergie")
  row<-function(method,detail="Global",statistic=NA_real_,p="—",reading="Non calculable",note="")
    data.frame(Méthode=unname(method),Volet=detail,Statistique=statistic,p=p,Lecture=reading,Note=note,check.names=FALSE,stringsAsFactors=FALSE)
  X<-as.data.frame(data);total<-nrow(X)
  if(length(ordered)) return(list(table=do.call(rbind,lapply(methods,function(m) row(m,reading="Non applicable",note="Indicateurs ordinaux : test de normalité continue non proposé."))),n_used=NA_integer_,n_total=total,
    note="Des indicateurs sont déclarés ordinaux. Le test de normalité continue de leurs codes n’est pas proposé. Les hypothèses de réponse latente doivent être examinées séparément."))
  if(!ncol(X) || !all(vapply(X,is.numeric,logical(1)))) stop("Ces diagnostics requièrent des indicateurs numériques.")
  X<-X[stats::complete.cases(X),,drop=FALSE];n<-nrow(X)
  if(n<3 || ncol(X)<2) return(list(table=do.call(rbind,lapply(methods,function(m) row(m,note="Au moins trois cas complets et deux indicateurs sont requis."))),n_used=n,n_total=total,note="Échantillon insuffisant pour comparer les cinq diagnostics."))
  numeric_value<-function(x) suppressWarnings(as.numeric(gsub(",",".",gsub("^[<>=[:space:]]+","",as.character(x)))))
  rows<-lapply(names(methods),function(method) {
    warnings<-character()
    tryCatch({
      result<-withCallingHandlers(with_cfa_seed(123,runner(X,method)),warning=function(w){warnings<<-unique(c(warnings,conditionMessage(w)));invokeRestart("muffleWarning")})
      if(!is.data.frame(result) && !is.matrix(result)) result<-cfa_mvn_table(result)
      result<-as.data.frame(result)
      if(!nrow(result)) stop("Aucun résultat retourné.")
      keys<-tolower(gsub("[^[:alnum:]]","",names(result)))
      pick<-function(candidates) {i<-match(candidates,keys);i<-i[!is.na(i)];if(length(i)) result[[i[1]]] else rep(NA,nrow(result))}
      statistics<-numeric_value(pick(c("statistic","hz","h","estatistic","dh","stat")))
      p_raw<-as.character(pick(c("pvalue","p","pval")));ps<-numeric_value(p_raw)
      tests<-as.character(pick(c("test","testname")))
      do.call(rbind,lapply(seq_len(nrow(result)),function(i) {
        valid<-is.finite(ps[i]) && ps[i]>=0 && ps[i]<=1
        small<-valid && (ps[i]<.05 || (grepl("^\\s*<",p_raw[i]) && ps[i]<=.05))
        p_text<-if(!valid) "—" else if(grepl("^\\s*<",p_raw[i])) gsub("\\.",",",p_raw[i]) else if(ps[i]<.001) "< 0,001" else formatC(ps[i],digits=3,format="f",decimal.mark=",")
        detail<-if(method=="mardia" && grepl("skew|asym",tests[i],ignore.case=TRUE)) "Asymétrie" else if(method=="mardia" && grepl("kurt|aplat",tests[i],ignore.case=TRUE)) "Aplatissement" else "Global"
        row(methods[method],detail,statistics[i],p_text,
          if(!valid) "Non calculable" else if(small) "Écart signalé (p < 0,05)" else "Pas de rejet à 0,05",
          paste(warnings,collapse=" ; "))
      }))
    },error=function(e) row(methods[method],note=conditionMessage(e)))
  })
  table<-do.call(rbind,rows);rownames(table)<-NULL
  list(table=table,n_used=n,n_total=total,
    note=paste0(n," cas complets sur ",total," lignes pour les cinq méthodes. Mardia distingue asymétrie et aplatissement. Les p sont non ajustés; les tests ne constituent pas un vote et l’absence de rejet ne prouve pas la normalité. Le test d’Énergie utilise son bootstrap paramétrique (1 000 réplications, graine 123)."))
}
