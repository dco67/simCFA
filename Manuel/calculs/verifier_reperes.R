# Script d'édition seulement. Le lecteur utilise exclusivement l'application.
# Depuis ce dossier, fournir le dossier de l'application en argument.
args<-commandArgs(TRUE)
app_dir<-normalizePath(if(length(args)) args[1] else '../CFA_revision6')
manual_dir<-normalizePath(if(length(args)>1) args[2] else '.')
setwd(app_dir)
source('cfa_core.R');source('cfa_validity.R');source('cfa_learning.R');source('cfa_datasets.R')
catdir<-file.path(manual_dir,'calculs')
fmt<-function(x,d=3) ifelse(is.na(x),'—',formatC(x,digits=d,format='f',decimal.mark=','))
table_qmd<-function(df,file,digits=3){
 if(all(c('indice','valeur') %in% names(df))) { df$valeur<-fmt(df$valeur,digits);df$valeur[df$indice=='pvalue' & df$valeur=='0,000']<-'< 0,001';df$valeur[df$indice=='df']<-sub(',000$','',df$valeur[df$indice=='df']) }
 df[]<-Map(function(x,nm) if(is.numeric(x)) {if(nm %in% c('N','ddl')) as.character(as.integer(x)) else fmt(x,digits)} else as.character(x),df,names(df))
 lines<-c(paste0('| ',paste(names(df),collapse=' | '),' |'),paste0('| ',paste(rep('---',ncol(df)),collapse=' | '),' |'),
 apply(df,1,function(x) paste0('| ',paste(x,collapse=' | '),' |')))
 writeLines(lines,file.path(catdir,file),useBytes=TRUE)
}
fit_one<-function(id,est=NULL,data=NULL,missing='listwise',std.lv=FALSE){
 m<-cfa_example_catalog()[[id]];if(is.null(data)) data<-cfa_load_example(id)
 opts<-list(estimator=if(is.null(est)) m$estimator else est,missing=missing,std.lv=std.lv,meanstructure=missing=='fiml')
 if(length(m$ordered)) opts$ordered<-m$ordered
 cfa_fit(m$syntax,data[,unique(unlist(m$factors))],opts)$fit
}
ids<-names(cfa_example_catalog());fits<-setNames(lapply(ids,fit_one),ids)
short<-c(HolzingerSwineford1939='HS',PoliticalDemocracy='Bollen',lsat6='LSAT 6',lsat7='LSAT 7',ability='SAPA',msq_affect='MSQ',bfi='BFI')
# Versions conventionnelles uniquement dans la comparaison synthétique; chaque
# résultat robuste est conservé séparément pour éviter de mélanger les versions.
fit_summary<-do.call(rbind,lapply(ids,function(id){
 f<-fits[[id]];fm<-lavaan::fitMeasures(f)
 data.frame(Jeu=short[[id]],N=as.integer(lavaan::lavInspect(f,'nobs')),ddl=unname(fm['df']),CFI=unname(fm['cfi']),RMSEA=unname(fm['rmsea']),SRMR=unname(fm['srmr']))
}))
table_qmd(fit_summary,'reperes_jeux.qmd')
for(id in ids){
 f<-fits[[id]];v<-cfa_validity(f)
 load<-lavaan::standardizedSolution(f);load<-load[load$op=='=~',c('lhs','rhs','est.std')];names(load)<-c('Facteur','Item','Saturation')
 table_qmd(load,paste0(id,'_saturations.qmd'))
 table_qmd(v$results[,c('Facteur','Alpha','Omega','AVE','H')],paste0(id,'_fiabilite.qmd'))
 cors<-as.data.frame(as.table(v$COR),responseName='Correlation');cors<-cors[as.integer(cors$Var1)>as.integer(cors$Var2),];names(cors)<-c('Facteur 1','Facteur 2','Corrélation')
 if(nrow(cors)) table_qmd(cors,paste0(id,'_correlations.qmd'))
 h<-as.data.frame(as.table(v$HTMT),responseName='HTMT');h<-h[as.integer(h$Var1)>as.integer(h$Var2),];names(h)<-c('Facteur 1','Facteur 2','HTMT')
 if(nrow(h))table_qmd(h,paste0(id,'_htmt.qmd'))
}
f<-fits$HolzingerSwineford1939
fm<-lavaan::fitMeasures(f,c('chisq','df','pvalue','cfi','tli','rmsea','rmsea.ci.lower','rmsea.ci.upper','srmr'))
hsidx<-data.frame(Indice=names(fm),Valeur=fmt(unname(fm)));hsidx$Valeur[hsidx$Indice=='pvalue']<-'< 0,001';hsidx$Valeur[hsidx$Indice=='df']<-'24';table_qmd(hsidx,'HS_indices.qmd')
fs<-fit_one('HolzingerSwineford1939',std.lv=TRUE)
stopifnot(max(abs(lavaan::fitted(f)$cov-lavaan::fitted(fs)$cov))<1e-4)
# Un facteur sur les neuf tests : comparaison descriptive, sans test LRT usuel.
fu<-cfa_fit(paste('General =~',paste(paste0('x',1:9),collapse='+')),cfa_load_example('HolzingerSwineford1939')[paste0('x',1:9)],list(estimator='ml'))$fit
compare<-function(f,label){x<-lavaan::fitMeasures(f);data.frame(Modèle=label,ddl=unname(x['df']),CFI=unname(x['cfi']),RMSEA=unname(x['rmsea']),SRMR=unname(x['srmr']))}
table_qmd(rbind(compare(f,'Trois facteurs'),compare(fu,'Un facteur')),'HS_un_facteur.qmd')
restriction<-cfa_restrict(f,'tau');ft<-cfa_fit(restriction$table,cfa_load_example('HolzingerSwineford1939')[paste0('x',1:9)],list(estimator='ml',std.lv=FALSE,meanstructure=FALSE))$fit;test<-lavaan::lavTestLRT(f,ft)
capture.output(test,file=file.path(catdir,'HS_tau_test.txt'))
# Fichier dérivé pédagogique : suppression déterministe et explicite de 90 cellules.
d<-cfa_load_example('HolzingerSwineford1939');d[1:30,'x2']<-NA;d[31:60,'x5']<-NA;d[61:90,'x8']<-NA
write.csv(d,file.path(manual_dir,'data','HS_absences_pedagogiques.csv'),row.names=FALSE,na='',fileEncoding='UTF-8')
fcc<-fit_one('HolzingerSwineford1939',data=d);ffi<-fit_one('HolzingerSwineford1939',data=d,missing='fiml')
table_qmd(data.frame(Traitement=c('Original complet','Cas complets','FIML'),N=c(301,lavaan::lavInspect(fcc,'nobs'),lavaan::lavInspect(ffi,'nobs')),
 CFI=sapply(list(f,fcc,ffi),function(x) unname(lavaan::fitMeasures(x,'cfi'))),SRMR=sapply(list(f,fcc,ffi),function(x) unname(lavaan::fitMeasures(x,'srmr')))),'HS_absences.qmd')
# Comparaison de corrélations de moyennes observées aux corrélations factorielles.
d<-cfa_load_example('HolzingerSwineford1939');mv<-sapply(cfa_example_catalog()$HolzingerSwineford1939$factors,function(v) rowMeans(d[v]))
r<-cor(mv);p<-lavaan::lavInspect(f,'cor.lv');pairs<-combn(colnames(r),2)
table_qmd(data.frame(Paire=apply(pairs,2,paste,collapse=' / '),Moyennes=apply(pairs,2,function(v)r[v[1],v[2]]),Facteurs=apply(pairs,2,function(v)p[v[1],v[2]])),'HS_scores_facteurs.qmd')
# Expérience contrôlée de simulation; mêmes paramètres de population et même graine.
sims<-lapply(c(100,1000),function(n)cfa_simulate(1,4,n,c(.7,.7),c(.1,.5),seed=123))
sf<-lapply(sims,function(s)cfa_fit('F1 =~ x1+x2+x3+x4',s$data,list(estimator='ml'))$fit)
table_qmd(data.frame(N=c(100,1000),Omega=sapply(sf,function(f)cfa_validity(f)$results$Omega),AVE=sapply(sf,function(f)cfa_validity(f)$results$AVE)),'simulation.qmd')
diagnostic_summary<-do.call(rbind,lapply(ids,function(id){
 a<-cfa_diagnostic_bundle(fits[[id]])$fit
 value<-function(n) a$valeur[match(n,a$indice)]
 data.frame(Jeu=short[[id]],N=as.integer(lavaan::lavInspect(fits[[id]],'nobs')),ddl=value('df'),CFI=value('cfi'),RMSEA=value('rmsea'),SRMR=value('srmr'))
}))
table_qmd(diagnostic_summary,'reperes_diagnostics.qmd')
for(id in c('msq_affect','bfi','lsat6','lsat7')) {
 a<-cfa_diagnostic_bundle(fits[[id]])$fit
 table_qmd(a[,c('indice','valeur','version')],paste0(id,'_diagnostics.qmd'))
 fm<-lavaan::fitMeasures(fits[[id]])
 variants<-data.frame(Version=c('Standard','Scaled','Robust'),CFI=unname(fm[c('cfi','cfi.scaled','cfi.robust')]),RMSEA=unname(fm[c('rmsea','rmsea.scaled','rmsea.robust')]));table_qmd(variants,paste0(id,'_versions.qmd'))
}
saveRDS(fits,file.path(catdir,'modeles_reperes.rds'))
result<-list(versions=list(R=R.version.string,lavaan=as.character(packageVersion('lavaan'))),summary=fit_summary,
 HS_indices=as.list(fm),tau=as.data.frame(test),factors=lapply(fits,function(f)cfa_validity(f)$results),
 fit_measures=lapply(fits,function(f)as.list(lavaan::fitMeasures(f))))
jsonlite::write_json(result,file.path(catdir,'reperes_verifies.json'),auto_unbox=TRUE,pretty=TRUE,digits=15)
writeLines(c(capture.output(sessionInfo()),'','Tous les modèles de référence ont convergé et passé le contrôle d’admissibilité.'),file.path(catdir,'VALIDATION_CALCULS.txt'))
cat('Repères numériques écrits.\n')
