# Facultatif : reconstruire les copies depuis les packages sources.
# À exécuter depuis la racine de l'application. Packages de préparation seulement :
# lavaan 0.7-3, psych 2.4.12, psychTools 2.6.4 pour les copies livrées.
source('cfa_datasets.R')
e<-new.env()
data('bock',package='psych',envir=e)
data('bfi',package='psych',envir=e)
data('ability',package='psychTools',envir=e)
data('msqR',package='psychTools',envir=e)
m<-cfa_example_catalog()
datasets<-list(
 HolzingerSwineford1939=lavaan::HolzingerSwineford1939,
 PoliticalDemocracy=lavaan::PoliticalDemocracy,
 lsat6=as.data.frame(e$lsat6),lsat7=as.data.frame(e$lsat7),
 ability=as.data.frame(e$ability),
 msq_affect=e$msqR[e$msqR$time==1,m$msq_affect$ordered],
 bfi=e$bfi[,m$bfi$ordered])
for(id in names(datasets)) {
 write.csv(datasets[[id]],m[[id]]$file,row.names=FALSE,na='',fileEncoding='UTF-8')
 writeLines(cfa_example_notice(id),file.path('data','exemples',paste0(id,'_notice.txt')),useBytes=TRUE)
}
