data(decathlon)
res <- PCA(decathlon,quanti.sup=11:12,quali.sup=13)
plot(res,habillage=13)
res$eig
x11()
barplot(res$eig[,1],main="Eigenvalues",names.arg=1:nrow(res$eig))
res$ind$coord
res$ind$cos2
res$ind$contrib
dimdesc(res)
aa=cbind.data.frame(decathlon[,13],res$ind$coord)
bb=coord.ellipse(aa,bary=TRUE)
plot.PCA(res,habillage=13,ellipse=bb)


library(FactoMineR)
temperature <- read.table("http://factominer.free.fr/book/temperature.csv",header=TRUE,sep=";",dec=".",row.names=1)
res.pca <- PCA(temperature[1:23,],scale.unit=TRUE,ncp=Inf,graph=FALSE,quanti.sup=13:16,quali.sup=17)
res.hcpc <- HCPC(res.pca)