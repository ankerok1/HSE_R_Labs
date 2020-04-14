# Cluster analysis

rm(list=ls())

# Parameters

if (!require(openxlsx))
{
  print("Пакет openxlsx не установлен")
} 

if (!require(car))
{
  print("Пакет car не установлен")
}

if (!require(cluster))
{
  print("Пакет cluster не установлен")
}

if (!require(fpc))
{
  print("Пакет fpc не установлен")
}

if (!require(pvclust))
{
  print("Пакет pvclust не установлен")
}

source("./scripts/01/read_write.r", echo = F)

#pth <- "./data/text01.txt"
#pth <- "./data/model01.txt"

# Матрица расстояний
# "euclidean", "manhattan", "gower"
iris.dist <- daisy(iris[seq(1,nrow(iris),5),1:4], stand = T, metric="manhattan")
ird <- as.matrix(iris.dist)

# "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA),
# "median" (= WPGMC) or "centroid" (= UPGMC)
irclst <- hclust(iris.dist, method = "complete", members = NULL)

# Дендрограмма
plot(irclst, labels = NULL, hang = 0.3, check = TRUE, horiz = F,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram",
     sub = Sys.time(), xlab = NULL, ylab = "Height")

# Bootstrep. Транспонировать
irisst <- t(iris[seq(1,nrow(iris),5),1:4])
iriss.pv <- pvclust(irisst, method.dist="manhattan", method.hclust="ward", nboot=10)
# col.pv=c(AU, BP, #edge) (способы подсчета частоты появления (AU - точнее) и номер узла)
plot(iriss.pv, col.pv=c(1,0,0), horiz = F, main="")

# Оптимизационная кластеризация
#kmeans(x, centers, iter.max = 10, nstart = 1, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)

cl_res <- kmeans(iris[,1:4], 3, iter.max = 10, nstart = 5, algorithm = "Hartigan-Wong", trace=FALSE)
tabulate(cl_res$cluster)
print(cl_res$centers)
iris01 <- cbind(iris, cl_res$cluster)
names(iris01)[6] <- "cluster"
tabulate(iris01$Species)
tabulate(iris01[iris01$cluster==1,]$Species)
tabulate(iris01[iris01$cluster==2,]$Species)
tabulate(iris01[iris01$cluster==3,]$Species)
tbl <- table(iris01$cluster, iris01$Species)
print(tbl)
summary(tbl)

# krange=1:10 Duda-Hart test is applied. criterion="ch" average silhouette width or Calinski-Harabasz 
cl_res1 <- kmeansruns(iris[,1:4],krange=1:5, criterion="ch", iter.max=100, runs=100, scaledata=T,alpha=0.001, critout=FALSE, plot=FALSE)
iris02 <- cbind(iris, cl_res1$cluster)
names(iris02)[6] <- "cluster1"
tabulate(iris02$Species)
z <- paste("Оптимальное количество кластеров: ",cl_res1$bestk, collapse="")
print(z)
tabulate(iris02[iris02$cluster1==1,]$Species)
tabulate(iris02[iris02$cluster1==2,]$Species)
tabulate(iris02[iris02$cluster1==3,]$Species)
tb2 <- table(iris02$cluster1, iris02$Species)
print(tb2)
summary(tb2)

