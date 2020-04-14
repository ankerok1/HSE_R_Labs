x=5
x
x=c(1,0)
x
y=c(2,3)
y
x+y
y^2
utils:::menuInstallPkgs()
local({pkg <- select.list(sort(.packages(all.available = TRUE)),graphics=TRUE)
if(nchar(pkg)) library(pkg, character.only=TRUE)})
x=getSymbols('IBM',auto.assign=FALSE)
tail(x)
tail(x,9)
head(x,9)
xCl=Cl(x)
tail(xCl,9)
plot(x)
plot(xCl)
x1=as.numeric(xCl)
mean(x1)
mean(xCL)
mean(xCl)
xUN=getSymbols('UNRATE',src="FRED",auto.assign=FALSE)
tail(xUN)
x=c(2,3)
y=c(10,20)
x
y
x=c(2,3)
hist(x1,breaks=10)
hist(x1,breaks=10,col="red")
hist(x1,breaks=10,col="wheat")
lines(density(x1),col=2,lwd=2)
hist(x1,breaks=10,col="wheat",prob=TRUE)
lines(density(x1),col=2,lwd=2)
xMean=mean(x1)
xMean
xStd=sd(x1)
xStd
curve(dnorm(x, mean(GSPC$ret.log, na.rm=T), sd(GSPC$ret.log,
, add=TRUE, col="blue",lwd=2)
curve(dnorm(x, xMean, xStd), add=TRUE, col="blue",lwd=2)
curve(dnorm(x, xMean, xStd), xlim=c(0,500), add=TRUE, col="blue",lwd=2)
hist(x1,breaks=10,xlim=c(0,500),col="wheat",prob=TRUE)
lines(density(x1),col=2,lwd=2)
curve(dnorm(x, xMean, xStd), add=TRUE, col="blue",lwd=2)
hist(x1,breaks=10,xlim=c(0,500),ylim=(0,0.02),col="wheat",prob=TRUE)
lines(density(x1),col=2,lwd=2)
curve(dnorm(x, xMean, xStd), add=TRUE, col="blue",lwd=2)
hist(x1,breaks=10,xlim=c(0,500),ylim=c(0,0.02),col="wheat",prob=TRUE)
lines(density(x1),col=2,lwd=2)
curve(dnorm(x, xMean, xStd), add=TRUE, col="blue",lwd=2)
xx=c(1,5,3,6,4,2,5,2,3,2,5,8,5,2,3,5,2,5)
stem(xx)
xx=c(11,35,23,46,34,32,65,26,34,23,53,84,53,22,33,52,23,54)
stem(xx)
xx2=runif(20,-4,5)
xx2
xx2=runif(20,20,50)
xx2
mean(xx)
median(xx)
xxxx=c(123,145,156,147,254,236,238)
stem(xxxx)
