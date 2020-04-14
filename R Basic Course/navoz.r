  # 1a Global price of Shrimp
  #Units:  U.S. Dollars per Kilogram, Not Seasonally Adjusted
  #Frequency:  Monthly
library("UsingR", lib.loc="~/R/win-library/3.2")
library("ggplot2", lib.loc="~/R/win-library/3.2")
library("quantmod", lib.loc="~/R/win-library/3.2")
library("ISwR", lib.loc="~/R/win-library/3.2")
library(MVA)
  x2=read.csv('PSHRIUSDM.csv',stringsAsFactors = TRUE)
  length(x2$PSHRIUSDM)
  tail(x2,10)
  x2is=x2$PSHRIUSDM
  stem(x2is)
  which(table(x2is)==max(table(x2is)))
  hist(x2is,breaks = 35,freq = FALSE)
  lines(density(x2is),col=1,lwd=2)
  # 1b,c
  summary(x2is)
  quantile(x2is, c(.90))
  # 1d
  diff(range(x2is))
  sd(x2is)
  IQR(x2is)
  boxplot(x2is)
  #1e
  qqnorm(x2is)
  qqline(x2is)  
  summary(x2)
  
  #2a
  library(readxl)
  test <- read_excel("H:/мучёба/1rehdsi/1mod/petrop/test.xlsx")
  x_1=test$`Fish and crustaceans, molluscs`
  x_2=test$`Fresh and Frozen Seafood`
  length(x_1)
  length(x_2)
  boxplot(x_1,x_2,names=c("molluscs","Fresh"))
  #2b,2d
  plot(x_1,x_2) 
  rug(x_1,side = 1)
  rug(x_2,side=2)
  #2c
  cor(x_1,x_2,method=c("pearson"))
  cor(x_1,x_2,method=c("spearman"))
  #2e
  test3=test[2:3]
  bvbox(test3)
  #2f
  (hull <- with(test, chull(x_1, x_2)))
  plot(x_1, x_2, pch = 1)
  with(test, polygon(x_1[hull], x_2[hull], density = 15, angle = 30))
  with(test, cor(x_1[-hull],x_2[-hull],method=c("pearson")))
  #3a
  test33 <- read_excel("H:/мучёба/1rehdsi/1mod/petrop/test33.xlsx")
  ylim <- with(test33, range(test33$`Fish and crustaceans, molluscs`)) * c(0.95, 1)
  plot(test33$`Fish and crustaceans, molluscs` , test33$`Fresh and Frozen Seafood`, data=test33,
       xlab = "dsa",
       ylab = "ssad", pch = 10,
       ylim = ylim)
  with(test33, symbols(test33$`Fish and crustaceans, molluscs`, test33$`Fresh and Frozen Seafood`, circles = test33$`Canned Seafood`,
                         inches = 0.5, add = TRUE))
  #3b
  stars(test33[2:4], cex = 0.55)
  #3c
  plot(test33[2:4])
  