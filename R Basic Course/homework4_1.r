

install.packages("dplyr")
library(dplyr)

df <- read.csv("https://raw.githubusercontent.com/allatambov/R-programming-3/master/homeworks/hw4/wgi_fh.csv",  sep = ";", encoding = "UTF-8")
View(df)


df[df == "#N/A"] <- NA
df <- na.omit(df)

write.csv(df, "wgi_fh_new.csv")

?read.csv

wgifh <- read.csv("wgi_fh_new.csv", sep = ",", dec = ",", stringsAsFactors = FALSE)
str(wgifh)
wgifh <- na.omit(wgifh)

colnames(wgifh)[1] <- "id"

install.packages("dplyr")
library(dplyr)

str(wgifh)
wgifh$fh


wgifh <- wgifh %>% mutate (free = as.integer(c(fh < 3.0)),
                          partly_free = as.integer(c(fh > 3.0 & fh < 5.0)),
                          not_free = as.integer(c(fh > 5.0)))

wgifh$fh_type <- names(wgifh[12:14])[max.col(wgifh[12:14])]
wgifh$fh_type <- factor(wgifh$fh_type)

t <- table(wgifh$fh_type)
t
# free    not_free partly_free 
#99          54          42 


barplot(t, main = "���� ����� �� Freedom Rating",
        names.arg = c("free",
                      "partly free",
                      "not free"), 
        col = c("cornflowerblue", "darkorange", "darkred"),
        ylab = "����� �����",
        ylim = c(0,150))

boxplot(va ~ fh_type, data = wgifh)
#���������� �������� ���� � ��� ��� � ���. ��� �� �����, ������� ������� �� �������� "�����", �������

boxplot(va ~ fh_type, data = wgifh, main = "���������� ������� Voice & Accountability �� ���� �����",
        names = c("Free","Partly Free","Not Free"),
        col = c("cornflowerblue", "darkorange", "darkred"))

hist(wgifh$cc, main = "���������� ������� Control of Corruption �� �������", 
     col = "paleturquoise2",
     xlab = "������",
     ylab = "����������� ��������� � �����. �����",
     xlim = c(-2, 3),
     freq = FALSE)
   
curve(dnorm(x, mean = mean(wgifh$cc, na.rm = TRUE),
             sd = sd(wgifh$cc)), add = TRUE, col = "orchid")
#����� �� ���������� �������������, ��� ����� �� "�����������" 

shapiro.test(wgifh$cc)

#������� �������� "������������� ������� ���������� �� ����������� ������������, ������� ���������� �������������"
#�������� ������-�����, ������� �����: ����������� ������ � ������ ������ ���������� - ������� ��� �������� �� �����������.

cols <- c("green","blue","red")[wgifh$fh_type]

plot(wgifh$ps, wgifh$ge, main = "����� ����� ������� ������������ ������������ \n ������� ������������� ������������� ",
     xlab = "������ Political Stability and Lack of Violence",
     ylab = "������ Government Effectiveness", col = cols)
text(x = wgifh$ps, y = wgifh$ps, labels = wgifh$cnt_code) 

#����� ������, �������

cor.test(wgifh$ps, wgifh$ge, method = "spearman")


install.packages("car")
library(car)

?scatterplotMatrix

scatterplotMatrix(wgifh[5:10], diagonal = "histogram", 
                  smooth = FALSE, var.names = c(""))


M <- cor(wgifh[5:10])

install.packages(corrplot)
library(corrplot)

corrplot(M, method = "ellipse")

corrplot(M, method = "number")

corrplot(M, method = "color") 











install.packages("mice")
install.packages("VIM")