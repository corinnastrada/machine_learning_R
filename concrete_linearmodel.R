
library(lubridate)
library(GGally)
library(hydroGOF)
library(mvtnorm)
library(dplyr)
library(broom)
library(ggpubr)


concrete <- read.table("C:/Users/HP/OneDrive/Desktop/PROBABILITY AND STATISTICS/progetto_2022/concrete_data.csv", 
                  header = TRUE, sep = ",")


head(concrete)
tail(concrete)

##name of the variables
nome_colonne <- list(colnames(concrete))
nome_colonne

##target = Strength

str(concrete)

concrete$Fly.Ash 

sum(is.na(concrete))
##0

summary(concrete)

library(psych)
psych::describe(concrete)

library(reshape)
library(ggplot2)

meltData <- melt(concrete)
p <- ggplot(meltData, aes(factor(variable), value))
p + geom_boxplot() + facet_wrap(~variable, scale="free")

##Age is not gaussian
par(mfrow=c(1, 2))
boxplot(concrete$Age, main = "DISTRIBUZIONE DELLA VARIABILE Age")
qqnorm(concrete$Age, main = "DISTRIBUZIONE DELLA VARIABILE Age")
qqline(concrete$Age)

##Shapiro 
shapiro.test(concrete$Age)
##low p-value

##a normality test for each variable
shapiro.test(concrete$Blast.Furnace.Slag) ##non è normale
shapiro.test(concrete$Fly.Ash) ##non normale
shapiro.test(concrete$Water) ##nn
shapiro.test(concrete$Superplasticizer) ##nn
shapiro.test(concrete$Coarse.Aggregate ) ##nn
shapiro.test(concrete$Fine.Aggregate)  ##nn
shapiro.test(concrete$Strength ) ##nn



res <- cor(concrete)
round(res, 2) ##arrotondo


library(corrgram)
require(corrgram)
corrgram(concrete, order=TRUE)

##no problems regarding correlation

##cuter version
library(corrplot)
corrplot(res, type = "upper", order = "hclust", addCoef.col ='black',
         tl.col = "black", tl.srt = 45)




##histogramm

nclass.Sturges(concrete$Strength) ## 12
nclass.FD(concrete$Strength) ##19

hist(concrete$Strength, freq = FALSE,  main = " Istogramma della variabile dipendente", xlab = "forza del cemento", ylab = "densità") ##mostra la densità
hist(concrete$Strength, freq = FALSE, breaks = 19,  main = " Istogramma della variabile dipendente", xlab = "forza del cemento", ylab = "densità") ##mostra la densità


library(ggplot2) # histogram with density plot
ggplot(data=concrete, aes(Strength)) + geom_histogram(aes(y =..density..), bins = 12, fill = "orange") + geom_density()



##divide in train and test 

library(caret)
index <- createDataPartition(concrete$Strength, p = .70, list = FALSE)
train <- concrete[index, ]
test <- concrete[-index, ]

dim(train) 
dim(test)

##SOME MODELS
mod1 <- lm(Strength ~ Cement + Blast.Furnace.Slag + Fly.Ash + Water + Superplasticizer + Coarse.Aggregate + Fine.Aggregate + Age, data = train)

summary(mod1)
##Multiple R-squared:  0.6088,	Adjusted R-squared:  0.6044 

par(mfrow=c(2,2))
plot(mod1)
par(mfrow=c(1,1))
##i grafici non sono male

##fit some models after cancelling irrelevant variables

##delete fine aggregate
mod2 <- lm(Strength ~ Cement + Blast.Furnace.Slag + Fly.Ash + Water + Superplasticizer + Coarse.Aggregate  + Age, data = train)
summary(mod2)

par(mfrow=c(2,2))
plot(mod2)
par(mfrow=c(1,1))


##Multiple R-squared:  0.6071,	Adjusted R-squared:  0.6032 

##deletecoarse aggregate
mod3 <- lm(Strength ~ Cement + Blast.Furnace.Slag + Fly.Ash + Water + Superplasticizer  + Age, data = train)
summary(mod3)

##ultiple R-squared:  0.6069,	Adjusted R-squared:  0.6036 
##F-statistic: 183.9 on 6 and 715 DF,  p-value: < 2.2e-16

par(mfrow=c(2,2))
plot(mod3)
par(mfrow=c(1,1))


##CHOOSE THE MODEL USING AKAIKE INFORMAITON CRITERION

##install.packages("AICcmodavg")
library(AICcmodavg)

models <- list(mod1, mod2, mod3)
model_names <- c('mod1', 'mod2', 'mod3') ##do per pigrizia gli stessi nomi

aictab(cand.set= models, modnames = model_names)
##the best one is the third model



##install.packages("ggstance")
library("jtools")
plot_summs(mod1, mod2, mod3, scale = TRUE)

plot_summs(mod1, mod2, mod3, scale = TRUE, plot.distributions = TRUE)


##install.packages("huxtable")
library("huxtable")
export_summs(mod1, mod2, mod3, scale = TRUE)
##a nice table to confront some results

##error statistics - tests

export_summs(mod1, mod2, mod3, scale = TRUE, error_format = "[{conf.low}, {conf.high}]") #confidence levels




