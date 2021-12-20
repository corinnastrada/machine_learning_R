
##https://www.kaggle.com/elikplim/forest-fires-data-set


d <- read.csv("forestfires_senza_zero.csv", sep=",", dec = ".",  
                         stringsAsFactors=TRUE, na.strings=c("NA","NaN", " "))

head(d)



##  X Y month day FFMC  DMC    DC  ISI temp RH wind rain area

hist(d$area, main = "distribuzione della variabile 'area'")

##the target is asymmetric

##1.	X - x-axis spatial coordinate within the Montesinho park map: 1 to 9
##2.	Y - y-axis spatial coordinate within the Montesinho park map: 2 to 9


d$X = as.factor(d$X)
d$Y = as.factor(d$Y)

nrow(d)

sapply(d, function(x) sum(is.na(x)))


fit = lm(area ~ . , data= d)

summary(fit)

##Residual standard error: 87.32

## partial plot

library(car)
par(mfrow=c(2,2))
avPlots(fit)
##residuals diagnostics

par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))



anova(fit)



drop1(fit, test="F")



library(coefplot)
coefplot(fit, intercept= FALSE)


library(forestmodel)
forest_model(fit)


##X Y month day FFMC  DMC    DC  ISI temp RH wind rain area


library(dplyr)

d$area = as.factor(d$area)
d$area = as.numeric(d$area)


d_numeric <- d%>% dplyr::select_if(is.numeric)

##look at the correlation matrix

require(corrgram)

corrgram(d_numeric)

corrgram(d_numeric, lower.panel=panel.cor, cex = 1, cex.labels = 1)


##FFMC and IISI: 0.7 - DMC and DC 0.67


##TOL / VIF

library(mctest)
imcdiag(fit)

##delete the variable DC

d$area = as.numeric(d$area)

fit2 = lm(area ~ X  + Y +month +day+ FFMC+  DMC    
         +ISI +temp +RH+ wind +rain, data= d)

summary(fit2)


par(mfrow=c(2,2))
plot(fit2)
par(mfrow=c(1,1))

##a little bit better regarding to VIF values

library(mctest)
imcdiag(fit2)

##The tol of temp is 0.1661

##delete temp

fit3= lm(area ~ X  + Y +month +day+ FFMC+  DMC    
          +ISI  +RH+ wind +rain, data= d)

summary(fit3)

par(mfrow=c(2,2))
plot(fit3)
par(mfrow=c(1,1))


library(coefplot)
coefplot(fit3, intercept=FALSE, decreasing = TRUE, sort= "magnitude")


##Residual standard error: 70.64
##Adjusted R-squared:  0.0618

library(mctest)
imcdiag(fit3)

##it looks like fit3 is the best model

##some statistics
library(gvlma)
gvlma(fit3)


##chi square test

library(dplyr)
b_fac <- d%>% dplyr::select_if(is.factor)
head(b_fac)

##X Y month day


library(plyr)
combos <- combn(ncol(b_fac),2)
adply(combos, 2, function(x) {
  test <- chisq.test(b_fac[, x[1]], b_fac[, x[2]])
  
  out <- data.frame("Row" = colnames(b_fac)[x[1]]
                    , "Column" = colnames(b_fac[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    , "df"= test$parameter
                    , "p.value" = round(test$p.value, 3)
  )
  return(out)
  
})  



library(plyr)
adply(combos, 2, function(x) {
  test <- chisq.test(b_fac[, x[1]], b_fac[, x[2]])
  tab  <- table(b_fac[, x[1]], b_fac[, x[2]])
  out <- data.frame("Row" = colnames(b_fac)[x[1]]
                    , "Column" = colnames(b_fac[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    , "df"= test$parameter
                    , "p.value" = round(test$p.value, 3)
                    , "n" = sum(table(b_fac[,x[1]], b_fac[,x[2]]))
                    , "u1" =length(unique(b_fac[,x[1]]))-1
                    , "u2" =length(unique(b_fac[,x[2]]))-1
                    , "nMinu1u2" =sum(table(b_fac[,x[1]], b_fac[,x[2]]))* min(length(unique(b_fac[,x[1]]))-1 , length(unique(b_fac[,x[2]]))-1) 
                    , "Chi.Square norm"  =test$statistic/(sum(table(b_fac[,x[1]], b_fac[,x[2]]))* min(length(unique(b_fac[,x[1]]))-1 , length(unique(b_fac[,x[2]]))-1)) 
  )
  
  
  return(out)
  
}) 



##target and covariates transformations


library(MASS)
boxcox2 <- boxcox(fit3)

lambda2=boxcox2$x[which.max(boxcox2$y)]
lambda2

##0.6666667

##square

fit_bc_sqrt = lm((area^0.5) ~ X  + Y +month +day+ FFMC+  DMC    
              +ISI  +RH+ wind +rain , data= d)

summary(fit_bc_sqrt)

##Residual standard error: 3.623 on 235 degrees of freedom
##Multiple R-squared:   0.1855,	Adjusted R-squared:  0.06861


##diagnostic plots

par(mfrow=c(2,2))
plot(fit_bc_sqrt)
par(mfrow=c(1,1))


par(mfrow=c(2,2))
plot(fit3)
par(mfrow=c(1,1))


##FIT_bc_sqrt seems to be the best model

##covariate transfromation

library(mgcv)

fit_bc_gam= gam(area ~ X  + Y +month +day+ FFMC+  DMC    
         +ISI  +RH+ wind +rain, data= d)

summary(fit_bc_gam)

##R-sq.(adj) =  0.0642


gam1 = gam(area ~ X  + Y +month +day+ c(FFMC)+  c(DMC)    
           +c(ISI)  +c(RH)+ c(wind) +c(rain) , data= d)

summary(gam1)

library(mgcv)
plot(gam1)


gam1_r = gam(area ~ X  + Y +month +day+ s(FFMC)+  s(DMC)    
           +s(ISI)  +s(RH)+ s(wind) +rain , data= d)

summary(gam1_r)

library(mgcv)
plot(gam1_r)


##try with  RH, WIND both squared and ^3

##just RH

gam_2 = gam(area ~ X  + Y +month +day+ FFMC+ DMC    
            +ISI  +RH +I(RH^2) + wind +rain , data= d)

summary(gam_2)

gam_2_ab = gam(area ~ X  + Y +month +day+ FFMC+ DMC    
            +ISI  +RH + wind + I(rain^2) , data= d)

summary(gam_2_ab)

fit_bc_2 = lm(area ~ X  + Y +month +day+ FFMC+ DMC    
               +ISI  +RH +I(RH^2) + wind +rain , data= d)
summary(fit_bc_2)

##R squared: 0.0789

## just WIND

gam_2_a = gam(area ~ X  + Y +month +day+ FFMC+ DMC    
            +ISI  +RH + wind +I(wind ^2) +rain , data= d)

summary(gam_2_a)

##R squared adjusted: 0.0682



#just RH squared is fine


##Breusch pagan test

library(lmtest)
bptest(fit3)

##BP = 42.155, df = 35, p-value = 0.189

bptest(gam_2)
##BP = 41.416, df = 36, p-value = 0.24627

##White Test
library(car)

ncvTest(fit3)
##Chisquare = 0.1701539, Df = 1, p = 0.67998


gam_2_lm = lm(area ~ X  + Y +month +day+ FFMC+ DMC    
            +ISI  +RH +I(RH^2) + wind +rain , data= d)

ncvTest(gam_2_lm)

##some residual plots
par(mfrow=c(2,2))
plot(gam_2_lm)
par(mfrow=c(1,1))


----------------------------------------

##i try to reduce the number of classes

d_nuovo = d  ## 

head(d_nuovo)
library(factorMerger)
library(dplyr)
reduce_levels = mergeFactors(response = d_nuovo$area, factor=d_nuovo$month)

plot(reduce_levels, panel="GIC", panelGrid = FALSE)

##3 groups:
##a)may - oct - dec 
##b) apr - mar- feb - sep
##c) aug - un -jul

##add the optimal grouping variable

og=cutTree(reduce_levels)

d_nuovo$optimal_group = og

head(d_nuovo)

rec=lm(d_nuovo$area  ~ d_nuovo$month, data = d_nuovo)
orig = lm(d_nuovo$area ~ d_nuovo$optimal_group, data=d_nuovo)

summary(rec)

summary(orig)


##THE MODEL WITH OPTIMAL GROUPING IS THE BEST ONE



plot(d_nuovo$optimal_group, d_nuovo$area)

##no outliers or relevant problems


##FINAL MODEL

fit4 = lm(area ~ X  + Y  +day+ FFMC+ DMC    
              +ISI  +RH +I(RH^2) + wind +rain + optimal_group, data= d_nuovo)

summary(fit4)


##diagnostic plots
par(mfrow=c(2,2))
plot(fit4)
par(mfrow=c(1,1))


##MODEL SELECTION

library(MASS)
step <- stepAIC(fit4, direction="both")

##final model
##area ~ DMC + RH + I(RH^2) + optimal_group

fit4_modsel = lm(area ~ DMC + RH + I(RH^2) + optimal_group, data=d_nuovo)

drop1(fit4_modsel, .~., test="F")


summary(fit4_modsel)

par(mfrow=c(2,2))
plot(fit4_modsel)
par(mfrow=c(1,1))


##influential values
library(car)
influencePlot(fit4_modsel, main="Influence Plot", sub="Circle size is proportional to Cook's distance")


stzed <- rstudent(fit4_modsel)
##

lever <- hat(model.matrix(fit4_modsel))

dffits1 <- dffits(fit4_modsel)

##cook distances
cooksd <- cooks.distance(fit4_modsel)

cooksda <- data.frame(cooksd)
summary(cooksd)


cooksd_1 <- data.frame(cooks.distance(fit4_modsel))
cutoff <- 4/(nrow(d_nuovo))

plot(fit4_modsel, which=4, cook.levels = cutoff)
abline(h=cutoff)


##select the influential values

influential <- d_nuovo[cooksd >= cutoff,]

filtered <- d_nuovo[cooksd <cutoff,]

fit4_modsel_fil <- lm(area ~ DMC + RH + I(RH^2) + optimal_group, data=filtered)
##model fitted only on non-influent observations


summary(fit4_modsel_fil)

par(mfrow=c(2,2))
plot(fit4_modsel_fil)
par(mfrow=c(1,1))


plot(fit4_modsel_fil)

summary(fit4_modsel_fil)

##BREUSCH-pagan test
library(lmtest)
bptest(fit4_modsel_fil)

##robust regression

library("car")

BOOT.MOD =Boot(fit4_modsel_fil, R=1999)
confint (BOOT.MOD, level = c(.95), type = "perc")
hist(BOOT.MOD, legend = "separate")

final_model <- lm(area ~ DMC + optimal_group, data=filtered)

summary(final_model)

par(mfrow=c(2,2))
plot(final_model)
par(mfrow=c(1,1))

BOOT.MOD =Boot(final_model, R=1999)
confint (BOOT.MOD, level = c(.95), type = "perc")
hist(BOOT.MOD, legend = "separate")



##predicted vs observed graph
plot(d$area, predict(fit),
     xlab="Observed values", ylab = "Predicted values")
abline(a=0,b=1)


plot(filtered$area, predict(final_model),
     xlab="Observed values", ylab = "Predicted values")
abline(a=0,b=1)

plot(filtered$area, predict(fit4_modsel_fil),
     xlab="Observed values", ylab = "Predicted values")
abline(a=0,b=1)

library(lmtest)
bptest(final_model)
