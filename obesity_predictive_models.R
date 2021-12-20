
dataset_iniziale <- read.csv("obesity_dataset.csv", sep=",", dec = ".",  
              stringsAsFactors=TRUE, na.strings=c("NA","NaN", " "))

head(dataset_iniziale)

library(funModeling)
library(dplyr)
library(caret)
library(PerformanceAnalytics)

nrow(dataset_iniziale)
##2111


sapply(dataset_iniziale, function(x) sum(is.na(x)))



dataset_iniziale$FCVC <- cut(dataset_iniziale$FCVC,
                     breaks=c(-Inf, 1.1, 2, Inf),
                     labels=c("1","2","3"))

dataset_iniziale$NCP <- cut(dataset_iniziale$NCP,
                             breaks=c(-Inf, 1.1, 2, Inf),
                             labels=c("1","2","3"))

dataset_iniziale$CH2O <- cut(dataset_iniziale$CH2O,
                             breaks=c(-Inf, 1.1, 2, Inf),
                             labels=c("1","2","3"))


dataset_iniziale$FAF <- cut(dataset_iniziale$FAF,
                             breaks=c(-Inf, 1.1, 2, 3, Inf),
                             labels=c("0","1","2", "3"))

dataset_iniziale$TUE <- cut(dataset_iniziale$TUE,
                             breaks=c(-Inf, 1.1, 2, Inf),
                             labels=c("0","1","2"))




str(dataset_iniziale)

##--- MODELS BUILDING

table(dataset_iniziale$NObeyesdad)

##target variable recodification
##in order to have a binary target

dataset_iniziale$obesita <- dataset_iniziale$NObeyesdad
dataset_iniziale$NObeyesdad <- NULL


library(car)
dataset_iniziale$obesita<-recode(dataset_iniziale$obesita, 
                                 recodes="'Normal_Weight'='c0'")

dataset_iniziale$obesita<-recode(dataset_iniziale$obesita, 
                                 recodes="'Insufficient_Weight'='c0'")


dataset_iniziale$obesita<-recode(dataset_iniziale$obesita, 
                                 recodes="'Overweight_Level_I'='c0'")

dataset_iniziale$obesita<-recode(dataset_iniziale$obesita, 
                                 recodes="'Overweight_Level_II'='c0'")



dataset_iniziale$obesita<-recode(dataset_iniziale$obesita, 
                                 recodes="'Obesity_Type_I'='c1'")

dataset_iniziale$obesita<-recode(dataset_iniziale$obesita, 
                                 recodes="'Obesity_Type_II'='c1'")

dataset_iniziale$obesita<-recode(dataset_iniziale$obesita, 
                                 recodes="'Obesity_Type_III'='c1'")

table(dataset_iniziale$obesita)

##c0   c1 
##1139  972 
##quite a balanced dataset

##TARGET VARIABLE: c1


library(caret)

set.seed(1234)

cpart=createDataPartition(y=dataset_iniziale$obesita,times=1,p=.6)

train.df=dataset_iniziale[cpart$Resample1,]
test.df=dataset_iniziale[-cpart$Resample1,]


str(train.df) ##1268 obs. of  15 variables
str(test.df) ##843 obs. of  15 variables 

##PREPROCESSING


library(dplyr)
dataset_numeric <- dataset_iniziale%>% dplyr::select_if(is.numeric)

library(caret)
R=cor(dataset_numeric)

correlatedPredictors = findCorrelation(R, cutoff = 0.95, names = TRUE)
correlatedPredictors

correlatedPredictors = findCorrelation(R, cutoff = 0.9, names = TRUE)
correlatedPredictors

##no problems regarding collinearity


# 1.1 for each model requiring model selection do it before tuning models with caret####
#   select covariates with tree 

set.seed(1234)
cvCtrl <- trainControl(method = "cv", number=10, search="grid", 
                       classProbs = TRUE, summaryFunction=twoClassSummary)
tree <- train(obesita~. ,data=train.df, 
              method = "rpart",
              tuneLength = 10,
              trControl = cvCtrl)
# best accuracy using best cp
tree
# final model
getTrainPerf(tree)
plot(tree)


##The final value used for the model was cp = 0.


# var imp of the tree
plot(varImp(object=tree),main="Importanza delle variabili")


# select only important variables
vi=as.data.frame(tree$finalModel$variable.importance)
vi
viname=row.names(vi)
head(viname)

##[1] "Weight"                            "Height"                           
##[3] "Age"                               "CH2O3"                            
##[5] "family_history_with_overweightyes"  

---------------------
  
# new train and test with selected covariates  target
  
train2 = train.df

head(train2)

train2$FAVC<-NULL
train2$CAEC<-NULL
train2$FAF<-NULL
train2$MTRANS<-NULL
train2$CALC<-NULL
train2$TUE<-NULL
train2$FCVC<-NULL
train2$SCC <- NULL
train2$SMOKE <- NULL
train2$NCP <- NULL
train2$Gender <- NULL

##ora test

test2 = test.df

test2$FAVC<-NULL
test2$CAEC<-NULL
test2$FAF<-NULL
test2$MTRANS<-NULL
test2$CALC<-NULL
test2$TUE<-NULL
test2$FCVC<-NULL
test2$SCC <- NULL
test2$SMOKE <- NULL
test2$NCP <- NULL
test2$Gender <- NULL

head(test2)



##-------------------###

##predictors with variance 0 or close to 0


nzv = nearZeroVar(dataset_iniziale, saveMetrics = TRUE)
nzv

## SMOKE, scc have NEAR ZERO VARIANCE 

##remove the columns

train.df$SMOKE <- NULL
train.df$SCC <- NULL

test.df$SMOKE <- NULL
test.df$SCC <- NULL


##MODELS

##to select the model, use the ROC measure

##1 glm model

set.seed(1234)
ctrl =trainControl(method="cv", number = 10, classProbs = T,
                   summaryFunction=twoClassSummary)
glm=train(obesita~.,
          data=train.df,method = "glm",
          trControl = ctrl, tuneLength=5, trace=TRUE, na.action = na.pass)
glm

plot(glm)



confusionMatrix(glm)

##Reference
##Prediction   c0   c1
##c0 53.2  0.5
##c1  0.8 45.6

##Accuracy (average) : 0.9874


set.seed(1234)
ctrl =trainControl(method="cv", number = 10, classProbs = T,
                   summaryFunction=twoClassSummary)
glm_IMP=train(obesita~.,
          data=train2,method = "glm", metric = 'ROC',
          trControl = ctrl, tuneLength=5, trace=TRUE, na.action = na.pass)
glm_IMP

confusionMatrix(glm_IMP)

##Accuracy (average) : 0.9961

#2. Lasso 

set.seed(1234)
grid_1 = expand.grid(.alpha=1, .lambda=seq(0, 1, by= 0.01))
ctrl =trainControl(method="cv", number = 10, classProbs = T,
                   summaryFunction=twoClassSummary)

lasso=train(obesita~.,
            data=train.df,method = "glmnet", family="binomial",
            trControl = ctrl, tuneLength=5, tuneGrid = grid_1, na.action = na.pass)
lasso

plot(lasso)

confusionMatrix(lasso)

##Accuracy (average) : 0.9937


#3. Naive Bayes

set.seed(1234)

ctrl =trainControl(method="cv", number = 10, classProbs = T,
                   summaryFunction=twoClassSummary)
naivebayes=train(obesita~.,
                 data=train.df, method = "naive_bayes",
                 trControl = ctrl, tuneLength=5, na.action = na.pass) 

naivebayes
plot(naivebayes)
confusionMatrix(naivebayes)

##Accuracy (average) : 0.8084

#4.  PLS regression
library(pls)
set.seed(1234)
Control=trainControl(method= "cv",number=10, classProbs=TRUE,
                     summaryFunction=twoClassSummary)
pls=train(obesita~. , data=train2 , method = "pls",
          trControl = Control, tuneLength=5)
pls
plot(pls)
confusionMatrix(pls)

## Accuracy (average) : 0.9629


#Reti neurali
set.seed(1234)
metric <- "Accuracy"
ctrl = trainControl(method="cv", number=10, search = "grid")
nnet <- train(train2[-6], train2$obesita,
                     method = "nnet",
                     preProcess = "range", 
                     metric=metric, trControl=ctrl,
                     trace = TRUE, #use to see convergence
                     maxit = 300)

plot(nnet)
nnet

##final  value 0.717163 
##stopped after 300 iterations
# weights:  25
##initial  value 911.772724 
##iter  10 value 497.532987
##iter  20 value 128.743957
##final  value 128.732503 
##converged

print(nnet)
plot(nnet)

confusionMatrix(nnet)

## Accuracy (average) : 0.9984


##-----------TUNING ---------


results <- resamples(list( glm=glm, glm_IMP=glm_IMP, lasso=lasso, 
                          naivebayes=naivebayes, pls=pls))

summary(results)

bwplot(results)



###---ROC-----



test.df$glm=predict(glm,test.df, "prob")[,2]
test2$glm_IMP=predict(glm_IMP,test2, "prob")[,2]
test.df$lasso=predict(lasso,test.df, "prob")[,2]
test.df$naivebayes=predict(naivebayes,test.df, "prob")[,2]
test2$pls=predict(pls,test2, "prob")[,2]
test2$nnet=predict(nnet,test2, "prob")[,2]


##ROC VALUES

library(pROC)

roc.glm=roc(obesita ~ glm, data = test.df)
roc.glm_IMP=roc(obesita ~ glm_IMP, data = test2)
roc.lasso=roc(obesita ~ lasso, data = test.df)
roc.naivebayes=roc(obesita ~ naivebayes, data = test.df)
roc.pls=roc(obesita ~ pls, data = test2)
roc.nnet=roc(obesita ~ nnet, data = test2)


roc.glm

roc.glm_IMP

roc.lasso

roc.naivebayes

roc.pls

roc.nnet


plot(roc.glm)
plot(roc.glm_IMP, add= T, col="pink")
plot(roc.lasso,add=T,col="blue")
plot(roc.naivebayes,add=T,col="yellow")
plot(roc.pls,add=T,col="red")
plot(roc.nnet,add=T,col="green")
legend( "bottomleft", c("glm - AUC = 0.9931",
                        "glm_IMP - AUC = 0,9974",
                        "naive - AUC = 0.9526",
                        "lasso - AUC = 0.9999",
                        "nnet - AUC = 0.9866",
                        "pls - AUC = 0.9976"),
        col=c("black", "pink", "yellow","blue", "green","red")) 



#######################################################################################
###                                   LIFT OF A GLM MODEL                           ###
#######################################################################################


gain_lift(data = test.df, score = 'lasso', target = 'obesita')
gain_lift(data = test2, score = 'glm_IMP', target='obesita')
gain_lift(data = test2, score = 'pls', target = 'obesita')
gain_lift(data = test.df, score = 'glm', target = 'obesita')



#--------------------choose the threshold-----------------------


df=test2
df$obesita=test2$obesita
head(df)

df$pls <- NULL
df$nnet <- NULL


df$obesita=ifelse(df$obesita=="c1","OB","NOB") # il nostro event c1 cioè l'obesità è ora M
head(df)
df$ProbOB=test2$glm_IMP  
# winner model



library(dplyr)
# for each threshold, find tp, tn, fp, fn and the sens=prop_true_M, 
##spec=prop_true_R, precision=tp/(tp+fp)

thresholds <- seq(from = 0, to = 1, by = 0.01)
prop_table <- data.frame(threshold = thresholds, prop_true_OB = NA,  prop_true_NOB = NA, true_OB = NA,  true_NOB = NA ,fn_OB=NA)

for (threshold in thresholds) {
  pred <- ifelse(df$ProbOB > threshold, "OB", "NOB")  # be careful here!!!
  pred_t <- ifelse(pred == df$obesita, TRUE, FALSE)
  
  group <- data.frame(df, "pred" = pred_t) %>%
    group_by(obesita, pred) %>%
    dplyr::summarise(n = n())
  
  group_OB <- filter(group, obesita == "OB")
  
  true_OB=sum(filter(group_OB, pred == TRUE)$n)
  prop_OB <- sum(filter(group_OB, pred == TRUE)$n) / sum(group_OB$n)
  
  prop_table[prop_table$threshold == threshold, "prop_true_OB"] <- prop_OB
  prop_table[prop_table$threshold == threshold, "true_OB"] <- true_OB
  
  fn_OB=sum(filter(group_OB, pred == FALSE)$n)
  # true M predicted as R
  prop_table[prop_table$threshold == threshold, "fn_OB"] <- fn_OB
  
  
  group_NOB <- filter(group, obesita == "NOB")
  
  true_NOB=sum(filter(group_NOB, pred == TRUE)$n)
  prop_NOB <- sum(filter(group_NOB, pred == TRUE)$n) / sum(group_NOB$n)
  
  prop_table[prop_table$threshold == threshold, "prop_true_NOB"] <- prop_NOB
  prop_table[prop_table$threshold == threshold, "true_NOB"] <- true_NOB
  
}

head(prop_table, n=10)

# prop_true_OB = sensitivity
# prop_true_NOB = specificicy

# now think to your best cell in the matrix ad decide the metric of interest##########
##########
#pred	
#true	OB	   NOB
#OB  	 TP	  FN
#NOB	   FP	  TN
##########

# calculate other missing measures

# n of observations of the validation set    
prop_table$n=nrow(df)

# false positive (fp_M) by difference of   n and            tn,                 tp,         fn, 
prop_table$fp_OB=nrow(df)-prop_table$true_NOB-prop_table$true_OB-prop_table$fn_OB

# find accuracy
prop_table$acc=(prop_table$true_NOB+prop_table$true_OB)/nrow(df)

# find precision
prop_table$prec_OB=prop_table$true_OB/(prop_table$true_OB+prop_table$fp_OB)

# find F1 =2*(prec*sens)/(prec+sens)
# prop_true_M = sensitivity

prop_table$F1=2*(prop_table$prop_true_OB*prop_table$prec_OB)/(prop_table$prop_true_OB+prop_table$prec_OB)

# verify not having NA metrics at start or end of data 
head(prop_table)
tail(prop_table)
# we have typically some NA in the precision and F1 at the boundary..put,impute 1,0 respectively 

library(Hmisc)
#impute NA as 0, this occurs typically for precision
prop_table$prec_OB=impute(prop_table$prec_OB, 1)
prop_table$F1=impute(prop_table$F1, 0)
tail(prop_table)
colnames(prop_table)
# drop counts, PLOT only metrics
prop_table2 = prop_table[,-c(4:8)] 
head(prop_table2)
tail(prop_table2)

# plot measures vs soglia
# before we must impile data vertically: one block for each measure
library(dplyr)
library(tidyr)
gathered=prop_table2 %>%
  gather(x, y, prop_true_OB:F1)
head(gathered)


gathered=prop_table2 %>%
  gather(x, y, acc:F1)
head(gathered)


gathered
##threshold to maximise accuracy = 0.6


##########################################################################
# SHOW RESULTS IN A GRAPH



library(ggplot2)
gathered %>%
  ggplot(aes(x = threshold, y = y, color = x)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  labs(y = "measures",
       color = "OB: event\nNOB: nonevent")

pred <- ifelse(test2$glm_IMP > 0.5, "c1", "c0")
table(actual=test2$obesita, pred)

pred <- ifelse(test2$glm_IMP > 0.1, "c1", "c0")
table(actual=test2$obesita, pred)

pred <- ifelse(test2$glm_IMP > 0.8, "c1", "c0")
table(actual=test2$obesita, pred)

pred <- ifelse(test2$glm_IMP > 0.05, "c1", "c0")
table(actual=test2$obesita, pred)















