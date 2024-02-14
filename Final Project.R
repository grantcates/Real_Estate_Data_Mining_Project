library(regclass)
library(corrplot)
library(tidyr)
library(knitr)
library(regclass)
library(robustbase)
library(MASS)
library(caret)
library(plotrix)
library(glmnet)
library("pROC")
library(caret)
library(ggpubr)
library(ggplot2)
library(plotly)
library(ggthemes)

# Final Project
################################################################################
################################################################################
################################################################################
# Read the file into the lab
DATA <- read.csv("data.csv")

which(is.na(DATA))
DATA <- na.omit(DATA)

#took out all the missingness values
which(is.na(DATA))
DATA <- na.omit(DATA)

#Turning the data in the data set all to numeric in TRAIN dataset
M <- factor(DATA$TAX)
DATA$TAX<-as.numeric(M)

MT <- factor(DATA$CHAS)
DATA$CHAS<-as.numeric(MT)

G <- factor(DATA$RAD)
DATA$RAD<-as.numeric(G)


# must create the Train and the HOLDOUT for data
dim(DATA)
# 14 columns
set.seed(474)
train.rows <-sample(1:nrow(DATA), 0.6*nrow(DATA))
TRAIN <- DATA[train.rows, ]
HOLDOUT <- DATA[-train.rows, ]
head(DATA[,1:8])



################################################################################
# This is where the fun begins (MEDV)
TC <- trainControl(method="cv",
                   number=5)
set.seed(474)
Linear_Regression <- train(MEDV~.,
                           data=TRAIN,
                           method="glm",
                           trControl= TC, 
                           preProc=c("center", "scale") )

Linear_Regression$results

################################################################################
Model <- expand.grid(alpha = seq(0,1,.05),
                     lambda = 10^seq(-4,-1,.5))
set.seed(474)
RR <- train(MEDV~.,
            data=TRAIN,
            method='glmnet',
            Model=Model, 
            trControl=TC, 
            preProc = c("center", "scale"))


#the regression plotted
plot(RR)

# best parameters
RR$bestTune 

RR$results[rownames(RR$bestTune),]

#Tree Based Model
rpartGrid <- expand.grid(cp=10^seq(-3,-1,length=40)) #set up grid of CPs to use
fitControl <- trainControl(method="cv",
                   number=5)
TREE <- train(MEDV~.,
              data = DATA,
              method= "rpart",
              trControl = fitControl,
              tuneGrid = rpartGrid,
              preProc = c("center","scale"))

plot(TREE)
TREE$bestTune

################################################################################

#Visualization of the Predicted Home Owner Value
pred <- predict(Linear_Regression, HOLDOUT)
pred <- data.frame(pred = pred, MEDV = HOLDOUT$MEDV)

ggplot(data = pred, aes(x = pred, y = MEDV)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Prediction of Median Value of Homes")
###############################################################################
###############################################################################
TC <- trainControl(method="cv",
                   number=5)
gbmGrid <- expand.grid(n.trees=c(200,500),
                       interaction.depth= 1:3,
                       shrinkage= c(.05,.1),
                       n.minobsinnode=c(2,5,10))
GLC <- train(MEDV~.,
             data=TRAIN,
             method="gbm",
             trControl=TC,
             tuneGrid=gbmGrid,
             verbose=F,
             preProc=c("center","scale"))
plot(GLC)

###############################################################################
###############################################################################
#scatter plot

library(ggpubr)
ggscatter(TRAIN,
          x = "MEDV",
          y = "AGE",
          ellipse = T) +
  ggthemes::theme_economist_white()+
  ggtitle("Scatterplot of MEDV and AGE")

###############################################################################
df <-data.frame(TRAIN)
str(df)
summary(df)
colSums(is.na(df))
GC <- lapply(df,median,na.rm=T)
GC
complete_df <- replace_na(df,GC)
WOOF <- subset(complete_df,select= -c(RAD,DIS))
sample <- sample(c(T,F),nrow(WOOF),prob=c(0.7,0.3),replace = T)
BARK <- WOOF[sample,]
GRR <- WOOF[!sample,]

#Cross Vaildate or someting like that
TC <- trainControl(method = 'cv',number = 5)

goodcaretvisual <- train(MEDV~.,
                     data= BARK,
                     trControl= TC,
                     method='lm',
                     na.action=na.pass)

goodcaretvisual$resample

summary(goodcaretvisual$finalModel)

predict <- predict(goodcaretvisual$finalModel, newdata=GRR)

plot(goodcaretvisual$finalModel)

ggplot(aes(x=predict, y= MEDV),
       data = GRR)+
  geom_point()+
  labs(title = "Predicited vs Actual MEDV")+
  ggtitle("Predicited vs Actual MEDV")+
  geom_abline(intercept = 0,
              slope = 1,
              color='purple',
              size=2)

ggplot(aes(x=predict, y= MEDV),
       data = GRR)+
  geom_point()+
  ggtitle("Predicited vs Actual MEDV")+
  geom_abline(intercept = 0,
              slope = 1,
              color='purple',
              size=2)

