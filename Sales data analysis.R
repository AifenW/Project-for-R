## Data for RFM analysis
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(ggplot2)

library(readr)
SalesData<-read_csv("~/RProject/Customer_Segmentation_Using_RFM_Analysis/sample-sales-data/sales_data_sample.csv ")

## SALES prediction using Multiple Linear Regression

## Mltiple linear regression is used to explain the relationship between one continuous dependent variabel and two ot more independent variables.

install.packages("caret")

library(caret)

## subset SalesData

sales_s<-SalesData[,c(2,3,4,5,11,21,25)]

set.seed(60)

nrows<-sample(nrow(sales_s))

sales_s<-sales_s[nrows,]

split<-round(nrow(sales_s)*0.6)

sales_train<-sales_s[1:split,]

sales_test<-sales_s[(split+1):nrow(sales_s),]

## fit the multiple linear regression model

sales_lm<-lm(log(SALES)~., data=sales_train)


## Evaluating the linear model

summary(sales_lm)  

## take a look at the summary, 
## the p-value of QUANTITYORDERED, PRICEEACH,ORDERDATE,some of PRODUCTLINE,MSRP, some COUNTRY and  DEALSIZE is 
## much less than 0.05. They are significant in explaining the SALES. We are confident to include these variables 
## into the model. So we can also add it into the model. 
## The Adjusted R-squared is 0.93, which indicates 93% of the variation in SALES can be explained by these variables.
## 
## When the residual standard error is exactly 0 then the model fits the data perfectly (likely due to overfitting)
## Degrees of freedom is given by the difference between the number of observations in my sample and the number of variables in my model.
## Degrees of freedom are important for finding critical cutoff values for inferential statistical tests. Depending on the type of the analysis you run, degrees of freedom typically (but not always) relate the size of the sample.
## the small residual standard error (0.1298) tells that the model fits the data well.
## ????????????????????????????????????,???????????????????????? P ???????????? 0.05,???????????????????????????????????? F ????????? P ??????????????? 0.05,??????????????????????????????,???????????????????????????????????????????????????,???????????????????????????????????????????????????????????????????????????
## Caculate RMSE on our train and test data

sales_train_lm_pred<-predict(sales_lm, data=sales_train)

sales_test_lm_pred<-predict(sales_lm, data=sales_test)

sqrt(mean(sales_train_lm_pred-sales_train$SALES)^2)
sqrt(mean(sales_test_lm_pred-sales_test$SALES)^2)

## 3606.01961349285
## 3422.57332069474

## Plot the residuals vs the fitted values

library(broom)   # tidy the model with broom

sales_fitted<-augment(sales_lm)  # build the augmented dataframe

sales_fitted %>%
  ggplot(aes(x=.fitted,y=.resid))+
  geom_point(alpha=0.6)+
  geom_hline(yintercept=0)

sales_fitted %>%
  ggplot(aes(x=.fitted,y=log.SALES.))+
  geom_point(alpha=0.6)+
  geom_smooth(aes(y=.fitted), color="red")

## check the heteroscedasticity

install.packages("lmtest")
library(lmtest)
bptest(sales_lm)
 
  #  bptest (Breusch-Pagan) have a p-value less that a significance level of 0.05, therefore we can reject the null hypothesis that the variance of the residuals is constant and infer that heteroscedasticity is indeed present, thereby confirming our graphical inference.
   # p-value is very small, there is heteroscedasticity

## Rectify heteroscedasticity
 # method 1: weighted regression
lm.test<-lm(log(resid(sales_lm)^2)~.,data=sales_train)
lm.test2<-lm(log(SALES)~., weights=1/exp(fitted(lm.test)), data=sales_train)
summary(lm.test2)

test2_fitted<-augment(lm.test2)  # build the augmented dataframe

test2_fitted %>%
  ggplot(aes(x=.fitted,y=.resid))+
  geom_point(alpha=0.6)+
  geom_hline(yintercept=0)

test2_fitted %>%
  ggplot(aes(x=.fitted,y=log.SALES.))+
  geom_point(alpha=0.6)+
  geom_smooth(aes(y=.fitted), color="red")

library(lmtest)
bptest(lm.test2)

par(mfrow=c(2,2))
plot(lm.test2)

 # there is no change in variance

glm.test<-glm(log(SALES)~., weights=1/sales_fitted$.resid^2, data=sales_train)

summary(glm.test)

glm_fitted<-augment(glm.test)  # build the augmented dataframe

glm_fitted %>%
  ggplot(aes(x=.fitted,y=.resid))+
  geom_point(alpha=0.6)+
  geom_hline(yintercept=0)

glm_fitted %>%
  ggplot(aes(x=.fitted,y=log.SALES.))+
  geom_point(alpha=0.6)+
  geom_smooth(aes(y=.fitted), color="red")

library(lmtest)
bptest(glm_fitted)

# library(nlme)
# sales_gls<-gls(log(SALES)~., weights=varPower(), data=sales_train)
#summary(sales_gls)
#plot(sales_gls)



## Visualize the regression model with one explanatory variable 

ggplot(augment(sales_lm),aes(x=QUANTITYORDERED, y=log.SALES.))+
  geom_point()+
  geom_smooth(aes(y=.fitted), color="red")

ggplot(augment(sales_lm),aes(x=MSRP, y=log.SALES.))+
  geom_point()+
  geom_smooth(aes(y=.fitted), color="red")
