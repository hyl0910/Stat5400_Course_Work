---
  title: "HW8"
author: "HeiYee Lau"
format: html
editor: visual
---
  
  Problem 1. The dataset caeserian.csv contains information about Caesarian section results of 80 pregnant women with the most important characteristics of delivery problems in the medical field. It gives information on Age,Delivery number (1, 2, 3,4),Delivery time classified as (0, 1, 2) with 0 = timely, 1 = premature, 2 = latecomer,Blood Pressure (0 = low, 1 = normal, 2 = high),Heart Problem (0 = apt, 1 = inept), Caesarian(0 = No, 1 = Yes) - response variable.

Answer the following questions.

a.  Use the predictor Age in a binary logit model for the incidence of Caesarian. Interpret the results.

```{r}
caesarian<-read.csv("caesarian.csv", header=TRUE)
caesarian$delivery_number<- as.factor (caesarian$delivery_number)
caesarian$delivery_time <- as.factor(caesarian$delivery_time)
caesarian$blood_pressure <-as.factor(caesarian$blood_pressure)
caesarian$heart_problem <- as.factor(caesarian$heart_problem)
caesarian$Caesarian <- as.factor(caesarian$Caesarian)
```

```{r}
set.seed(123457)
train.prop <- 0.80
strats <- caesarian$Caesarian
rr <- split(1:length(strats), strats)
idx <- sort(as.numeric(unlist(sapply(rr, 
                                     function(x) sample(x, length(x)*train.prop)))))
caesarian.train <- caesarian[idx, ]
caesarian.test <- caesarian[-idx, ]
```

```{r}
age_pred <- glm(Caesarian ~ age, data = caesarian.train, family = binomial(link="logit"))
summary(age_pred)
```

The intercept represents the log-odds of Caesaeian when Age is zero. The coefficient for Age is 0.03445, with a standard error of 0.04994 This means that for each one-unit increase in Age, the log-odds of having a Caesarian increases by 0.03445 However, the p-value associated with Age is 0.490, which is greater than the significant level (0.05). This suggests that Age may not have a statistically significant predictor of Caesarian in this model.

The output also shows the null deviance of 86.046 on 62 degree of freedom which indicated how well the response variable can be predicted by a model with only an intercept term, and the residual deviance of 85.566 on 61 degree of freedom, which is show how well the response variable can be predicted by a model with the intercept and the selected predictor variables. Since the residual deviance is smaller than the null deviance, it suggests that the model with the Predictor variable "Age" is better fit than the null model. The AIC is 89.566


```{r}
null.logit<-glm(Caesarian~1, data=caesarian.train, family = binomial(link="logit"))
summary(null.logit)
```

```{r}
full.logit <- glm(Caesarian ~ . , data = caesarian.train, family = "binomial"(link="logit"))
summary(full.logit)
```

b.  Among the remaining four predictors, which along with Age best explains the incidence of a Caeserian section? What is the reduction in deviance by including this additional predictor?
  
  ```{r}
both.logit<-step(null.logit, list(lower=formula(null.logit),upper=formula(full.logit)),direction="both", trace=0, data=caesarian.train)
formula(both.logit)
```

In the following code, we can find out the best predictor that model with the smallest residual deviance. We using for-loop to fit the predictors into different model and find the difference between the residual deviance reduction.

```{r}
deviance_reductions <- numeric()
predictors <- c("delivery_number", "delivery_time", "blood_pressure", "heart_problem")

for (predictor in predictors) {
  model <- glm(Caesarian ~ age + as.factor(get(predictor)), data = caesarian.train, family = "binomial" (link = "logit"))
  deviance_reductions <- c(deviance_reductions, deviance(age_pred) - deviance(model))
}

best_predictor <- predictors[which.max(deviance_reductions)]
best_deviance_reduction <- max(deviance_reductions)
cat("The variable that along with Age best explains the incidence of a Caesarian section is",best_predictor,"\n")
print(best_deviance_reduction)
```
```{r}
age_pred <- glm(Caesarian ~ age+heart_problem, data = caesarian.train, family = "binomial"(link="logit"))
summary(age_pred)
```

In the above coding, we first initializes an empty numeric vector to store the reduction in deviance for each predictor and create a vector that contains the names of the remain four predictors we want to assess. The for-loop is to evaluate each predictor in the context of a logistic regression model. We have calculate the reduction in deviance by subtracting the deviance of the model with only Age (age_pred) from the deviance of the model that includes Age and the other predictor. After evaluating all predictors, uses the 'which.max' function to identify the index of the maximum reduction in deviance, and extracts the corresponding predictor name from the vector.

From the result, we can see that the predictor "heart_problem" with predictor "Age" have the best explains the incidence of a Caesarian section. The reduction in deviance by including predictor "heart_problem" is 10.15274 (85.566 - 75.413).

c.  Fit and interpret a binary logit regression for the incidence of Caeserian section using all five predictors.

```{r}
full.logit <- glm(Caesarian ~ . , data = caesarian.train, family = "binomial"(link="logit"))
summary(full.logit)
```

From the full fitted model, the intercept represents the log-odds of Caesarian when all predictor variables are zero. For each one-unit increase in Age, the log-odds of having a Caesarian decrease by 0.02548, but the p-value for Age is not significant since the p-value of Age is 0.7003 which is larger than the significant level (0.05), suggesting that Age might not be a significant predictor in this model.

The other predictors also have coefficients and p-values, all of them have a larger p-value than the significant level except predictor delivery_time2, blood_pressure1 and heart_problem1, have the p-value which smaller than the significant level (0.05) and indicates that they are statistically significant impact to Caesarian.

The null deviance of model is 86.046 on 62 d.f. and residual deviance of model is 62.231 on 53 d.f..The deviance values represent goodness-of-fit statistics. The residual deviance is smaller than the null deviance, which indicate that the model with five predictors is better fit than the null model. And the AIC is 82.231

d.  Construct the fitted values from the model in (c). Assume that the fit corresponds to Caesarian = 1 if the estimated logit exceeds 0.65, else assume Caesarian = 0. Then, construct a confusion (or misclassification) table and discuss what it says about the model fit.

```{r}
pred.full <- predict(full.logit, newdata = caesarian.test, type = "response")
(table.full<-table(pred.full>0.65, caesarian.test$Caesarian))
```

```{r}
library(caret)
f<-ifelse(pred.full>0.65,1,0)
(cm.full<-confusionMatrix(reference=as.factor(caesarian.test$Caesarian),data=as.factor(f), mode="everything"))
```

The confusion table in above is used to assess the performance of the logistic regression model. It shows that how well the model's predictions match the actual outcome.

In the confusion matrix, we can see that there are 5 True Negative that correctly predicted Negative. 5 True Positive that correctly predicted Positive. 5 False Negative that incorrectly predicted Negative and 2 False Positive that incorrectly predicted Positive.

The accuracy of full model is 58.82%. Sensitivity and Specificity of the model are 71.43% and 50% respectively.This means the model correctly identifies Caesarian cases 71.43% of the time and the model correctly identifies non-Caesarian cases 50% of the time. The F1 score is 58.82% and Recall is 71.43%. We can see that the Sensitivity and recall are high and prove that this model can make a better prediction and accuracy result.


**Problem 2.**

```{r}
levee.data<-read.csv("leveefailure.csv")
cols_with_na <- colnames(levee.data)[colSums(is.na(levee.data)) > 0]
# Drop columns with missing values
levee.data <- levee.data[, !colnames(levee.data) %in% cols_with_na]
levee.data$Failure<-as.factor(levee.data$Failure)
levee.data$Year<-as.factor(levee.data$Year)
levee.data$Sediments<-as.factor(levee.data$Sediments)
levee.data$Borrowpit<-as.factor(levee.data$Borrowpit)
levee.data$Meander<-as.factor(levee.data$Meander)
levee.data$Landcover<-as.factor(levee.data$Landcover)
levee.data$Revetment<-as.factor(levee.data$Revetment)
```

In the data, since we found out that there is a NaN column, we would drop this column as data cleaning and better analysis the data.

```{r}
table(levee.data$Failure)
```

We can see that the distribution of Failure, which there are 41 observations that Failure = 1 and 41 observations that Failure = 0.

We would train the data into 80-20 split to train data and test data.

```{r}
set.seed(123457)
train.prop <- 0.80
strats <- levee.data$Failure
rr <- split(1:length(strats), strats)
idx <- sort(as.numeric(unlist(sapply(rr, 
        function(x) sample(x, length(x)*train.prop)))))
levee.train <- levee.data[idx, ]
levee.test <- levee.data[-idx, ]
```

```{r}
summary(levee.train$Failure)/nrow(levee.train)
summary(levee.train$Failure)/nrow(levee.train)
```

We can see that the proportions of the two level of the response Failure are the same in the train, test and the entire data since all of them is 0.5.

```{r}
levee_null.logit <- glm(Failure~1, data = levee.train, family = binomial(link = "logit"))
summary(levee_null.logit)
```

In the above is the null model. We an see that the coefficient of intercept is 1.665e-16 and the p-value is 1 which is much larger than the significant level (0.05) and suggests the intercept is not significant.

The null deviance and residual deviance is 88.723 on 63 d.f. The AIC is 90.723.

```{r}
full.logit <- glm(Failure ~ . ,data = levee.train, 
family = binomial(link = "logit"))
summary(full.logit)
```

In the above table, we have fit a binary regression witha logit link to the training data in levee.train. It is the full model that the response Failure and the model includes all the predictors.

We can see that there are several variables are statisticlly significant which are "loan", "contactunknown", "month", "duration", "campaign" and "poutcomesuccess", which are have smaller p-value than smaller than the significant level (0.05). Moreover, the variable "housing" is marginally significant which have the p-value (0.0099561) suggesting that having a housing loan may have a modest effect on the log-odds of "Failure".

Since the residual deviance (34.336) is smaller than the null deviance(88.723), it suggests that the model with all the predictors have a better fit than a null model. The AIC is 72.336 which can be used for model comparison.

Then, we will do the variable selection that using the option direction = "both" in the step() function, which implies both forward and backward selection.

```{r}
levee_both.logit <- step(full.logit, list(lower=formula(levee_null.logit),upper=formula(full.logit)),direction="both",trace=0, data = levee.train)
formula(levee_both.logit)
```

```{r}
summary(levee_both.logit)
```

Stepwise selection for the leveefailure data uses the above code and selects a model whose coefficients are shown above. We can see that this model selects a model with 6 out of 13 predictors.

The intercept represents the log-odds of the response variable "Failure" when all predictor variables are set to zero. And the intercept is not statisitcally significant. Moreover, Sinuousity, ChannelWidth and Dredging are also the significant variable in the model since the p-value is smaller than the significant level (0.05).However, Landcover, Year and Borrowpit have the larger p-value than the significant level (0,05), which is suggest that they are not significant. It has been selected might because they might highly correlated with one or more of the other predictor variables in the model, or since the smaller dataset that the model may not have enough statistical power to detect smaller effects, making some variables appear less significant.

Overall, the model's goodness of fit is evaluated using the null deviance, residual deviance and AIC. Since the residual deviance is smaller than the null deviance,although the residual deviance of both logit model is higher than the full model, but it has lower AIC value (64.575) for the model with predictor suggests that both of the model are better fit and we can check the accuracy, sensitivity and specifity to decide which model is the best model to make prediction.

```{r}
pred.both <- predict(levee_both.logit, newdata = levee.test, type="response")
pred.full <- predict(full.logit, newdata = levee.test, type="response")
```

```{r}
library(caret)
b_both<-ifelse(pred.both>0.5, 1, 0)
cm.both <- confusionMatrix(reference=as.factor(levee.test$Failure),data=as.factor(b_both), mode="everything")
cm.both
```
For the confusion matrix above, there are 5 true negative and 5 true positive, that correctly determine 10 cases. However, there are 4 instances that determine to be positive incorrectly which is the false positive and 4 false negative, that have 8 cases that is incorrectly predicted. The accuracy of the both model is 55.56%. Sensitivity, Specificity and F1 score of this model are also 55.56%.

```{r}
b_full<-ifelse(pred.full>0.5, 1, 0)
cm.full <- confusionMatrix(reference=as.factor(levee.test$Failure),data=as.factor(b_full), mode="everything")
cm.full
```

For the confusion matrix above, there are 5 true negative and 6 true positive. There are 11 instances that determine to be positive incorrectly which is the 3 false positive and 4 false negative that have 7 cases are incorrectly predicted. The accuracy of full model is 61.11%. Sensitivity and Specificity are 55.56% and 66.67% respectively. The F1 score is 58.82%.

On the other hand, after calculate the sensitivity, specificity and F1 score, we can see that full model is better than the both model because full model has a higher accuracy, specificity and F1 score, therefore, we can conclude that based on the these result, it suggests that full model can give a more accuracy and better fit prediction result.


```{r}
library(pROC)
```

```{r}
roc.both <- roc(levee.test$Failure, pred.both, levels=c(1,0))
```

```{r}
roc.both.1<-roc(levee.test$Failure, pred.both,levels=c(1,0),plot=T,print.AUC=T)
auc(levee.test$Failure, pred.both)
```

```{r}
roc.full <- roc(levee.test$Failure, pred.full, levels=c(1,0))
```

```{r}
roc.full.1<-roc(levee.test$Failure, pred.both,levels=c(1,0),plot=T,print.AUC=T)
auc(levee.test$Failure, pred.full)
```

We can see that the two model give similar performance. The AUC of the both model is 0.6296 and that of the full model is 0.6667 Since the AUC is between 0.5 to 0.7, it suggests that two models has limited discriminatory power and its ability to correctly classify the classes is relatively weak.

```{r}
library(caret)
b <- ifelse(pred.both > 0.5,1,0)
cm.both <- confusionMatrix(reference=as.factor(levee.test$Failure),data=as.factor(b), mode="everything")
f <- ifelse(pred.full > 0.5,1,0)
cm.full <- confusionMatrix(reference=as.factor(levee.test$Failure), data=as.factor(f), mode="everything")
```

In the following, we would also like to predict the train data and assess accuracy under both model

```{r}
## Predict train data using both.logit and full.logit
pred.tr.both <- predict(levee_both.logit, newdata = levee.train, type="response")
pred.tr.full <- predict(full.logit, newdata = levee.train, type="response")
# Accuracy of both.logit and full.logit
# Confusion matrix
(table.tr.both <- table(pred.tr.both > 0.5, levee.train$Failure))
```

For the confusion matrix of training data both model, there are 29 and 27 cases which are True Negative and True Positive. 5 and 3 cases for the False Positive and False Negative cases.

```{r}
(table.tr.full <- table(pred.tr.full > 0.5, levee.train$Failure))
```

For the confusion matrix of training data both model, there are 28 and 27 cases which are True Negative and True Positive. 5 and 4 cases for the False Positive and False Negative cases.

```{r}
# Accuracy
(accuracy.tr.both <- round((sum(diag(table.tr.both))/sum(table.tr.both))*100,2)) 
```

```{r}
(accuracy.tr.full <- round((sum(diag(table.tr.full))/sum(table.tr.full))*100,2))
```

We an see that the accuracy of both model is 87.5% and that of full model is 85.94%, since the accuracy of full model is higher, it suggests that the both model is better fit and make better accuracy prediction in full model.

```{r}
# AUC 
roc.tr.both <- roc(levee.train$Failure, pred.tr.both, levels=c(1,0))
```

```{r}
auc(levee.train$Failure, pred.tr.both)
```

```{r}
roc.tr.full <- roc(levee.train$Failure, pred.tr.full, levels=c(1,0))
```

```{r}
auc(levee.train$Failure, pred.tr.full)
```

Since in the above, we have suggest that there are variable that suggest multicollinearity, since after variable selection, the variable do not show statistically significant to the model, so that we can test the VIF of the variables in the selected model (full logit model).

```{r}
library(car)
car::vif(full.logit)
```
From the VIF result, we can see that Year exceed 70 and Borrowpit even exceed 4000, which suggesting multicollinearity. We would drop the predictor from the model.

```{r}
levee.train.1<- subset(levee.train, select=-c(Year))
mod.dropBorrowpit<-glm(Failure~., data=levee.train.1, family = binomial(link="logit"))
summary(mod.dropBorrowpit)
```
```{r}
car::vif(mod.dropBorrowpit)
```
Now, we can see that, after dropping the variable Year, the VIF of all variable is much smaller , especially Borrowpit is decrease from 4127.379172 to 2.039004 

In addition, we will do the variable selection to choose the predictors that better explaining the response Failure.

```{r}
reduce_logit <- step(mod.dropBorrowpit,direction="both",trace=0, data =levee.train.1)
formula(reduce_logit)
```

```{r}
summary(reduce_logit)
```

After variable selection, Meander, ChannelWidth, Floodwaywidth , ConstrictionFactor, Landcover, Dredging and Revetment have been selected in the model. We can see that the residual deviance (53.907) is smaller than the null deviance (88.723), which indicated that the model is better fit compared with the null model. The AIC is 77.907, which is better than the model that dropped variable "Year" (84.094). Therefore, the model that after variable selection is better than the model after dropped "Year".

```{r}
pred.drop <- predict(mod.dropBorrowpit, newdata = levee.test, type="response")
pred.reduce <- predict(reduce_logit, newdata = levee.test, type="response")
```
```{r}
b_drop<-ifelse(pred.drop>0.5, 1, 0)
cm.drop <- confusionMatrix(reference=as.factor(levee.test$Failure),data=as.factor(b_drop), mode="everything")
cm.drop
```

In the confusion matrix of drop "Year" model, there are 4 Ture Negative and 7 True Positive prediction. On the other hand, there are 5 False Positive and 2 False Negative in the prediction. The accuracy is 61.11%. Sensitivity and Specificity are 44.44% and 77.78% respectively. The F1 score is 53.33%.

```{r}
b_reduce<-ifelse(pred.reduce>0.5, 1, 0)
cm.reduce <- confusionMatrix(reference=as.factor(levee.test$Failure),data=as.factor(b_reduce), mode="everything")
cm.reduce
```
Same with the confusion matrix of dropped "Year" model, the model that after variable selection, there are 4 Ture Negative and 7 True Positive prediction, 5 False Positive and 2 False Negative in the prediction. Same accuracy, sensitivity, speificity and F1 score.

Compared with all the model, which are full model, both model, null model, drop model and reduce model, we can see that overall the full model have the highest sensitivity and F1 score score, although the specificity is not the highest, we focus on the accuracy of correct prediction and accuracy of the result, we suggested that the full model is the best model in this case. The predictors in the best model are "Year", "Sediments", "Borrowpit", "Meander", "ChannelWidth", "Flooddwaywidth", "ConstricitionFactor", "Landcover", "VegWidth", "Sinuosity", "Dredging" and "Revetment"