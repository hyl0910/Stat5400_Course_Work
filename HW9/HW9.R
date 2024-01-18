---
  title: "HW9"
author: "Hei Yee Lau"
format: html
editor: visual
---
  
  Consider the Kyphosis dataset in the R package rpart. The dataset has 81 rows and 4 columns representing data on children who have had corrective spinal surgery.
The goal is to model the binary response Kyphosis, which indicates if a kyphosis (a type of deformation) was present or absent after the operation.

(a). Use set.seed(123457) to do a 90-10 train-test split of the dataset, making sure that the proportion of the binary response Kyphosis = present is about the same in the train, test, and full data sets. Why is checking this important?
  
  ```{r}
library(rpart)
data(kyphosis)
str(kyphosis)
```
```{r}
table(kyphosis$Kyphosis)
```

We can see that there are 64 absents and 17 presents in the dataset.

```{r}
unique(kyphosis$Kyphosis)
```
From the above code, we can see that there are only two level in the Kyphosis variable.

```{r}
kyphosis$Kyphosis <- as.factor(ifelse(kyphosis$Kyphosis == "present", 1, 0))
head(kyphosis,5)
```

We would like to convert the Reponse Kyphosis to 0 or 1.

In the following, I have set the seed to 2345678 to train the model and do the 90-10 traing test data split.

```{r}
set.seed(2345678)
train.prop <- 0.90
strats <- kyphosis$Kyphosis
rr <- split(1:length(strats), strats)
idx <- sort(as.numeric(unlist(sapply(rr, 
                                     function(x) sample(x, length(x)*train.prop)))))
kyphosis.train <- kyphosis[idx, ]
kyphosis.test <- kyphosis[-idx, ]
```

We can see the proportions of the two levels of Kyphosis is similar from the training and entire dataset are similar, but the proportion of test data is sightly lower than others in the absent level but higher in the present level. But overall, the proportion of present level is about the same in train, test and entire datasets.

```{r}
summary(kyphosis.train$Kyphosis)/nrow(kyphosis.train)
```
```{r}
summary(kyphosis.test$Kyphosis)/nrow(kyphosis.test)
```

```{r}
summary(kyphosis$Kyphosis)/nrow(kyphosis)
```

It is important to check the proportion of the binary response "Kyphosis = present" in the train, test and full dataset because we need to ensure that the proportion of the target class is similar in both the train and test datasets is crucial to maintain the representativeness of the data. Otherwise, if one dataset has a significantly different class distribution, it can lean to biased model performance evaluations. 

A balanced distribution helps the model learn and generalize effectively. Moreover, having a balanced class distribution ensures that the evaluation metrics like accuracy, sensitivity and specificity are not biased by class imbalance. It can ensure that our model is trained and evaluated in a way that reflects its real-world performance. 

(b). Fit a logit regression model to Kyphosis using all available predictors. Also, fit the null model. Compare the AIC values between the two models. Which model does AIC prefer?
  ```{r}
full.logit<- glm(Kyphosis~., data = kyphosis.train, family = binomial(link="logit"))
summary(full.logit)
```

In the full model, we have three predictors which are Age, Number and Start. We can see that the coefficient of intercept is -3.670245. When all other predictor variables are zero, the log-od of Kyphosis being present is approximately -3.670245. The coefficient of Age is 0.019701 which mean for every one-unit increase in the Age, the log-odds of having kyphosis increases by approximately 0.019701 Furthermore, we can see that all the variables are statistically significant to predict Kyphosis, since the p-value of all variables are smaller than the significant level (0.05)

The dispersion parameter for binomial family is 1 which means the data's variance matches the model's predictions. The null deviance is 73.691 and residual deviance is 48.724 Since the residual deviance is smaller than the null deviance, which suggests the model with predictors is better fit than the null model. The AIC is 56.724.

```{r}
null.logit<-glm(Kyphosis~1, data=kyphosis.train, family = binomial(link = "logit"))
summary(null.logit)
```

In the null model, the coefficient of intercept is -1.3350, which mean when there is no predictor variable is considered, the log-odds is approximately -1.3350. Since the p-value is super small in the null model, which smaller than the significant level (0.05), it suggests that the intercept is highly significant in the model. The null deviance and residual deviance is the same, 73.691 and the AIC is 75.691.

The AIC value for the full model with all predictors is 56.724. The AIC value for the null model is 75.691. Since a lower AIC value indicates a better trade-off between goodness of fit and model complexity. In the case, the full regression model with all predictors has a lower AIC (56.724) compared to the null model (75.691). Therefore, AIC prefers the model with all available predictors. This suggests that the model with predictors provides a better fit to the data compared to a model with just a constant term. Full model is preferred.


(c). How will you use the roc curve and AUC in interpreting the full and null models? Compute using R code, and then explain.

```{r}
library(pROC)
# Test data accuracy
pred.full <- predict(full.logit, newdata = kyphosis.test, type = "response")
pred.null <- predict(null.logit, newdata = kyphosis.test, type = "response")

# Compute ROC and AUC for the full model
roc.full <- roc(kyphosis.test$Kyphosis, pred.full, levels = c(1, 0))

roc.full.1<-roc(kyphosis.test$Kyphosis, pred.full,levels=c(1,0),plot=T,print.AUC=T)

auc(kyphosis.test$Kyphosis, pred.full)
```

In the above coding and result, the setting direction is "controls > cases" indicates that higher predicted values are associated with the control group (0), and lower predicted values are associated with the case group(1). An AUC of 0.7143 suggests that our model's ability to distinguish between the two classes is fairly good.

```{r}
# Compute ROC and AUC for the full model
roc.null <- roc(kyphosis.test$Kyphosis, pred.null, levels = c(1, 0))

roc.null.1<-roc(kyphosis.test$Kyphosis, pred.null,levels=c(1,0),plot=T,print.AUC=T)

auc(kyphosis.test$Kyphosis, pred.null)
```

The AUC value of 0.5 for the null model suggests that the model is not able to discriminate between the Kyphosis present and Kyphosis absent. It might indicates random chance or no discriminatory power.

After that, we can also check the accuracy of train data.

```{r}
# Train data accuracy
pred.full <- predict(full.logit, newdata = kyphosis.train, type = "response")
pred.null <- predict(null.logit, newdata = kyphosis.train, type = "response")

# Compute ROC and AUC for the full and null models
roc.full <- roc(kyphosis.train$Kyphosis, pred.full, levels = c(1, 0))
roc.full.1<-roc(kyphosis.train$Kyphosis, pred.full,levels=c(1,0),plot=T,print.AUC=T)
auc.full <- auc(roc.full)
auc.full
```

The AUC value of 0.8713 for the full model on the training data indicates a moderate level of discriminatory power. It suggests that the full logistic regression model is able to distinguish between Kyphosis present and absent in the training data.

```{r}
# Null model for train data
roc.null <- roc(kyphosis.train$Kyphosis, pred.null, levels = c(1, 0))
roc.null.1<-roc(kyphosis.train$Kyphosis, pred.null,levels=c(1,0),plot=T,print.AUC=T)
auc.null <- auc(roc.null)
auc.null
```

Same with the result of test data in null model, the AUC is 0.5 which suggests that the model is not able to discriminate between the Kyphosis present and Kyphosis absent. We can see that the 

(d). Pull out and interpret the effect of Age on

(i). the logit of the probability of presence of Kyphosis, and

In the full model on above, the coefficient for "Age" is estimated as 0.019701 For each one-unit increase in Age, the log-odd of Kyphosis being present increase by 0.019701 This means that as Age increases, the log-odds of Kyphosis being present also increase. Older children have a higher log-odds of having Kyphosis, while younger children have a lower log-odds.

The p-value for "Age" is 0.02147, which is smaller than the significant level (0.05). This suggests that the coefficient for "Age" is statistically significant, meaning that there is strong evidence that "Age" has a significant effect on the log-odds of Kyphosis being present.


(ii). the probability of presence of Kyphosis.

From the full model, to understand the effect on the probability of Kyphosis presence, we can use the logistic function (sigmoid function) to covert the log-odds to probabilities, which show the following:

π(Age) = 1 / (1 + exp(-Coefficient * Age))

Coefficient: 0.019701
π(Age) = 1 / (1 + exp(-0.019701 * Age))

Assume Age = 10,Probability(Kyphosis = present): 
= 1 / (1 + exp(-0.019701 * 10))
=1/(1+exp(-0.19701))
=1/(1+0.82052) = 0.5497

Therefore, take an example, if the age is 10, the estimated probability of Kyphosis presence is approximately 54.97%.

With a positive coefficient of Age, the probability of Kyphosis presence increases as Age increases. This means that older children are more likely to have Kyphosis after operation compared to younger children. Conversely, for a one-unit decrease in Age, the probability of Kyphosis presence decreases. Younger children are less likely to have Kyphosis.


(e). Carry out a test of hypothesis to see whether the regression coefficient β for Age is significantly different than zero. Write H0 and H1 both in notation and in words, write the formula for the z test statistic, and carry out the test. Hint. Keep in mind that unlike the MLR model, we do not have t or F-tests in GLIM, instead we have z (or Wald) and chi-square tests.

Null Hypothesis (H0): β(Age)=0. There is no significant relationship between Age and the log-odds of Kyphosis being present.

Alternative Hypothesis (H1) β(Age)!≠0. There is a significant relationship between Age and the log-odds of Kyphosis being present

Formula for the z test statistic: Z= β/SE(β)

```{r}
summary_result <- summary(full.logit)
z_test_age <- summary_result$coefficients["Age", c("z value", "Pr(>|z|)")]
cat("Z Value:", z_test_age["z value"], "\n")
cat("P-Value:", z_test_age["Pr(>|z|)"], "\n")
```
The z-test for the "Age" coefficient resulted in a Z value of approximately 2.299536the p-value is 0.02147451 Since the p-value is smaller than the significant level (0.05), we have enough evidence to reject the null hypothesis and accept the alternative hypothesis.


```{r}
confint(full.logit)
```

The 95% confidence interval for the "Age" coefficient, which ranges from 0.00464 to 0.0389, supports the significant of the "Age" coefficient. The interval does not include zero, which further indicates that the coefficient is statistically significant. 

Overall, based on both Z-Test and the confidence interval, there is sufficient evidence to reject the null hypothesis. Therefore, we have enough evidence to conclude that the regression coefficient for Age is significantly difference from zero. It suggests that Age have a significant effect on Kyphosis presence in the model.

(f). Fit a reduced model removing the variables Number and Start from the full model. How will you use the test data to compare which is a better model: the full model or the reduced model?

```{r}
reduced.logit <- glm(Kyphosis ~ Age, data = kyphosis.train, family = binomial(link = "logit"))
summary(reduced.logit)
```

In the reduced model, the coefficient of intercept is -1.932304. The negative value suggests that at Age = 0, the log-odd of Kyphosis presence is negative which is low probability of Kyphosis. The coefficient of Age is 0.006735, indicates the effects of Age on the log-odds of Kyphosis presence if relatively small. A one-year increase in Age lead to 0.006735 increase in the log-odds of Kyphosis presence. In the model, the p-value of intercept is smaller than the significant level (0.05), which suggests that the intercept is statistically significant but Age has a larger p-value which has no significant effect on the log-odds of Kyphosis presence.

The null deviance is 73.691 and the residual deviance is 71.903. The difference of null deviance and residual deviance is little but still show that the model with Age is better fit than the null model. The AIC value is 75.903.

```{r}
library(caret)

# Calculate predictions from the reduced model
pred.reduced <- predict(reduced.logit, newdata = kyphosis.test, type = "response")

# Create binary predictions from probabilities
b_reduced <- ifelse(pred.reduced > 0.5, 1, 0)
b_reduced <- factor(b_reduced, levels = levels(as.factor(kyphosis.test$Kyphosis)))

# Create the confusion matrix
cm.reduced <- confusionMatrix(reference = as.factor(kyphosis.test$Kyphosis), data = as.factor(b_reduced), mode = "everything")

# Display the confusion matrix
cm.reduced
```

In the confusion matrix, there are 7 ture negative and 2 false positive. Both true positive and false negative are 0. The accuracy of the reduced model is 77.78%. The sensitivity is 1.00 which indicating that the model is very good at correctly predicting true positives. But the specificity is 0.00, suggesting that the model is not good at correctly prediciting true negative. The F1 score is 0.8750, indicating a reasonable balance between precision and recall. The recall is 1.

We can also create the confusion matrix to the full model.

```{r}
pred.full <- predict(full.logit, newdata = kyphosis.test, type = "response")
b_full <- ifelse(pred.full > 0.5, 1, 0)

# Create the confusion matrix
cm.full <- confusionMatrix(reference = as.factor(kyphosis.test$Kyphosis), data = as.factor(b_full), mode = "everything")

# Display the confusion matrix
cm.full
```

In the confusion matrix of full model, there are 6 true negative and 1 true positive. In the prediction, there are 1 false positive and 1 false negative. The accuracy, sensitivity, specificity and F1 score of the model are 0.7778, 0.8571, 0.5000 and 0.8571. The recall is 0.8571.

In this comparison, the reduced model performs better in terms of sensitivity and F1 score, indicating that it does a better job of correctly classifying positive cases. Even though the reduced model sacrifices specificity, making it less effective classifying negative cases. But, we would like to focus on classifying positive cases that is more critical, therefore, we would perfer the reduced model.

(g). Find the best decision tree trained using the CART approach to classify the test data into 1 (presence of kyphosis) or 0 (absence of kyphosis). Make sure you compute and discuss all the metrics you have seen to assess the fit.

```{r}
library(rpart)
fit.allp<- rpart(Kyphosis~., method="class", data=kyphosis.train, control=rpart.control(minsplit=1, cp=0.01))

printcp(fit.allp)
```

```{r}
(rootnode_err<-sum(kyphosis.train$Kyphosis==1)/nrow(kyphosis.train))
```

The tree used Age, Number, and Start as predictor variables. Root node error indicates the misclassification error at the root which is the starting point of the decision tree. In this case, the root node error is 0.20833 which means that initially, there is bout 20.83% of the cases were misclassified. We can see that the relative error for each level of the tree is decreases as the tree grows. 

```{r}
max(fit.allp$cptable[,"nsplit"])
min(fit.allp$cptable[,"nsplit"])
```
The maximum of the node is 15 and the minimum is 0.

```{r}
(cp= fit.allp$cptable[which.min(fit.allp$cptable[, "xerror"]), "CP"])
```

The calculated CP value is 0.03333333, which is the value of the complexity parameter that results in the minimum cross-validation prediction error for the decision tree model.

```{r}
(xerr = fit.allp$cptable[which.min(fit.allp$cptable[, "xerror"]), "xerror"])
```
The calculated minimum cross-validated prediction error (xerror) is 0.9333333.

```{r}
plotcp(fit.allp)
```
The plot in above display the complexity parameter values and their corresponding cross-validated error rates for the decision tree. We can see how the CP parameter affects the size and accuracy of the decision tree.

```{r}
summary(fit.allp)
print(fit.allp)
```

Here we can visualize the decision tree.

```{r}
library(rpart.plot)
rpart.plot(fit.allp, extra = "auto")
```


```{r}
plot(fit.allp, uniform = TRUE, main = " ")
text(fit.allp, use.n = TRUE, all = TRUE, cex = .8)
```

In the above summary and plots, the tree is displayed as a hierarchy of nodes, where each node represents a decision point or terminal outcome. Node numbers are assigned to each node in the tree. 

In the plot, there are three numbers associated with each node. The top one is represent the predicted class which are 0 or 1, that 0 indicate Kyphosis is absent and 1 indicate Kyphosis is present. The middle number is typically associated with the probability or likelihood of an instance falling into this node being assigned to a particular class. For example, if the number is 0.59, indicating that there is a 59% probability of an instance in this node belonging to that class. Finally, there is a percentage in every node. It is to express the probability associated with one of the class labels. For example, 24%, is means that approximately 24% of the instance in the node are likely to be classified as that particular class.

```{r}
# Test with the fit decision tree
test_df <- data.frame(actual = kyphosis.test$Kyphosis, pred = NA)
test_df$pred <- predict(fit.allp, newdata = kyphosis.test, type = "class")
(conf_matrix_base <- table(test_df$actual, test_df$pred)) #confusion matrix
```
The confusion matrix shows that 6 cases are correctly classified as 0, while 1 was correctly classified as 1. The rest were misclassified.

```{r}
sensitivity(conf_matrix_base)
```

```{r}
specificity(conf_matrix_base)
```

```{r}
(mis.rate <- conf_matrix_base[1, 2] + 
   conf_matrix_base[2, 1])/sum(conf_matrix_base) 
```
The sensitivity is 0.8571429, the specificity is 0.5, while the overall misclassification rate is about 0.2222.

In the following, we would prun the tree to improve the generalization ability of the model and also prevent the overfitting.

```{r}
pfit.allp <- prune(fit.allp, cp = fit.allp$cptable[which.min(fit.allp$cptable[, "xerror"]), "CP"])
```

```{r}
library(rpart.plot)
rpart.plot(pfit.allp, extra = "auto")
```

```{r}
# Test with the prun decision tree
test_df <- data.frame(actual = kyphosis.test$Kyphosis, pred = NA)
test_df$pred <- predict(pfit.allp, newdata = kyphosis.test, type = "class")
(conf_matrix_base_p <- table(test_df$actual, test_df$pred)) #confusion matrix
```
```{r}
sensitivity(conf_matrix_base_p)
```

```{r}
specificity(conf_matrix_base_p)
```

```{r}
(mis.rate <- conf_matrix_base_p[1, 2] + 
   conf_matrix_base_p[2, 1])/sum(conf_matrix_base_p) 
```

The sensitivity is 0.8571429, the specificity is 0.5, while the overall misclassification rate is about 0.2222.

Overall, we can see that the result of sensitivity, specificity and misclassification rate is same for full decision tree and pruned decision tree. Therefore, both of them are the best decision tree.

(h). Use the R package ranger to train a random forest to the data and then validate on the test data. Optional. For this data, can you also try and run the randomForestSRC package and compare the run times for the two packages?

```{r}
library(ranger)
fit.rf.ranger<- ranger(Kyphosis~ .,data=kyphosis.train, importance = 'impurity', mtry = 3 )
print(fit.rf.ranger)
```

In the above result, this is a classification problem and there are 500 decision trees were created and combined to make decision. There are 72 observations and 3 predictors in the training dataset. We set Mtry to 3 that represents the number of variables considered at each split in a decision tree. The target node size specifies the minimum node size for terminal nodes in the decision tree. We set it to 1 so that the trees are allowed to continue splitting nodes until there is only one observation in each terminal node. We use gini index to measure the impurity in decision tree algorithms. The out-of-bag prediction error is an estimate of how well the random forest model is likely to perform on new, unseen data. It is reported as 18.06%, which means that the model is expected to make errors on about 18.06% of new, unseen data points.

```{r}
library(vip)
(v1<-vi(fit.rf.ranger))
```

```{r}
vip(v1)
```

We have calculate the variable importance for a random forest model created with the "ranger" function. For "Start" has an importance score of approximately 9.645254 "Age" has an importance score of approximately 9.128193 "Number" has an importance score of approximately 4.696776, which also plot a graph for the importance score.

```{r}
pred <- predict(fit.rf.ranger, data = kyphosis.test)
test_df <- data.frame(actual = kyphosis.test$Kyphosis, pred = NA)
test_df$pred <- pred$predictions
(conf_matrix_rf <- table(test_df$actual, test_df$pred)) #confusion matrix
```

For the random forest using ranger function, there are 6 true negative, 1 true positive, 1 false positive and 1 false negative in the confusion matrix.

```{r}
library(caret)
# Sensitivity
sensitivity(conf_matrix_rf)
```

```{r}
# Specificity
specificity(conf_matrix_rf)
```
```{r}
# Missclassification error rate:
(conf_matrix_rf[1,2] + conf_matrix_rf[2,1])/sum(conf_matrix_rf) 
```

The sensitivity is 0.8571429. Specificity is 0.5 and the misclassification error rate is 0.2222222. 

We can see that the performance of the random forest model from ranger have the same performance with the best decision tree.

In the following, I am goning to try and run the randomForestSRC package.

```{r}
library(randomForestSRC)
set.seed(1234567)
rf_model_rfsrc<-rfsrc(Kyphosis~ ., data=kyphosis.train)
rf_model_rfsrc
```

Same as ranger, we use classification method. Especially, we can see the Brier score, AUC, G-mean and the Requested performance. The brier score and the normalized brier score are 0.14 and 0.56 that are a metric for assessing the accuracy of probabilistic prediction, evaluates the model's performance using samples that were not used during training. The AUC and PR-AUC are 0.79 and 0.44 which also is the classification model performance. The G-mean is 0.61 and the requested performance error is 0.18, 0.07, 0.6.

In the confusion matrix, it shows that there are 53 True negatives, 4 False positives, 9 False negative and 6 True positives. The out-of-bag misclassification rate is 0.18, represents the overall classification error based on out-of-bag samples.

```{r}
rf_pred_rfsrc <- predict(rf_model_rfsrc, data = kyphosis.test)$predicted
```


```{r}
# Measure run time for ranger
ranger_time <- system.time({
  ranger(Kyphosis ~ ., data = kyphosis.train)
})

# Measure run time for randomForestSRC
rfsrc_time <- system.time({
  rfsrc(Kyphosis ~ ., data = kyphosis.train)
})

# Compare the run times
print(ranger_time)
print(rfsrc_time)
```

To compare the run time of two difference random forest algorithms, there are three times, which are user time, system time and elapsed time. User time is the amount of CPU time used by the user's code. For ranger, the user time is 0.01 and for rfsrc is 0.02. System time is the amount of CPU time used by the operating system for system-level operations, like I/O operations. For ranger and rfsrc are both 0.00 in system time. Finally, elapsed time is the total time elapsed from the start of the code execution to its completion. This includes both user time and system time. For ranger, the time is 0.02 and for rfsrc is 0.03 second.

It suggests that ranger used less CPU time in the user model compared to ranger for this specific dtaset and task and also less elapsed time from the start of the code execution to its completion. Therefore, it suggested that ranger is run more faster than rfsrc.

(i). Use and discuss the XGBooost approach to analyze the Kyphosis dataset.


```{r}
library(xgboost)
library(Matrix)
```
We would like to transforming the predictor matrics using dummy encoding. In the following, we have transform the train and test data into two matrices which are "matrix_predictors.train" and "matrix_predictors.test" that have been encoded using dummy variables, making them suitable for use in machine learning models that require numerical input.

```{r}
# Transform the predictor matrix using dummy (or indictor or one-hot) encoding 
matrix_predictors.train <- 
  as.matrix(sparse.model.matrix(Kyphosis ~., data = kyphosis.train))[, -1]
matrix_predictors.test <- 
  as.matrix(sparse.model.matrix(Kyphosis ~., data = kyphosis.test))[, -1]
```

We would extract the predictor variables from the training dataset and converts them into a matrix. Moreover, converts the factor Kyphosis in the training dataset to a numeric vector. Same as the testing dataset.

```{r}
# Train dataset
pred.train.gbm <- data.matrix(matrix_predictors.train) # predictors only
#convert factor to numeric
kyphosis.train.gbm <- as.numeric(as.character(kyphosis.train$Kyphosis)) 
dtrain <- xgb.DMatrix(data = pred.train.gbm, label = kyphosis.train.gbm)
# Test dataset
pred.test.gbm <- data.matrix(matrix_predictors.test) # predictors only
 #convert factor to numeric
kyphosis.test.gbm <- as.numeric(as.character(kyphosis.test$Kyphosis))
dtest <- xgb.DMatrix(data = pred.test.gbm, label = kyphosis.test.gbm)
```

We have set up the watchlist and parameter configurations for training an xgboost model.

watchlist is a list specifying the datasets for monitoring during the training process. It includes the training dataset and the test dataset. This allows us to evaluate the model's performance on both datasets during training

```{r}
watchlist <- list(train = dtrain, test = dtest)
param <- list(max_depth = 2, eta = 1, nthread = 2,
              objective = "binary:logistic", eval_metric = "auc")
```

```{r}
model.xgb <- xgb.train(param, dtrain, nrounds = 2, watchlist)
```

We would like ot monitoring the area under the receiver operating characteristic curve (AUC) on both the training an testing datasets for two rounds. 

For the first round, train-auc is 0.790643 and test-auc is 0.678571. In the second round, train-auc is 0.890058 and test-auc is 0.821429.

```{r}
pred.y.train <- predict(model.xgb, pred.train.gbm)
prediction.train <- as.numeric(pred.y.train > 0.5)
# Measure prediction accuracy on train data
(tab<-table(kyphosis.train.gbm, prediction.train))
```

We would like to calculated the prediction accuracy on our training data using XGBoost model. In the confusion matrix, there are 56 true negative, 1 false positive, 6 false negative and 9 true positive.

```{r}
sum(diag(tab))/sum(tab)
```

The accuracy of XGBost model on the training data is 0.9027778. This means that ourr model correctly classified about 90.28% of the instances in the training dataset.

```{r}
pred.y = predict(model.xgb, pred.test.gbm)
prediction <- as.numeric(pred.y > 0.5)
print(head(prediction))
```

In the above code, we have make predictions on the test data. The predictions are then thresholded at 0.5 to convert the predicted probabilities into binary predictions.

```{r}
# Measure prediction accuracy on test data
(tab1<-table(kyphosis.test.gbm,prediction))
```

In the above is the confusion matrix of the testing data. We can see that there are 6 true negative and 1 true positive. For the false negative and false positive are both 1 respectively.

```{r}
b_gbm <- ifelse(pred.y > 0.5,1,0)
(cm.y <- confusionMatrix(reference=as.factor(kyphosis.test$Kyphosis), 
                         data=as.factor(b_gbm), mode="everything"))
```
About the accuracy of the model, it measures the proportion of correct predictions out of all predictions. It is 77.78%, the model correctly predicted 77.78% of the cases. The sensitivity is 85.61% and the specificity is 50%. An F1 score of 0.8571 indicates a good balance between precision and recall. Overall, the model appears to perform resonably well that with a high sensitivity and specificity, a fairly high accuracy 77.78%.








