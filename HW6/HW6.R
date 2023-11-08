---
  title: "HW6"
author: "Hei Yee Lau Hayley"
date: "2023-10-10"
output: html_document
---
  
  Q1. Consider the data set in deforest.csv that we discussed in Chapter 6.
```{r}
deforest<-read.csv('deforest.csv',header=TRUE)
#deforest
```

a. How do the MLR model fit results change if you used log(area) as the response? Check some initial plots, and also run the MLR model using log(fcover) as response instead of sqrt(fcover), if possible. Then, discuss whether you would prefer to use the sqrt or the log transformation?
  
  ```{r}
par(mfrow=c(2,2))
hist(deforest$fcover, breaks = 30, main='',xlab='fcover')
deforest$sqrtfcover <- sqrt(deforest$fcover)
hist(deforest$sqrtfcover, breaks=30, main="Sqrt(fcover)", xlab="sqrt(fcover)")
deforest$logfcover <- log(deforest$fcover)
hist(deforest$logfcover, breaks = 30, main = "Log(fcover)", xlab = "log(fcover)")
```

```{r}
set.seed(1234567)
train.prop<-0.80
trnset<-sort(sample(1:nrow(deforest),ceiling(nrow(deforest)*train.prop)))
train.set<-deforest[trnset, ]
test.set<-deforest[-trnset, ]
```
```{r}
library(caret)
library(car)
```
```{r}
contpredcols <- 5:20
# Find mean and std dev of train.set.1
normParam <- preProcess(train.set[,contpredcols],
                        method = c("center", "scale"))
# standardize the training set based on its mean and std
data.train1 <- cbind(train.set[,c("sqrtfcover", "policy")],
                     predict(normParam, train.set[,contpredcols])) 
# standardize test set based on the above mean and std dev of training set
data.test1 <- cbind(test.set[,c("sqrtfcover", "policy")],
                    predict(normParam, test.set[,5:20]))
```


```{r}
sqrtfcover<-lm(sqrtfcover~., data=data.train1)
summary(sqrtfcover)
```
We have do an 80-20 split of deforest into a training set and a test set for response sqftfcover. And standardize the training and test set. 
The above is the MLR model of sqrtfcover. We can see that the R-squared is 0.8343, which indicated that the variance of sqftfcover is 83.43% explained by other predictor in this model, which is quite high and a better fit model. And the p-value of the model is smaller than the significant level (0.05), which we have sufficient evidence to reject the null hypothesis for those predictors. This means that most of the predictors variable is considered statistically significant in explaining the variation in the sqrtfcover.

```{r}
#Check the normality of sqrt(fcover)
qqnorm(deforest$sqrtfcover)
qqline(deforest$sqrtfcover)
shapiro.test(deforest$sqrtfcover)
```

We would like to check the normality of sqrtfcover, we can see that most of the data points are lie on the straight line and there are several outlier in the left side and right side.However, we can see that from the shapiro-wilk test, the p-value is smaller than the significant level, which we can rejected the null hypothesis and concluded that the data is not normally distributed.  

```{r}
#check the constant variance for sqrt(fcover)
plot(sqrtfcover$fitted.values, sqrtfcover$residuals, 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted for sqrt(fcover)")
abline(h = 0, col = "red")
ncvTest(sqrtfcover)
```
According to the ncvtest, since the p-value is smaller than the significant level (0.05),which we have evidence to reject the null hypothesis of homoscedasticity. In other words, it suggests that there is a statistically significant presence of heteroscedasticity in your regression model.

```{r}
library(caret)
ccontpredcols <- 5:20

normParam <- preProcess(train.set[,contpredcols],
                        method = c("center", "scale"))

data.train2 <- cbind(train.set[,c("logfcover", "policy")],
                     predict(normParam, train.set[,contpredcols])) 

data.test2 <- cbind(test.set[,c("logfcover", "policy")],
                    predict(normParam, test.set[,5:20]))
```
```{r}
logfcover <- lm(logfcover ~., data = data.train2)
summary(logfcover)
```

We do the same way as sqrtfcover, We have do an 80-20 split of deforest into a training set and a test set for response sqftfcover. And standardize the training and test set. 
The above is the MLR model of sqrtfcover. We can get the similar result with the sqrt(fcover), have the lower p-value than significant level(0.05) which suggested most of the predictors variable is considered statistically significant in explaining the variation in the logfcover. However, the R-squared is lower than sqrt(fcover), indicated the model is not as good as the model of sqft(fcover).

```{r}
#check the normality of log(fcover)
qqnorm(deforest$logfcover)
qqline(deforest$logfcover)
shapiro.test(deforest$logfcover)
```
We can see that from the normal QQ plot of log(fcover), there is a lot of outliers on the left hand side and right hand side. Only data point in the middle in lie on the straight line, which is not follow the assumption of normal distribution.
Since the p-value of shapiro-wilk is smaller than the p-value, it suggested that to reject the null hypothesis and concluded that the data is not normally distributed.

```{r}
#check the constant variance for log(fcover)
plot(logfcover$fitted.values, logfcover$residuals, 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted for log(fcover)")
abline(h = 0, col = "red")
ncvTest(logfcover)
```

For the ncvTest, same with sqrt(fcover), the p-value is smaller than the significant level(0.05) and concluded that the residual have non-constant variance.

From the above results, I have provide the MLR model of sqrt(fcover) and MLR model of log(fcover). The model with sqrt(fcover) as the response has a higher R-squared (0.8343) and residual standard error(1.885).This suggests a better fit to the data in terms of explaining variance.On the other hand, the model with log(fcover) as the response has a lower R-squared (0.7547) and a lower residual standard error (0.5473). The residuals of log(fcover) are closer to a normal distribution. 

I have also check both of the normality and constant variance to see if there satisfy the assumption of MLR model. Both of them do not satisfied the assumption of normality and constant variance. However, the sqrtfover is more normal distributed than logfover, because in the shapiro-wilk test, it has the larger p-value than that of logfcover.

In this case, I preferred the model with sqrt(fcover), because the R-squared of sqft(fcover) is higher than log(fcover). This means the model of sqft(fcover) have a better fit, more normal distributed and higher explained variance, and to provide better prediction for user.


b. Starting with fcover, discuss the use of the Box-Cox transformation for making the distribution of the
response closer to normality. Is use of the square root of fcover as the response (as in Chapter 6) in the MLR
model reasonable?
  ```{r}
library(haven) # to import data from Stata
library(lsmeans) # to perform pairwise comparisons
library(phia) # for the interactionMeans function
```

```{r}
library(MASS)

# Find the optimal lambda (λ) value for Box-Cox transformation
lambda <- boxcox(fcover ~ 1, data = deforest)
cat("Optimal Lambda (λ) Value:", lambda$x[which.max(lambda$y)], "\n")
# Transform the fcover variable using the Box-Cox transformation
if (lambda$y[which.max(lambda$y)] == 0) {
  # If lambda is 0, use log transformation
  fcover_transformed <- log(deforest$fcover)
} else {
  # Otherwise, use Box-Cox transformation
  fcover_transformed <- (deforest$fcover^lambda$x[which.max(lambda$y)] - 1) / lambda$x[which.max(lambda$y)]
}
deforest$boxcoxfcover <- fcover_transformed
hist(deforest$boxcoxfcover, breaks = 30, main = "Boxcox(fcover)", xlab = "Boxcox(fcover)")

```

We can see that the best lambda value is 0.3838 which is close to lambda= 1/2 in the Boxcox table, so it prove that the use of sqrt(fcover) is more reasonable.

Moreover, I have train a dataset and fit an MLR model to boxcofcover.

```{r}
set.seed(1234567)
train.prop<-0.80
trnset<-sort(sample(1:nrow(deforest),ceiling(nrow(deforest)*train.prop)))
train.set<-deforest[trnset, ]
test.set<-deforest[-trnset, ]
```

```{r}
ccontpredcols <- 5:20

normParam <- preProcess(train.set[,contpredcols],
                        method = c("center", "scale"))

data.train3 <- cbind(train.set[,c("boxcoxfcover", "policy")],
                     predict(normParam, train.set[,contpredcols])) 

data.test3 <- cbind(test.set[,c("boxcoxfcover", "policy")],
                    predict(normParam, test.set[,5:20]))
```
```{r}
boxcoxfcover<-lm(boxcoxfcover~., data=data.train3)
summary(boxcoxfcover)
```
```{r}
#Check the normality of boxcox(fcover)
qqnorm(boxcoxfcover$residuals)
qqline(boxcoxfcover$residuals)
shapiro.test(boxcoxfcover$residuals)
```

```{r}

#check the constant variance for boxcox(fcover)
plot(boxcoxfcover$fitted.values, boxcoxfcover$residuals, 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted for boxcox(fcover)")
abline(h = 0, col = "red")
ncvTest(boxcoxfcover)
```
```{r}
par(mfrow=c(2,2))
plot(sqrtfcover)
```
```{r}
par(mfrow=c(2,2))
plot(boxcoxfcover)
```

We can see that the p-value of shapiro-Wilk test for Box-Coxfcover is 1.239e-11. Although it is smaller than the significant level and not satisfied the assumption of normality, the p-value of Box-Coxfover is larger than the p-value of sqrtfcover (3.333e-13) which indicated that Box-Cox transformation for making the distribution of the response closer to normality. Both of the transformation have the similar result in assumption of normality, linearity, constant variance and independence. I think the use of square root of focver in MLR model is more reasonable because the R-squared on the square root MLR model has a higher percentage which means to have a better fit compared to the box cox transformation, and get a accurate prediction result from the model 

Q2. Consider data pertaining to a study of production waste and land use which is available in the R package RobStatTM as the data set waste. There are observations on a response variable and predictor variables as described below:
  
  Y: Solid waste (in millions of tons); X1: Industrial land (acres); X2:Fabricated metals (acres); X3: Trucking and wholesale trade (acres); X4: Retail trade (acres); and X5: Restaurants and hotels.
```{r}
library(RobStatTM)
data (waste)
```
a. Fit an MLR model using the lm() function. Obtain the residuals, and check the linear regression assumptions.

```{r}
waste_MLR <- lm(SolidWaste ~., data = waste)
summary(waste_MLR)
residuals<-residuals(waste_MLR)
```
From the multiple regression model, we can see that except Metals, all of the predictor variable have the smaller p-value than the significant level, which are having significant effect on SolidWaste. The multiple R-squared indicated approximately 84.91% of the variability in SolidWaste is explained by the predictors in the model. And the adjusted R-squared value of 0.8269 suggests that the model performs well even after adjusting for the number of predictors. The F-statistic is 38.25, and it assesses the overall significance of the regression model. With the small p-value, it is indicates that the model is statistically signifcant.

```{r}
residuals(waste_MLR)
```
```{r}
par(mfrow=c(2,2))
plot(waste_MLR)
```

The residual vs. fitted in the above, the data point is not randomly scattered and the the pattern show the funnel shape, not a horizontal line at zero. Most of the points are located on the left hand side. It indicated that the linearity assumption is not satisfied. We can see that from the normal Q-Q plot in the top-right position, most of the data point is not lie on the straight line, indicates that the residuals are not approximately normally distributed.

For the scale-location plot in the bottom-left position. It shows the square root of the absolute standardized residuals against the fitted values. Since the red line is not horizontal across the plot and the spread of the residual is not equal at all fitted values. There is a clear pattern among the residual that most of the data point locate on the left hand side, so we suggested that the assumption of homoscedasticity is not satisfied. 

```{r}
shapiro.test(residuals)
```


```{r}
library(car)
ncvTest(waste_MLR)
```
The shapiro-wilk test prove that the data is normally distributed since the p-value is larger than the significant level (0.05) and we have the evidence to accept the null hypothesis, reject the alternative hypothesis, suggest that it is normally distributed.
From the ncv test, we can see that the p-value is smaller than the significant level, which suggested that we have no sufficient evidence to accept the null hypothesis and prove that the data is not satisfied the assumption of constant variance.


b. Identify outliers, if any, based on Studentized residuals.Discuss what they mean with respect to the data frame.

```{r}

plot(waste$SolidWaste, predict(waste_MLR,newdata = waste), 
     col=4, cex=0.3, xlab="Actual", ylab="Predicted", axes=FALSE)
extpts <- which(abs(rstudent(waste_MLR)) > 3)
text(waste$SolidWaste[extpts], 
     predict(waste_MLR,newdata = waste)[extpts],
     rownames(waste)[extpts], cex=0.5, col=2)
axis(1); axis(2); grid(); abline(0,1, col=4, lwd=3)
studentized_residuals <- rstudent(waste_MLR)

# Define a threshold for identifying outliers
threshold <- 3

# Identify and extract outliers
outliers <- waste[abs(studentized_residuals) > threshold,]

# Print the outliers
print(outliers)
```


From the above plot, we can see that there are 3 outliers in the data are out of the range which are observation 2, 31 and 40 respectively.
Outlier that identified based on Studentized residuals in a linear regression model, are the data points that deviate significantly from the expected pattern of the data. It can be the Unusual data points that exhibit unusual behavior compared of the majority of the data, these could be observations that do not follow the linear relationship expected by the regression model. Moreover, outlier can have a significant influence on the regression mode;'s coefficients, causing them to be biased and these influential points can have a substantial impact on the model's predictions.
Outlier can also suggest potential issues with the assumptions of our regression model. This might indicate that there are non-linear relationship, or other problems in the data that the model does not capture.
Outlier can be indicative of heteroscedasticity, where the variance of the residual is not constant across all levels of the predictor variables and it can affect the model's performance and assumption

c. Identify high leverage points, if any. Discuss what they mean with respect to the data frame. 
```{r}
set.seed(123457)
train.prop <- 0.80
trnset <- sort(sample(1:nrow(waste), ceiling(nrow(waste) * train.prop)))
train.set <- waste[trnset, ]
test.set <- waste[-trnset, ]

contpredcols <- 1:6
```

```{r}
library(caret)

# Find mean and std dev of train.set
normParam <- preProcess(train.set[, contpredcols], method = c("center", "scale"))

# Standardize the training set based on its mean and std
data.train4 <- cbind(train.set[, "SolidWaste"],
                    predict(normParam, train.set[, contpredcols])) 

# Standardize test set based on the mean and std dev of the training set
data.test4 <- cbind(test.set[, "SolidWaste"],
                   predict(normParam, test.set[, contpredcols]))
```
```{r}
mod.1 <- lm(SolidWaste ~., data = data.train4)
summary(mod.1)
```


```{r}
leverage <- hatvalues(waste_MLR)
high_leverage <- which(leverage > 2 * (ncol(waste) + 1) / nrow(waste))
waste[high_leverage, ]
boxplot(influence(waste_MLR)$hat, sub='leverages')
```
```{r}
n <- nrow(data.train4)
p <- ncol(data.train4)-1
(hilev <- which(influence(mod.1)$hat > max(2*(p+1)/n,0.5)))
```
From the result, the high-leverage cases are 2 , 31, 35, and 40, which are rows 2, 23, 27, and 32 in the training data

```{r}
par(mfrow=c(1,1))
plot(rstandard(waste_MLR)^2, influence(waste_MLR)$hat, pch =19, cex=0.5, col="blue",
xlab = "squared residual", ylab = "leverage")
inf0 <- which(influence(waste_MLR)$hat > 0.5)
text(rstandard(waste_MLR)[hilev]^2, influence(waste_MLR)$hat[hilev],
labels = inf0, cex = 0.9, font =2, pos =1)
```

In the above plot, x-axis represents the sqaured residuals for each observation, y-axis represents the leverage value for each observation. Each blue dot on the plot represents an observation in the dataset.

In the plot, most of the point have the lower leverage and lower squared residual. However, there are some outlier that are high up and far to the right, which mean they are both high leverage and high influence points. These observations  have a strong impact on the model's coefficients and can potentially affect the model's fittness. They can pull the regression line toward them or push it away, which can lead to changes in the slope and intercept of the model Moreover, they are the outlier in the dataframe, indicated observations that are both far from the mean and have large residual.

d. Identify influential points using (i) Cook’s distance, and (ii) DFFITS. Can you explain what aspect of the regression fit these measure influence on?

```{r}
boxplot(cooks.distance(waste_MLR),sub="Cook's D")
cooks_crit = 0.5
influence3_cooks <- cooks.distance(waste_MLR)
df <- data.frame(obs = names(influence3_cooks),
                 cooks = influence3_cooks)
influential<-which(influence3_cooks>4/(n-p-1))
influential
ggplot(df, aes(y = cooks, x = obs)) +
  geom_point() +
  geom_hline(yintercept = cooks_crit, linetype="dashed") +
  labs(title = "Cook's Distance",
       subtitle = "Influential Observation",
       x = "Observation Number",
       y = "Cook's")

```

```{r}
boxplot(dffits(waste_MLR),sub='DFFITS')
df <- waste_MLR$df.residual
p <- length(waste_MLR$coefficients)
n <- nrow(waste_MLR$model)
dffits_crit = 2 * sqrt((p + 1) / (n - p - 1))
influence3_dffits <- dffits(waste_MLR)
t <- 2 * sqrt(length(coefficients(waste_MLR)) / length(residuals(waste_MLR)))
influential_dffits <- which(abs(influence3_dffits) > t) 
influential_dffits

df <- data.frame(obs = names(influence3_dffits),
                 dffits = influence3_dffits)
ggplot(df, aes(y = dffits, x = obs)) +
  geom_point() +
  geom_hline(yintercept = c(dffits_crit, -dffits_crit), linetype="dashed") +
  labs(title = "DFFITS",
       subtitle = "Influential Observation.",
       x = "Observation Number",
       y = "DFFITS")
```
From the boxplot of Cook's distance, we can see that there are more than 6 outliers above the range of the box, which are observation 2,8,15,31,35 and 40.Moveover, in the cooks.distance model, we can see that observation 2, 15, 31, 40 that out of the dotted line which are the outlier in the data. 
For the Cook's distance, it is primarily used to identify outliers and influential points. High values fo Cook's distance suggest that deleting a specific data point would result in a significant change in the regression coefficients, indicating that the data point has a strong influence on the model's overall fit. 

From the boxplot of DFFITS, there are multiple outlier that out of the range of the box. And from the diffits(model) function , we can see that the observation of 2, 8, 15, 31, 35, 40 are the outlier in the data from the ggplot and the dffits() function.
For DFFITS, it helps identify observations that have a notable impact on the predicted values. An observation with a high DFFITS value contributes significantly to the model's overall fit. It is used to detect influential points that may distort the model's predictions.

Q3. Continue with the waste data from the previous question. Use the VIF to understand whether the problem of multicollinearity exists in fitting an MLR model to the response Y using all the predictors. If there is multicollinearity, implement a remedy using ridge regression

```{r}
pred.df<-data.train4[,-1]
cor.pred<-cor(pred.df)
off.diag<-function(x) x[col(x)>row(x)]
v<-off.diag(cor.pred)
table(v>=0.95)
table(v>=0.99)
```
To detect if there is multicollinearity, we would like to find out if there is a correlation coefficient exceed 0.95 ro 0.99
The output show that there is one correlations exceeding 0.95 and no correlation exceeding 0.99. Therefore, the one correlation that exceed 0.95, suggests a strong linear relationship between the two variable.

We would also find out if there is a simple correlation exceed the multiple correlation R-squared from the MLR fit.

```{r}
#table(v > summary(mod.1)$r.sqaured)
```
```{r}
library(ppcor)
pcor1 <- pcor(pred.df)
vp <- off.diag(pcor1$estimate)
table(vp >= 0.5)
```
The code above computes partial correlations between predictor variables which adjust for the influence of other variables. There are three partial correlations exceed 0.5. It suggests a strong relationship between two variables, even after considering the other variables. 

```{r}
car::vif(mod.1)
```
VIF measures how much the variance of the estimated regression coefficients increases due to multicollinearity. A high VIF (greater than 5 or 10) indicates that a predictor variable is highly correlated with other predictors in the model.
Since VIF's for Retail and Restaurants exceed 10, it suggesting multicollinearity. 

```{r}
library(olsrr)
(mod.condind <- ols_eigen_cindex(mod.1)[,2])
(mod.condnum <- max(mod.condind)/min(mod.condind))
```
The condition index and nuber help assess multicollinearity in the design matrox of the linear regression model.
The condition number is 11.28, indicating multicollinearity.
We would like to drop the predictor which is highly correlated with other predictors. Exclude Restaurants which has the largest VIF, and refit the MLR model. 

```{r}
data.train.1 <- subset(data.train4, select = -c(Restaurants))
mod.droprest <- lm(SolidWaste ~., data = data.train.1)
summary(mod.droprest)
```
We can see that the F-statistic is change from 1.625e+32 to 1.451e+32. A decrease in the F-statistic indicates that the model's ability to explain the variance in the dependent variable has reduced. And it might indicate that the model is less effective 

```{r}
anova(mod.droprest)
```
```{r}
car::vif(mod.droprest)
```
Now, we can see that after drop Restaurants, the VIF's are now much smaller than 10 and the VIF for Retail is only 4.66.

```{r}
par(mfrow=c(2,2))
plot(mod.droprest)
```

```{r}
pred.df<-data.train.1[,-1]
cor.pred<-cor(pred.df)
off.diag<-function(x) x[col(x)>row(x)]
v<-off.diag(cor.pred)
table(v>=0.95)
```
And there is no correlation coefficient greater or equal to 0.95, which indicated that there is no strong linear relationship between the two variable.

```{r}
library(glmnet)
pred.mat <- as.matrix(pred.df) # predictors
resp <- data.train.1$SolidWaste # response
mod.ridge.1<-glmnet(pred.mat, resp, alpha = 0, nlambda = 500, standardize = FALSE, intercept=TRUE)
```

```{r}
mod.ridge.1=glmnet(pred.mat, resp, alpha=0, standardize = FALSE)
```

```{r}
(lambda.m<-mod.ridge.1$lambda[100])
```

The value of lambda is 0.096875, it means that this value was selected as the optimal regularization strength for the ridge regression model. This value indicates that the model is applying some level of regularization but not very strong regularization. It is a moderately small value for lambda, suggesting that the ridge regression is striking a balance between fitting the data and preventing overfitting. A lambda value like 0.096875 indicates that the model should provide reasonably good predictive performance without dropping other predictor variables

```{r}
coef(mod.ridge.1, s=lambda.m)
```
For the result in above, it shows that Land has a negative coefficient which indicates that an increase in Land is associated with a decrease in SolidWaste. For the other predictors, it has a positive coefficient that increase thier value will associated with an increase in SolidWaste.

```{r}
cvfit.ridge<-cv.glmnet(pred.mat, resp, alpha=0, standardize=FALSE, ttype.measure='mse', nfold=10)
plot(cvfit.ridge)
```
The x-axis shows different lambda values and y-axis represents the cross-validated mean squared error (MSE) for each lambda value. Lower MSE values indicate better model performance in terms of prediction accuracy. 
In the above plot, the minimum MSE occurs at approximately lambda = -2.5. This lambda value represents the best trade-off between model complexity and predictive performance. Model with this level of regularization have the lowest cross-validated prediction error. The dotted line on the right side is provide an alternative choice for a more parsimonious model with slightly higher error but fewer variables.

```{r}
cvfit.ridge$lambda.min
cvfit.ridge$lambda.1se
```

The lambda value (0.096875) is that minimizes the mean squared error (MSE) during cross-validation, making it the choice for the best model in terms of prediction accuracy. 
The lambda value (0.1405), which is the largest lambda within one standard error of the lambda that minimized MSE. It represents an alternative choice that results in a more parsimonious model with slightly higher prediction error but fewer predictor variable. This choice is suitable when we prioritize a simpler and more interpretable model. 

```{r}
(all.coef<-cbind(coef(cvfit.ridge, s="lambda.min"), coef(cvfit.ridge, s="lambda.1se")))
```

```{r}
bigmat <- cbind(resp,pred.mat)
mod.ridge.new <- MASS::lm.ridge(resp ~ ., as.data.frame(bigmat))
plot(lm.ridge(resp ~ ., as.data.frame(bigmat), lambda = seq(0,2,0.05)))
```

We can values of lambda in the x-axis and coefficients of the predictor variable on the y-axis. Each line in the l=plot corresponds to a different predictor variable. These line represent the path of each coefficient as lambda changes. The slope of these lines indicates how strongly each coefficient is regularized.















