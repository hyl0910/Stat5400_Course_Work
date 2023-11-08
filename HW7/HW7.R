---
  title: "HW7"
author: "Hei Yee Lau Hayley"
date: "2023-10-17"
output: html_document
---
  Consider the data set snails.csv. We are interested in modeling counts of the snail species Gaenig. Do a random 80-20 train-test split of this data, using set.seed(123457), fitting suitable models to the train data and evaluating the fit on the test data.

In each case, compute and discuss

model coefficients 

dispersion parameter

model adequacy (chi-squared test, deviance residuals)

information criteria

in-sample MAD on the training data

out-of-sample MAD on the test data.

Based on (i) - (vi), do you have a preference for one of the models (a), (b), or (c)? In your selected model, which predictors are useful for explaining and predicting Gaening counts?
  
  ```{r}
library(readr)
library(Metrics)
library(MASS)
```

```{r}
snails <- read.csv("snails.csv", header = TRUE)
```
```{r}
snails$Aspect[snails$Aspect==6]=5
snails$Aspect[snails$Aspect==2]=1
snails$Aspect <- as.factor(snails$Aspect)
snails$Soil <- as.factor(snails$Soil)
snails$CC <- as.factor(snails$CC)
snails$LC <- as.factor(snails$LC)
```

```{r}
cat.id <- which(colnames(snails) %in% c("Aspect","Soil","CC","LC"))
snails[,cat.id] <- lapply(snails[,cat.id],as.factor)
```
```{r}
Asp1 <- ifelse(snails$Aspect == 1, 1, 0)
Asp5 <- ifelse(snails$Aspect == 5, 1, 0)
Asp7 <- ifelse(snails$Aspect == 7, 1, 0)
Asp8 <- ifelse(snails$Aspect == 8, 1, 0)
```

```{r}
set.seed(123457)
train.prop <- 0.80
trnset <- sort(sample(1:nrow(snails), ceiling(nrow(snails)*train.prop)))
train.set <- snails[trnset, ]
test.set <- snails[-trnset, ]
```



**Fit and assess the Poisson loglinear model to Gaenig counts using the train data.**
  
  ```{r}
gaenig.pf <- glm(Gaenig~Elevation+Slope+Aspect+Soil+CC+CO+LC+PA.sp+PA.other, family='poisson', data=train.set)
summary(gaenig.pf)
```
For the Poisson loglinear model to Gaenig counts using the train data, the family is specified as "poisson" and the dataset we used is the train.set on which the model was trained. The model provided the estimates for the coefficients of the model. Most of the variable have the negative correlation with Gaenig. For each one-unit increase in 'Elevation', the log of the count is expected to increase by 0.002851 and for each one-unit decrease in 'Slope', the log of the count is expected to decrease by 0.007304 The coefficient of Aspect 5,7 and 8 indicate how the log count change when Aspect is 5,7 or 8 compared to a reference category. Same with the variables of Soil, CC, PA.sp and PA.other. The null deviance is 205.68 and the residual deviance is 174.65.

The reduction of deviance indicates that the model with the selected predictor variables provide a better fit to the data compared to a null model with no predictors. Since the residual deviance is smaller than the null deviance suggests that the Poisson model is doing a good job for explaining the observed data, as it reduces the deviance substantially be incorporating the predictor variables.


```{r}
# null model
gaenig.npf <- glm(Gaenig~1, 
                  family='poisson', data=train.set)
summary(gaenig.npf)
```

```{r}
#dispersion parameter
(disp.est <- gaenig.pf$deviance/gaenig.pf$df.residual)
```
```{r}
#dispersion parameter for null
(disp.est <- gaenig.npf$deviance/gaenig.npf$df.residual)
```
The dispersion parameter in a Poisson regression model is a measure of how well the model accounts for the variability in the data. It quantifies the relationship between the observed variance and the expected variance under the model. In the case, the dispersion parameter value is 1.679, suggests that the model has overdispersion, it occurred when the observed variance in the data is greater than what is expected based on a Poisson distribution. 

```{r}
#chi-square test
with(gaenig.pf, cbind(deviance = deviance, df = df.residual,
                      p = pchisq(deviance, df.residual, lower.tail=FALSE)))
```
Null Hypothesis: The data prefer full fitted model

Alternative Hypothesis: The data does not prefer the full fitted model

In the Chi-squared test, the p-value is 1.771981e-05, is smaller than the significant level (0.05), shows that the data rejects H0 (null model) and not prefer the full fitted model ,which suggests that the Poisson regression model is not significantly improves the fit over the null model. We can compare the deviance value (177.9315) with other model to see if it is the better fit model among others.

```{r}
with(gaenig.pf, cbind(deviance = null.deviance-deviance, 
                      df = df.null-df.residual,
                      p = pchisq(null.deviance-deviance, 
                                 df.null-df.residual, 
                                 lower.tail=FALSE)))
```

We would like to see whether all the predictors aken together are useful for explaining log count over and above the intercept along. We have compared with the null model.

The extra deviance is 31.0267 and extra degree freedom is 16. The p-value is 0.01335116 which is smaller than the significant level (0.05), indicate we can reject H0 and prefers the full fitted model. But if we adjust the significant level to 0.01, we can accept the null hypthoesis and prefer the null model.

```{r}
#deviance residuals
gaenig.pf.res <- resid(gaenig.pf, type = "deviance") 
summary(gaenig.pf.res)
```

From the deviance residual, we can see that the model has a mix of both overpredictions and underpredictions. The mean residual being negative (-0.2642), which suggests that the model is slightly underpredicts the count for your data. Since the maximum value of 3.1467 that has a wide gap between the 3rd quantile, it suggests that there might be outliers or extreme values among the residual and indicate the model's predictions are particularly poor.

```{r}
# Information criteria
AIC(gaenig.pf, gaenig.npf)
BIC(gaenig.pf, gaenig.npf)
```
AIC is a measure of the trade-off between model fit and model complexity. Lower AIC values indicate a better balance between explaining the data and avoiding overfitting. In the null model, we can see that the AIC value for full fitted model is approximately 378.7204 and for the null model is 377.7470. It shows that null model is the better fit model because it has the smaller AIC.

BIC is similar to AIC but places a stronger penalty on model complexity. The BIC value in this model is 426.2488 and for null model is 380.5428, which show the same result with AIC that null model is the better model since the smaller BIC.


```{r}
#in-sample mad full fitted model
in_sample_mad_poisson <- mean(abs(train.set$Gaenig - predict(gaenig.pf, newdata=train.set, type = "response")))
cat('In-sample MAD: ', in_sample_mad_poisson)
```

```{r}
#out-sample mad on full fitted model
out_sample_mad_poisson <- mean(abs(test.set$Gaenig - predict(gaenig.pf, newdata = test.set, type = "response")))
cat('Out-sample MAD: ', out_sample_mad_poisson)
```

```{r}
#in-sample mad on null model
in_sample_mad_poisson_null <- mean(abs(train.set$Gaenig - predict(gaenig.npf, newdata=train.set, type = "response")))
cat('In-sample MAD for null model: ', in_sample_mad_poisson)
```

```{r}
#out-sample mad on null model
out_sample_mad_poisson_null <- mean(abs(test.set$Gaenig - predict(gaenig.npf, newdata = test.set, type = "response")))
cat('Out-sample MAD for null model: ', out_sample_mad_poisson)
```

The in-sample MAD is approximately 0.9860363 indicates that on average, the model's predictions are off by about 0.9860363 counts compared to the actual observed counts in the training data.The out-sample MAD of approximately 1.037381 indicates that on average, the model predictions are off by about 1.037381 counts compared to the actual observed counts in the testing data.

And the in-sample MAD is same with the out-sample MAD.

**Fit and assess the quasi-Poisson loglinear model to Gaenig counts using the train data.**
  
  ```{r}
gaenig.qpf <- glm(Gaenig~Elevation+Slope+Aspect+Soil+CC+CO+LC+PA.sp+PA.other,
                  family=quasipoisson,data=train.set)
summary(gaenig.qpf)
```

For the model coefficient, the intercept is approximately 0.433161. In one-unit increase in Elevation results in an expected increase of approximately 0.002851 in the log count of Gaenig. However, it is not statistically significant at conventional significant at conventional significant levels (p-value>0.05). In one-unit increase in Slope results in an expected decrease of approximately 0.007304 in the log count of Gaenig. The coefficient represents the effect of Aspect 5,7 and 8 on the log count of Gaenig compared to the reference category. Same with Soil, cc, LC, PA.sp and PA.other.The null deviance is 205.68 and the residual deviance is 174.65. The reduction in deviance from the null deviance to the residual deviance indicates that the model is providing a better fit than the null model. 
Depend on the p-value, Soil4 is significant to the full fit model since the p-value is smaller than the significant level (0.05).

```{r}
gaenig.nqpf <- glm(Gaenig~1, family=quasipoisson,data=train.set)
summary(gaenig.nqpf)
```

For the null model, the estimate intercept is 0.1387 and the deviance is 205.68 on 120 degrees of freedom. The p-value of intercept is 0.211 which is larger than the significant level (0.05) and suggests that it is not significant.

```{r}
#dispersion parameter
(disp.est <- gaenig.qpf$deviance/gaenig.qpf$df.residual)
```

The dispersion parameter of 1.679366 suggests that the variance in the data is larger than what would be expected under a typical quasi-Poisson loglinear model. this indicates overdispersion in the data.

```{r} 
#check the adequate (chi-square test)
with(gaenig.qpf, cbind(deviance = deviance, df = df.residual,
                       p = pchisq(deviance, df.residual, lower.tail=FALSE)))
```
H0: Data prefer full fitted model

H1: Data  does not prefer the full fitted model

In the Chi-squared test, the p-value is 1.866122e-06, is smaller than the significant level (0.05), shows that the data rejects H0 (null model) and not prefer the full fitted model (model under H1) ,which suggests that the Quasi-Poisson loglinear model is not a good fit for the data, and there may be a better model that fits the data more effectively. We can compare the deviance value (205.6807) with other model to see if it is the better fit model among others.

```{r}
#deviance residuals
gaenig.qpf.res <- resid(gaenig.qpf, type = "deviance") 
summary(gaenig.qpf.res)
```
The minimum deviance residual is approximately -1.8901. This suggests that there are observations in the dataset for which the actual counts are lower than what the model predicts.The 1st quartile of the deviance residuals is approximately -1.2320. This indicates that 25% of the residuals fall below this value. The mean (average) deviance residual is approximately -0.2642. On average, the actual counts deviate by about -0.2642 from the model's predictions.The third quartile (75th percentile) of the deviance residuals is approximately 0.5024. This indicates that 75% of the residuals fall below this value.The maximum deviance residual is approximately 3.1467. This is the largest positive deviation between the observed counts and the model's predictions. It suggests that there are data points for which the actual counts are much higher than what the model predicts.
There is variability in how the model's predictions match the observed data. The model tends to overestimate counts on average, as indicated by the negative mean residual.

```{r}
# Information criteria
AIC(gaenig.qpf,gaenig.nqpf )
BIC(gaenig.qpf, gaenig.nqpf)
```
AIC and BIC for full fitted model and null model are marked as 'NA' which typically occurs when the model's likelihood or deviance is not calculable due to specific circumstances, such as convergence isses or other factors affecting the model's estimation.

```{r}
#in-sample mad on ful fitte model
in_sample_mad_poisson_qpf <- mean(abs(train.set$Gaenig - predict(gaenig.qpf, type = "response")))
cat('In-sample MAD: ', in_sample_mad_poisson_qpf)
```

```{r}
#out-sample mad on full fitted model
out_sample_mad_poisson_qpf <- mean(abs(test.set$Gaenig - predict(gaenig.qpf, newdata = test.set, type = "response")))
cat('Out-sample MAD: ', out_sample_mad_poisson_qpf)
```

```{r}
#in-sample mad on null data
in_sample_mad_poisson_nqpf <- mean(abs(train.set$Gaenig - predict(gaenig.nqpf, type = "response")))
cat('In-sample MAD: ', in_sample_mad_poisson_nqpf)
```

```{r}
#out-sample mad on null data
out_sample_mad_poisson_nqpf <- mean(abs(test.set$Gaenig - predict(gaenig.nqpf, newdata = test.set, type = "response")))
cat('Out-sample MAD: ', out_sample_mad_poisson_nqpf)
```

For the full fitted model, the in-sample MAD is approximately 0.9860363 and the out-of-sample MAD is approximately 1.037381. The MAD values suggests that your model might be too complex or overfitting the training data. It occurs when a model captures noise or random fluctuations in the training data, making it perform well on the training data but poorly on new, unseen data. 

For the null model, the in-sample MAD is approximately 1.080117 and the out-of-sample MAD is approximately 0.9126722.It implies that your model may not have learned the underlying patterns in the training data effectively and is not generalizing well to new data.


**Fit and assess the negative binomial loglinear model to Gaenig counts using the train data.**
  
  ```{r}
library(MASS)
gaenig.nbn <- glm.nb(Gaenig~1, data = train.set)
summary(gaenig.nbn)
```

We have made a negative regression with only an intercept term. The coefficient for the intercept is 0.1387, but it is not statistically significant because the p-value is 0.218, which is smaller than the significant level (0.05), means that there is not enough evidence to reject the null hypothesis and the intercept is not statistically significant in the model. The dispersion parameter is estimated to be 1.5156. The model's deviance is 129.13 and the residual deviance is the same. The AIC for this model is 361.63 

```{r}
gaenig.nbf <- glm.nb(Gaenig~Elevation+Slope+Aspect+Soil+CC+CO+LC+PA.sp+PA.other, data=train.set)
summary(gaenig.nbf)
```

From the above result, the intercept represents the estimated log count of the response variable (Gaenig) when all the predictor variables are zero, it is estimated to be -0.161328. The coefficient for Elevation is 0.004123. This means that for a one- unit increase in Elevtion, the log count of Gaenig is expected to increase by 0.0004123. The coefficient for Slope is -0.006415. This suggests that for a one-unit increase in Slope, the log coubt of Gaenig is expected to decrease by 0.006415. In the model, variable Soil4 is sinificant to Gaenig since the p-value is smaller than the significant level (0.05).

The null deviance is 153.75 and the Residual deviance is 132.03. A smaller residual deviance compared to the null deviance indicates that the model with predictor variables provides a better fit to the data compared to the null model. It quantifies the reduction in deviance achieved by including the predictor variables.

```{r}
#dispersion parameter
(disp.est <- gaenig.nbf$deviance/gaenig.nbf$df.residual)
```

For dispersion parameter is estimated to be 1.269503, this value suggests that there ay be overdispersion in the data, especially if it is significantly larger than 1.

```{r}
#chi-square test
with(gaenig.nbf, cbind(deviance = deviance, df = df.residual,
     p = pchisq(deviance, df.residual, lower.tail=FALSE)))
```

H0: Data not prefer full fitted model

H1: Data prefer the full fitted model

In the Chi-squared test, the p-value is 0.03308387, is smaller than the significant level (0.05), which support the H0, shows that the data not prefer the full fitted model,which suggests that the Negative Binomial Regression model is not significantly improves the fit over the null model. We can compare the deviance value (132.0283) with other model to see if it is the better fit model among others.


```{r}
(an.nb <- anova(gaenig.nbf, gaenig.nbn, test="Chisq"))
```
We can see that the p-value is 0.25, which larger than the significant level and suggested that the null negative binomial regression model is more better fit and better model.

```{r}
#deviance residuals
gaenig.nbf.res <- resid(gaenig.nbf, type = "deviance") 
summary(gaenig.nbf.res)
```
The minimum deviance residual is -1.7398. This indicate that for some data points, the model overpredicts the response variable by a substantial amount. The 1st quartile value is -1.1679. This means that 25% of the data points have deviance residuals less than or equal to -1.1679. The median deviance residual is -0.4000, indicating that roughly half of the data points have deviance residuals less than -0.4000, and the other half have residuals greater than this value. 

The mean deviance residual is -0.2853. On average, the model underpredicts the response variable by approximately 0.2853 units. The 3 rd quartile value is 0.4315. This means that 75% of the data points have deviance residuals less than or equal to 0.4315. The maximum deviance residual is 2.3940. This indicates that for some data points, the model underpredicts the response variable by a substantial amount.

We explore the model fit in more detail. We can fit a reduced model by dropping the predictor Elevation, slope, CC, CO, Aspect, LC, and PA.sp from thr null model, since the predictor Soil and PA.other have the smaller p-value than others.

```{r}
gaenig.nbr <- glm.nb(Gaenig~Soil+PA.other,data=train.set)
summary(gaenig.nbr)
```
We can see that the AIC has reduced to 358.06, which become a better fit model. We can see that the intercept and PA.other are significant to the model since the p-value is smaller than the significant level. The residual deviance is smaller than the null deviance, prove than the model with the predictor variables provides a better git to the data compared to the null model.

```{r}
with(gaenig.nbr, cbind(res.deviance = deviance, df = df.residual,p = pchisq(deviance, df.residual, lower.tail=FALSE)))
```
The p-value is 0.2113389, which larger than the significant level (0.05), indicate than after dropping those predictor, the model is become more adequate. Since null model is more better fit, we will compare the null model with the reduced model.

```{r}
(comp.nb <- anova(gaenig.nbn, gaenig.nbr, test="Chisq"))
```
The null hypothesis (H0) assumes that there is no significant difference between the two models, and Model 1 is sufficient.
The alternative hypothesis (H1) suggests that Model 2, which includes the "Soil" variable, is significantly better in terms of fit.

The p-value of 0.2210795 is greater than the significance level (0.05). Since the p-value is not less than 0.05, we fail to reject the null hypothesis. In other words, there is not enough evidence to suggest that reduced model provides a significantly better fit compared to null model. Therefore, null model is not significantly worse than reduced model, given the chi-squared test results.

```{r}
# Information criteria
AIC(gaenig.nbr,gaenig.nbn)
BIC(gaenig.nbr,gaenig.nbn)
```

For AIC, the model "gaenig.nbr" has an AIC of 358.0638, while "gaenig.nbn" has an AIC of 361.6281. For BIC, "gaenig.nbr" has a BIC of 372.0427, while "gaenig.nbn" has a BIC of 367.2197. Since we are prioritize model fit, we would choose the model with the lower AIC, which the reduced model is the better model. But overall, the AIC of reduced model and null model are similar. 


```{r}
#in-sample mad on full fitted model
in_sample_mad_nbf <- mean(abs(train.set$Gaenig - predict(gaenig.nbf, type = "response")))
cat('In-sample MAD: ', in_sample_mad_nbf)
```

```{r}
#out-sample mad on full fitted model
out_sample_mad_nbf <- mean(abs(test.set$Gaenig - predict(gaenig.nbf, newdata = test.set, type = "response")))
cat('Out-sample MAD: ', out_sample_mad_nbf)
```
For the full fitted model, the in-sample MAD is approximately 0.9921456 and the out-of-sample MAD is approximately 1.019177.

```{r}
#in-sample mad on null model
in_sample_mad_nbn <- mean(abs(train.set$Gaenig - predict(gaenig.nbn, type = "response")))
cat('In-sample MAD: ', in_sample_mad_nbn)
```

```{r}
#out-sample mad on  null model
out_sample_mad_nbn <- mean(abs(test.set$Gaenig - predict(gaenig.nbn, newdata = test.set, type = "response")))
cat('Out-sample MAD: ', out_sample_mad_nbn)
```
For the null model, the in-sample MAD is approximately 1.080117 and the out-of-sample MAD is approximately 0.9126722.


```{r}
#in-sample mad on reduced model
in_sample_mad_nbr <- mean(abs(train.set$Gaenig - predict(gaenig.nbr, type = "response")))
cat('In-sample MAD: ', in_sample_mad_nbr)
```

```{r}
#out-sample mad on reduced model
out_sample_mad_nbr <- mean(abs(test.set$Gaenig - predict(gaenig.nbr, newdata = test.set, type = "response")))
cat('Out-sample MAD: ', out_sample_mad_nbr)
```

For the null model, the in-sample MAD is approximately 1.056339 and the out-of-sample MAD is approximately 0.9585369

From the result of in-sample MAD and out-sample MAD, we can see that both of them have the same result, For in-sample MAD, both full fitted model are 1.080117. For out-sample MAD, both fitted model are 0.9127.

In this case, I would suggest the Negative Binomial model because it is with a lower dispersion parameter, accounts for overdispersion, and relatively low AIC values which have the goodness of fit and model complexity. In the several model of negative binomial regression model, I would prefer the null model of negative binomial regression model because the AIC is lower than the reduced model. Moreover, the out-sample MAD of null model is lower than that of others negative binomial regression model. It suggests that the null model's predictions are more accurate and have better generalization performance when applied to new, unseen data. For null negative binomial model, there is no any predictor that use for predictions but provide a better fit. It might because the full fitted model may be overly complex and may overfit the training data. Also there might be capture spurious or random correlations between predictors and the response variable.s







