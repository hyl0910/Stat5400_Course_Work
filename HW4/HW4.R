---
  title: "Stat5405-Homework 4"
author: "Hei Yee Lau Hayley"
date: "09/30/2023"
format:
  html:
  embed-resources: true
theme: cosmo
code-line-numbers: true
number_examples: true
number_sections: true
number_chapters: true
linkcolor: blue
editor: visual
fig-cap-location: top
---
  
  ## Due: Sun. Oct. 1 2023 at 11:59 pm - submit on HuskyCT
  
  **Q1. (5 points).** Interpret this statement in your own words using a criterion you have studied: *The one-factor ANOVA* $F$-stat is very small, so that the chance of getting an $F$-statistic value which is as small or smaller than the $F$-stat obtained from the observed data is only $0.00004$.

In the context of a one-factor analysis of variance (ANOVA), if the F-statistic is very small, it typically means that there is not much variation among the group means compared to the variation within the groups. This could be indicate that the difference between the group means are not statistically.

A very small F-statistic in a one-factor ANOVA implies that the observed differences between group means may not be statistically significant, and there may not be strong evidence to conclude that there are meaningful differences among the groups being compared.

In this statement, it mention the change of getting an F-statistic value which is as small or smaller than the F-statistic obtained from the observed data is only 0.00004. Since a p value of 0.00004 is exceptionally small. In hypothesis testing, a small p-value like this indicates that the observed F-statistic is highly unlikely to have occurred due to random chance. Therefore, based on this criterion, the statement implies that the differences among the groups, as indicated by F-statistic are very likely to be genuine and significant rather than being due to random fluctuation in the data.


**Q2. (45 points).** Consider the dataset tensile from the R package *ACSWR*. You looked at the one-factor ANOVA $F$-test in HW3. Now, use suitable R functions to answer these questions.

(a) Obtain solutions of the effects of the levels of ***CWP*** in the one-factor ANOVA model you fit for the response.

```{r}
library(ACSWR)
library(ggplot2)
library("BSDA")
library("car")
library(gridExtra)
data(tensile)
CWPSize <- cut(tensile$CWP, breaks = quantile(tensile$CWP, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)),labels = c("A", "B", "C", "D", "E"), right = T,include.lowest = TRUE)
strength<- data.frame(logStr=log10(tensile$Tensile_Strength), tensile$CWP, CWPSize)
ggplot(strength, aes(x=CWPSize, y=logStr, fill=CWPSize)) +geom_boxplot(show.legend = F)
```
```{r}
aovmod <- aov(logStr ~ CWPSize, data = strength)
aovfits <- fitted(aovmod)
aovres <- residuals(aovmod)
car::qqPlot(aovres, main = NA, pch = 19, col = 2, cex = 0.7)
```
```{r}
leveneTest(logStr ~ CWPSize, data = strength)
```
```{r}
shapiro.test(aovres)
```
```{r}

summary(lm(aovmod))

```


In the HW3, we have divided CWPSize into 5 level  find out that all CWPSize have the equal variance based on the Levene Test since the p-value of that test is larger than the significant level (0.05) which we have no evidence to reject the Null hypothesis and concluded that the variance of 5 level of CWPSize is equal. So, even though the box plot is showed that the variance of 5 level of CWPSize are different, we still assume they have the equal variance.

In the Shapiro-Wilk normality test, we can see that the p-value is larger than the significant level (0.05), which suggested that the data is normally distributed and accepted the Null hypothesis. In the normal Q-Q plot also showed that the point of residual is almost fall along a straight line and lie in the confidence interval. Therefore, we can concluded that the residual is follow normal distribution.

The p-value from the  summary of anova test is smaller than the significant level (0.05) which provide sufficient evidence to reject the Null hypothesis and suggested that there are significant difference for the means of tensile strength in different level of CWPSize, at least one of the mean is different.

In the summary of anova test, we can see the effect of different CWP levels from the 'Estimate' column. The 'Estimate' column in this table contain the estimated effects of the level of CWP and showing how mean Tensile values for each level differ from each other. 

From the result, we can see CWPSize D has a relative higher estimate which represents the difference in the mean Tensile value between the group associated with CWP level D and the intercept. It suggests that Tensile is estimated to be 0.35954 unites higher than the Tensile value for the baseline level when all other factors are zero. CWPSize D has the largest effect among all the levels of CWP in the one-factor ANOVA model as it has the larger coefficients for the other levels

CWPSize E has the relative lower estimate which means that Tensile value for CWP level E is 0.04841 units higher than the Tensile value for the baseline level when all other factor are zero. Since the coefficient of CWPSize E is the smallest, it has the lowest effect among all the level of CWPSize.

(b) Obtain point estimates, and 95% C.I. estimates of the true means $\mu_i$ of the five groups.

```{r}
pred.CWPSize<-expand.grid(CWPSize=unique(strength$CWPSize))
lsmeans<-predict(aovmod, newdata=pred.CWPSize, se=TRUE, interval='confidence')
cbind(pred.CWPSize, lsmeans$fit)
```

From the result we can see that CWPSize D have the larger point estimates among the 5 levels of CWP. It means CWPSize D is estimated to have the highest average value for all the variables. The value in CWPSize D tend to have higher values compared to the other size.

CWPSize A has the smallest point estimate. It indicates that CWPSize A has the lowest values for the variable among 5 group.

In summary, CWPSize D has the largest point estimate among others signifies the CWPSize D with the highest estimated mean for the variable under investigation.


(c) Use the Bonferroni multiple comparison procedures to conduct pairwise tests of equality of the group means. Interpret the results.

```{r}
pairwise.t.test(strength$logStr, strength$CWPSize,p.adjust.method = "bonf")
```

The above test will provide pairwise comparisons between the levels of CWP along with p-values adjusted the Bonferroni correction. Each row in the output corresponds to a pairwise comparison between two levels of CWP.

In the result, we can see that the the group of AB, AC, AD, CE and DE, they all have the smaller p-value than the significant level(0.05), which suggests that there is a statistically significant difference between the means of the corresponding groups.

For the other group, they have greater p-value than the significant level (0.05), which indicated that there is no statistically significant difference between the means of the groups.


(d) Use the Tukey multiple comparison procedures to conduct pairwise tests of equality of the group means. Interpret the results.

```{r}
TukeyHSD(aovmod)
```

In the above test, it provide pairwise comparisons between the levels of CWP along with adjusted p-values. 

From the above result, the p-value of BA, CA, DA, EC and ED have smaller than the significant level (0.05), that indicated we can reject the Null and hypothesis and suggested that there is a statistically significant difference betwenn the means of the corresponding groups.

For the rest of the group, they all have the greater p-value than the significant level (0.05), which indicated that we have not enough evidence to reject the Null hypothesis and concluded that there is no statistically significant difference between the means of the groups.

(e) Compute and interpret a suitable effect size to see whether the levels of ***CWP*** have a practically meaningful effect on tensile strength.

```{r}
library(heplots)
anova_table <- anova(aovmod)
# Calculate the total sum of squares
#ss_total <- sum(anova_table$"Sum Sq")

# Calculate partial eta-squared (η²p)
#partial_eta_squared <- ss_cwp / ss_total

# View the result
#partial_eta_squared
partial_eta_squared2 <- etasq(aovmod)

# View the result
partial_eta_squared2
```

CWPSize under investigation has a very strong and dominant effect on the variation in the dependent variable because it has 0.7021125 of partial eta squared that indicates a relatively large effect size as it is very close 1. It also indicates that approximately 70.21% of the variance in the dependent variable, suggesting a strong relationship between the CWPSize and tensile strength. This effect is substantial from both statistical and practical perspectives

**Q3. (50 points)** Consider the ToothGrowth dataset from the R package *datasets*. The response is ***len***, the length of odontoblasts (cells responsible for tooth growth) in 60 guinea pigs. Each animal received one of three dose levels of vitamin C (0.5, 1, and 2 mg/day) denoted as the factor ***dose*** (Factor A) by one of two delivery methods, orange juice or ascorbic acid (a form of vitamin C and coded as VC), denoted as the factor ***supp*** (Factor B).

(a) Fit and interpret a one-factor ANOVA model relating the response ***len*** to ***dose*** (Factor A).

```{r}
data(ToothGrowth)
str(ToothGrowth)
```
```{r}
anova_model <- aov(len ~ dose, data = ToothGrowth)

summary(lm(anova_model))
```

In the above result of one-factor ANOVA model relating to the response odontoblasts and dose. The Null hypothesis (H0) and Alternative hypothesis are in the following:
  
  Null hypothesis: There are equal in the mean lengths of odontoblasts among the all the levels of dose. In other words, all three dose levels have the same mean length of odontoblasts.

Alternative hypothesis: There is at least one dose level for which the mean length of odontoblasts is signifcantly different from the others. There is a statistically significant effect of the dose factor on the length of odontoblasts.

First,  the F-statistic is 105.1 on 1 and 58 degree of freedom. This is a larget F-value, which suggests that the differences between the groups are more significant.Moreover, we can see that the p-value of one-factor is 1.233e-14, which is smaller than the significant level (0.05), we have sufficient evidence to reject the Null hypothesis and concluded that there are statistically significant differences in odontoblasts among the different dose levels. 


(b) Fit and interpret a one-factor ANOVA model relating the response ***len*** to ***supp*** (Factor B).
```{r}
anova_model <- aov(len ~ supp, data = ToothGrowth)

summary(lm(anova_model))
```

In the above result of one-factor ANOVA model relating to the response odontoblasts and supp. The Null hypothesis (H0) and Alternative hypothesis are in the following:
  
  Null hypothesis: There are equal in the mean lengths of odontoblasts among the different delivery methods of vitamin C. In other words, all delivery methods have the same mean length of odontoblasts.

Alternative hypothesis: There is at least one delivery method for which the mean length of odontoblasts is significantly different from the others. There is a statistically significant effect of the supp factor on the length of odontoblasts.

First,  the F-statistic is 3.668 on 1 and 58 degree of freedom. This is a quite small F-value, which suggests that the differences between the groups are not significant.Moreover, we can see that the p-value of one-factor is 0.06039, which is larger than the significant level (0.05), we have not sufficient evidence to reject the Null hypothesis and concluded that there are not statistically significant differences in odontoblasts among the different delivery methods (supp). 

(c) Fit and interpret an additive two-factor ANOVA model relating the response ***len*** to ***dose*** (Factor A) and ***supp*** (Factor B).
```{r}
anova_model <- aov(len ~ dose + supp, data = ToothGrowth)

summary(lm(anova_model))
```

**Null hypothesis **
  
  Factor A (dose): There are no significant differences in the mean lengths of odontoblasts among the different dose level of vitamin C. All dose levels have the same mean length of odontoblasts.

Factor B (supp): There are no significant differences in the mean length of odontoblasts between the two delivery methofs of vitamin C. Both delivery methods have the same mean length of odontablasts.

**Alternative hypothesis**
  
  Factor A (dose): At least one dose level has a different mean length of odontoblasts compared to the others. There is statistically significant effect of the dose factor on the length of odontoblasts.

Factor B (supp): The two delivery methods have different mean lengths of odontoblasts. There is a statistically significant effect of the supp factor on the length of odontoblasts

For the interaction Hypothesis:
  
  Interaction H0: There is no statistically significant interaction between dose and supp factors, which mean that the combined effect of both factors on the length of odontoblasts  is not significantly different from the sum of their individual effects.

Interaction H1: There is no statistically significant interaction between dose and supp factors, which mean that the combined effect of both factors on the length of odontoblasts is significantly different from the sum of their individual effects

From the result, the F-statistic is 67.72 on 2 and 57 degree of freedom, which indicate that they have more significant difference between len and the sum of dose and supp. The overall p-value is 8.716e-16. It is smaller than the significant level (0.05), which showed that we have enough evidence to reject the H0 and concluded that there is at least one of these factors has a statistically significant effect on len.


(d) Fit and interpret a two-factor ANOVA model with interaction relating the response ***len*** to ***dose*** (Factor A) and ***supp*** (Factor B).
```{r}
interaction_anova_model <- lm(aov(len ~ dose * supp, data = ToothGrowth))
summary(interaction_anova_model)
```

**Null hypothesis **
  
  Factor A (dose): There are no significant differences in the mean lengths of odontoblasts among the different dose level of vitamin C. All dose levels have the same mean length of odontoblasts.

Factor B (supp): There are no significant differences in the mean length of odontoblasts between the two delivery methofs of vitamin C. Both delivery methods have the same mean length of odontablasts.

**Alternative hypothesis**
  Factor A (dose): At least one dose level has a different mean length of odontoblasts compared to the others. There is statisticially significant effect of the dose factor on the length of odontoblasts.

Factor B (supp): The two delivery methods have different mean lengths of odontoblasts. There is a statistically significant effect of the supp factor on the length of odontoblasts

For the interaction Hypothesis:
  
  Interaction H0: There is no statistically significant interaction between dose and supp factors, which mean that the combined effect of both factors on the length of odontoblasts  is not significantly different from the sum of their individual effects.

Interaction H1: There is no statistically significant interaction between dose and supp factors, which mean that the combined effect of both factors on the length of odontoblasts is significantly different from the sum of their individual effects.

From the above result, we can see the p-value of the interaction term (dose*supp) is less than the significant level (0.05), which indicated a significant interaction between dose and supp. This means that the effect of one factor on len depends on the level of the other factor. Also, the p-value of dose and supp are also smaller than significant level(0.05), which suggested that each factor individually has a significant effect on len.

F-statistic 50.36, which we can suggested that there are more significantly difference for the factor of dose and supp, also there are significantly effects of the two factors together.

(e) Summarize what you learned from the analyses in (a) - (d).

In summary, the analyze shows that the dose level significantly affects odontoblast length and there are no statistically significant differences in odontoblast lengths among the different delivery methods (supp) when considering the main effect along.

When considering the interaction between dose and supp, there is evidence of a significant interaction, indicating that the combined effect of these factors on odontoblast length is different from what would be expected based on their individual effects.

In this analyses, it provide insights into how both dose and delivery method of vitamin C influence odontoblast growth, and the interaction suggests that the effect of dose may vary depending on the delivery method.


**Q4. Open question. Not graded for credit.** Choose an application domain that interests you. Find/download an interesting data set in this domain which lends to analysis using one-factor or tw-factor ANOVA analysis. Carry out a comprehensive analysis (testing assumptions, testing a suitable hypothesis, estimating effects etc.). Summarize as a report. You can share this on Discussion Board, and include on your Github page.
