
#**Question 1**
  
#**1(a)**

library(PairedData)
data("PrisonStress")
str(PrisonStress)
head(PrisonStress)
#PrisonStress[,3]


# The NULL hypothese: There is no significant difference in average stress level between before sport training and after sport training. 
#The average stress level of them at the time of their entry into a prison is same as at the time of exit.
 
# The alternative hypothesis: There is significant difference in average stress level between before sport training and after sport training. 
#The average stress level of them at the time of their entry into a prison is different to at the time of exit.
 
# H0:ud equal to 0
# H1:ud not equal to 0

##Test the normality of average stress level between before sport training and after sport training.

library(ggplot2)
combine<-stack(PrisonStress[,3:4])
colnames(combine)<-c("Value","PSS")
ggplot(combine, aes(sample=Value, shape=PSS, color=PSS))+stat_qq()+stat_qq_line()



# To find out if two populations are normal, I have plot a line chart to see if the data points lie on the diagonal. In the graph, 
#we can see that most of the points are lie on the line and prove the normality. We can also do the Shapiro-Wilk significance test to have the accurate value to prove the normality.
 
# The Shapiro-Wilk significance test gives p-value of 0.330 and 0.618, supporting the assumption of normality in both population. The group of 
#PSS before and PSS after are normally distributed.

shapiro.test(PrisonStress[,3])$p.value

shapiro.test(PrisonStress[,4])$p.value

#The side-by-side box plot show that the spreads in the two groups are approximately the same. There are only the size of the box have slightly 
#different, which showed that PSSbefore have more spread data in the dataset. And the median of both group are similar around 21.
 
ggplot(combine, aes(x=PSS, y=Value, fill=PSS))+geom_boxplot(show.legend=F)
 

##Use F test for checking H0: sqr(σ1)=sqr(σ2) versus H1: sqr(σ1) not= sqr(σ2)

 
# F-test for equal variance between PSSbefore and PSSafter
var.test(PrisonStress[,3], PrisonStress[,4])
 
#In the F-test, the p-value is 0.140, which is lager than the alpha (0.05). Therefore, we have no sufficient evidence to reject the null 
#hypothesis and accept the null hypothesis. We concluded that group of PSSbefore and PSSafter have the equal variance.


#From the above result, we can see that the data of average stress level of before training and after training are normally distributed. And 
#according to the F-test, there is same variance between group. Therefore, the suitable paired procedure is Pooled t Procedure. Because pooled 
#t-test has assumption in normality and equal variance, but pooled t-test is the most powerful test among the three testes. Therefore, if the two 
#datasets is satisfy the assumption of normality and equal variance. It would be the best test to get a more accurate result. 


 
sport_data <- subset(PrisonStress, Group == "Sport")
result<-t.test(sport_data$PSSbefore,sport_data$PSSafter, mu=0, alternative = "two.side", var.equal=TRUE)
print(result)
 

# As we can see in the above result, the paired t-statistic for testing H0 versus H1 is 1.4954 with n-1 = 28 d.f. The mean of PSSbefore from 
#sport group is 23.933 and the mean of PSSafter from sport group is 20. The p-value is 0.146.
# 
# The 95% C.I. for ud is (-1.45, 9.32). We conclude that the data provide insufficient evidence to reject H0 at level of significant alpha = 0.05,
#so to accept the null hypothesis, the mean difference of average stress level is equal to 0. There is no significant difference in average 
#stress level between sport training and after sport training.


#**1(b)**
  
#In the following, I will subset the data of group Sport and Control for identify the dataset easily in this section.

 
str(PrisonStress)
subset_data<-PrisonStress[c("Group", "PSSafter")]
stacked_data <- stack(subset_data)
colnames(stacked_data) <- c("Group", "PSSafter")
 

##Use ggplot to test the normality of both groups
 
library(ggplot2)
ggplot(subset_data, aes(sample=PSSafter,shape=Group, color=Group)) + stat_qq()+ stat_qq_line()
 
 
library(ggplot2)
library(car)
sport_data <- PrisonStress$PSSafter[PrisonStress$Group == "Sport"]
control_data <- PrisonStress$PSSafter[PrisonStress$Group == "Control"]
par(mfrow = c(1, 2))
qqPlot(sport_data, main = NA, pch = 19, col = 2, cex = 0.7)
qqPlot(control_data, main = NA, pch = 19, col = 2, cex = 0.7)
 
#We can see that the graph in both group, the data point in lie within the region and most of them are lie on the diagonl line, which indicated 
#that both group are normally distributed.


##Use the shapiro test to test the normality of two groups
#From the Shapiro test, as both of the p-value is greater than the alpha value (0.05), which are 0.905 and 0.579, we have not enough evidence to
#reject the null hypothesis. We accepted the null hypothesis and suggested that group of Sport and Control are approximately normally distributed.

shapiro.test(sport_data)$p.value
shapiro.test(control_data)$p.value
 

#In the box plot, we found that the interquartile from the two groups are different. The box of the Control group is higher than the Sports group.
#For example, the median of the Control group is 26 and the median of the sports group is 21.

library(ggplot2)
ggplot(subset_data, aes(x=Group, y=PSSafter, fill=Group)) + geom_boxplot(show.legend = F)
 

sport_summary <- summary(PrisonStress$PSSafter[PrisonStress$Group == "Sport"])

# Calculate summary statistics for the "Control" group
control_summary <- summary(PrisonStress$PSSafter[PrisonStress$Group == "Control"])

cat("Summary statistics for the Sport group:\n")
print(sport_summary)

cat("\nSummary statistics for the Control group:\n")
print(control_summary)
 

#We can see that in the summmary of statistic, the mode of two group is similar. However, the median of them are different. One is 21, 
#another one is 26. The Control group get the higher mean than Sport group which are 23.73 and 20. Both group get the same maxmum value but 
#the similar minimum value and the interquartile of Control group is higher than Sport group. 



#**1(c)** 
  
#   To know if the spread and the variance equal in both group, it can be used box plot and F-test to verify it.
# 
# For the boxplot, we can see that the side-by-side box plot show that the spreads in the two groups are quite different. For example, 
# they have different median and different interquartile value. It suggested that they have different variance.
# 
# The variance of two populations are the same. In the F-test, we can see the p-value is 0.895, which is much larger than the alpha (0.05). 
#Therefore, we have insufficient evidence to reject H0 and conclude that they have the equal population variance.

 
library(ggplot2)
ggplot(subset_data, aes(x=Group, y=PSSafter, fill=Group)) + geom_boxplot(show.legend = F)
 


 
sport_data <- PrisonStress$PSSafter[PrisonStress$Group == "Sport"]
control_data <- PrisonStress$PSSafter[PrisonStress$Group == "Control"]

# Perform the F-test to compare variances
var.test(sport_data, control_data)
 


#**1(d)**
  
#   In this question,we need to setup the NULL hypothesis and alternative hypothesis. 
# 
# **NULL hypothesis**: The mean stress levels between Sport and Control at exit are the same.
# 
# **Alternatvie hypothesis**: The mean stress level between Sport and Control at exit are different.

 
sport_data <- PrisonStress$PSSafter[PrisonStress$Group == "Sport"]
control_data <- PrisonStress$PSSafter[PrisonStress$Group == "Control"]

# Perform a two-sample t-test
t_test_result <- t.test(sport_data, control_data)

# Print the test result
print(t_test_result)
 

#From the above result, the t-statistic is -1.3361. At the 95% C.I. (-9.523,2.069), the mean of Sport group is 20 and the mean of control group 
#is 23.72. The p-value is 0.1956, which is greater than the alpha (0.05). We can insufficient evidence to reject H0 and accept the Null 
#hypothesis. We concluded that the mean of Sport data is equal to the mean of the Control data.


#**1(e)**
   
# Cohen's d
library(effsize)
cohen.d(sport_data,control_data,pooled = TRUE)
 

#In the result, we can see that the effect size of sport group and control group is -0.533. They have the medium effect size, which suggests a 
#moderate effect size, indicating a noticeable difference between the two groups that has practical relevance.

#**Question 2**
  
#First, I have load the Loblolly data into RStudio and print out the structure of the dataset.

 
data("Loblolly")
str(Loblolly)
 

#I have create two lists, which are age_20 and age_25, that can more easily to classify in the following section.

 
age_20 <- Loblolly$height[Loblolly$age == 20]
age_25 <- Loblolly$height[Loblolly$age == 25]
 

#In the histogram, we can see that the height distribution is different from two group. In age 20, the height is distributed around 48 to 56 and
#52-54 get the highest frequency. However, in age 25, the height is distributed around 56-66 and 60-62 get the highest frequency in the group.

 
par(mfrow = c(1, 2))  # Set up a 1x2 grid for plots
hist(age_20, main = "Height Distribution (Age 20)", xlab = "Height")
hist(age_25, main = "Height Distribution (Age 25)", xlab = "Height")
 

#We can look at the box plot, same as the result in histogram, both group have different distribution. Age 20 is distributed around 48-56. 
#Age 25 is distributed around 56-64. The median of Age 20 is around 52 and the median of Age 25 is 60, which is have a big difference with each 
#other. Moreover, the box size of two group is different. Age 20 apparently have a larger box size than Age 25. It indicated that the data in 
#Age 20 is more spread than Age 25. 

 
par(mfrow = c(1, 2))
boxplot(age_20, main = "Boxplot of Height (Age 20)", ylab = "Height")

boxplot(age_25, main = "Boxplot of Height (Age 25)", ylab = "Height")
 

# In the summary, we can have the more accurate data for age 20 and age 25. First of all, the median of age 20 is 51.4 and that of age 25 is 
#60.17. The maximum and minimum of both group are also have a big difference. The maximum of age 20 is 55.82 but of age 25 is 64.10. The minimum
#of age 20 is 48.31 but of age 25 is 56.43. Finally, we can see the mean of both group are 51.47 and 60.29.

 
summary(age_20)
 
 
summary(age_25)
 
 
library(moments)
skewness(age_20)
 
 
skewness(age_25)
 

# The skewness of age 20 is 0.219175 and of age 25 is 0.00567.
# In the normal Q-Q plot, we can see that both of them, most of the points are lie on the diagonal line, which can prove that both group are 
#approximately normally distributed. And the shapiro test has also give the evidence to prove the normality.

 
par(mfrow=c(1,2))

qqnorm(age_20, main = "Normal Q-Q Plot (Age 20)")
qqline(age_20, col = 2) 


qqnorm(age_25, main = "Normal Q-Q Plot (Age 25)")
qqline(age_25, col = 2)  
 

# The shapiro test can test the normality of two samples. The p-value of aged 20 and aged 25 are 0.8528 and 0.855, which is greater than the 
#alpha value (0.05). It proves that both of them are normality distributed. 

 
shapiro.test(age_20)
 
 
shapiro.test(age_25)
 

# Next, we need to have the F-test to see if two groups have the equal variace to determine which t-procedure is the most suitable for these 
#two groups. 

# H0: The variance of age 20 is equal to the variance of age 25.
# H1: The variance of age 20 is not equal to the variance of age 25.

 
var.test(age_20, age_25)
 

# We can see that the p-value is 0.9283 which is larger than the alpha (0.05). It provides insufficient evidence to reject the Null hypothesis 
#and we accept the Null hypothesis. We can concluded that the variance of age 20 and the variance of age 25 is equal. 

# Since both group have the normality and equal variance, we can decided to use Pooled t-test to test if the mean of age 20 is same as age 25. 
#The following is the Null hypothesis and Alternative hypothesis:
  
#   H0: μd1 =μd2
# The average height of trees aged 20 years and 25 years are equal
# 
# H1: μd1 NOT = μd2
# The average height of trees aged 20 years and 25 years are not equal

 
t_test_result <- t.test(age_25, age_20, paired=TRUE)

# Print the test result
print(t_test_result)
 
# In the Paired t-test result, the sample difference is 8.82. The paired t-statistic for testing H0: μd1 =μd2 verse H1:μd1 NOT = μd2 is 39.127 
#with n-1 = 13 d.f.The p-value is 7.125e-15, which is smaller than the alpha (0.05)
# 
# The 95% C.I. for μd is (8.3337, 9.3077). We conclude that the data provide enough evidence to reject H0 at level of significant alpha = 0.05, 
#so the average height is significant different between aged 20 and aged 25.


#**Question 3**
  
#   If the pooled t-test is suitable to use for two independent samples that to compare the averages of the population, Wilcoxon's rank-sum 
#test will also be suitable for compare the averages for two independent samples, but not recommended. 
# 
# It is also suitable because there are two assumptions for pooled t test which are normality and equal variance. Therefore, it indicates 
#that the two sample are normally distributed and have equal variance. For the Welch's rank-sum test, it is a non-parametric test that does not 
#assume normality or equal variance, but only the two independent samples must have the same shape. 
# 
# However, Wilcoxon's rank-sum test might not be the most appropriate choice for comparing the two independent samples because the pooled t-test 
#is typically the more powerful test for comparing means. Pooled t-test makes full use of the information in the data regarding the means and 
#variances of the population and to provide more precise estimates of the population means. When the sample sizes are sufficiently largest, 
#the pooled t-test can be quite robust to departures from normality. Moreover, pooled t-test provides straightforward and easily interpretable 
#results in terms of means and standard deviation. It making it a practical choice for communicating findings.
# 
# In the following, I will show an example using BloodLead dataset and to determine if the mean of exposed group is same as that of control. 
# 
# H0: the mean of Exposed group is equal to the mean of Control group. There is no significant difference between two groups
# H1: The mean of Exposed group is different to the mean of Control group. There is significant difference between two groups

 
data(BloodLead)
lead <- stack(BloodLead[,2:3])
colnames(lead) <- c("Lead", "Group")
t.test(Lead ~ Group, data=lead, alternative = c("greater"),var.equal = TRUE)
 

 
exposed_data <- lead$Lead[lead$Group == "Exposed"]
control_data <- lead$Lead[lead$Group == "Control"]
wilcox.test(exposed_data, control_data, paired = F, alternative = "greater")
 

# We can see that both of two tests suggested there is enough evidence to prove that the mean of Exposed group is not equal to the mean of 
#Control group because the p-value of both testes are also smaller than the alpha (0.05), which is fail to reject null hypothesis and support 
#the alternative hypothesis that there is significant difference between them.
# 
# In the result of both test, it is clearly that the p-value Wilcoxon's rank sum test is similar with the p-value in Pooled t-test. However, 
#there are provide more information in the pooled t-test result such as the t-statistic, the mean of both group and the mean at 95% C.I., which 
#proved that pooled t-test have full use of information in the data. And the Wilcoxon's rank sum test only provide less information in the result
# 
# Therefore, if there are two independent sample that need one of the test to compare the average of the population, Pooled t-test is generally 
#recommended because of its greater statistical power and interpretability.It is the most powerful test for compared two independent samples.



# **Question 4**
# 
# In the article "A Defense of P-value, it talked about why P value is so popular and unpopular with some Statistical Theorists. It is 
#popular because it provides an extraordinarily versatile and simple framework for making a statistical inference. There are thousands of 
#different tyes of statistical tests are in widespread use. However, P-value is unpopular with somebody because they through p-value will 
#leads to a binary decision. It is one of the main concern of Statistical Theorists as it oversimplifies what shouldbe a judgement about strength 
#of evidence rather than an conclusion. Moreover, the significant testing also encourage false-negative interpretations of statistical test. 
#Therefore, statistical tests are frequently misapplied and misinterpreted by users. 
# 
# I think as an emerging data scientist, it is citical for us to have a more nuanced and careful approach to statistical significance. As the 
#article explained that P-value have traditionally been used as a binary measure to determine whether an observed result is significant or not. 
#However, it is important to recognize their limitations, especially wen dealing with complex data and multiple comparisons.
# 
# On the other hand, replication and transparency in statistical methodologies are paramount. Researchers must clearly report their methods, 
#disclose any potential biases and consider alternative statistical approaches for the better result. In conclude, the p-value controversy 
#underscores the necessity of a more thoughtful and comprehensive approach to statistical analysis to ensure that results re not only 
#statistically significant but also practically meaningful and reproducible.
# 





