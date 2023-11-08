Show in New Window

Show in New Window

Shapiro-Wilk normality test

data:  aovres
W = 0.96762, p-value = 0.5854

Show in New Window
[1] 15  4
R Console


Show in New Window
Levene's Test for Homogeneity of Variance (center = median)
      Df F value Pr(>F)
group  4  0.9784 0.4414
      20               
Show in New Window

Call:
lm(formula = aovmod)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.17570 -0.05317  0.01244  0.05058  0.20371 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.97238    0.04390  22.150 1.52e-15 ***
CWPSizeB     0.20749    0.06208   3.342 0.003247 ** 
CWPSizeC     0.27045    0.06208   4.356 0.000306 ***
CWPSizeD     0.35954    0.06208   5.791 1.15e-05 ***
CWPSizeE     0.04841    0.06208   0.780 0.444660    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.09816 on 20 degrees of freedom
Multiple R-squared:  0.7021,	Adjusted R-squared:  0.6425 
F-statistic: 11.78 on 4 and 20 DF,  p-value: 4.413e-05

Show in New Window

	Pairwise comparisons using t tests with pooled SD 

data:  strength$logStr and strength$CWPSize 

  A       B       C       D      
B 0.03247 -       -       -      
C 0.00306 1.00000 -       -      
D 0.00011 0.23662 1.00000 -      
E 1.00000 0.18578 0.01889 0.00067

P value adjustment method: bonferroni 
Show in New Window
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = logStr ~ CWPSize, data = strength)

$CWPSize
           diff         lwr         upr     p adj
B-A  0.20748678  0.02171012  0.39326343 0.0240014
C-A  0.27045154  0.08467489  0.45622820 0.0025243
D-A  0.35953504  0.17375838  0.54531169 0.0001016
E-A  0.04841043 -0.13736623  0.23418708 0.9335226
C-B  0.06296476 -0.12281189  0.24874142 0.8458538
D-B  0.15204826 -0.03372839  0.33782492 0.1430234
E-B -0.15907635 -0.34485301  0.02670030 0.1162074
D-C  0.08908350 -0.09669316  0.27486015 0.6135721
E-C -0.22204112 -0.40781777 -0.03626446 0.0144321
E-D -0.31112461 -0.49690127 -0.12534796 0.0005746

Show in New Window
Description:df [2 × 1]
 
 
Partial eta^2
<dbl>
CWPSize	0.7021125			
Residuals	NA			
2 rows
Show in New Window
'data.frame':	60 obs. of  3 variables:
 $ len : num  4.2 11.5 7.3 5.8 6.4 10 11.2 11.2 5.2 7 ...
 $ supp: Factor w/ 2 levels "OJ","VC": 2 2 2 2 2 2 2 2 2 2 ...
 $ dose: num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
Show in New Window

Call:
lm(formula = anova_model)

Residuals:
    Min      1Q  Median      3Q     Max 
-8.4496 -2.7406 -0.7452  2.8344 10.1139 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   7.4225     1.2601    5.89 2.06e-07 ***
dose          9.7636     0.9525   10.25 1.23e-14 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.601 on 58 degrees of freedom
Multiple R-squared:  0.6443,	Adjusted R-squared:  0.6382 
F-statistic: 105.1 on 1 and 58 DF,  p-value: 1.233e-14

Show in New Window

Call:
lm(formula = anova_model)

Residuals:
     Min       1Q   Median       3Q      Max 
-12.7633  -5.7633   0.4367   5.5867  16.9367 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   20.663      1.366  15.127   <2e-16 ***
suppVC        -3.700      1.932  -1.915   0.0604 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 7.482 on 58 degrees of freedom
Multiple R-squared:  0.05948,	Adjusted R-squared:  0.04327 
F-statistic: 3.668 on 1 and 58 DF,  p-value: 0.06039


R version 4.3.1 (2023-06-16 ucrt) -- "Beagle Scouts"
Copyright (C) 2023 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Workspace loaded from ~/.RData]


R version 4.3.1 (2023-06-16 ucrt) -- "Beagle Scouts"
Copyright (C) 2023 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Workspace loaded from ~/.RData]

> library(ACSWR)
> data(tensile)
> CWPSize <- cut(tensile$CWP, breaks = quantile(tensile$CWP, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)),labels = c("A", "B", "C", "D", "E"), right = T,include.lowest = TRUE)
> strength<- data.frame(logStr=log10(tensile$Tensile_Strength), tensile$CWP, CWPSize)
> library(ACSWR)
> data(tensile)
> CWPSize <- cut(tensile$CWP, breaks = quantile(tensile$CWP, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)),labels = c("A", "B", "C", "D", "E"), right = T,include.lowest = TRUE)
> ggplot(strength, aes(x=CWPSize, y=logStr, fill=CWPSize)) +geom_boxplot(show.legend = F)
Error in ggplot(strength, aes(x = CWPSize, y = logStr, fill = CWPSize)) : 
  could not find function "ggplot"
> library(ACSWR)
> library(ggplot2)
> data(tensile)
> CWPSize <- cut(tensile$CWP, breaks = quantile(tensile$CWP, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)),labels = c("A", "B", "C", "D", "E"), right = T,include.lowest = TRUE)
> ggplot(strength, aes(x=CWPSize, y=logStr, fill=CWPSize)) +geom_boxplot(show.legend = F)
> leveneTest(logStr ~ CWPSize, data = strength)
Error in leveneTest(logStr ~ CWPSize, data = strength) : 
  could not find function "leveneTest"
> library(ACSWR)
> data(tensile)
> CWPSize <- cut(tensile$CWP, breaks = quantile(tensile$CWP, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)),labels = c("A", "B", "C", "D", "E"), right = T,include.lowest = TRUE)
> table(CWPSize)
CWPSize
A B C D E 
5 5 5 5 5 
> 
> strength<- data.frame(logStr=log10(tensile$Tensile_Strength), tensile$CWP, CWPSize)
> strength<- data.frame(logStr=log10(tensile$Tensile_Strength), tensile$CWP, CWPSize)
> leveneTest(logStr ~ CWPSize, data = strength)
Error in leveneTest(logStr ~ CWPSize, data = strength) : 
  could not find function "leveneTest"
> library("BSDA")
Loading required package: lattice

Attaching package: ‘BSDA’

The following object is masked _by_ ‘.GlobalEnv’:

    Stress

The following object is masked from ‘package:datasets’:

    Orange

> library("car")
Loading required package: carData

Attaching package: ‘carData’

The following object is masked _by_ ‘.GlobalEnv’:

    States

The following objects are masked from ‘package:BSDA’:

    Vocab, Wool

> library(gridExtra)
> library(ggplot2)
> ggplot(strength, aes(x=CWPSize, y=logStr, fill=CWPSize)) +geom_boxplot(show.legend = F)
> leveneTest(logStr ~ CWPSize, data = strength)
Levene's Test for Homogeneity of Variance (center = median)
Df F value Pr(>F)
group  4  0.9784 0.4414
20               
> bartlett.test(logStr ~ CWPSize, data = strength)

Bartlett test of homogeneity of variances

data:  logStr by CWPSize
Bartlett's K-squared = 5.1758, df = 4, p-value = 0.2697

> aovmod <- aov(logStr ~ CWPSize, data = strength)
> summary(lm(aovmod))

Call:
lm(formula = aovmod)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.17570 -0.05317  0.01244  0.05058  0.20371 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.97238    0.04390  22.150 1.52e-15 ***
CWPSizeB     0.20749    0.06208   3.342 0.003247 ** 
CWPSizeC     0.27045    0.06208   4.356 0.000306 ***
CWPSizeD     0.35954    0.06208   5.791 1.15e-05 ***
CWPSizeE     0.04841    0.06208   0.780 0.444660    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.09816 on 20 degrees of freedom
Multiple R-squared:  0.7021,	Adjusted R-squared:  0.6425 
F-statistic: 11.78 on 4 and 20 DF,  p-value: 4.413e-05

> aovfits <- fitted(aovmod)
> aovres <- residuals(aovmod)
> strength<- data.frame(logStr=log10(tensile$Tensile_Strength), tensile$CWP, CWPSize)
> strength<- data.frame(logStr=log10(tensile$Tensile_Strength), tensile$CWP, CWPSize)
> car::qqPlot(aovres, main = NA, pch = 19, col = 2, cex = 0.7)
[1] 15  4
> shapiro.test(aovres)

	Shapiro-Wilk normality test

data:  aovres
W = 0.96762, p-value = 0.5854

> leveneTest(logStr ~ CWPSize, data = strength)
Levene's Test for Homogeneity of Variance (center = median)
Df F value Pr(>F)
group  4  0.9784 0.4414
20               
> aovmod <- aov(logStr ~ CWPSize, data = strength)
> summary(lm(aovmod))

Call:
  lm(formula = aovmod)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.17570 -0.05317  0.01244  0.05058  0.20371 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.97238    0.04390  22.150 1.52e-15 ***
  CWPSizeB     0.20749    0.06208   3.342 0.003247 ** 
  CWPSizeC     0.27045    0.06208   4.356 0.000306 ***
  CWPSizeD     0.35954    0.06208   5.791 1.15e-05 ***
  CWPSizeE     0.04841    0.06208   0.780 0.444660    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.09816 on 20 degrees of freedom
Multiple R-squared:  0.7021,	Adjusted R-squared:  0.6425 
F-statistic: 11.78 on 4 and 20 DF,  p-value: 4.413e-05

> 
  > pairwise.t.test(strength$logStr, strength$CWPSize,p.adjust.method = "bonf")

Pairwise comparisons using t tests with pooled SD 

data:  strength$logStr and strength$CWPSize 

A       B       C       D      
B 0.03247 -       -       -      
  C 0.00306 1.00000 -       -      
  D 0.00011 0.23662 1.00000 -      
  E 1.00000 0.18578 0.01889 0.00067

P value adjustment method: bonferroni 
> TukeyHSD(aovmod)
Tukey multiple comparisons of means
95% family-wise confidence level

Fit: aov(formula = logStr ~ CWPSize, data = strength)

$CWPSize
diff         lwr         upr     p adj
B-A  0.20748678  0.02171012  0.39326343 0.0240014
C-A  0.27045154  0.08467489  0.45622820 0.0025243
D-A  0.35953504  0.17375838  0.54531169 0.0001016
E-A  0.04841043 -0.13736623  0.23418708 0.9335226
C-B  0.06296476 -0.12281189  0.24874142 0.8458538
D-B  0.15204826 -0.03372839  0.33782492 0.1430234
E-B -0.15907635 -0.34485301  0.02670030 0.1162074
D-C  0.08908350 -0.09669316  0.27486015 0.6135721
E-C -0.22204112 -0.40781777 -0.03626446 0.0144321
E-D -0.31112461 -0.49690127 -0.12534796 0.0005746

> library(heplots)
Loading required package: broom
Registered S3 method overwritten by 'htmlwidgets':
  method           from         
print.htmlwidget tools:rstudio
> anova_table <- anova(aovmod)
> 
  > # Extract the sum of squares for CWP
  > ss_cwp <- anova_table$"Sum Sq"[1]
> 
  > eta_sqaured<-sum(anova_table$Sum[1:length(anova_table$Sum)-1]^2)/sum(anova_table$Sum^2)
> 
  > eta_sqaured
[1] 0.8474521
> 
  > 
  > # Calculate the total sum of squares
  > ss_total <- sum(anova_table$"Sum Sq")
> 
  > # Calculate partial eta-squared (η²p)
  > partial_eta_squared <- ss_cwp / ss_total
> 
  > # View the result
  > partial_eta_squared
[1] 0.7021125
> partial_eta_squared2 <- etasq(aovmod)
> 
  > # View the result
  > partial_eta_squared2
> library(heplots)
> anova_table <- anova(aovmod)
> # Calculate the total sum of squares
  > #ss_total <- sum(anova_table$"Sum Sq")
  > 
  > # Calculate partial eta-squared (η²p)
  > #partial_eta_squared <- ss_cwp / ss_total
  > 
  > # View the result
  > #partial_eta_squared
  > partial_eta_squared2 <- etasq(aovmod)
> 
  > # View the result
  > partial_eta_squared2
> data(ToothGrowth)
> str(ToothGrowth)
'data.frame':	60 obs. of  3 variables:
  $ len : num  4.2 11.5 7.3 5.8 6.4 10 11.2 11.2 5.2 7 ...
$ supp: Factor w/ 2 levels "OJ","VC": 2 2 2 2 2 2 2 2 2 2 ...
$ dose: num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
> data(ToothGrowth)
> str(ToothGrowth)
'data.frame':	60 obs. of  3 variables:
  $ len : num  4.2 11.5 7.3 5.8 6.4 10 11.2 11.2 5.2 7 ...
$ supp: Factor w/ 2 levels "OJ","VC": 2 2 2 2 2 2 2 2 2 2 ...
$ dose: num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
> anova_model <- aov(len ~ dose, data = ToothGrowth)
> 
  > summary(lm(anova_model))

Call:
  lm(formula = anova_model)

Residuals:
  Min      1Q  Median      3Q     Max 
-8.4496 -2.7406 -0.7452  2.8344 10.1139 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   7.4225     1.2601    5.89 2.06e-07 ***
  dose          9.7636     0.9525   10.25 1.23e-14 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.601 on 58 degrees of freedom
Multiple R-squared:  0.6443,	Adjusted R-squared:  0.6382 
F-statistic: 105.1 on 1 and 58 DF,  p-value: 1.233e-14

> anova_model <- aov(len ~ supp, data = ToothGrowth)
> 
  > summary(lm(anova_model))

Call:
  lm(formula = anova_model)

Residuals:
  Min       1Q   Median       3Q      Max 
-12.7633  -5.7633   0.4367   5.5867  16.9367 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   20.663      1.366  15.127   <2e-16 ***
  suppVC        -3.700      1.932  -1.915   0.0604 .  
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 7.482 on 58 degrees of freedom
Multiple R-squared:  0.05948,	Adjusted R-squared:  0.04327 
F-statistic: 3.668 on 1 and 58 DF,  p-value: 0.06039

> library(ACSWR)
> library(ggplot2)
> data(tensile)
> CWPSize <- cut(tensile$CWP, breaks = quantile(tensile$CWP, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)),labels = c("A", "B", "C", "D", "E"), right = T,include.lowest = TRUE)
> ggplot(strength, aes(x=CWPSize, y=logStr, fill=CWPSize)) +geom_boxplot(show.legend = F)
> strength<- data.frame(logStr=log10(tensile$Tensile_Strength), tensile$CWP, CWPSize)
> car::qqPlot(aovres, main = NA, pch = 19, col = 2, cex = 0.7)
[1] 15  4
> leveneTest(logStr ~ CWPSize, data = strength)
Levene's Test for Homogeneity of Variance (center = median)
      Df F value Pr(>F)
group  4  0.9784 0.4414
      20               
> shapiro.test(aovres)

	Shapiro-Wilk normality test

data:  aovres
W = 0.96762, p-value = 0.5854

> aovmod <- aov(logStr ~ CWPSize, data = strength)
> summary(lm(aovmod))

Call:
lm(formula = aovmod)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.17570 -0.05317  0.01244  0.05058  0.20371 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.97238    0.04390  22.150 1.52e-15 ***
CWPSizeB     0.20749    0.06208   3.342 0.003247 ** 
CWPSizeC     0.27045    0.06208   4.356 0.000306 ***
CWPSizeD     0.35954    0.06208   5.791 1.15e-05 ***
CWPSizeE     0.04841    0.06208   0.780 0.444660    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.09816 on 20 degrees of freedom
Multiple R-squared:  0.7021,	Adjusted R-squared:  0.6425 
F-statistic: 11.78 on 4 and 20 DF,  p-value: 4.413e-05

> 
> 