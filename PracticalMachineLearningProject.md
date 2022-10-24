Practical Machine Learning Project
================
2022-10-22

## Background

*Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now
possible to collect a large amount of data about personal activity
relatively inexpensively. These type of devices are part of the
quantified self movement – a group of enthusiasts who take measurements
about themselves regularly to improve their health, to find patterns in
their behavior, or because they are tech geeks. One thing that people
regularly do is quantify how much of a particular activity they do, but
they rarely quantify how well they do it. In this project, your goal
will be to use data from accelerometers on the belt, forearm, arm, and
dumbell of 6 participants. They were asked to perform barbell lifts
correctly and incorrectly in 5 different ways. More information is
available from the website here:
<http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har#literature>
(see the section on the Weight Lifting Exercise Dataset).*

### I. Load Packages and Download Data

``` r
#Load Packages
library(caret)
```

    ## Loading required package: ggplot2

    ## Loading required package: lattice

``` r
library(randomForest)
```

    ## randomForest 4.7-1.1

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
library(rattle)
```

    ## Loading required package: tibble

    ## Loading required package: bitops

    ## Rattle: A free graphical interface for data science with R.
    ## Version 5.5.1 Copyright (c) 2006-2021 Togaware Pty Ltd.
    ## Type 'rattle()' to shake, rattle, and roll your data.

    ## 
    ## Attaching package: 'rattle'

    ## The following object is masked from 'package:randomForest':
    ## 
    ##     importance

``` r
#Download Data
training_url <- 'http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
testing_url <- 'http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'

training_csv <- read.csv(training_url,na.strings = c("NA",""))
testing_csv <- read.csv(testing_url,na.strings = c("NA",""))

dim(training_csv)
```

    ## [1] 19622   160

``` r
dim(testing_csv)
```

    ## [1]  20 160

### II. Cleaning the Data

From analysing the data, it is clear that there is an issue with NAs in
certain variables, which should be removed. Additionally, the first 7
variables are descriptive (name, time stamps, date) are not needed. We
are left with 53 variables. This a medium sized dataset so the 60% will
be allocated to training set and 40% to the testing (i.e. validation)
set.

    ## [1] 11776    53

### III. Random Forest

The first model we will try is a Random Forest. Using the validation
variable from the splicing, we observe that the cross validation
accuracy rate is 99% and out of sample error is less than 1%.

A `trainControl` funciton is used to improve the `caret` models.

    ## Random Forest 
    ## 
    ## 11776 samples
    ##    52 predictor
    ##     5 classes: 'A', 'B', 'C', 'D', 'E' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 9420, 9422, 9420, 9421, 9421 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa    
    ##    2    0.9889607  0.9860341
    ##   27    0.9896406  0.9868957
    ##   52    0.9857346  0.9819559
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was mtry = 27.

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2230   15    0    0    0
    ##          B    1 1501    6    1    1
    ##          C    1    2 1354   16    8
    ##          D    0    0    8 1269    5
    ##          E    0    0    0    0 1428
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9918          
    ##                  95% CI : (0.9896, 0.9937)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9897          
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9991   0.9888   0.9898   0.9868   0.9903
    ## Specificity            0.9973   0.9986   0.9958   0.9980   1.0000
    ## Pos Pred Value         0.9933   0.9940   0.9804   0.9899   1.0000
    ## Neg Pred Value         0.9996   0.9973   0.9978   0.9974   0.9978
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2842   0.1913   0.1726   0.1617   0.1820
    ## Detection Prevalence   0.2861   0.1925   0.1760   0.1634   0.1820
    ## Balanced Accuracy      0.9982   0.9937   0.9928   0.9924   0.9951

### V. Decision Tree

The second model we will try is a Decision Tree. Here the accuracy has
fallen to 49%.

![](PracticalMachineLearningProject_files/figure-gfm/Decision%20Tree-1.png)<!-- -->

    ## CART 
    ## 
    ## 11776 samples
    ##    52 predictor
    ##     5 classes: 'A', 'B', 'C', 'D', 'E' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 9420, 9421, 9420, 9422, 9421 
    ## Resampling results across tuning parameters:
    ## 
    ##   cp          Accuracy   Kappa     
    ##   0.03345990  0.5606318  0.44624403
    ##   0.04186047  0.4066936  0.19188738
    ##   0.11710963  0.3496999  0.09950626
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was cp = 0.0334599.

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1376  258   44   73   17
    ##          B  237  842   53  186  350
    ##          C  459  346 1087  673  357
    ##          D  152   72  184  354   80
    ##          E    8    0    0    0  638
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.5477          
    ##                  95% CI : (0.5366, 0.5587)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.4325          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.6165   0.5547   0.7946  0.27527  0.44244
    ## Specificity            0.9302   0.8695   0.7167  0.92561  0.99875
    ## Pos Pred Value         0.7783   0.5048   0.3720  0.42043  0.98762
    ## Neg Pred Value         0.8592   0.8906   0.9429  0.86693  0.88833
    ## Prevalence             0.2845   0.1935   0.1744  0.16391  0.18379
    ## Detection Rate         0.1754   0.1073   0.1385  0.04512  0.08132
    ## Detection Prevalence   0.2253   0.2126   0.3724  0.10732  0.08233
    ## Balanced Accuracy      0.7733   0.7121   0.7557  0.60044  0.72060

### VI. Gradient Boosted Trees/Model (GBM)

The final model is a GBM. Here the accuracy improves compared with
Decision Tree: 96% accuracy and 4% error.

    ## Stochastic Gradient Boosting 
    ## 
    ## 11776 samples
    ##    52 predictor
    ##     5 classes: 'A', 'B', 'C', 'D', 'E' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 9420, 9423, 9420, 9421, 9420 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  Accuracy   Kappa    
    ##   1                   50      0.7523775  0.6860236
    ##   1                  100      0.8195472  0.7715472
    ##   1                  150      0.8524104  0.8132029
    ##   2                   50      0.8565722  0.8183179
    ##   2                  100      0.9053999  0.8802706
    ##   2                  150      0.9304500  0.9119888
    ##   3                   50      0.8977553  0.8705619
    ##   3                  100      0.9382619  0.9218715
    ##   3                  150      0.9573699  0.9460633
    ## 
    ## Tuning parameter 'shrinkage' was held constant at a value of 0.1
    ## 
    ## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final values used for the model were n.trees = 150, interaction.depth =
    ##  3, shrinkage = 0.1 and n.minobsinnode = 10.

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2200   48    0    0    0
    ##          B   15 1417   34    3   17
    ##          C   16   51 1317   39   24
    ##          D    1    1   15 1230   23
    ##          E    0    1    2   14 1378
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9613          
    ##                  95% CI : (0.9567, 0.9654)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.951           
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9857   0.9335   0.9627   0.9565   0.9556
    ## Specificity            0.9914   0.9891   0.9799   0.9939   0.9973
    ## Pos Pred Value         0.9786   0.9536   0.9102   0.9685   0.9878
    ## Neg Pred Value         0.9943   0.9841   0.9920   0.9915   0.9901
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2804   0.1806   0.1679   0.1568   0.1756
    ## Detection Prevalence   0.2865   0.1894   0.1844   0.1619   0.1778
    ## Balanced Accuracy      0.9886   0.9613   0.9713   0.9752   0.9765

### VII. Prediction

Finally, we will use the models to predict 20 different test cases using
the original testing data. The results of the Random Forest and GBM
models were the same and were shown to be accurate the Course Project
Prediction Quiz. While the results of the Decision Tree were not
accurate, which the high out of sample error rate.

    ##  [1] B A B A A E D B A A B C B A E E A B B B
    ## Levels: A B C D E

    ##  [1] B A B A A E D B A A B C B A E E A B B B
    ## Levels: A B C D E

    ##  [1] D C C C B C C C A A C C B A C B D C D B
    ## Levels: A B C D E
