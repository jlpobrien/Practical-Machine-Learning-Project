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

``` r
#Removing NAs
#sum(is.na(training_csv))

training_1 <- training_csv[,colSums(is.na(training_csv))==0]
testing_1 <- testing_csv[,colSums(is.na(testing_csv))==0] 

#sum(is.na(training_1))

#Removing Unneeded Variables

training_2 <- training_1[,-c(1:7)]
testing_2 <- testing_1[,-c(1:7)]

#Splicing the Data

inTrain <- createDataPartition(y=training_2$classe, p=0.6, list=FALSE)
training <- training_2[inTrain,]
testing <- training_2[-inTrain,]

dim(training)
```

    ## [1] 11776    53

### III. Random Forest

The first model we will try is a Random Forest. Using the validation
variable from the splicing, we observe that the cross validation
accuracy rate is 99% and out of sample error is less than 1%.

A `trainControl` funciton is used to improve the `caret` models.

``` r
#Control variable
fitControl <- trainControl(method = "cv", number = 5)

#Random Forest Model

modFitRF <- train(classe~., data=training, method="rf", trControl=fitControl, ntree=250)

#Confusion Matrix
predRF <- predict(modFitRF, testing)
cmRF <- confusionMatrix(predRF, factor(testing$classe))

modFitRF
```

    ## Random Forest 
    ## 
    ## 11776 samples
    ##    52 predictor
    ##     5 classes: 'A', 'B', 'C', 'D', 'E' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 9420, 9420, 9421, 9422, 9421 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa    
    ##    2    0.9882812  0.9851745
    ##   27    0.9898097  0.9871080
    ##   52    0.9802130  0.9749660
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was mtry = 27.

``` r
cmRF
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2225   14    0    0    0
    ##          B    3 1500    7    2    0
    ##          C    2    4 1356   13    7
    ##          D    0    0    5 1271    3
    ##          E    2    0    0    0 1432
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9921          
    ##                  95% CI : (0.9899, 0.9939)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.99            
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9969   0.9881   0.9912   0.9883   0.9931
    ## Specificity            0.9975   0.9981   0.9960   0.9988   0.9997
    ## Pos Pred Value         0.9937   0.9921   0.9812   0.9937   0.9986
    ## Neg Pred Value         0.9988   0.9972   0.9981   0.9977   0.9984
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2836   0.1912   0.1728   0.1620   0.1825
    ## Detection Prevalence   0.2854   0.1927   0.1761   0.1630   0.1828
    ## Balanced Accuracy      0.9972   0.9931   0.9936   0.9936   0.9964

### V. Decision Tree

The second model we will try is a Decision Tree. Here the accuracy has
fallen to 49%.

``` r
modFitDT <- train(classe~., data=training, method="rpart", trControl=fitControl)

fancyRpartPlot(modFitDT$finalModel, main="Weight Lifting Exercise Decision Tree", sub="")
```

![](PracticalMachineLearningProject_files/figure-gfm/Decision%20Tree-1.png)<!-- -->

``` r
predDT <- predict(modFitDT, testing)
cmDT <- confusionMatrix(predDT, factor(testing$classe))

modFitDT
```

    ## CART 
    ## 
    ## 11776 samples
    ##    52 predictor
    ##     5 classes: 'A', 'B', 'C', 'D', 'E' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 9421, 9421, 9420, 9420, 9422 
    ## Resampling results across tuning parameters:
    ## 
    ##   cp          Accuracy   Kappa     
    ##   0.03678215  0.4994925  0.34559568
    ##   0.06003797  0.4670490  0.29351213
    ##   0.11461794  0.3158142  0.04801738
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was cp = 0.03678215.

``` r
cmDT
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2023  634  635  567  201
    ##          B   33  512   41  229  188
    ##          C  174  372  692  490  400
    ##          D    0    0    0    0    0
    ##          E    2    0    0    0  653
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.4945          
    ##                  95% CI : (0.4834, 0.5056)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.3396          
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9064  0.33729   0.5058   0.0000  0.45284
    ## Specificity            0.6372  0.92241   0.7783   1.0000  0.99969
    ## Pos Pred Value         0.4983  0.51047   0.3252      NaN  0.99695
    ## Neg Pred Value         0.9448  0.85299   0.8818   0.8361  0.89028
    ## Prevalence             0.2845  0.19347   0.1744   0.1639  0.18379
    ## Detection Rate         0.2578  0.06526   0.0882   0.0000  0.08323
    ## Detection Prevalence   0.5175  0.12784   0.2712   0.0000  0.08348
    ## Balanced Accuracy      0.7718  0.62985   0.6421   0.5000  0.72627

### VI. Gradient Boosted Trees/Model (GBM)

The final model is a GBM. Here the accuracy improves compared with
Decision Tree: 96% accuracy and 4% error.

``` r
modFitGBM <- train(classe~., data=training, method="gbm", trControl=fitControl, verbose=FALSE)

predGBM <- predict(modFitGBM, testing)
cmGBM <- confusionMatrix(predGBM, factor(testing$classe))

modFitGBM
```

    ## Stochastic Gradient Boosting 
    ## 
    ## 11776 samples
    ##    52 predictor
    ##     5 classes: 'A', 'B', 'C', 'D', 'E' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 9420, 9421, 9421, 9421, 9421 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  Accuracy   Kappa    
    ##   1                   50      0.7516129  0.6852299
    ##   1                  100      0.8175099  0.7689808
    ##   1                  150      0.8531757  0.8141591
    ##   2                   50      0.8522414  0.8128089
    ##   2                  100      0.9049758  0.8797355
    ##   2                  150      0.9319801  0.9139216
    ##   3                   50      0.8918981  0.8631180
    ##   3                  100      0.9419154  0.9264943
    ##   3                  150      0.9588992  0.9479988
    ## 
    ## Tuning parameter 'shrinkage' was held constant at a value of 0.1
    ## 
    ## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final values used for the model were n.trees = 150, interaction.depth =
    ##  3, shrinkage = 0.1 and n.minobsinnode = 10.

``` r
cmGBM
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2189   48    0    1    2
    ##          B   28 1420   48    4    5
    ##          C   13   42 1298   36   13
    ##          D    0    5   18 1237   20
    ##          E    2    3    4    8 1402
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9618          
    ##                  95% CI : (0.9573, 0.9659)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9516          
    ##                                           
    ##  Mcnemar's Test P-Value : 7.833e-05       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9807   0.9354   0.9488   0.9619   0.9723
    ## Specificity            0.9909   0.9866   0.9839   0.9934   0.9973
    ## Pos Pred Value         0.9772   0.9435   0.9258   0.9664   0.9880
    ## Neg Pred Value         0.9923   0.9845   0.9891   0.9925   0.9938
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2790   0.1810   0.1654   0.1577   0.1787
    ## Detection Prevalence   0.2855   0.1918   0.1787   0.1631   0.1809
    ## Balanced Accuracy      0.9858   0.9610   0.9664   0.9777   0.9848

### VII. Prediction

Finally, we will use the models to predict 20 different test cases using
the original testing data. The results of the Random Forest and GBM
models were the same and were shown to be accurate the Course Project
Prediction Quiz. While the results of the Decision Tree were not
accurate, which the high out of sample error rate.

``` r
predRF <- predict(modFitRF, testing_csv)

predDT <- predict(modFitDT, testing_csv)

predGBM<- predict(modFitGBM, testing_csv)

predRF
```

    ##  [1] B A B A A E D B A A B C B A E E A B B B
    ## Levels: A B C D E

``` r
predGBM
```

    ##  [1] B A B A A E D B A A B C B A E E A B B B
    ## Levels: A B C D E

``` r
predDT
```

    ##  [1] C A C A A C C A A A C C C A C A A A A C
    ## Levels: A B C D E
