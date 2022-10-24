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
    ## Summary of sample sizes: 9422, 9420, 9421, 9421, 9420 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa    
    ##    2    0.9885364  0.9854972
    ##   27    0.9889606  0.9860335
    ##   52    0.9763079  0.9700266
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
    ##          A 2227   23    0    0    2
    ##          B    4 1488    5    1    0
    ##          C    1    7 1357   20    4
    ##          D    0    0    6 1263    4
    ##          E    0    0    0    2 1432
    ## 
    ## Overall Statistics
    ##                                          
    ##                Accuracy : 0.9899         
    ##                  95% CI : (0.9875, 0.992)
    ##     No Information Rate : 0.2845         
    ##     P-Value [Acc > NIR] : < 2.2e-16      
    ##                                          
    ##                   Kappa : 0.9873         
    ##                                          
    ##  Mcnemar's Test P-Value : NA             
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9978   0.9802   0.9920   0.9821   0.9931
    ## Specificity            0.9955   0.9984   0.9951   0.9985   0.9997
    ## Pos Pred Value         0.9889   0.9933   0.9770   0.9921   0.9986
    ## Neg Pred Value         0.9991   0.9953   0.9983   0.9965   0.9984
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2838   0.1897   0.1730   0.1610   0.1825
    ## Detection Prevalence   0.2870   0.1909   0.1770   0.1622   0.1828
    ## Balanced Accuracy      0.9967   0.9893   0.9935   0.9903   0.9964

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
    ## Summary of sample sizes: 9421, 9421, 9420, 9421, 9421 
    ## Resampling results across tuning parameters:
    ## 
    ##   cp          Accuracy   Kappa    
    ##   0.03630755  0.4997467  0.3465891
    ##   0.05999842  0.4161799  0.2090645
    ##   0.11805885  0.3336420  0.0755717
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was cp = 0.03630755.

``` r
cmDT
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1979  608  611  587  193
    ##          B   35  503   45  235  190
    ##          C  182  407  712  464  393
    ##          D    0    0    0    0    0
    ##          E   36    0    0    0  666
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.492           
    ##                  95% CI : (0.4808, 0.5031)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.3372          
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.8866  0.33136  0.52047   0.0000  0.46186
    ## Specificity            0.6439  0.92020  0.77678   1.0000  0.99438
    ## Pos Pred Value         0.4975  0.49901  0.32994      NaN  0.94872
    ## Neg Pred Value         0.9346  0.85156  0.88467   0.8361  0.89138
    ## Prevalence             0.2845  0.19347  0.17436   0.1639  0.18379
    ## Detection Rate         0.2522  0.06411  0.09075   0.0000  0.08488
    ## Detection Prevalence   0.5070  0.12847  0.27504   0.0000  0.08947
    ## Balanced Accuracy      0.7653  0.62578  0.64863   0.5000  0.72812

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
    ## Summary of sample sizes: 9421, 9420, 9421, 9422, 9420 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  Accuracy   Kappa    
    ##   1                   50      0.7559465  0.6905958
    ##   1                  100      0.8212489  0.7737343
    ##   1                  150      0.8534324  0.8145373
    ##   2                   50      0.8546214  0.8158021
    ##   2                  100      0.9065910  0.8817702
    ##   2                  150      0.9298580  0.9112348
    ##   3                   50      0.8991174  0.8723071
    ##   3                  100      0.9391138  0.9229583
    ##   3                  150      0.9575412  0.9462772
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
    ##          A 2189   55    0    2    2
    ##          B   27 1420   32    7   12
    ##          C    9   42 1319   42   15
    ##          D    4    1   15 1227   20
    ##          E    3    0    2    8 1393
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.962           
    ##                  95% CI : (0.9576, 0.9661)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9519          
    ##                                           
    ##  Mcnemar's Test P-Value : 3.79e-10        
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9807   0.9354   0.9642   0.9541   0.9660
    ## Specificity            0.9895   0.9877   0.9833   0.9939   0.9980
    ## Pos Pred Value         0.9738   0.9479   0.9243   0.9684   0.9908
    ## Neg Pred Value         0.9923   0.9846   0.9924   0.9910   0.9924
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2790   0.1810   0.1681   0.1564   0.1775
    ## Detection Prevalence   0.2865   0.1909   0.1819   0.1615   0.1792
    ## Balanced Accuracy      0.9851   0.9616   0.9738   0.9740   0.9820

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
