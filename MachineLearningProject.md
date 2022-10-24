MachineLearningProject
================
2022-10-24

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

training_1 <- training_csv[,colSums(is.na(training_csv))==0]
testing_1 <- testing_csv[,colSums(is.na(testing_csv))==0] 
sum(is.na(training_csv))
```

    ## [1] 1921600

``` r
sum(is.na(training_1))
```

    ## [1] 0

``` r
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
    ## Summary of sample sizes: 9420, 9423, 9421, 9420, 9420 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa    
    ##    2    0.9879420  0.9847449
    ##   27    0.9871769  0.9837775
    ##   52    0.9768169  0.9706659
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was mtry = 2.

``` r
cmRF
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2230    5    0    0    0
    ##          B    2 1505   10    0    0
    ##          C    0    8 1358   29    0
    ##          D    0    0    0 1256    1
    ##          E    0    0    0    1 1441
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9929          
    ##                  95% CI : (0.9907, 0.9946)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.991           
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9991   0.9914   0.9927   0.9767   0.9993
    ## Specificity            0.9991   0.9981   0.9943   0.9998   0.9998
    ## Pos Pred Value         0.9978   0.9921   0.9735   0.9992   0.9993
    ## Neg Pred Value         0.9996   0.9979   0.9984   0.9954   0.9998
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2842   0.1918   0.1731   0.1601   0.1837
    ## Detection Prevalence   0.2849   0.1933   0.1778   0.1602   0.1838
    ## Balanced Accuracy      0.9991   0.9948   0.9935   0.9883   0.9996

### IV. Decision Tree

The second model we will try is a Decision Tree. Here the accuracy has
fallen to 49%.

``` r
modFitDT <- train(classe~., data=training, method="rpart", trControl=fitControl)

fancyRpartPlot(modFitDT$finalModel, main="Weight Lifting Exercise Decision Tree", sub="")
```

![](MachineLearningProject_files/figure-gfm/Decision%20Tree-1.png)<!-- -->

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
    ## Summary of sample sizes: 9421, 9420, 9421, 9422, 9420 
    ## Resampling results across tuning parameters:
    ## 
    ##   cp          Accuracy   Kappa     
    ##   0.03690081  0.5065361  0.35582110
    ##   0.06090808  0.4430155  0.25335398
    ##   0.11568581  0.3322051  0.07294084
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was cp = 0.03690081.

``` r
cmDT
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2027  614  652  568  219
    ##          B   33  510   50  235  196
    ##          C  165  394  666  483  378
    ##          D    0    0    0    0    0
    ##          E    7    0    0    0  649
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.491           
    ##                  95% CI : (0.4798, 0.5021)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.3347          
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9082   0.3360  0.48684   0.0000  0.45007
    ## Specificity            0.6343   0.9188  0.78080   1.0000  0.99891
    ## Pos Pred Value         0.4968   0.4980  0.31927      NaN  0.98933
    ## Neg Pred Value         0.9456   0.8522  0.87812   0.8361  0.88971
    ## Prevalence             0.2845   0.1935  0.17436   0.1639  0.18379
    ## Detection Rate         0.2583   0.0650  0.08488   0.0000  0.08272
    ## Detection Prevalence   0.5200   0.1305  0.26587   0.0000  0.08361
    ## Balanced Accuracy      0.7712   0.6274  0.63382   0.5000  0.72449

### V. Gradient Boosted Trees/Model (GBM)

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
    ## Summary of sample sizes: 9422, 9420, 9420, 9421, 9421 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  Accuracy   Kappa    
    ##   1                   50      0.7499984  0.6828939
    ##   1                  100      0.8245576  0.7779569
    ##   1                  150      0.8561464  0.8179639
    ##   2                   50      0.8557220  0.8171775
    ##   2                  100      0.9073522  0.8827192
    ##   2                  150      0.9295165  0.9108020
    ##   3                   50      0.8979273  0.8707243
    ##   3                  100      0.9405563  0.9247747
    ##   3                  150      0.9586446  0.9476763
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
    ##          A 2198   44    0    0    5
    ##          B   24 1432   45    5   22
    ##          C    6   41 1306   46   13
    ##          D    2    1   14 1218    9
    ##          E    2    0    3   17 1393
    ## 
    ## Overall Statistics
    ##                                          
    ##                Accuracy : 0.9619         
    ##                  95% CI : (0.9574, 0.966)
    ##     No Information Rate : 0.2845         
    ##     P-Value [Acc > NIR] : < 2.2e-16      
    ##                                          
    ##                   Kappa : 0.9518         
    ##                                          
    ##  Mcnemar's Test P-Value : 2.85e-10       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9848   0.9433   0.9547   0.9471   0.9660
    ## Specificity            0.9913   0.9848   0.9836   0.9960   0.9966
    ## Pos Pred Value         0.9782   0.9372   0.9249   0.9791   0.9845
    ## Neg Pred Value         0.9939   0.9864   0.9904   0.9897   0.9924
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2801   0.1825   0.1665   0.1552   0.1775
    ## Detection Prevalence   0.2864   0.1947   0.1800   0.1586   0.1803
    ## Balanced Accuracy      0.9880   0.9641   0.9692   0.9716   0.9813

### VI. Prediction

Finally, we will use the models to predict 20 different test cases using
the original testing data.

The results of the Random Forest and GBM models were the same and were
shown to be accurate the Course Project Prediction Quiz. While the
results of the Decision Tree were not accurate, which the high out of
sample error rate.

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
