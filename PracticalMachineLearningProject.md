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
    ## Summary of sample sizes: 9421, 9420, 9421, 9422, 9420 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa    
    ##    2    0.9889610  0.9860343
    ##   27    0.9893856  0.9865725
    ##   52    0.9831862  0.9787302
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
    ##          A 2229   22    4    0    0
    ##          B    2 1494    7    0    0
    ##          C    0    2 1347   10    2
    ##          D    0    0   10 1274    7
    ##          E    1    0    0    2 1433
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9912          
    ##                  95% CI : (0.9889, 0.9932)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9889          
    ##                                           
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9987   0.9842   0.9846   0.9907   0.9938
    ## Specificity            0.9954   0.9986   0.9978   0.9974   0.9995
    ## Pos Pred Value         0.9885   0.9940   0.9897   0.9868   0.9979
    ## Neg Pred Value         0.9995   0.9962   0.9968   0.9982   0.9986
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2841   0.1904   0.1717   0.1624   0.1826
    ## Detection Prevalence   0.2874   0.1916   0.1735   0.1645   0.1830
    ## Balanced Accuracy      0.9970   0.9914   0.9912   0.9940   0.9966

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
    ##   0.03749407  0.5311629  0.39653237
    ##   0.05980066  0.4171246  0.20989746
    ##   0.11580446  0.3161477  0.04853572
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was cp = 0.03749407.

``` r
cmDT
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 2027  637  635  574  215
    ##          B   34  518   40  241  207
    ##          C  165  363  693  471  373
    ##          D    0    0    0    0    0
    ##          E    6    0    0    0  647
    ## 
    ## Overall Statistics
    ##                                          
    ##                Accuracy : 0.4952         
    ##                  95% CI : (0.484, 0.5063)
    ##     No Information Rate : 0.2845         
    ##     P-Value [Acc > NIR] : < 2.2e-16      
    ##                                          
    ##                   Kappa : 0.34           
    ##                                          
    ##  Mcnemar's Test P-Value : NA             
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9082  0.34124  0.50658   0.0000  0.44868
    ## Specificity            0.6329  0.91751  0.78821   1.0000  0.99906
    ## Pos Pred Value         0.4958  0.49808  0.33559      NaN  0.99081
    ## Neg Pred Value         0.9454  0.85307  0.88324   0.8361  0.88948
    ## Prevalence             0.2845  0.19347  0.17436   0.1639  0.18379
    ## Detection Rate         0.2583  0.06602  0.08833   0.0000  0.08246
    ## Detection Prevalence   0.5210  0.13255  0.26319   0.0000  0.08323
    ## Balanced Accuracy      0.7705  0.62937  0.64739   0.5000  0.72387

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
    ## Summary of sample sizes: 9420, 9420, 9420, 9423, 9421 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  Accuracy   Kappa    
    ##   1                   50      0.7550125  0.6894362
    ##   1                  100      0.8262572  0.7801416
    ##   1                  150      0.8552166  0.8167882
    ##   2                   50      0.8576787  0.8197400
    ##   2                  100      0.9068444  0.8821024
    ##   2                  150      0.9312162  0.9129652
    ##   3                   50      0.8932579  0.8648895
    ##   3                  100      0.9396221  0.9236026
    ##   3                  150      0.9582200  0.9471421
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
    ##          A 2193   61    0    0    7
    ##          B   25 1408   39    4   25
    ##          C    6   44 1304   39   12
    ##          D    4    2   22 1239   22
    ##          E    4    3    3    4 1376
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9585          
    ##                  95% CI : (0.9538, 0.9628)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9474          
    ##                                           
    ##  Mcnemar's Test P-Value : 1.88e-10        
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9825   0.9275   0.9532   0.9635   0.9542
    ## Specificity            0.9879   0.9853   0.9844   0.9924   0.9978
    ## Pos Pred Value         0.9699   0.9380   0.9281   0.9612   0.9899
    ## Neg Pred Value         0.9930   0.9827   0.9901   0.9928   0.9898
    ## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
    ## Detection Rate         0.2795   0.1795   0.1662   0.1579   0.1754
    ## Detection Prevalence   0.2882   0.1913   0.1791   0.1643   0.1772
    ## Balanced Accuracy      0.9852   0.9564   0.9688   0.9779   0.9760

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
