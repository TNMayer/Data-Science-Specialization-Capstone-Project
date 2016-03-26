Background
----------

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: <http://groupware.les.inf.puc-rio.br/har> (see the section on the Weight Lifting Exercise Dataset).

Data
----

The training data for this project are available here: <https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv>

The test data are available here: <https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv>

The data for this project come from this source: <http://groupware.les.inf.puc-rio.br/har>

### Intended Results of the Project

The goal of your project is to predict the manner in which they did the exercise. This is the “classe” variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

1.  Your submission should consist of a link to a Github repo with your R markdown and compiled HTML file describing your analysis. Please constrain the text of the writeup to \< 2000 words and the number of figures to be less than 5. It will make it easier for the graders if you submit a repo with a gh-pages branch so the HTML page can be viewed online (and you always want to make it easy on graders :-).
2.  You should also apply your machine learning algorithm to the 20 test cases available in the test data above. Please submit your predictions in appropriate format to the programming assignment for automated grading. See the programming assignment for additional details.

Analysis
--------

In this section all analysis stepd needed to achieve the expected results are documented.

### Loading Packages

``` r
library(abind)
library(arm)
library(caret)
library(randomForest)
library(kernlab)
library(klaR)
library(rattle)
library(caTools)

# IOT get reproducible results following seed will be used:
# set.seed(12345)
```

### Getting the Data

At first the needed data sets have to be downloaded from the web and loaded/read into R.

``` r
# set the web URL´s to get the data
train_Url <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test_Url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

# Load and read in the data
pml_training <- read.csv(train_Url, na.strings=c("NA","#DIV/0!",""))
pml_testing <- read.csv(test_Url, na.strings=c("NA","#DIV/0!",""))
```

### Cleaning the training dataset

After getting a rough overview about the dataset there are several predictors with a near zero variance which can lead to problems for the learning algorithm. IOT identify predictors that have: 1. very few unique values relative to the number of samples and 2. the ratio of the frequency of the most common value to the frequency of the second most common value is large the nearZeroVar function out of the caret-package is applied to the training data set.

``` r
nzv_predictors <- nearZeroVar(pml_training, saveMetrics = T)
training <- pml_training[!nzv_predictors$nzv]
```

Another concern for the prediction can be predictors with a high NA-ratio. Hence pedictors with a NA-ratio \> 50 % will be removed in the following step.

``` r
select_col <- rep(T,ncol(training))
for (i in 1:ncol(training)) {
  ratio <- sum(is.na(training[,i]))/ncol(training)
  if (ratio > 0.5) {select_col[i] <- F}
}
training <- training[select_col]
```

Also variables who are related to the data collection process like id, user\_name, ... should be removed from the training set IOT avoid problems for the learning algorithm. In the remaining training dataset columns 1 to 6 are related to the collection process. Hence columns 1:6 will be removed from the training dataset.

``` r
training <- training[,-c(1:6)]
```

Now 53 columns (52 predictors) are left. There are also many variables who are highly correlated with each other. Here the variables with a mean absolute correlation \>= 0.8 are listed.

``` r
correlated_vars <- findCorrelation(cor(training[, -53]), cutoff=0.8)

names(training[correlated_vars])
```

    ##  [1] "accel_belt_z"     "roll_belt"        "accel_belt_y"    
    ##  [4] "accel_dumbbell_z" "accel_belt_x"     "pitch_belt"      
    ##  [7] "accel_arm_x"      "accel_dumbbell_x" "magnet_arm_y"    
    ## [10] "gyros_forearm_y"  "gyros_dumbbell_x" "gyros_dumbbell_z"
    ## [13] "gyros_arm_x"

13 predictors have a high mean absolute correlation and are highly correlated. This implies that a principal component analysis (PCA) should be applied as a pre-processing step.

### ML algorithms: Model selection

IOT minimize overfitting and to reduce out of sample errors, TrainControl out of the caret-package is used to perform a PCA and a 10 fold cross validation with the training dataset.

``` r
train_control <- trainControl(method = "cv", number = 10, verboseIter=FALSE , preProcOptions="pca", allowParallel=TRUE)
```

After that several models will be applied IOT determine the best prediction model for our data. Following models will be estimated:

1.  Random Forest
2.  Support Vector Machine (radial)
3.  Boosted Logistic Regression
4.  k-Nearest Neighbor

``` r
set.seed(12345)

random_forest <- train(classe ~ ., data = training, method = "rf", trControl= train_control)

svm_radial <- train(classe ~ ., data = training, method = "svmRadial", trControl= train_control)

boosted_logreg <- train(classe ~ ., data = training, method = "LogitBoost", trControl= train_control)

kNN <- train(classe ~ ., data = training, method = "knn", trControl= train_control, tuneLength = 20)
```

Now perform an accuracy comparison of the four models.

``` r
model <- c("Random Forest", "SVM (radial)","Boosted Logistic Regression", "kNN")
Accuracy <- c(max(random_forest$results$Accuracy),
        max(svm_radial$results$Accuracy),
        max(boosted_logreg$results$Accuracy),
        max(kNN$results$Accuracy))
        
Kappa <- c(max(random_forest$results$Kappa),
        max(svm_radial$results$Kappa),
        max(boosted_logreg$results$Kappa),
        max(kNN$results$Kappa))

performance <- cbind(model,Accuracy,Kappa)

knitr::kable(performance)
```

| model                       | Accuracy          | Kappa             |
|:----------------------------|:------------------|:------------------|
| Random Forest               | 0.995362550370993 | 0.994133675856133 |
| SVM (radial)                | 0.93675420716301  | 0.9198502077386   |
| Boosted Logistic Regression | 0.895545769333077 | 0.867014508238784 |
| kNN                         | 0.932115170126758 | 0.91411457316371  |

All models perform quite well and are in accuracy around or above 90 %, but the random forest classifier performs outstanding with more than 99 % out of sample prediction-accuracy. Hence the random forest classifier will be used to predict the results for the test set.

### Prediction of the test set data (20 samples)

``` r
select_col <- names(pml_testing) %in% names(training)
testing <- pml_testing[select_col]
rfPred <- predict(random_forest, testing)
rfPred
```

    ##  [1] B A B A A E D B A A B C B A E E A B B B
    ## Levels: A B C D E

Now our predictions are applied in the quiz IOT check how well our prediction fit the testing data.

Discussion
----------

The trained Random Forest classifier was able to predict all (100 %) of the 20 test-cases correctly and underlined it´s outstanding prediction-performance in this example and under the given circumstances.

In this analyses, 19622 observations from sports exercises were used to analyze and predict correct body movement from others during the exercise. Several machine learning algorithms were trained and cross-validated (10 fold CV) on the training dataset. The model statistics showed that the finally chosen random forest model had an overall out of sample accuracy of more than 99%. Overall, the model is able to predict the exercise classes during weight lifting upon a very high accuracy. The observation data used in the study was collected from 6 young healthy participants in an experiment while using Microsoft Kinect. Under the given experiment limitations (and exactly those), the trained random forest model can be expected to perform over 99% prediction accuracy. Under changed or other circumstances the model accuracy might change significantly.
