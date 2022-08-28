# Source of data and code: Dimitris Bertsimas
library(moments)
library(Hmisc)
library (caTools)
library(corrplot)
set.seed(123)
MusicRecord<-read.csv("/Users/minxiangliu/Desktop/MFIT/867/Assignment2/MusicData.csv") #load data
colSums(is.na(MusicRecord))
M <- cor(as.matrix(MusicRecord[sapply(MusicRecord,is.numeric)]))
corrplot(M, method='circle')
inx <- sample.split(seq_len(nrow(MusicRecord)), 0.7)

# How many songs does the dataset include for which the artist name is "Michael Jackson"?
table(MusicRecord$artistname == "Michael Jackson")
#install.packages("dplyr")
# Alternatively, use the pipe %>% function in "dplyr" package
library(dplyr)
MusicRecord %>% filter(artistname == "Michael Jackson") %>% summarize(count = n())
#MusicRecord$key <- factor(MusicRecord$key)
#MusicRecord$timesignature <- factor(MusicRecord$timesignature)

# first use the filter function to split the data into a training set "SongsTrain" 
# consisting of all the observations up to and including 2009 song releases, and a testing set "SongsTest", 
# consisting of the 2010 song releases.
#SongsTrain<-MusicRecord[inx,]
#SongsTest<-MusicRecord[!inx,]
SongsTrain = MusicRecord %>% filter(year <= 2009)
SongsTest = MusicRecord %>% filter(year == 2010)

# we want to exclude some of the variables in our dataset from being used as independent variables 
# ("year", "songtitle", "artistname", "songID", and "artistID"). To do this, we can use the following trick. 
# First define a vector of variable names called nonvars - these are the variables that we won't use in our model.
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

# To remove these variables from your training and testing sets:
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

# build a logistic regression model to predict Top10 using the training data. 
# We can now use "." in place of enumerating all the remaining independent variables in the following way:

#hist.data.frame(SongsTrain)
#hist.data.frame(log1p(abs(SongsTrain)))
skewness(SongsTrain, na.rm = TRUE)
skewness(log1p(abs(SongsTrain)), na.rm = TRUE)
cols_skewed <- c("loudness","pitch", "timbre_0_min", "timbre_3_min", "timbre_1_max", "timbre_5_min", "timbre_6_max", 
                 "timbre_10_min", "timbre_9_max", "timbre_8_min", "timbre_10_max")
SongsTrain[, cols_skewed] <- log1p(abs(SongsTrain[, cols_skewed]))
SongsTest[, cols_skewed] <- log1p(abs(SongsTest[, cols_skewed]))

summary(SongsLog1)
#SongsLog1 = glm(Top10 ~ . +timesignature*tempo*key*energy*pitch*energy, data=SongsTrain, family=binomial)
#SongsLog1 = glm(Top10 ~ . +timesignature*tempo*key*energy*pitch*loudness, data=SongsTrain, family=binomial)
SongsLog1 = glm(Top10 ~ . -timesignature-key-tempo-timbre_5_max-timbre_9_min-timbre_2_max-timbre_1_max
                          -timbre_3_min-timbre_8_min-timbre_9_max-timbre_2_min-timbre_8_max
                          -energy-timbre_10_min,
                data=SongsTrain, family=binomial)
summary(SongsLog1)
# True or False?
# 1. The higher our confidence about time signature, key and tempo, the more likely the song is to be in the Top 10
# 2. In general, if the confidence is low for the time signature, tempo, and key, then the song is more likely to be complex. What does our model suggest in terms of complexity?


# You can make predictions on the test set by using the command:
#testPredict = predict(SongsLog1, newdata=SongsTest, type="response")


#Lasso
library(glmnet)
X<-model.matrix(~. 
                    ,subset(SongsTrain, select=-c(Top10)))[, -1]
X_test<-model.matrix(~. 
                     ,subset(SongsTest, select=-c(Top10)))[, -1]
lasso.fit<-glmnet(x = X, y = SongsTrain$Top10, alpha = 1, family="binomial")
plot(lasso.fit, xvar = "lambda")
crossval <-  cv.glmnet(x = X, y = SongsTrain$Top10, alpha = 1, family="binomial") #create cross-validation data. By default, the function performs ten-fold cross-validation, though this can be changed using the argument nfolds. 
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
lasso.opt.fit <-glmnet(x = X, y = SongsTrain$Top10, alpha = 1, lambda = penalty.lasso, family="binomial") #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

testPredict <- predict(lasso.opt.fit, s = penalty.lasso, newx =X_test, type="response")


# Then, you can create a confusion matrix with a threshold of 0.15 by using the table command:
confusion.matrix<-table(SongsTest$Top10, testPredict >= 0.15)

# The accuracy of the model is? 
Count.correct<-confusion.matrix[1,1]+confusion.matrix[2,2]
Count.wrong<-confusion.matrix[1,2]+confusion.matrix[2,1]

Accuracy.rate<-Count.correct/(Count.correct+Count.wrong)
# What is the prediction accuracy of the model?

# To generate the ROC curve
#install.packages("pROC")
library(pROC)
test_prob = predict(SongsLog1, newdata = SongsTest, type = "response")
test_roc = roc(SongsTest$Top10 ~ test_prob, plot = TRUE, print.auc = TRUE)
lasso_test_prob = predict(lasso.opt.fit, s = penalty.lasso, newx =X_test, type = "response")
lasso_test_roc = roc(SongsTest$Top10 ~ testPredict, plot = TRUE, print.auc = TRUE)

