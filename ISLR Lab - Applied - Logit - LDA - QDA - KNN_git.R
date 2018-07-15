# Applied - Logit, LDA, QDA and KNN
'''
This question should be answered using the “Weekly” data set, which is part of the “ISLR” package. 
This data is similar in nature to the “Smarket” data from this chapter’s lab, except that it contains 
1089 weekly returns for 21 years, from the beginning of 1990 to the end of 2010.
'''

# (a) Produce some numerical and graphical summaries of the “Weekly” data. Do there appear to be any 
# patterns ?
library(ISLR)
?Weekly
names(Weekly)
summary(Weekly)

# Correlation Matrix, excluding the response (Direction)
cor(Weekly[, -9])

attach(Weekly)
plot(Volume)
'''
The correlations between the “lag” variables and today’s returns are close to zero. 
The only substantial correlation is between “Year” and “Volume”. When we plot “Volume”, 
we see that it is increasing over time.
'''

# (b) Use the full data set to perform a logistic regression with “Direction” as the response and the 
# five lag variables plus “Volume” as predictors. Use the summary function to print the results. 
# Do any of the predictors appear to be statistically significant ? If so, which ones ?
fit.glm <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data = Weekly,
               family = binomial)
summary(fit.glm)
'''
Coefficients:
            Estimate Std. Error z value Pr(>|z|)   
(Intercept)  0.26686    0.08593   3.106   0.0019 **
Lag1        -0.04127    0.02641  -1.563   0.1181   
Lag2         0.05844    0.02686   2.175   0.0296 * 
Lag3        -0.01606    0.02666  -0.602   0.5469   
Lag4        -0.02779    0.02646  -1.050   0.2937   
Lag5        -0.01447    0.02638  -0.549   0.5833   
Volume      -0.02274    0.03690  -0.616   0.5377

It would seem that “Lag2” is the only predictor statistically significant as its p-value is 
less than 0.05.
'''

# (c) Compute the confusion matrix and overall fraction of correct predictions. 
# Explain what the confusion matrix is telling you about the types of mistakes made by 
# logistic regression.
probs <- predict(fit.glm, type = 'response')
pred.glm <- rep('Down', length(probs))
pred.glm[probs > .5] <- 'Up'
table(pred.glm, Direction)
'''
        Direction
pred.glm Down  Up
    Down   54  48
    Up    430 557

We may conclude that the percentage of correct predictions on the training data is (54+557)/1089 
which is equal to 56.1065197%. In other words 43.8934803% is the training error rate, which is often 
overly optimistic.
We could also say that for weeks when the market goes up, the model is right 92.0661157% of the time 
(557/(48+557)). For weeks when the market goes down, the model is right only 11.1570248% of the time 
(54/(54+430)).
'''

# (d) Now fit the logistic regression model using a training data period from 1990 to 2008, with “Lag2” 
# as the only predictor. Compute the confusion matrix and the overall fraction of correct predictions 
# for the held out data (that is, the data from 2009 to 2010).
train <- (Year < 2009) # boolean vector equalling the observations (True for all until 2009 and False for others)
length(train)
table(train) # 104 False and 985 True

fit.glm2 <- glm(Direction~Lag2, 
                data = Weekly,
                family = binomial,
                subset = train)
summary(fit.glm2)
probs.glm2 <- predict(fit.glm2, Weekly[!train,], type = 'response')
pred.glm2 <- rep('Down', length(probs.glm2))
pred.glm2[probs.glm2  > .5] <- 'Up'
# Confusion Matrix
table(pred.glm2, Direction[!train]) # Predicted class values pitted against test class values
'''
pred.glm2 Down Up
     Down    9  5
      Up    34 56

CORRECT PREDICTION ON TEST DATA = 62.5% ((9+56)/104) Diagnol Elements
TEST ERROR RATE = 37.5% ((9+56)/104), i.e. Off Diagnol OR 100 - 62.5
WHEN MARKET GOES UP MODEL IS RIGHT 91.8032787% of the time (56/(5+56))
WHEN MARKET GOES DOWN MARKET IS ONLY RIGHT 20.9302326% of the time (9/(9+34))

In this case, we may conclude that the percentage of correct predictions on the test data
is (9+56)/104 wich is equal to 62.5%. In other words 37.5% is the test error rate. 
We could also say that for weeks when the market goes up, the model is right 91.8032787% 
of the time (56/(56+5)). For weeks when the market goes down, the model is right only 
20.9302326% of the time (9/(9+34)).
'''
# (e) Repeat (d) using LDA.
library(MASS)
lda.fit <- lda(Direction~Lag2, data = Weekly, subset = train)
lda.probs <- predict(lda.fit, Weekly[!train,]) # Predict probabilities on train dataset
# Confusion Matrix
table(lda.probs$class, Direction[!train]) # Predicted class vs test class
'''
       Down Up
  Down    9  5
    Up   34 56
In this case, we may conclude that the percentage of correct predictions on the test 
data is 62.5%. In other words 37.5% is the test error rate. We could also say that for 
weeks when the market goes up, the model is right 91.8032787% of the time. For weeks 
when the market goes down, the model is right only 20.9302326% of the time. 
These results are very close to those obtained with the logistic regression model which 
is not surpising.
'''
# (f) Repeat (d) using QDA.
fit.qda <- qda(Direction~Lag2, data = Weekly, subset = train)
qda.probs <- predict(fit.qda, Weekly[!train,])
#Confusion Matrix
table(qda.probs$class, Direction[!train]) # Predicted class vs test class values
'''
       Down Up
  Down    0  0
    Up   43 61

CORRECt PREDICTIONS (0+61)/(0+0+43+61) = 58.65%
TEST ERROR RATE (0+43)/(0+0+43+61) = 41.34%
FOR WEEKS WHEN MARKET GOES UP (61)/(0+61) is right 100% of time
FOR WEEKS WHEN MARKET GOES DOWN (0)/(0+43) is right only 0% of time

In this case, we may conclude that the percentage of correct predictions on the test data
is 58.6538462%. In other words 41.3461538% is the test error rate. We could also say that
for weeks when the market goes up, the model is right 100% of the time. For weeks when 
the market goes down, the model is right only 0% of the time. We may note, that QDA 
achieves a correctness of 58.6538462% even though the model chooses “Up” the whole time!
'''
# (g) Repeat (d) using KNN with K=1.
'''
Rather than a two-step approach in which we ﬁrst ﬁt the model and then we use the model to make 
predictions, knn() forms predictions using a single command. The function requires four inputs 
(training matrix, testing matrix, vector of class labels for training observations and K value)
'''
library(class)
train.df <- as.matrix(Lag2[train]) # 1st Arg, Training matrix, class(train.df) = "matrix"
test.df <- as.matrix(Lag2[!train]) # 2nd Arg Test martix, class(train.df) = "matrix"
traindirection.vect <- Direction[train] # 3rd Arg, vector of class for training observations
knn.pred <- knn(train.df, test.df, traindirection.vect, k = 1) # Now predict the class with k=1 neighbour
# Print confusion matrix
table(knn.pred, Direction[!train])
'''
knn.pred Down Up
    Down   21 30
      Up   22 31

CORRECT PREDICTIONS (21+31)/(21+30+22+31) is 50%
TEST ERROR RATE (30+22)/(21+30+22+31) is 50%
FOR WEEKS WHEN THE MARKET GOES UP THE MODEL IS RIGHT (31)/(30+31) i.e. 50.82% of time
FOR WEEKS WHEN THE MARKET GOES DOWN THE MODEL IS RIGHT (21)/(21+22) i.e. 48.84% of time
'''
# (h) Which of these methods appears to provide the best results on this data ?
'''
If we compare the test error rates, we see that logistic regression and LDA have the minimum 
error rates, followed by QDA and KNN
'''

#======================== Logit, LDA, QDA and KNN using Auto data set ========================

#Q11. In this problem, you will develop a model to predict whether a given car gets high or low 
# gas mileage based on the “Auto” data set.

# (a) Create a binary variable, “mpg01”, that contains a 1 if “mpg” contains a value above its 
# median, and a 0 if “mpg” contains a value below its median. You can compute the median using 
# the median() function. Note you may find it helpful to use the data.frame() function to create 
# a single data set containing both “mpg01” and the other “Auto” variables
head(Auto)
attach(Auto)
Auto['mpg01'] <- ifelse(mpg > median(mpg), 1, 0)
# Another way as below
'''
mpg01 <- rep(0, length(mpg))
mpg01[mpg > mean(mpg)] <- 1
Auto <- data.frame(Auto, mpg01)
'''

# (b) Explore the data graphically in order to investigate the association between “mpg01” and 
# the other features. Which of the other features seem most likely to be useful in predictiong 
# “mpg01” ? Scatterplots and boxplots may be useful tools to answer this question. Describe your 
# findings.
names(Auto)
cor(Auto[,sapply(Auto, is.numeric)]) # Exclude non-numeric feature(ie. name)
pairs(Auto)
attach(Auto)
# boxplot(cylinders, mpg01, data = Auto, main = "Cylinders vs mpg01")
boxplot(cylinders~mpg01, data = Auto, main = "Cylinders vs mpg01")
boxplot(displacement~mpg01, data = Auto, main = "Displacement vs mpg01")
boxplot(horsepower~mpg01, data = Auto, main = "Horsepower vs mpg01")
boxplot(weight~mpg01, data = Auto, main = "Weight vs mpg01")
boxplot(acceleration~mpg01, data = Auto, main = "Acceleration vs mpg01")
boxplot(year~mpg01, data = Auto, main = "Year vs mpg01")
'''
We may conclude that there exists some association between “mpg01” and “cylinders”, “weight”, 
“displacement” and “horsepower”.
'''
# (c) Split the data into a training set and a test set.
train.bool <- (year %% 2 == 0)
Auto.train <- Auto[train.bool,]
Auto.test <- Auto[!train.bool,]
mpg01.test <- mpg01[!train.bool]

# (d) Perform LDA on the training data in order to predict “mpg01” using the variables that 
# seemed most associated with “mpg01” in (b). What is the test error of the model obtained ?
fit.lda <- lda(mpg01~cylinders+weight+displacement+horsepower, data = Auto, subset = train.bool)
fit.lda
lda.pred <- predict(fit.lda, Auto.test)
names(lda.pred)
# Confusion Matrix
table(lda.pred$class, mpg01.test)
mean(lda.pred$class != mpg01.test) # Test Error, 0.1263736 or 12.63%
'''
   mpg01.test
     0  1
  0 88 11
  1 12 71

CORRECT PREDICTIONS : (88+71)/(88+11+12+71) is 87.36% CORRECT
TEST ERROR : (11+12)/(88+11+12+71) is 12.63%
'''




#--------------------------- Rough Work --------------------------------------------------------
(88+71)/(88+11+12+71)
(11+12)/(88+11+12+71)


(31)/(30+31)
(21)/(21+22)


(21+31)/(21+30+22+31)
(30+22)/(21+30+22+31)

dim(Weekly)
head(Weekly)
max(Weekly$Volume)
sum(Weekly$Volume > 7)
nrow(Weekly[!train,])
length(Direction[!train])
names(qda.probs)

aov(as.matrix(c(10,20,12,28,90,100,2,150)))

