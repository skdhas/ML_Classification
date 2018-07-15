#======================== An Application to Caravan Insurance Data (KNN Approach) ======================

'''
we will apply the KNN approach to the Caravan data set, which is part of the ISLR library. 
This data set includes 85 predictors that measure demographic characteristics for 5,822 individuals. 
The response variable is Purchase, which indicates whether or not a given individual purchases a caravan
insurance policy. In this data set, only 6% of people purchased caravan insurance.

Further information on the individual variables can be obtained at 
http://www.liacs.nl/~putten/library/cc2000/data.html
'''

library(ISLR)
dim(Caravan)
names(Caravan)
?Caravan

attach(Caravan)
summary(Purchase)
'''
No    Yes 
5474  348
'''
'''
Because the KNN classiﬁer predicts the class of a given test observation by identifying 
the observations that are nearest to it, the scale of the variables matters. 
Any variables that are on a large scale will have a much larger eﬀect on the distance 
between the observations, and hence on the KNN classiﬁer, than variables that are on a 
small scale.

For instance, imagine a data set that contains two variables, salary and age (measured in dollars and 
years, respectively). As far as KNN is concerned, a diﬀerence of $1,000 in salary is enormous compared
to a diﬀerence of 50 years in age. Consequently, salary will drive the KNN classiﬁcation results, and 
age will have almost no eﬀect. This is contrary to our intuition that a salary diﬀerence of $1,000 is 
quite small compared to an age diﬀerence of 50 years. Furthermore, the importance of scale to the KNN
classiﬁer leads to another issue: if we measured salary in Japanese yen, or if we measured age in 
minutes, then we’d get quite diﬀerent classiﬁcation results from what we get if these two variables 
are measured in dollars and years.

A good way to handle this problem is to standardize the data so that all variables are given a mean 
of zero and a standard deviation of one. 

Then all variables will be on a comparable scale. The scale() function does just this. 
In standardizing the data, we exclude column 86, because that is the qualitative Purchase variable.
'''
standardized.X <- scale(Caravan[,-86]) # Standardize the data
var(Caravan[,1]) # 165.0378
sd(Caravan[,1]) # sqrt(var(Caravan[,1]))
var(Caravan[,2]) # 0.1647078

# Now every column of standardized.X has a standard deviation of one and a mean of zero.
var(standardized.X[,1]) # 1
var(standardized.X[,2]) # 1

'''
We now split the observations into a test set, containing the ﬁrst 1,000 observations, and a training 
set, containing the remaining observations. We ﬁt a KNN model on the training data using K = 1, and 
evaluate its performance on the test data.
'''
test.rowids <- 1:1000
train.rows <- standardized.X[-test.rowids,] # Training Observations
test.rows <- standardized.X[test.rowids,]
train.response <- Purchase[-test.rowids] # Response Variable Values (Class - Yes, No) for training obs
test.response <- Purchase[test.rowids] # Response Variable Values (Class - Yes, No) for testing obs

set.seed(1)
knn.predicted <- knn(train.rows, test.rows, train.response, k = 1)
mean(test.response != knn.predicted) # 0.118
'''
BUSINESS INFERENCE:

The KNN error rate on the 1,000 test observations is just under 12%. At ﬁrst glance, this may appear 
to be fairly good. However, since only 6% of customers purchased insurance, we could get the error 
rate down to 6% by always predicting "No" regardless of the values of the predictors! 

If the company tries to sell insurance to a random selection of customers, then the success rate will 
be only 6%, which may be far too low given the costs involved. Instead, the company would like to try
to sell insurance only to customers who are likely to buy it. So the overall error rate is not of 
interest. Instead, the fraction of individuals that are correctly predicted to buy insurance is of 
interest.
'''
# Confusion Matrix
table(knn.predicted, test.response)

'''
        test.response
knn.predicted  No Yes
          No  873  50
         Yes   68   9

It turns out that KNN with K = 1 does far better than random guessing among the customers that are 
predicted to buy insurance. Among 77 such customers, 9, or 11.7%, actually do purchase insurance. 
This is double the rate that one would obtain from random guessing.
'''
'''
Using K = 3, the success rate increases to 20%, and with K = 5 the rate is 26.7%. This is over four 
times the rate that results from random guessing. It appears that KNN is ﬁnding some real patterns 
in a diﬃcult data set!
'''
set.seed(1)
knn.predicted <- knn(train.rows, test.rows, train.response, k = 3)
mean(test.response != knn.predicted) # 0.074
# Confusion Matrix
table(knn.predicted, test.response)
'''
             test.response
knn.predicted  No Yes
          No  921  54
          Yes  20   5
'''
5/25 # 20% Success Rate

# With k = 5
set.seed(1)
knn.predicted <- knn(train.rows, test.rows, train.response, k = 5)
mean(test.response != knn.predicted) # 0.074
# Confusion Matrix
table(knn.predicted, test.response)
'''
             test.response
knn.predicted  No Yes
          No  930  55
          Yes  11   4
'''
4/15 # Success Rate 0.2666667, 26.7%

#================================== Logistic Regression (comparison) =============================
'''
As a comparison, we can also ﬁt a logistic regression model to the data. If we use 0.5 as the 
predicted probability cut-oﬀ for the classiﬁer, then we have a problem: only seven of the test 
observations are predicted to purchase insurance. Even worse, we are wrong about all of these! 
However, we are not required to use a cut-oﬀ of 0.5. If we instead predict a purchase any time 
the predicted probability of purchase exceeds 0.25, we get much better results: we predict that 
33 people will purchase insurance, and we are correct for about 33% of these people. 
This is over ﬁve times better than random guessing!
'''
glm.fits <- glm(Purchase~., data = Caravan, family = binomial, subset = -test)
glm.probs <- predict(glm.fits, Caravan[test,], type = 'response')
glm.pred <- rep('No', length(test))
glm.pred[glm.probs > .5] <- 'Yes'
# Confusion Matrix
table(glm.pred, test.response)
'''
        test.response
glm.pred  No   Yes
      No  934  59
      Yes   7   0
'''
# Now change the threshold to .25 instead of .5
glm.pred <- rep('No', length(test))
glm.pred[glm.probs > .25] <- 'Yes'
table(glm.pred, test.response)
'''
        test.response
glm.pred  No Yes
     No  919  48
    Yes   22  11
'''
11/(22+11) # 0.3333333


#---------------------------------------- Rough Work ----------------------------------------------
attach(Caravan)
standardized.X=scale(Caravan[,-86])
test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase [-test]
test.Y=Purchase [test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred) 

nrow(train.rows)
nrow(test.rows)

length(train.response)
length(test.response)
length(knn.pred)

