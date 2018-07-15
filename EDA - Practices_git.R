# Exploratory Data Analysis - Collection
'''
Exploratory data analysis(EDA) is an approach to data analysis for summarising and visualising the 
important characteristics of a data set. It helps to

1. Understand variables
2. Find patterns in data
3. Suggest modeling strategies

EDA can be broadly classified into the following:

  1. Graphical or Quantitative
  2. Univariate or Bivariate

Quantitative methods include summary statisitcs while graphical methods include plots and charts. 
Univariate methods involve analysing one variable at a time while bivariate involves analysing two or 
more variables to examine their underlying relationships. 

Further division is based on the type of variable being examined:

1. CATEGORICAL VARIABLE
    Binary
    Ordinal
    Nominal
2. QUANTATIVE VARIABLE
    Discrete
    Continuous

UNIVARIATE - ANALYSIS
  Univariate Quantitative
    1. Measures of central tendancy: Mean, Median and Mode
    2. Measures of dispersion: Min, Max, Range, Quartiles, Variance and Standard Deviation
    3. Other measures include: Skewness and Kurtosis
  Univariate Graphical
    1. Histogram
    2. Box plots
    3. Bar plots
    4. Kernel density plots

BIVARIATE - ANALYSIS
  Bivariate Quantative
    1. Crosstabs
    2. Covariance
    3. Correlation
    Advanced techniques include:
      4. Cluster analysis
      5. Analysis of variance (ANOVA)
      6. Factor analysis
      7. Principal component analysis (PCA)
  Bivariate Graphical
      1. Scatterplot
      2. Box plot
'''
#============================= https://rpubs.com/Jovial/R ========================================
# https://archive.ics.uci.edu/ml/datasets/Bank+Marketing
'''
The data is related with direct marketing campaigns of a Portuguese banking institution. The marketing
campaigns were based on phone calls. Often, more than one contact to the same client was required, in 
order to access if the product (bank term deposit) would be ('yes') or not ('no') subscribed. 

The classification goal is to predict if the client will subscribe (yes/no) a term deposit (variable y).

Attribute Information:

Input variables:
# bank client data:
1 - age (numeric)
2 - job : type of job (categorical: 'admin.','blue-collar','entrepreneur','housemaid','management','retired','self-employed','services','student','technician','unemployed','unknown')
3 - marital : marital status (categorical: 'divorced','married','single','unknown'; note: 'divorced' means divorced or widowed)
4 - education (categorical: 'basic.4y','basic.6y','basic.9y','high.school','illiterate','professional.course','university.degree','unknown')
5 - default: has credit in default? (categorical: 'no','yes','unknown')
6 - housing: has housing loan? (categorical: 'no','yes','unknown')
7 - loan: has personal loan? (categorical: 'no','yes','unknown')
# related with the last contact of the current campaign:
8 - contact: contact communication type (categorical: 'cellular','telephone') 
9 - month: last contact month of year (categorical: 'jan', 'feb', 'mar', ..., 'nov', 'dec')
10 - day_of_week: last contact day of the week (categorical: 'mon','tue','wed','thu','fri')
11 - duration: last contact duration, in seconds (numeric). Important note: this attribute highly 
affects the output target (e.g., if duration=0 then y='no'). Yet, the duration is not known before a 
call is performed. Also, after the end of the call y is obviously known. Thus, this input should only
be included for benchmark purposes and should be discarded if the intention is to have a realistic 
predictive model.
# other attributes:
12 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
13 - pdays: number of days that passed by after the client was last contacted from a previous campaign 
     (numeric; 999 means client was not previously contacted)
14 - previous: number of contacts performed before this campaign and for this client (numeric)
15 - poutcome: outcome of the previous marketing campaign (categorical: 'failure','nonexistent','success')
# social and economic context attributes
16 - emp.var.rate: employment variation rate - quarterly indicator (numeric)
17 - cons.price.idx: consumer price index - monthly indicator (numeric) 
18 - cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
19 - euribor3m: euribor 3 month rate - daily indicator (numeric)
20 - nr.employed: number of employees - quarterly indicator (numeric)

Output variable (desired target):
21 - y - has the client subscribed a term deposit? (binary: 'yes','no')
'''

# D:/Machine Learning/Data Hacks/Bank Marketing Data Set/bank-full-converted.csv
# CSV opened as single colum, hence converted into text and read into Excel
# The excel text import has few options to help shape the format
# Once opened replaced all "" (double quotes) with blank
data <- read.csv('D:/Machine Learning/Data Hacks/Bank Marketing Data Set/bank-full-converted.csv', header = T)
attach(data)

# Explore the basic details of the dataset (high level overview)
fix(data)
names(data)
dim(data) # 45211    17
str(data)
class(data) # "data.frame"
head(data, n = 5)  # displays the first 5 rows 
tail(data, n = 5)  # displays the last 5 rows
summary(data) # My Inference: Ideally the feature 'day' represents the date, it should be a 
#factor not a number

#====================================== Univariate - Analysis =======================================

''' Feature - age (Numerical - discrete) '''
# Central tendency
summary(age)
boxplot(age)
# Spread
hist(age)
plot(density(age), main='Age density spread')

''' Feature - job (Categorical - Nominal) '''
summary(job)
plot(job) # or use any of the alternative function below
# barplot(summary(job))
# barplot(table(job))

''' Feature - marital (Categorical - Nominal) '''
summary(marital)
plot(marital)

''' Feature - education (Categorical - Ordinal) '''
summary(education)
plot(education)

''' Feature - default (Categorical - Nominal) '''
summary(default)
plot(default)

''' Feature - housing (Categorical - Nominal) '''
summary(housing)
plot(housing)

''' Feature - loan (Categorical - Nominal) '''
summary(loan)
plot(loan)

''' Feature - contact (Categorical - Nominal) '''
summary(contact)
plot(contact)

''' Feature - day (Numeric - discrete) '''
# Should this be converted into a factor instead ? May be if we can derive the conversions (Yes) based
# on the day
summary(day)
plot(day)
plot(data$y, data$day)

# After coercing day feature into a factor, trying to see the conversion rates across days,
# is any day being influential?
data.temp <- data
str(data.temp)
data.temp$day <- as.factor(data.temp$day)
plot(data.temp$day, xlab = 'Days across years', ylab = '# of calls')
plot(data.temp$day, data.temp$y)

''' Feature - month (Category - nominal) '''
summary(month)
plot(month)

''' Target - y (Category - nominal) '''
summary(data$y)
plot(data$y)
# Look at only purchases across days
library(dplyr)
data.yes <- filter(data, y == 'yes') # Create a dataset with y == 'yes'
data.yes$day <- as.factor(data.yes$day)
# plot(density(data.yes$day), main = 'Purchase Density across Days') # Only for numeric
plot(data.yes$day)
nrow(data.yes[data.yes$day == 30,]) # 271, highest conversion happened on 30th day

# Some more analysis
# What is the most common education overall? Visualize it?
summary(data$education) # secondary (23202)
plot(data$education, main = 'Overall Education Level')
# What is the most common education among buyers? Visualize it?
summary(data.yes$education)
plot(data.yes$education, main = 'Education Level among Purchasers')
# Did married people purchase more? Visualize it
summary(data.yes$marital) # Yes married did purchase more
plot(data.yes$marital)
# Who among the single purchase more? Male or Female? Visualize It
data.yes.single <- filter(data.yes, data.yes$marital == 'single')
# We do not have a feature to discern between Male and Female ... it might be a 
#good feature to add
# Effect of the last contact duration (in seconds)
summary(data.yes$duration)
plot(density(data.yes$duration), main = 'Successful Purchase - Call Duration - Density')
summary(data[data$y == 'no', 'duration'])
plot(density(data[data$y == 'no', 'duration']), main = 'Unsuccessful Purchase - Call Duration - Density')
# The age of top 10 purchasers
data.purchase.agegrp <- group_by(data.yes, data.yes$age)
summarise(data.purchase.agegrp, 
          count = n()) %>% arrange(desc(count)) %>% head(10)
plot(summarise(data.purchase.agegrp, 
               count = n())) # Visualize
# The age group of bottom 10 purchasers
summarise(data.purchase.agegrp, 
          count = n()) %>% arrange(desc(count)) %>% tail(10)
plot(summarise(data.purchase.agegrp, 
               count = n())) # Visualize
# Among the purchasers how many have housing loan?
# 36.6% purchasers have housing loans, prospects of conversion with housing loan borrowers is not bad
1935/(3354+1935)
summary(data.yes$housing) 
plot(data.yes$housing, main = 'Housing Loan Among Purchasers')
# Among the purchasers how many have Personal Loan
# Only 9.2% purchasers have Personal Loan, people with personal loan tend not to purchase
484/(4805+484)
summary(data.yes$loan)
plot(data.yes$loan)
# Among the Purchasers how many are without loans (no Housing and Personal loans)
# 77% of purchasers are without loans, so the prospects are high for persons without loans
(3354+4805)/(3354+1935+4805+484) 
Loan_Type <- c('Housing Loan', 'Personal Loan')
cbind(Loan_Type, rbind(summary(data.yes$housing), summary(data.yes$loan)))
# Job type among purchasers
summary(data.yes$job)
plot(data.yes$job)
# Subjects by Job Type and purchasers among them
job.conversion.freq <- table(data$job, data$y)
barplot(job.conversion.freq, 
        main = 'Purchasers across Jobs',
        xlab = 'Job Type',
        legend = rownames(job.conversion.freq),
        beside = TRUE)

# Group by Job Type to perform ggplotting
data.jobgrp <- group_by(data, data$job, data$y)
jobgrp <- summarise(data.jobgrp, 
          count = n())
jobgrp <- data.frame(jobgrp)
names(jobgrp) # "data.job" "data.y"   "count"

library(ggplot2)
# Conversion by each Job Type
# Set geom_bar's stat function for summarised data. (stat = 'identity')
ggplot(data = jobgrp, aes(x = data.job, y = count, fill = data.y)) + 
  geom_bar(stat = 'identity') + coord_flip() 
# + scale_fill_brewer(palette = 16)

#====================================== Bivariate Analysis  ===========================================
# 1. Relationship between 2 Continuous variables
# 2. Relationship between 2 Categorical variables
# 3. Relationship between Categorical and Continuous variables

# you are interested in 'how'/'what' is the relationship
# Visualization: scatter plot, box plot, density plot

# 1. Between 2 Continuous Variables

  str(data)
  scatter.smooth(data$age, data$balance, main = 'Age and Balance')
  
  # Use pairs() and corr()


# 2. Between 2 Categorical Variables

  ?xtabs
  # How does job type compare with purchase rate?
  cross.tabulation <- xtabs(~job+y, data)
  plot(cross.tabulation)
  # How does education compare with purchase rate?
  cross.tabulation <- xtabs(~education+y, data)
  plot(cross.tabulation)
  # How does marital status compare with purchase rate?
  cross.tabulation <- xtabs(~marital+y, data)
  plot(cross.tabulation)
  
  ?CrossTable
  ?plot
  # To predict the significance, chi-square test
  install.packages('gmodels')
  library(gmodels)
  CrossTable(data$marital, data$y, chisq = T, prop.t = T)
  CrossTable(data$education, data$y, chisq = T, prop.t = T)
  CrossTable(data$housing, data$y, chisq = T, prop.t = T)
  CrossTable(data$loan, data$y, chisq = T, prop.t = T)
  
  
# 3. Categorical and Continuous

  # by function - Apply a Function to a Data Frame Split by Factors
  # Below give an intutive summary of all the attributes split by education feature. The data-frame
  # holds only subjects who purchased.
  by(data.yes, data.yes$education, summary)
  by(data.yes[, c('marital', 'housing', 'loan')], data.yes$education, summary)
  # Summarize purchases by marital status on the whole data-frame
  by(data[, c('y','housing','loan')], data$marital, summary)
  
  # Box plot
  ?boxplot
  '''
  formula	- a formula, such as y ~ grp, where y is a numeric vector of data values to be split into 
  groups according to the grouping variable grp (usually a factor).
  '''
  # boxplot(Numeric_Vector~Categorical_Variable_to_Group, ........)
  boxplot(data$age~data$y, notch = T, col = c('Orange', 'Brown'), main = 'Age Split by Purchase Status')
  
  install.packages('sm')
  library(sm)
  ?sm
  '''
  The functions in the package use kernel methods to construct nonparametric estimates of density 
  functions and regression curves in a variety of settings, and to perform some inferential operations.
  '''
  ?sm.density.compare
  sm.density.compare(data$age, data$education, xlab = 'Age')


# ---------------------------- Rough Work ------------------------------------------------------

  have_child <- c(Nicolas = "yes", Thierry = "yes", 
                  Bernard = NA, Jerome = NA)
 have_child[is.na(have_child)] <- 'No'
  
summary(data)

attach(data)
boxplot(day~y, data = data)
boxplot(age~y, data = data)
boxplot(duration~y, data = data)
boxplot(balance~y, data = data)
boxplot(campaign~y, data = data)
boxplot(pdays~y, data = data)

# summary(data$contact)
# plot(data$y, data$contact)


boxplot(previous~y, data = data)
cor(data[,sapply(data, is.numeric)])

