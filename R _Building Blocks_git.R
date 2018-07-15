#==================================== R Building Blocks ==============================================

'''
In this section, we’ll analyze a dataset called …wait for it…pirates! The dataset contains data 
from a survey of 1,000 pirates.

A data frame containing 1,000 rows and 14 columns

id : An integer giving the pirates id number
sex : A string with the pirates self reported sex
age : An integer giving the age of the pirate in years
height : Height in cm
weight : Weight in kg
headband : A binary variable indicating whether the pirate wears a headband
college : A string indicating the college the pirate went to. JSSFP stands for Jack Sparros School of Fashion and Piratery, while CCCC stands for Captain Chunks Cannon Crew
tattoos : An integer indicating the number of tattoos the pirate has
tchests : An integer indicating the number of treasure chests found by the pirate
parrots : An integer indicating the number of parrots owned by the pirate in his/her lifetime
favorite.pirate : A string indicating The pirates favorite pirate
sword.type : A string indicating the type of sword the pirate uses
eyepatch : An integer indicating the number of eyepatches worn by the pirate
sword.time : A number indicating how long it takes (in seconds) for the pirate to draw his/her sword. Smaller times are better!
beard.length : A number indicating length of the pirates beard in centimeters
fav.pixar : A string indicating Pirates favorite pixar movie
grogg : How many mugs of grogg the pirate drinks a day on average.

'''

  install.packages('yarrr')
  library(yarrr)
  
  # Some information about the dataset
  ?pirates
  
  # You can look at the names of the columns in the dataset with the names() function
  names(pirates)
  
  # Finally, you can also view the entire dataset in a separate window using the View() function:
  View(pirates)
  fix(pirates) # Opens in a data editor window

#================================== Basic Descriptive statistics =================================
'''
Now let’s calculate some basic statistics on the entire dataset. 
We’ll calculate the mean age, maximum height, and number of pirates of each sex:

'''
  mean(pirates$age) # 27.36 - What is the mean age?
  max(pirates$height) # 209.25 - The height ot the tallest pirate?
  table(pirates$sex) # How many pirates are there of each sex?
  
  # Compute the mode (to check later)
  install.packages("modeest")
  require(modeest)
  #mfv(pirates$sex)
  #sum(is.na(pirates$sex))

'''

Now, let’s calculate statistics for different groups of pirates. 
For example, the following code will use the aggregate() function to calculate the mean age of 
pirates, separately for each sex.

'''
  aggregate(age~sex, data = pirates, FUN = mean)

'''

     sex      age
1 female 29.92241
2   male 24.96735
3  other 27.00000

'''

# -------------------------------- Measure of variablity -------------------------------------

-----
Range
-----

  min(pirates$height)
  max(pirates$height)
  range(pirates$height)


-------------------
Interquartile range
-------------------
quartiles divide the data into 4 parts. Note that, the interquartile range 
(IQR) - corresponding to the difference between the first and third quartiles - is sometimes 
used as a robust alternative to the standard deviation.

http://www.sthda.com/english/wiki/descriptive-statistics-and-graphics#measure-of-variablity


  quantile(pirates$weight)
  quantile(pirates$weight, probs = seq(0, 1, 0.5))
  quantile(pirates$weight, probs = seq(0, 1, 0.1)) # To compute deciles (0.1, 0.2, 0.3, …., 0.9)
  IQR(pirates$weight) # To compute the interquartile range

'''
     0%     25%     50%     75%    100% 
 33.000  62.075  69.550  76.900 105.600

    0%    50%   100% 
 33.00  69.55 105.60

    0%    10%    20%    30%    40%    50%    60%    70%    80%    90%   100% 
 33.00  56.09  60.60  63.47  66.70  69.55  71.84  75.10  79.12  84.31 105.60
'''

-------------------------------
Variance and standard deviation
-------------------------------
The variance represents the average squared deviation from the mean. The standard deviation is 
the square root of the variance. It measures the average deviation of the values, in the data, 
from the mean value.

  mean(pirates$beard.length) # 10.384
  var(pirates$beard.length) # 106.3589
  sd(pirates$beard.length) # 10.31305

-------------------------
Median absolute deviation
-------------------------
The median absolute deviation (MAD) measures the deviation of the values, in the data, 
from the median value.

  median(pirates$beard.length) # 9
  # Compute the median absolute deviation
  mad(pirates$beard.length) # 13.34


# ---------------------------------- Summary - Measure of Variability --------------------------

'''
Which measure to use?
  
Range. It’s not often used because it’s very sensitive to outliers.

Interquartile range. It’s pretty robust to outliers. It’s used a lot in combination with the 
median.

Variance. It’s completely uninterpretable because it doesn’t use the same units as the data. 
It’s almost never used except as a mathematical tool

Standard deviation. This is the square root of the variance. It’s expressed in the same units as 
the data. The standard deviation is often used in the situation where the mean is the measure of 
central tendency.

Median absolute deviation. It’s a robust way to estimate the standard deviation, for data with 
outliers. It’s not used very often.

In summary, the 'IQR and the standard deviation' are the two most common measures used to report 
the variability of the data.
'''

# -------- Computing an overall summary of a variable and an entire data frame -----------------
  
  summary() function
# The function summary() can be used to display several statistic summaries of either one 
# variable or an entire data frame.
# If the column is a numeric variable, mean, median, min, max and quartiles are returned.
# If the column is a factor variable, the number of observations in each group is returned.
  
  summary(pirates, digits = 1)
  summary(pirates$age)
  summary(pirates$headband)

  sapply() function
# It’s also possible to use the function sapply() to apply a particular function over a list or 
# vector. For instance, we can use it, to compute for each column in a data frame, the mean, sd, 
# var, min, quantile, …
  
  # Compute the mean of each numeric column
  sapply(pirates[, sapply(pirates, is.numeric)], mean)
  
  # Compute quartiles
  sapply(pirates[,sapply(pirates, is.numeric)], quantile)

# ============================================ Plotting ==========================================

'''

Now let’s make a plot! We’ll plot the relationship between pirate’s height and weight using the 
plot() function

'''
# Scatter plot - for continuous variables
plot(pirates$height, # x-coordinates
     pirates$weight) # y-coordinates

# Now let’s make a fancier version of the same plot by adding some customization

plot(
      pirates$height, # x-coordinates
      pirates$weight, # y-coordinates
      main = 'My first scatterplot of pirate data!',
      xlab = 'Height (in cm)',   # x-axis label
      ylab = 'Weight (in kg)',   # y-axis label
      pch = 16,                  # Filled circles, creates solid dots
      cex = 1.3,                 # creates dots that are 1.3 times bigger than the default (cex = 1)
      col = gray(.0, .1))        # Transparent gray

'''

Now let’s make it even better by adding gridlines and a blue regression line to measure the 
strength of the relationship.

The R function abline() can be used to add vertical, horizontal or regression lines to a graph.

Example
-------
http://www.sthda.com/english/wiki/abline-r-function-an-easy-way-to-add-straight-lines-to-a-plot-using-r-software
set.seed(1234); mydata<-rnorm(200)
hist(mydata, col="lightblue")
abline(v = mean(mydata), col="red", lwd=3, lty=2)

'''

grid() # Add gridlines

model <- lm(weight~height, data = pirates)
abline(model, col = 'purple') # # Add regression to plot

'''
Scatterplots are great for showing the relationship between two continuous variables, 
but what if your independent variable is not continuous?
'''

# syntax: boxplot(Numeric_Vector~Categorical_Variable_to_Group, ........)

boxplot(pirates$age~pirates$sword.type,
        notch = T, 
        col = c('Orange', 'Brown', 'Green', 'Blue'), 
        main = 'plot of ages by favorite sword')

pirateplot(formula = age ~ sword.type, 
           data = pirates,
           main = "Pirateplot of ages by favorite sword")

'''
Now let’s make another pirateplot showing the relationship between sex and height using a 
different plotting theme
'''

# syntax: boxplot(Numeric_Vector ~ Categorical_Variable_to_Group, ........)
bp <- boxplot(pirates$height~pirates$sex, 
        col = c('Red', 'Purple', 'Green'), 
        notch = F, 
        main = 'Plot of Height across Sex'
)

# Plot the summary values on the graph

for (i in seq(ncol(bp$stats))) # Each run prints all the stats for one boxplot
{
  # Each col represents all the stats for each boxplot, text() fn will be run 5 times for every loop
  text(x=i, y=bp$stats[,i] -
         0.02*(max(pirates$height, na.rm=TRUE) -
                min(pirates$height, na.rm=TRUE)),
                  labels=bp$stats[,i],
                    cex = .75)
}

# ====================================== Hypothesis tests ======================================

A hypothesis is an 'educated guess' about something in the world around you. 
It 'should be testable', either by experiment or observation.

Now, let’s do some basic hypothesis tests. First, let’s conduct a two-sample t-test to see 
if there is a significant difference between the ages (quantative) of pirates who do 
wear a headband (qualitative), and those who do not:

# Age by headband t-test
# IMPORTANT CONDITION: grouping factor (here headband - yes or no) must have exactly 2 levels
  
  t.test(pirates$age ~ pirates$headband, 
         alternative = 'two.sided')

'''

Welch Two Sample t-test

data:  pirates$age by pirates$headband
t = 0.35135, df = 135.47, p-value = 0.7259

alternative hypothesis: true difference in means is not equal to 0

95 percent confidence interval:
  -1.030754  1.476126
sample estimates:
  mean in group no mean in group yes 
27.55752          27.33484 

---------
INFERENCE: 
---------

t score = 0.35 and p-value = 0.73
NULL Hypothesis: The average difference in age among pirates wearing headband and those not is equal
alternative hypothesis: true difference in means is not equal to 0

Based on t-score, i.e. 0.35 and p-value, i.e. 0.73 we cannot reject the NULL hypothesis. Generally
we do not have enough evidence to reject the NULL hypothese if t-score is between -1.96 and +1.96
or p-value is > 0.05.

'''

Next, let’s test if there is a significant correlation between a pirate’s height (quantative) and
weight (quantative) using the cor.test() function:
  
cor.test(~height+weight, 
         data = pirates)

'''

Pearsons product-moment correlation

data:  height and weight
t = 81.161, df = 998, p-value < 2.2e-16

alternative hypothesis: true correlation is not equal to 0

95 percent confidence interval:
  0.9232371 0.9396050

sample estimates:
  cor 
0.9318938 

---------
INFERENCE
---------

We got a p-value of p < 2.2e-16, that’s scientific notation for p < .00000000000000016 – which 
is pretty much 0. Thus, we’d conclude that there is a significant (positive) relationship between 
a pirate’s height and weight.

'''

cor(pirates$height, pirates$weight) # 0.9318938

'''
-----
ANOVA - Used to compare the means of groups with more than 2 levels. For 2 levels use t.test()
-----

Now, let’s do an ANOVA testing if there is a difference between the number of tattoos pirates 
have based on their favorite sword

'''

anova(lm(pirates$tattoos ~ pirates$sword.type))

'''
Sure enough, we see another very small p-value of p < 2.2e-16, suggesting that the number of 
tattoos pirate’s have are different based on their favorite sword.
'''

'''
-------
Q-Q Plot - used to check whether the data is normally distributed.
http://www.sthda.com/english/wiki/descriptive-statistics-and-graphics#measure-of-variablity
https://stats.stackexchange.com/questions/52293/r-qqplot-how-to-see-whether-data-are-normally-distributed
-------
'''

qqnorm(pirates$age)
qqline(pirates$age)


'''
----------------
Frequency tables
http://www.sthda.com/english/wiki/descriptive-statistics-and-graphics#frequency-tables
----------------
'''

# Hair/eye color data
df <- as.data.frame(HairEyeColor)
head(df)
hair_eye_col <- df[rep(row.names(df), df$Freq), 1:3]
head(hair_eye_col)
rownames(hair_eye_col) <- 1:nrow(hair_eye_col)
head(hair_eye_col)

#frequency distribution: one categorical variable
hair.color <- hair_eye_col$Hair
eye.color <- hair_eye_col$Eye

# One categorical variable
table(hair.color)

'''
Black Brown   Red Blond 
  108   286    71   127
'''

# Graphics: to create the graphics, we start by converting the table as a data frame.
hair.df <- as.data.frame(table(hair.color))

head(hair.df)
plot(hair.color)
barplot(table(hair.color))

library(ggplot2)
ggplot(hair_eye_col,
        aes(x = hair_eye_col$Hair)) +
          geom_bar(aes(color = hair_eye_col$Eye))
            

# Two-way contingency table: Two categorical variables
contingency.tbl <- table(hair.color , eye.color)
contingency.tbl

# It’s also possible to use the function xtabs(), which will create cross tabulation of 
# data frames with a formula interface.
xtabs(~ Hair + Eye, data = hair_eye_col)

# Multiway tables: More than two categorical variables
xtabs(~ Hair + Eye + Sex, data = hair_eye_col)

# You can also use the function ftable() [for flat contingency tables]. 
# It returns a nice output compared to xtabs() when you have more than two variables:

ftable(Sex + Eye ~ Hair, data = hair_eye_col)

#------------------------------ Some Basic R Building Blocks -----------------------------------
# Pirates Guide 
# https://bookdown.org/ndphillips/YaRrr/test-your-r-might-1.html

1. Create the vector [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] in three ways: 
   once using c(), once using a:b, and once using seq().

    v.1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    v.2 <- 1:10
    v.3 <- seq(1,10)
    
2. Create the vector [2.1, 4.1, 6.1, 8.1] in two ways, once using c() and once using seq()

    v.4 <- c(2.1, 4.1, 6.1, 8.1)
    v.5 <- seq(2.1, 8.1, 2)
    
3. Create the vector [0, 5, 10, 15] in 3 ways: using c(), seq() with a by argument, 
   and seq() with a length.out argument.

    v.6 <- c(0, 5, 10, 15)
    v.7 <- seq(0, 15, by = 5)
    v.8 <- seq(0, 15, length.out = 4)
    
4. Create the vector [101, 102, 103, 200, 205, 210, 1000, 1100, 1200] using a combination 
   of the c() and seq() functions
   
    v.9 <- c(seq(101, 103), seq(200, 210, by = 5), seq(1000, 1200, by = 100))
    
5. A new batch of 100 pirates are boarding your ship and need new swords. You have 10 scimitars, 
   40 broadswords, and 50 cutlasses that you need to distribute evenly to the 100 pirates as they 
   board. Create a vector of length 100 where there is 1 scimitar, 4 broadswords, and 5 cutlasses 
   in each group of 10. That is, in the first 10 elements there should be exactly 1 scimitar, 
   4 broadswords and 5 cutlasses. The next 10 elements should also have the same number of each 
   sword (and so on).
   
    v.10 <- rep(c("scimitars", rep("broadswords", times = 4), rep("cutlasses", times = 5)), times = 10)

6. Create a vector that repeats the integers from 1 to 5, 10 times. 
   That is [1, 2, 3, 4, 5, 1, 2, 3, 4, 5, …]. The length of the vector should be 50!
     
    v.11 <- rep(c(1, 2, 3, 4, 5), times = 10)
   
7. Now, create the same vector as before, but this time repeat 1, 10 times, then 2, 10 times, etc., 
   That is [1, 1, 1, …, 2, 2, 2, …, … 5, 5, 5]. The length of the vector should also be 50

    v.12 <- rep(c(1, 2, 3, 4, 5), each = 10)
    
8. Create a vector containing 50 samples from a Normal distribution with a population mean of 20 and 
   standard deviation of 2.
   
    v.13 <- rnorm(50, mean = 20, sd = 2)
    
9. Create a vector containing 25 samples from a Uniform distribution with a lower bound of -100 
   and an upper bound of -50.
   
   v.14 <- runif(n = 25, min = -100, max = -50)
   
10. Create a vector that shows the square root of the integers from 1 to 10.

   v.sqrt <- sqrt(1:10)
   v.square <- (1:10)^2 # Square of integres from 1 to 10, also (1:10)**2
   
11. Place one grain in the first square of the chess board. Double the grain in the next square. 
    Follow this until the last square of the chess board. What would be the sum of the grains.

   v.chess.sq <- sum(2**(1:63))
   
12. Table contains data about 10 of my favorite movies. 

    0] Create new data vectors for each column
      
        movie <- c('Whatever Works', 'It Follows', 'Love and Mercy', 'The Goonies', 'Jiro Dreams of Sushi',	'There Will be Blood', 'Moon', 'Spice World',	'Serenity',	'Finding Vivian Maier')
        year <- c(2009, 2015, 2015, 1985, 2012, 2007, 2009, 1988, 2005, 2014)
        box.office <- c(35, 15, 15, 62, 3, 10, 321, 79, 39, 1.5)
        genre <- c('Comedy', 'Horror', 'Drama', 'Adventure', 'Documentary', 'Drama', 'Science Fiction', 'Comedy', 'Science Fiction', 'Documentary')
        time <- c(92, 97, 120, 90, 81, 158, 97, -84, 119, 84)
        rating <- c('PG-13', 'R', 'R', 'PG', 'G', 'R', 'R', 'PG-13', 'PG-13', 'Unrated')
        
    1] What is the name of the 10th movie in the list?
  
        movie[10] # "Finding Vivian Maier"

    2] What are the genres of the first 4 movies?
  
        genre[1:4] # "Comedy"    "Horror"    "Drama"     "Adventure"

    3] Some joker put Spice World in the movie names – it should be The Naked Gun’’ 
       Please correct the name.

       movie[which(movie == 'Spice World')] <- 'The Naked Gun’’' # which will return the index number 
       # of the element where the condition is met
       
    4] What were the names of the movies made before 1990?
  
       movie[year < 1990]

    5] How many movies were Dramas? What percent of the 10 movies were Dramas?
      
       sum(genre == 'Drama')
       sum(genre == 'Drama')/length(movie)
       
    6] One of the values in the time vector is invalid. Convert any invalid values in this vector 
       to NA. Then, calculate the mean movie time.
       
       time[time < 0] <- NA # 92  97 120  90  81 158  97  NA 119  84
       mean(time, na.rm = T) # 104.2222
       
    7] What were the names of the Comedy movies? What were their boxoffice totals? 
       (Two separate questions)

       movie[genre == 'Comedy'] # "Whatever Works" "Serenity" 
       sum(box.office[genre == 'Comedy']) # 74
       
       # The above code can be combined as below to get the box office totals of the comedy genre movies
       sum(box.office[which(movie %in% movie[genre == 'Comedy'])])
       # Disecting the above code
          1. genre == 'Comedy' returns TRUE where genre is Comedy
          2. movie[genre == 'Comedy'], returns the movie names where genre is Comedy
          3. movie %in% movie[genre == 'Comedy'], returns T where movie names matches step 2
          4. which(movie %in% movie[genre == 'Comedy']), returns the index numbers where value is T
          5. box.office[which(movie %in% movie[genre == 'Comedy'])], returns the box office collections of the index numbers in step 4
          6. sum(box.office[which(movie %in% movie[genre == 'Comedy'])]), returns the sum of box office collections
    
    8] What were the names of the movies that made less than $50 Million dollars AND were Comedies?
  
       movie[genre == 'Comedy' & box.office < 50] # "Whatever Works"

    9] What was the median boxoffice revenue of movies rated either G or PG?
  
       median(box.office[rating == 'G' | rating == 'PG']) # 32.5

    10] What percent of the movies were rated R OR were comedies?
  
       sum(rating == 'R' | genre == 'Comedy')/length(movie)

    11] Rename the column 'tax' to 'tx.amt'
        
        names(wg.sale) # "name"  "age"   "price" "cost"  "tax" 
        names(wg.sale)[5] <- 'tx.amt' # Not recommended as the index is hard-coded
        names(wg.sale)[names(wg.sale) == 'tax'] <- 'tx.amt' # Or
        names(wg.sale)[names(wg.sale) %in% 'cost'] <- 'wg.cost'

    12] Create a dataframe as below

        survey <- data.frame(
          name = c("Astrid", "Lea", "Sarina", "Remon", "Letizia", "Babice", "Jonas", "Wendy", "Niveditha", "Gioia"),
          sex = c("F", "F", "F", "M", "F", "F", "M", "F", "F", "F"),
          age = c(30, 25, 25, 29, 22, 22, 35, 19, 32, 21),
          superhero = c("Batman", "Superman", "Batman", "Spiderman", "Batman",
                        "Antman", "Batman", "Superman", "Maggott", "Superman"),
          tattoos = c(11, 15, 12, 5, 65, 3, 9, 13, 900, 0),
          stringsAsFactors = FALSE
        )
        
        1] What is the median age of the 10 pirates?
      
            median(survey$age)

        2] What was the mean age of female and male pirates separately?
  
            with(survey, c(mean(age[sex == 'M']), mean(age[sex == 'F']))) # 32

        3] What was the most number of tattoos owned by a male pirate?
  
            with(survey, max(tattoos[sex == 'M'])) # 9

        4] What percent of pirates under the age of 32 were female?
  
            with(subset(survey, age < 32), mean(age[sex == 'F']))

        5] Add a new column to the dataframe called tattoos.per.year which shows how many 
           tattoos each pirate has for each year in their life
           
            survey <- survey %>% mutate('tattoos.per.year' = tattoos/age)
            
        6] Which pirate had the most number of tattoos per year?
  
            with(survey, name[tattoos.per.year == max(tattoos.per.year)])
    
        7] What are the names of the female pirates whose favorite superhero is Batman?
  
            with(subset(survey, sex == 'F'), name[superhero == 'Batman'])

        8] What was the median number of tattoos of pirates over the age of 20 whose favorite 
           superhero is Spiderman?
             
            with(subset(survey, age > 20 & superhero == 'Spiderman'), median(tattoos)) # 5
           
# ---------------------  advanced functions and procedures for manipulating dataframes -----------
           
   # Exam data
   exam <- data.frame(
     id = 1:5,
     q1 = c(1, 5, 2, 3, 2),
     q2 = c(8, 10, 9, 8, 7),
     q3 = c(3, 7, 4, 6, 4))
   
   # Demographic data
   demographics <- data.frame(
     id = 1:5,
     sex = c("f", "m", "f", "f", "m"),
     age = c(25, 22, 24, 19, 23))
   
   # Merge
   combined <- merge(exam, demographics, by = 'id')
   
   # Mean q1 score for each sex
   aggregate(FORMULA, DATA, FUN)
   aggregate(q1 ~ sex, combined, FUN = mean)
   
   # Median q3 score for each sex, but only for those
   # older than 20
   aggregate(formula = q3 ~ sex, 
             data = combined,
             subset = age > 20,
             FUN = median)
            
   # Many summary statistics by sex using dplyr!
   library(dplyr)
   combined %>% group_by(sex) %>% summarise(q1.mean = mean(q1),
                                            q2.mean = mean(q2),
                                            q3.mean = mean(q3),
                                            age.median = median(age),
                                            n = n())
   # order(): Sorting data
   # Sort the pirates dataframe by height
   pirates.ordered <- pirates[order(pirates$height), 1:4]
   pirates.ordered[1:5, 1:4]
   
   # Descending Order
   pirates.ordered <- pirates[order(pirates$height, decreasing = T), 1:4]
   
   # Sort the pirates dataframe by sex and then height
   pirates.ordered <- pirates[order(pirates$sex, pirates$height, decreasing = T), 1:4]
   
   
# -------------- aggregate(): Grouped aggregation ----------------------------------------
   
names(ChickWeight) 
   
   # General structure of aggregate()
   aggregate(formula = dv ~ iv, # dv is the data, iv is the group 
             FUN = fun, # The function you want to apply
             subset = filter condition # a subset of data to consider,
             data = df) # The dataframe object containing dv and iv
   
   # Calculate the mean/max weight for each value of Diet
   aggregate(formula = weight ~ Diet,
             FUN = mean,
             data = ChickWeight)
   
   aggregate(formula = weight ~ Diet,
             FUN = max,
             data = ChickWeight) # Or using dplyr's group_by as below
   
   # Finding Group Max
   ChickWeight %>% group_by(Diet) %>% summarise(Grp.Max = max(weight))
   
   # Calculate the mean chicken weights for each diet, but only when the chicks are less than 
   # 10 weeks old
   aggregate(formula = weight ~ Diet,
             FUN = mean,
             subset = Time < 10,
             data = ChickWeight)
   
   # Get the mean weight of the chicks for all combinations of both Diet and Time, 
   # but now only for weeks 0, 2, and 4
   aggregate(formula = weight ~ Diet + Time,
             FUN = mean, # Calculate the mean of each group
             subset = Time %in% c(0,2,4), # Only when Chicks are 0, 2, and 4 weeks old
             data = ChickWeight)
   
  # Using dplyr
   ChickWeight %>% filter(Time %in% c(0,2,4)) %>% 
      group_by(Diet, Time) %>% 
          summarise(mean.wt = mean(weight))
   
   
# ---------------------------------- dplyr ------------------------------------------------------
   
  # Template for using dplyr
   
   dat.df %>%
     filter(iv3 > 30) %>%
      group_by(iv1, iv2) %>%
        summarise(a = mean(col.a),
                  b = sd(col.b),
                  c = max(col.c),...)
   
  # When you use dplyr, you write code that sounds like: 
  # “The original dataframe is XXX, 
  # now filter the dataframe to only include rows that satisfy the conditions YYY, 
  # now group the data at each level of the variable(s) ZZZ, 
  # now summarize the data and calculate summary functions XXX…”
   
   pirates.agg <- pirates %>%      # Start with the pirates dataframe
     filter(headband == "yes") %>% # Only pirates that wear hb
     group_by(sex, college) %>%    # Group by these variables
     summarise( 
       age.mean = mean(age),      # Define first summary...
       tat.med = median(tattoos), # you get the idea...
       n = n()                    # How many are in each group?
     ) # End
  
   
# -------------------------------- apply family of functions -----------------------------------
   
   There is an entire class of apply functions in R that apply functions to groups of data. 
   For example, tapply(), sapply() and lapply() each work very similarly to aggregate(). 
   For example, you can calculate the average length of movies by genre with tapply() as follows.
   
   with(movies, tapply(X = time,        # DV is time
                       INDEX = genre,   # IV is genre
                       FUN = mean,      # function is mean
                       na.rm = TRUE))   # Ignore missing
   
   tapply(), sapply(), and lapply() all work very similarly, their main difference is in the 
   structure of their output. For example, lapply() returns a list.
   
 13] You’re in charge of analyzing the results of an experiment testing the effects of different 
     forms of caffeine on a measure of performance. In the experiment, 100 participants were given 
     either Green tea or coffee, in doses of either 1 or 5 servings. They then performed a cognitive 
     test where higher scores indicate better performance.
     
     1] Load the tab delimited dataset from https://raw.githubusercontent.com/ndphillips/ThePiratesGuideToR/master/data/caffeinestudy.txt 
        as a new dataframe called 'caffeine'.
        
        caffeine <- read.table(file = 'https://raw.githubusercontent.com/ndphillips/ThePiratesGuideToR/master/data/caffeinestudy.txt',
                               sep = '\t')
        
    2] Calculate the mean age for each gender

        aggregate(formula = age ~ gender,
                  FUN = mean,
                  data = caffeine)
        # dplyr
        caffeine %>%
            group_by(gender) %>%
                summarise(mean.age = mean(age))
        
    3] Calculate the mean age for each drink

        aggregate(formula = age ~ drink,
                  FUN = mean,
                  data = caffeine)
        
    4] Calculate the mean age for each combined level of both gender and drink

        aggregate(formula = age ~ gender + drink,
                  FUN = mean,
                  data = caffeine)
        # dplyr
        caffeine %>%
            group_by(gender, drink) %>%
                summarise(mean.age = mean(age))
        
    5] Calculate the median score for each age

        aggregate(formula = score ~ age,
                  FUN = median,
                  data = caffeine)
        
    6] For men only, calculate the maximum score for each age

        aggregate(formula = score ~ age,
                  FUN = max,
                  subset = gender == 'male',
                  data = caffeine)
        # dplyr
        caffeine %>%
            filter(gender == 'male') %>%
                group_by(age) %>%
                    summarise(max.age = max(age))
        
    7] Create a dataframe showing, for each level of drink, the mean, median, maximum, 
       and standard deviation of scores.

        caffeine.grp.summary <- aggregate(formula = score ~ drink,
                                          FUN = summary,
                                          data = caffeine)
        
        #dplyr
        caffeine.grp.summary <- caffeine %>%
                                    group_by(drink) %>%
                                        summarise(mean.score = mean(score),
                                                  median.score = median(score),
                                                  max.score = max(score),
                                                  sd.score = sd(score))
        
    8] Only for females above the age of 20, create a table showing, for each combined level of 
       drink and cups, the mean, median, maximum, and standard deviation of scores. Also include 
       a column showing how many people were in each group.
       
       caffeine.grp.summary <- caffeine %>%
          filter(age > 20 & gender == 'female') %>%
             group_by(drink, cups) %>%
               summarise(mean.score = mean(score),
                         median.score = median(score),
                         max.score = max(score),
                         sd.score = sd(score),
                         people.count = n())
         
       
#---------------------------------------- plotting ----------------------------------------------

Certain high–level plotting functions like plot() and hist() create brand new canvases, while 
other low–level plotting functions like points() and segments() place elements on top of existing 
canvases.

# Histogram: hist()

Histograms are the most common way to plot a vector of numeric data.


# Barplot: barplot()

A barplot typically shows summary statistics for different groups.

barplot(height = 1:5,  # A vector of heights
        names.arg = c("G1", "G2", "G3", "G4", "G5"), # A vector of names
        main = "Example Barplot", 
        xlab = "Group", 
        ylab = "Height")

# Clustered barplot

If you want to create a clustered barplot, with different bars for different groups of data, 
you can enter a matrix as the argument to height. R will then plot each column of the matrix as 
a separate set of bars. 

swim.data <- cbind(c(2.1, 3), # Naked Times
                   c(1.5, 3)) # Clothed Times

colnames(swim.data) <- c("Naked", "Clothed")
rownames(swim.data) <- c("No Shark", "Shark")

# Print result
swim.data

barplot(height = swim.data,
        beside = TRUE,                        # Put the bars next to each other
        legend.text = TRUE,                   # Add a legend
        col = c(transparent("green", .2), 
                transparent("red", .2)),
        main = "Swimming Speed Experiment",
        ylab = "Speed (in meters / second)",
        xlab = "Clothing Condition",
        ylim = c(0, 4))

# points()

To add new points to an existing plot, use the points() function. Use points() to create a plot 
with different symbol types for different data.

# Create a blank plot
plot(x = 1,
     type = "n",
     xlim = c(100, 225), 
     ylim = c(30, 110),
     pch = 16,
     xlab = "Height", 
     ylab = "Weight",
     main = "Adding points to a plot with points()")

# Add coral2 points for male data
points(x = pirates$height[pirates$sex == "male"],
       y = pirates$weight[pirates$sex == "male"],
       pch = 16,
       col = transparent("coral2", trans.val = .8))

# Add steelblue points for female data
points(x = pirates$height[pirates$sex == "female"],
       y = pirates$weight[pirates$sex == "female"],
       pch = 16,
       col = transparent("steelblue3", trans.val = .8))

# ------------------------------------ plot + point labels ---------------------------------------

# Create data vectors
height <- c(156, 175, 160, 172, 159, 165, 178)
weight <- c(65, 74, 69, 72, 66, 75, 75)
id <- c("andrew", "heidi", "becki", "madisen", "david", "vincent", "jack")

# Plot data
plot(x = height, 
     y = weight, 
     xlim = c(155, 180), 
     ylim = c(65, 80), 
     pch = 16,
     col = transparent("purple", .7))

# Add id labels
text(x = height, 
     y = weight,
     labels = id, 
     pos = 3)      # Put labels above the points

# Add gridlines
grid()


# ------------- Combining text and numbers with paste() ----------------------------------------

# Create the plot
plot(x = ChickWeight$Time,
     y = ChickWeight$weight, 
     col = gray(.3, .5), 
     pch = 16,
     main = "Combining text with numeric scalers using paste()")

# Add reference line
abline(h = mean(ChickWeight$weight), 
       lty = 2)

# Add text
text(x = 3, 
     y = mean(ChickWeight$weight), 
     labels = paste("Mean weight =", 
                    round(mean(ChickWeight$weight), 2)),
     pos = 3)

# ---------------------------------------- curve() ----------------------------------------------

# Plot the function x^2 from -10 to +10
curve(expr = x^2, 
      from = -10, 
      to = 10, lwd = 2)

# If you want to add a custom function to a plot, you can define the function and then use that 
# function name as the argument to expr. 

dnrm.vect <- function(x){
  dnorm(x, mean = 2, sd = 3)
}

curve(expr = dnrm.vect,
      from = -10,
      to = 10,
      lwd = 2)

# ------------------ Saving plots to a file with pdf(), jpeg() and png() -------------------------

syntax: 
  
  function(file = file path, width = plot width in inches, height = plot height in inches)
  dev.off() # finish creating the image file

# Step 1: Call the pdf command to start the plot

file.path <- c(getwd(), 'plot.pdf')
full.path <- paste(file.path, collapse="/") # "D:/Machine Learning/R WS/MyRProject/plot.pdf"

pdf(file = full.path,   # The path you want to save the file in
    width = 4, # The width of the plot in inches
    height = 4) # The height of the plot in inches

# Step 2: Create the plot with R code

plot(x = 1:10, 
     y = 1:10)

# Step 3: Run dev.off() to create the file

dev.off()

You’ll notice that after you close the plot with dev.off(), you’ll see a message in the prompt 
like “null device”. That’s just R telling you that you can now create plots in the main R plotting 
window again.

# -------- Making the size and color of each point dynamic -------------------------------------

# Just the first 100 pirates
pirates.r <- pirates[1:100,]

plot(x = pirates.r$height,
     y = pirates.r$weight,
     xlab = "height",
     ylab = "weight",
     main = "Specifying point sizes and colors with a 3rd variable",
     cex = pirates.r$tattoos  / 6,          # Point size reflects how many tattoos they have
     col = gray(1 - pirates.r$tattoos / 20)) # color reflects tattoos

grid()

# -------Test your R might (Test of Hypothesis - t.test(), cor.test(). chsq.test()) ----------

14] Do male pirates have significantly longer beards than female pirates? Test this by conducting 
    a t-test on the relevant data in the pirates dataset. (Hint: You’ll have to select just the 
    female and male pirates and remove the ‘other’ ones using subset())

    # This is about testing the mean beard length among males and females in pirates dataframe
    # we conduct a 2-samlpe t-test
    names(pirates)
    head(pirates)
    lng.beard.htest <- t.test(formula = beard.length ~ sex,
                              data = pirates,
                              subset = sex %in% c('male', 'female'))
    apa(lng.beard.htest) # "mean difference = 19.02, t(499.82) = -70.89, p < 0.01 (2-tailed)",
    # Answer: Yes, men have significantly longer beards than women

15] Are pirates whose favorite pixar movie is Up more or less likely to wear an eye patch than
    those whose favorite pixar movie is Inside Out? Test this by conducting a chi-square test 
    on the relevant data in the pirates dataset. (Hint: Create a new dataframe that only 
    contains data from pirates whose favorite move is either Up or Inside Out using. 
    Then do the test on this new dataframe.)
    
    Hint: Does the frequency of wearing eye patch is influenced by the name of the movie - 
          'Up' vs 'Inside Out'. It warrants 2-sample chi-square test.
    
    unique(pirates$fav.pixar)
    pirates.sub <- subset(pirates, fav.pixar %in% c('Up', 'Inside Out'))
    chsq.htest <- chisq.test(x = table(pirates.sub$eyepatch,
                                       pirates.sub$fav.pixar))
    apa(chsq.htest) # "X(1, N = 422) = 88.96, p < 0.01 (2-tailed)", Yes, pirates whose favorite 
    #movie is 'Inside Out' are much more likely to wear an eye patch than those whose favorite 
    #Pixar movie is 'Up'
    
16] Do longer movies have significantly higher budgets than shorter movies? Answer this question 
    by conducting a correlation test on the appropriate data in the movies dataset.
    
    Hint: The idea is to determine how strong the relationship is between the numerical features.
    It can be inferred via a correlation test.
    
    names(movies)
    cor.htest <- cor.test(formula = ~ time + budget,
                          data = movies)
    apa(cor.htest) # "r = 0.28, t(2313) = 14.09, p < 0.01 (2-tailed)", Yes, longer movies tend 
    #to have higher budgets than shorter movies
    
17] Do R rated movies earn significantly more money than PG-13 movies? Test this by conducting 
    a t-test on the relevant data in the movies dataset.
    
    Hint: The idea is to compare the mean earnings of 'R' rated movies vs the mean earnings
    of 'PG-13' rated movies. It warrants 2-sample t-test.
    
    names(movies)
    head(movies)
    unique(movies$rating)
    t.htest <- t.test(formula = revenue.all ~ rating,
                      data = movies,
                      subset = rating %in% c('PG-13','R')
                      )
    apa(t.htest) # "mean difference = -68.86, t(1779.2) = 10.67, p < 0.01 (2-tailed)", 
    #No, R Rated movies do not earn significantly more than PG-13 movies. 
    #In fact, PG-13 movies earn significantly more than R rated movies.
    '''
    sample estimates:
    mean in group PG-13     mean in group R 
              148.49046            79.62925
    '''
    
18] Are certain movie genres significantly more common than others in the movies dataset?
  
    Hint: It is the comparison of the frequency of occurrences of movies grouped by genre. It
    warrants 1-sample chi-square test.
    
    chisq.1.htest <- chisq.test(x = table(movies$genre))
    apa(chisq.1.htest) # "X(13, N = 4682) = 6408.91, p < 0.01 (2-tailed)", Yes, some movie genres 
    #are more common than others.
    
19] Do sequels and non-sequels differ in their ratings? Test this by conducting a 2-sample 
    chi-square test on the relevant data in the movies dataset.
    
    names(movies)
    unique(movies$sequel)
    sum(is.na(movies$sequel))
    chsq.2.htest <- chisq.test(x = table(movies$sequel,
                                         movies$rating)
                               )
    apa(chsq.2.htest) # "X(6, N = 3920) = 23.64, p < 0.01 (2-tailed)", Yes, sequels are more likely 
    #in some genres than others.
    
    Note: The error “Warning in chisq.test” we get in this code is due to the fact that some 
    cells have no entries. This can make the test statistic unreliable. You can correct it by 
    adding a value of 20 to every element in the table as follows:
      
    genre.sequel.table <- table(movies$genre, movies$sequel)
    # Add 20 to each cell to correct for empty cells
    genre.sequel.table <- genre.sequel.table + 20
    
# ----------------------------------------- ANOVA ----------------------------------------------
    
    https://bookdown.org/ndphillips/YaRrr/anova.html
    
    When do you conduct an ANOVA? You conduct an ANOVA when you are testing the effect of one 
    or more nominal (aka factor) independent variable(s) on a numerical dependent variable.
    
    One-way or two-way refers to the number of independent variables (IVs) in your Analysis of 
    Variance test. One-way has one independent variable (with 2 levels) and two-way has two 
    independent variables (can have multiple levels).
    
    names(poopdeck)
    sum(is.na(poopdeck))
    class(poopdeck$cleaner)
    plot(as.factor(poopdeck$cleaner), poopdeck$time)
    
    From the plot above, it looks like cleaners a and b are the same, and cleaner c is a bit 
    faster. To test this, we’ll create an ANOVA object with aov.
    
    # Step 1: aov object with time as DV and cleaner as IV
    cleaners.aov <- aov(formula = time ~ cleaner,
        data = poopdeck)
    
    # Step 2: Look at the summary of the anova object
    summary(cleaners.aov)
    
    '''
                 Df Sum Sq Mean Sq F value  Pr(>F)   
    cleaner       2   6057    3028   5.294 0.00526 ** # significant effect of cleaner on time
    Residuals   597 341511     572 
    '''
    
    The main result from our table is that we have a significant effect of cleaner on 
    cleaning time. However, the ANOVA table does not tell us which levels of the 
    independent variable differ. In other words, we don’t know which cleaner is better 
    than which. To answer this, we need to conduct a post-hoc test.
    
    If you’ve found a significant effect of a factor, you can then do post-hoc tests to test 
    the difference between each all pairs of levels of the independent variable. One of the most 
    common post-hoc tests for standard ANOVAs is the Tukey Honestly Significant Difference (HSD)
    test. 
    
    # Step 3: Conduct post-hoc tests
    TukeyHSD(cleaners.aov)
    '''
    $cleaner
    diff        lwr        upr     p adj
    b-a -0.42  -6.039575  5.1995746 0.9831441
    c-a -6.94 -12.559575 -1.3204254 0.0107324
    c-b -6.52 -12.139575 -0.9004254 0.0180906
    '''
    This above table shows pair-wise differences between each group pair. The diff column shows 
    the mean differences between groups, a confidence interval for the difference, and a p-value 
    testing the null hypothesis that the group differences are not different.
    
    It is helpful to combine an ANOVA summary table with a regression summary table. Because 
    ANOVA is just a special case of regression (where all the independent variables are factors),
    you’ll get the same results with a regression object as you will with an ANOVA object. 
    However, the format of the results are different and frequently easier to interpret.
    
    # Step 4: Create a regression object
    cleaner.lm <- lm(formula = time ~ cleaner,
                     data = poopdeck)
    summary(cleaner.lm)
    
    '''
    Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
    (Intercept)   66.020      1.691  39.037  < 2e-16 ***
    cleanerb      -0.420      2.392  -0.176  0.86066    
    cleanerc      -6.940      2.392  -2.902  0.00385 **
    '''
    
    As you can see, the regression table does not give us tests for each variable like the ANOVA 
    table does. Instead, it tells us how different each level of an independent variable is from 
    a default value. You can tell which value of an independent variable is the default variable 
    just by seeing which value is missing from the table. In this case, I don’t see a coefficient
    for cleaner 'a', so that must be the default value.
    
    The intercept in the table tells us the mean of the default value. In this case, 
    the mean time of cleaner 'a' was 66.02. The coefficients for the other levels tell us that 
    cleaner 'b' is, on average 0.42 minutes faster than cleaner a, and cleaner 'c' is on average 
    6.94 minutes faster than cleaner a. Not surprisingly, these are the same differences we saw 
    in the Tukey HSD test!
      
    #------------------------------- Two-way ANOVA --------------------------------------------
      
  
    To conduct a two-way ANOVA, just include additional independent variables in the regression 
    model formula with the + sign. That’s it. All the steps are the same. Let’s conduct a 
    two-way ANOVA with both cleaner and type as independent variables.
    
    # Step 1: Create ANOVA object with aov()
    cleaner.type.aov <- aov(formula = time ~ cleaner + type,
                            data = poopdeck)
    
    # Step 2: Get ANOVA table with summary()
    summary(cleaner.type.aov)
    '''
                 Df Sum Sq Mean Sq F value  Pr(>F)    
    cleaner       2   6057    3028   6.945 0.00104 ** 
    type          1  81620   81620 187.177 < 2e-16 ***
    Residuals   596 259891     436
    '''
    It looks like we found significant effects of both independent variables.
    
    # Step 3: Conduct post-hoc tests
    TukeyHSD(cleaner.type.aov)
    '''
    $cleaner
         diff        lwr       upr     p adj
    b-a -0.42  -5.326395  4.486395 0.9779465
    c-a -6.94 -11.846395 -2.033605 0.0027112
    c-b -6.52 -11.426395 -1.613605 0.0053376
    
    $type
                 diff     lwr      upr      p adj
    shark-parrot 23.32667 19.97811 26.67522     0
    '''
    The only non-significant group difference we found is between cleaner b and cleaner a. 
    All other comparisons were significant.
    
    # Step 4: Look at regression coefficients
    cleaner.type.lm <- lm(formula = time ~ cleaner + type,
                          data = poopdeck)
    summary(cleaner.type.lm)
    '''
    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept)   54.357      1.705  31.881  < 2e-16 ***
    cleanerb      -0.420      2.088  -0.201 0.840665    
    cleanerc      -6.940      2.088  -3.323 0.000944 ***
    typeshark     23.327      1.705  13.681  < 2e-16 ***
    '''
    
    Now we need to interpret the results in respect to two default values 
    (here, cleaner = a and type = parrot). The intercept means that the average time for 
    cleaner 'a' on parrot 'poop' was 54.36 minutes. Additionally, the average time to clean 
    shark 'poop' was 23.33 minutes slower than when cleaning parrot poop.
    
    library(dplyr)
    names(poopdeck)
    
    poopdeck %>% 
      filter(cleaner == 'a') %>% 
        group_by(type) %>% 
          summarise(mean.time = mean(time))
    
# -------------------------------- ANOVA (with interactions) -----------------------------------
    
    Interactions between variables test whether or not the effect of one variable depends on 
    another variable. For example, we could use an interaction to answer the question: Does the 
    effect of cleaners depend on the type of poop they are used to clean?
      
    Let’s repeat our previous ANOVA with two independent variables, but now we’ll include the 
    interaction between cleaner and type. 
    To do this, we’ll set the formula to time ~ cleaner * type.
    
    # Step 1: Create ANOVA object with interactions
    cleaner.type.int.aov <- aov(formula = time ~ cleaner * type,
                                data = poopdeck)
      
    # Step 2: Look at summary table
    summary(cleaner.type.int.aov)
    '''
                  Df Sum Sq Mean Sq F value   Pr(>F)    
    cleaner        2   6057    3028   7.824 0.000443 ***
    type           1  81620   81620 210.863  < 2e-16 ***
    cleaner:type   2  29968   14984  38.710  < 2e-16 ***
    Residuals    594 229923     387 
    '''
    Looks like we did indeed find a significant interaction between cleaner and type. 
    In other words, the effectiveness of a cleaner depends on the type of poop it’s being 
    applied to.
    
    # Step 3: Conduct post-hoc tests
    TukeyHSD(cleaner.type.int.aov) # Independently interpreting the results is difficult. 
    #Let us use the regression model summary along-side.
    '''
    $cleaner
     diff       lwr      upr     p adj
    b-a -0.42  -5.04266  4.20266 0.9751910
    c-a -6.94 -11.56266 -2.31734 0.0013118 # Independently on average cleaner 'c' takes 6.94 min less than cleaner 'a'
    c-b -6.52 -11.14266 -1.89734 0.0028018 # Independently on average cleaner 'c' takes 6.52 min less than cleaner 'b'
    
    $type
                 diff      lwr      upr p adj
    shark-parrot 23.32667 20.17176 26.48157     0 # On ave, cleaning shark poop takes 23.33 min more than parrot poop
    
    $`cleaner:type`
    diff         lwr         upr     p adj
    b:parrot-a:parrot   8.06   0.1051112  16.0148888 0.0449641
    c:parrot-a:parrot  10.37   2.4151112  18.3248888 0.0028993 # Cleaner 'c' on parrot poop takes 10.4 min more than cleaner 'a'
    a:shark-a:parrot   40.52  32.5651112  48.4748888 0.0000000
    b:shark-a:parrot   31.62  23.6651112  39.5748888 0.0000000
    c:shark-a:parrot   16.27   8.3151112  24.2248888 0.0000001 
    c:parrot-b:parrot   2.31  -5.6448888  10.2648888 0.9618601 # Cleaner 'c' on parrot poop takes 2.3 min more than cleaner 'b'
    a:shark-b:parrot   32.46  24.5051112  40.4148888 0.0000000
    b:shark-b:parrot   23.56  15.6051112  31.5148888 0.0000000
    c:shark-b:parrot    8.21   0.2551112  16.1648888 0.0385298
    a:shark-c:parrot   30.15  22.1951112  38.1048888 0.0000000
    b:shark-c:parrot   21.25  13.2951112  29.2048888 0.0000000
    c:shark-c:parrot    5.90  -2.0548888  13.8548888 0.2780324
    b:shark-a:shark    -8.90 -16.8548888  -0.9451112 0.0181288
    c:shark-a:shark   -24.25 -32.2048888 -16.2951112 0.0000000 # Cleaner 'c' on shark poop takes 24.3 min LESS than cleaner 'a'
    c:shark-b:shark   -15.35 -23.3048888  -7.3951112 0.0000008 # Cleaner 'c' on shark poop takes 15.4 min LESS than cleaner 'b'
    '''
    
    To understand the nature of the difference, we’ll look at the regression coefficients from a 
    regression object:
      
    # Step 4: Calculate regression coefficients
    cleaner.type.int.lm <- lm(formula = time ~ cleaner * type,
                              data = poopdeck)
    summary(cleaner.type.int.lm)
    '''
    Coefficients:
                        Estimate Std. Error t value Pr(>|t|)    
    (Intercept)          45.760      1.967  23.259  < 2e-16 ***
    cleanerb              8.060      2.782   2.897 0.003908 **  # Refer Line-1 of Tukey o/p. Cleaner 'b' on parrot poop takes 8 min more than cleaner 'a'
    cleanerc             10.370      2.782   3.727 0.000212 ***
    typeshark            40.520      2.782  14.563  < 2e-16 *** # Refer Line-3 of Tukey o/p and dplyr o/p. Cleaner 'a' on shark poop takes 40.5 min more than cleaning parrot poop
    cleanerb:typeshark  -16.960      3.935  -4.310 1.91e-05 *** # cleaner b is, on average 16.96 minutes faster when cleaning shark poop compared to parrot poop
    cleanerc:typeshark  -34.620      3.935  -8.798  < 2e-16 ***
    '''
    Use the regression model summary along side tukey output. From the summary it is 
    evident that cleaner 'a' and 'parrot poop' type are base/default categories (as they are not listed as co-efficients). 
    All inferences should be based on these. 
    
    The interaction terms tell us how the effect of cleaners changes when one is cleaning shark poop.
    
    The negative estimate (-16.96) for cleanerb:typeshark means that cleaner b is, on average 16.96 minutes 
    faster when cleaning shark poop compared to parrot poop.
    
    Because the previous estimate for cleaner b (for parrot poop) was just 8.06, this suggests that cleaner b 
    is slower than cleaner a for parrot poop, but faster than cleaner a for shark poop.
    
    Same thing for cleaner c which simply has stronger effects in both directions.
    
# ------------------------------- Type I, Type II, and Type III ANOVAs --------------------------------------
    
    It turns out that there is not just one way to calculate ANOVAs. In fact, there are three different types -
    called, Type 1, 2, and 3 (or Type I, II and III). These types differ in how they calculate variability 
    (specifically the sums of of squares).
    
    BALANCED DATA
    If data is relatively balanced, meaning that there are relatively equal numbers of observations in each 
    group, then all three types will give you the same answer.
    
    UNBALANCED DATA
    Meaning that some groups of data have many more observations than others, then you need to use 
    Type II (2) or Type III (3).
    
    The standard aov() function in base-R uses Type I sums of squares (only appropriate for balanced data).
    Use the Anova() function in the car package for unbalanced data. It has an argument 'type' to specify the
    type of Anova (2 or 3).
    
    Anova() function requires you to enter a regression object as the main argument, and not a formula and 
    dataset. That is, you need to first create a regression object from the data with lm() (or glm()), and 
    then enter that object into the Anova() function. You can also do the same thing with the standard aov() 
    function`.
    
    # Step 1: Calculate regression object with lm()
    time.lm <- lm(formula = time ~ type + cleaner,
                  data = poopdeck)
    
    # ANOVA - Type I, II and III
    
    # Type I ANOVA - aov()
    time.I.aov <- aov(time.lm)
    summary(time.I.aov)
    
    # Type II ANOVA - Anova(type = 2)
    time.II.aov <- car::Anova(time.lm, type = 2)
    time.II.aov
    
    # Type III ANOVA - Anova(type = 3)
    time.III.aov <- car::Anova(time.lm, type = 3)
    time.III.aov
    
    As it happens, the data in the poopdeck dataframe are perfectly balanced (so we’ll get exactly the same 
    result for each ANOVA type. However, if they were not balanced, then we should not use the Type I ANOVA 
    calculated with the aov() function.
                                                                              
    To see if your data are balanced, you can use the function:
      
      # Are observations in the poopdeck data balanced?
      with(poopdeck,
           table(cleaner, type))
    
    As you can see, in the poopdeck data, the observations are perfectly balanced, so it doesn’t matter 
    which type of ANOVA we use to analyse the data.
    
    
# --- Linear	models	and	linear	mixed	effects	models	in	R (Bodo Winter) --------------------------
    http://www.bodowinter.com/tutorial/bw_LME_tutorial1.pdf
    
    Linear	models	and	linear	mixed	models	are	an	impressively	powerful	and	flexible	
    tool	 for	 understanding	 the	 world.
    
    So,	what	does	 the	linear	model	do?	Assume	 you	knew	nothing	about	males	and	
    females,	and	you	were	interested	in	whether the	voice	pitch	of	males	and	females	
    differs,	and	if	so,	by	how	much.
    
    '''
    So	you	take	a	bunch	of	males	and	a	bunch	of	females,	and	ask	them	to	say	a	single	
    word,	say	“mama”,	and	you	measure	the	respective	voice	pitches.	Your	data	might	
    look	something	like	this:
    '''
    
    pitch = c(233,204,242,130,112,142)
    sex = c(rep("female",3),rep("male",3))
    
    # For	a	better	overview,	let’s	combine	these	two	objects	into	a	data	frame:
      
    my.df = data.frame(sex,pitch)
    
    # O.k.,	 now	 we’ll	 proceed	 with	 the	 linear	model.
    
    xmdl = lm(pitch ~ sex, my.df)
    
    '''
    We	modeled	pitch	as	a	 function	of	sex,	taken	 from	the	data	 frame my.df …	and	
    we	saved	this	model	into	an	object	that	we	named xmdl.	To	see	what	the	linear	
    model	did,	we	have	to	“summarize”	this	object	using	the	function	summary():
    '''
    summary(xmdl)
    
    '''
    Residuals:
      1       2       3       4       5       6 
    6.667 -22.333  15.667   2.000 -16.000  14.000 
    
    Coefficients:
    Estimate Std. Error t value Pr(>|t|)    
    (Intercept)   226.33      10.18  22.224 2.43e-05 ***
    sexmale       -98.33      14.40  -6.827  0.00241 **

    Residual standard error: 17.64 on 4 degrees of freedom
    Multiple R-squared:  0.921,	Adjusted R-squared:  0.9012 
    F-statistic: 46.61 on 1 and 4 DF,  p-value: 0.002407
    '''
    
    We have	to work	through	this output. Let’s start with	“Multiple	R-Squared”.
    
    '''Multiple R-squared:  0.921''' # 92% of variability is explained by the fixed effect / predictor 'Sex'
    
    R2 which is a measure of “variance explained” or it	is	a	measure	of “variance	accounted	for”.	
    R2 values range	from 0 to 1.	
    Our	R2 is	0.921, which	is	quite	high … you	can	interpret	this as showing	that	92.1%	of the stuff
    that’s	happening	in	our	dataset	is	“explained” by our	model.	 
    In this	case,	because	we have only one thing in our	model	doing the explaining (the fixed	effect “sex”),
    the	R2 reflects	how	much variance in our data	is accounted for by differences	between	males	and	females.
    
    '''Adjusted R-squared:  0.9012''' # Looks at how much variance is explained + how many fixed effects used
    
    The	“Adjusted	R-squared” value is	a	slightly different	R2 value that not	only looks at	how	much
    variance is	“explained”, but also at how	many fixed effects you used to do the explaining.
    
    Next is Your statistical test of “significance”. If	 you’ve	 already done research, your eyes will	
    probably immediately jump	to the	p-value, which	in	many	fields	is	your	ticket for publishing 
    your work.
    
    Technically	speaking,	the	p-value	is a conditional	probability, it	is a probability under the condition
    that the	null	hypothesis	is true.	In this	case,	the	null hypothesis is “sex	has	no effect	on pitch”.
    
    Usually,	however,	you	have	to	distinguish	between	the	significance	of	the	overall model	(the	
    p-value	at	the	very	bottom	of	the	output), which	considers	all	effects together, from the p-value 
    of individual coefficients (which you find in the coefficients table above the overall significance).
    
    The	p-values to	 the	 right	 of	 this	 table	 correspond	 to	 tests	 whether	 each	 
    coefficient	 is	 “nonzero”. Obviously,	226.33	Hz	is	different	from	zero,	so	the	
    intercept	is	“significant”	with	a	very	low	p-value.	The	slope	-98.33	is	also	
    different from	zero (but	in the negative	direction),	and	so	this	is	significant	as	well.
    
# ------------------ Test your R might --------------------------------------------------------
    
  20] Is there a significant relationship between a pirate’s favorite pixar movie and the 
      number of tattoos (s)he has? Conduct an appropriate ANOVA with fav.pixar as the 
      independent variable, and tattoos as the dependent variable. If there is a significant 
      relationship, conduct a post-hoc test to determine which levels of the independent 
      variable(s) differ.
      
      Hint: Choose the appropriate type of Analysis (aov - I or ANOVA - II/III). First find out
      if the data is balanced or unbalanced using table function
      
      table(pirates$fav.pixar) # Its unbalanced so go for ANOVA
      
      # Step 1: Calculate regression object with lm()
      tattoos.lm <- lm(formula = tattoos ~ fav.pixar,
                    data = pirates)
      
      # Type II ANOVA - Anova(type = 2)
      tattoos.II.aov <- car::Anova(tattoos.lm, type = 2)
      tattoos.II.aov # There is no significant relationship, no need for post-hoc test
      
  21] Is there a significant relationship between a pirate’s favorite pirate and how many 
      tattoos (s)he has? Conduct an appropriate ANOVA with favorite.pirate as the independent 
      variable, and tattoos as the dependent variable. If there is a significant relationship, 
      conduct a post-hoc test to determine which levels of the independent variable(s) differ.
      
      Hint: First check the nature of data - balanced or unbalanced
      table(pirates$favorite.pirate) # Its unbalanced data, so use ANOVA
      
      # Step 1: Calculate regression object with lm()
      tattoos.fav.pirate.lm <- lm(formula = tattoos ~ favorite.pirate,
                       data = pirates)
      # summary(tattoos.fav.pirate.lm)
      
      # Type II ANOVA - Anova(type = 2)
      fav.pirate.aov <- car::Anova(tattoos.fav.pirate.lm, type = 2)
      
      
# ------------------------- Regression ---------------------------------------------------
      
  library(yarrr)
  head(yarrr::diamonds)
  
  # Create a linear model of diamond values
  # DV = value, IVs = weight, clarity, color
  diamonds.df <- yarrr::diamonds
  diamonds.lm <- lm(formula = value ~ weight + clarity + color,
                    data = diamonds.df)
  summary(diamonds.lm)
  
  # Which components are in the regression object?
  names(diamonds.lm)
  
  # The coefficients in the diamond model
  diamonds.lm$coefficients
      
  # Coefficient statistics in the diamond model
  summary(diamonds.lm)$coefficients
    
  # Add the fitted values as a new column in the dataframe
  diamonds.df$value.lm <- diamonds.lm$fitted.values
  
  # Rename value.lm to fitted.values
  names(diamonds.df)[names(diamonds.df) %in% 'value.lm'] <- 'fitted.val'
  
  # Show the result
  head(diamonds.df)
  
  '''
  ------------------------------------------------------
  Original value (value) vs Predicted value (fitted.val)
  ------------------------------------------------------
  According to the model, the first diamond, with a weight of 9.35, a clarity of 0.88, 
  and a color of 4 should have a value of 186.08. As we can see, this is not far off from 
  the true value of 182.5.
  '''
  
  You can use the fitted values from a regression object to plot the relationship between 
  the true values and the model fits. If the model does a good job in fitting the data, 
  the data should fall on a diagonal line:
    
  # Plot the relationship between true diamond values
  # and linear model fitted values
    
  plot(diamonds.df$value, 
       diamonds.df$fitted.val,
       xlab = 'True Diamond Value',
       ylab = 'Fitted/Predicted Value',
       main = 'True vs Fitted Graph')
  abline(b = 1, a = 0)# Values should fall around this line!

#------- Using predict() to predict new data from a model ----------------------------------
  
  Use the predict() function to predict the value of each of these diamonds using the 
  regression model diamond.lm that was created before. 
  
  The two main arguments to predict() are,
  object – the regression object already defined, and 
  newdata – the dataframe of new data. 
  
  Warning! The dataframe that you use in the newdata argument to predict() must have 
  column names equal to the names of the coefficients in the model.
  
  # Create a dataframe of new diamond data
  diamonds.new <- data.frame(weight = c(12, 6, 5),
                             clarity = c(1.3, 1, 1.5),
                             color = c(5, 2, 3))
  
  # Predict the value of the new diamonds using
  # the diamonds.lm regression model
  predict(object = diamonds.lm,     # The regression model
          newdata = diamonds.new)   # dataframe of new data
  
  '''
  This result tells us the the new diamonds are expected to have values of 200.5, 182.3, 
  and 190.5 respectively according to our regression model.
  '''
  
# -------------- Interactions & Center variables before computing interactions! -------------

  When you include interaction terms in a model, you should always center the independent 
  variables first. Centering a variable means simply subtracting the mean of the variable 
  from all observations.
  
  # Create centered versions of weight and clarity
  diamonds.df$weight.cntrd <- diamonds.df$weight - mean(diamonds.df$weight)
  diamonds.df$clarity.cntrd <- diamonds.df$clarity - mean(diamonds.df$clarity)
  
  # Create a regression model with interactions of centered variables
  diamonds.int.lm <- lm(formula = value ~ weight.cntrd * clarity.cntrd,
                        data = diamonds.df)
  summary(diamonds.int.lm)
  
#----------------- Getting an ANOVA from a regression model with aov() ------------------
  # Create ANOVA object from regression
  diamonds.aov <- aov(diamonds.lm)
  
  # Print summary results
  summary(diamonds.aov)
  
# ----------- Comparing regression models with anova() -----------------------------------
  
  A good model not only needs to fit data well, it also needs to be parsimonious. That is, 
  a good model should only be as complex as necessary to describe a dataset. If you are 
  choosing between a very simple model with 1 IV, and a very complex model with, say, 
  10 IVs, the very complex model needs to provide a much better fit to the data in order 
  to justify its increased complexity. If it can’t, then the more simpler model should be 
  preferred.
  
  To compare the fits of two models, you can use the anova() function with the regression 
  objects as two separate arguments. The anova() function will take the model objects as 
  arguments, and return an ANOVA testing whether the more complex model is significantly 
  better at capturing the data than the simpler model.
  
  
  # model 1: 1 IV (only weight)
  diamonds.mod1 <- lm(value ~ weight, data = diamonds.df)
  
  # Model 2: 2 IVs (weight AND clarity)
  diamonds.mod2 <- lm(value ~ weight + clarity, data = diamonds.df)
  
  # Model 3: 3 IVs (weight AND clarity AND color)
  diamonds.mod3 <- lm(value ~ weight + clarity + color, data = diamonds.df)
  
  
  Now let’s use the anova() function to compare these models and see which one provides 
  the best parsimonious fit of the data. First, we’ll compare the two simplest models: 
  model 1 with model 2. Because these models differ in the use of the clarity IV 
  (both models use weight), this ANVOA will test whether or not including the clarity IV 
  leads to a significant improvement over using just the weight IV:
  
  # Compare model 1 to model 2
  anova(diamonds.mod1, diamonds.mod2)
  ## Analysis of Variance Table
  ## 
  ## Model 1: value ~ weight
  ## Model 2: value ~ weight + clarity
  ##   Res.Df  RSS Df Sum of Sq   F Pr(>F)    
  ## 1    148 5569                            
  ## 2    147 3221  1      2347 107 <2e-16 ***
  
  As you can see, the result shows a Df of 1 (indicating that the more complex model has 
  one additional parameter), and a very small p-value (< .001). This means that adding the 
  clarity IV to the model did lead to a significantly improved fit over the model 1.
  
  Next, let’s use anova() to compare model 2 and model 3. This will tell us whether adding
  color (on top of weight and clarity) further improves the model:
    
  # Compare model 2 to model 3
  anova(diamonds.mod2, diamonds.mod3)
  ## Analysis of Variance Table
  ## 
  ## Model 1: value ~ weight + clarity
  ## Model 2: value ~ weight + clarity + color
  ##   Res.Df  RSS Df Sum of Sq    F Pr(>F)
  ## 1    147 3221                         
  ## 2    146 3187  1        34 1.56   0.21
  
  The result shows a non-significant result (p = 0.21). Thus, we should reject model 3 and 
  stick with model 2 with only 2 IVs.
  
  #----------------- Logistic regression with glm(family = "binomial") --------------------
  
  Let’s run a logistic regression on the diamonds dataset. First, I’ll create a binary 
  variable called value.g190 indicating whether the value of a diamond is greater than 190
  or not. Then, I’ll conduct a logistic regression with our new binary variable as the 
  dependent variable. We’ll set family = "binomial" to tell glm() that the dependent 
  variable is binary.
  
  # Create a binary variable indicating whether or not
  #   a diamond's value is greater than 190
  diamonds.df$value.g190 <- diamonds.df$value > 190
  
  # Conduct a logistic regression on the new binary variable
  diamond.df.glm <- glm(formula = value.g190 ~ weight + clarity + color,
                     data = diamonds.df,
                     family = binomial)
  
  names(diamond.df.glm)
  
  # Print coefficients from logistic regression
  summary(diamond.df.glm)$coefficients
  
  Just like with regular regression with lm(), we can get the fitted values from the model
  and put them back into our dataset to see how well the model fit the data:
    
    # Add logistic fitted values back to dataframe as
    #  new column pred.g190
    diamonds.df$pred.g190 <- diamond.df.glm$fitted.values
  
  # Look at the first few rows (of the named columns)
  head(diamonds.df[c("weight", "clarity", "color", "value", "pred.g190")])
  ##   weight clarity color value pred.g190
  ## 1   9.35    0.88     4 182.5   0.16252
  ## 2  11.10    1.05     5 191.2   0.82130
  ## 3   8.65    0.85     6 175.7   0.03008
  ## 4  10.43    1.15     5 195.2   0.84559
  
  Just like we did with regular regression, you can use the predict() function along with 
  the results of a glm() object to predict new data. Let’s use the diamond.glm object to 
  predict the probability that the new diamonds will have a value greater than 190:
    
    # Predict the 'probability' that the 3 new diamonds 
    # will have a value greater than 190
    
    predict(object = diamond.df.glm,
            newdata = diamonds.new,
            type = 'response')
  
  So, the model predicts that the probability that the three new diamonds will be valued 
  over 190 is 99.23%, 2.86%, and 40.38% respectively.
  
  
#--------------------------------------- Rough Work ---------------------------------------------

  rep(1:3, 2)
  
  x <- rep(0:9, 3)
  group <- as.factor(rep(1:3, rep(5,3)))
  lv2   <- as.numeric(group==2)
  lv3   <- as.numeric(group==3)
  
  y <- 50 + 100 * lv2 + 200 * lv3 + 2.5 * x + 
       0 * (x * lv2) + 0 * (x * lv3) + rnorm(600, 0, 15)
  
  diamonds.df[5] <- NULL
  
      # Create new binary variable indicating whether
      #   a ship sold for more than 3500
      auction$price.gt.3500 <- auction$price > 3500
      
      # price.all.blr model
      # DV = price.gt.3500, IV = everything (except jbb)
      price.all.blr <- glm(price.gt.3500 ~ cannons + rooms + age + condition + color + style,
                           data = auction,
                           family = binomial   # Logistic regression
      )
      
      summary(price.all.blr)
      

      # Create a dataframe with new ship data
      new.ships <- data.frame(cannons = c(12, 8, 32),
                              rooms = c(34, 26, 65),
                              age = c(43, 54, 100),
                              condition = c(7, 3, 5),
                              color = c("black", "black", "red"),
                              style = c("classic", "modern", "modern"),
                              stringsAsFactors = FALSE)

      # Calculate logit of predictions
      log.pred <- predict(object = price.all.blr,
                          newdata = new.ships,
                          type = 'response'
      )
      
      
                  
head(auction)
      
      
    dfbeta(xmdl)
    
    my.temp.df <- my.df
    names(my.temp.df)[names(my.temp.df) %in% 'sex'] <- 'sx'
    
    
    names(poopdeck)
    head(poopdeck)
    unique(poopdeck$day)
    
    class(poopdeck$cleaner)
    
    ?require

file.path <- c(getwd(), 'plot.pdf')
paste(file.path, collapse="/")


url <- 'https://raw.githubusercontent.com/ndphillips/ThePiratesGuideToR/master/data/caffeinestudy.txt'
caffeine.df <- read.table(file = url, sep = '\t')

piratepal("google")
?dnorm

?demo("colors")
colors()

v.dnm <- dnorm(50, mean = 20, sd = 2)

dnorm(c(-10:10), mean = 20, sd = 2)
        
aggregate(formula = weight ~ Diet + Time,
                  FUN = mean, # Calculate the mean of each group
                  subset = Time %in% c(0,2,4), # Only when Chicks are 0, 2, and 4 weeks old
                  data = ChickWeight)
        
        
        
        by(loan.nona[, c('Gender', 'Married', 'Dependents', 'Education', 'Self_Employed')],
      loan.nona$Loan_Status,
      summary)


library(help = 'datasets') # Dataframes pre-loaded in R

x <- 1:5
y <- 6:10
z <- 11:15

cbind(x,y,z)
rbind(x,y,z)
matrix(c(x,y,z), nrow = 5, byrow = T)

A factor is a nominal variable that has a well-specified possible set of values that it can take on. 
For example, one can create a factor sex that can only take on the values "male" and "female"

Matrices and dataframes look very similar, they aren’t the same. 
While a matrix can contain either character or numeric columns, 
a dataframe can contain both numeric and character columns. 

matx <- matrix(c(1,2,3,'J'), nrow = 2, byrow = T) # All elements are co-erced into character type
class(matx[1,1]) # "character"
dat.frame <- data.frame(age = c(10,20,40,28),
                        sex = c('M', 'M', 'F', 'M')) # Dataframes can hold elements of diverse types, here number and factor types
str(dat.frame)
class(dat.frame[2,2])
class(dat.frame[1,1])

names(dat.frame) # does not print row names
rownames(dat.frame) <- c(1:nrow(dat.frame))
fix(dat.frame)

# Create a dataframe of boat sale data called bsale
bsale <- data.frame(name = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"),
                    color = c("black", "green", "pink", "blue", "blue", 
                              "green", "green", "yellow", "black", "black"),
                    age = c(143, 53, 356, 23, 647, 24, 532, 43, 66, 86),
                    price = c(53, 87, 54, 66, 264, 32, 532, 58, 99, 132),
                    cost = c(52, 80, 20, 100, 189, 12, 520, 68, 80, 100),
                    stringsAsFactors = FALSE)   # Don't convert strings to factors!


# What was the mean price of green boats?
mean(bsale$price[bsale$color == 'green']) # or as below
with(bsale, mean(price[color == 'green']))

# What were the names of boats older than 100 years?
with(bsale, name[age > 100])

# What percent of black boats had a positive profit?
mean(with(bsale, color == 'black' & price > cost)) # or
with(subset(bsale, color == 'black'), mean(price > 0))

# Save only the price and cost columns in a new dataframe
bsale.sub <- bsale[, c('price', 'cost')] # or use dplyr as below to subset
library(dplyr)
bsale.sub <- select(bsale, price, cost) # Selection by column name, or as below
bsale.2 <- bsale[c("price", "cost")]

# Change the names of the columns in bsale.sub to "p" and "c"
names(bsale.sub) <- c('p', 'c')

# Rename 'price' and 'cost' columns to 'p' and 'c'
names(wg.sale.sub) # "price" "cost"
names(wg.sale.sub) <- c('p', 'c') 
names(wg.sale.sub) # "p" "c"

# Create a dataframe called old.black.bsale containing only data from black boats older than 50 years
old.black.bsale <- bsale[bsale$color == 'black' & bsale$age > 50,]
old.black.bsale1 <- bsale[with(bsale, color == 'black' & age > 50),]
old.black.bsale2 <- subset(bsale, color == 'black' & age > 50)

* Subsetting a Data-Frame *
  
  # * Create a dataframe of wagon sales *
  wg.sale <- data.frame(name = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"),
                        age = c(143, 53, 356, 23, 647, 24, 532, 43, 66, 86),
                        price = c(53, 87, 54, 66, 264, 32, 532, 58, 99, 132),
                        cost = c(52, 80, 20, 100, 189, 12, 520, 68, 80, 100),
                        stringsAsFactors = FALSE)


# Adding a new column 'tax' to wg.sale dataframe
wg.sale$tax <- c(14, 5, 35, 3, 67, 4, 32, 3, 6, 6) # Or use dplyr's mutate() function as below
library(dplyr)
wg.sale <- wg.sale %>% mutate('tax' = c(14, 5, 35, 3, 67, 4, 32, 3, 6, 6))

wg.temp <- wg.sale
names(wg.temp) %in% 'tax'

wg.temp[names(wg.temp)=='tax'] <- 'tax cd'
wg.temp$tax <- c(14, 5, 35, 3, 67, 4, 32, 3, 6, 6)
names(wg.temp)[names(wg.temp) == 'tax'] <- 'tax cd'

# Order wg.sale dataframe by price
# By default it is ordered in ascending order
wg.sale[order(wg.sale$price),]

# Order it by descending order
wg.sale[order(wg.sale$price, decreasing = T),]




mean(TRUE,  TRUE,  TRUE, FALSE,  TRUE,  TRUE,  TRUE, FALSE,  TRUE,  TRUE)
class(with(bsale, price > cost))
       wh <- which(movie == 'Spice World')
       which(c(TRUE, FALSE, TRUE))
       
   Imputing Invalid/Missing Values: 
  
   Assume the vector happy representing the survey values. The valid values are from 1 through 5.
   However it contains invalid values. Let us impute them with NA.
   
   happy <- c(1, 4, 2, 999, 2, 3, -2, 3, 2, 999) # Valid values are from 1 through 5
   happy[!(happy %in% 1:5)] <- NA # Replace values outside the range (1:5) with NA
   happy[(happy %in% 1:5) == FALSE] <- NA # Another way
   
   happy[(happy %in% 1:5)]
   (10 %in% happy)
   
   colors <- c("black", "green", "pink", "blue", "blue")
   colors[5]
   colors[1:5]
   index <- 3:5
   colors[index]
   
   v.num <- c(100, 200, 300, 400, 500)
   v.num[c(TRUE, FALSE, TRUE, FALSE, TRUE)] # 100 300 500
   
   boat.ages <- c(143, 53, 356, 23, 647, 24, 532, 43, 66, 86)
   mean(boat.ages < 100)
   
   small.sample.size <- rnorm(n = 5, mean = 10, sd = 5) 
   mean(small.sample.size) # 11.91517
   sd(small.sample.size) # 2.350778
   
   For a small sample the mean and sd does not match the population mean(10) and sd(5)
   
   large.sample.size <- rnorm(n = 100000, mean = 10, sd = 5) 
   mean(large.sample.size) # 9.996753
   sd(large.sample.size) # 5.000035
   
   For a large sample size the mean and sd exactky matches the population mean(10) and sd(5). 
   This phenomenon is called Law of Large Numbers
   
sum(c(1,2,'M'))
gender <- c("M", "M", "F", "F", "F", "M", "F", "M", "F")
res <- table(gender)
res/sum(res)
names(res)
class(res[2])
   
hist(rnorm(n = 1200, mean = 10, sd = 5))
   
   v1 <- c(1,2)
   v2 <- c(1,2,3,4)   
   v3 <- v1 + v2
require(devtools)
?require

# Below t-test will fail as the grouping level (sword.type) is more than 2 levels, lets
# go with ANOVA

t.test(tattoos ~ sword.type,
       data = pirates,
       alternative = 'two.sided')

levels(pirates$sword.type)
class(pirates$sword.type)
class(pirates$headband)




i <- 2
text(x=i, y=bp$stats[,i] -
       0.02*(max(pirates$height, na.rm=TRUE) -
               min(pirates$height, na.rm=TRUE)),
     labels=bp$stats[,i],
     cex = .75)

bp$stats[,2]

text(3, 150, labels = '*')

bp
seq(ncol(bp$stats))

?text
max(pirates$height, na.rm=TRUE) - min(pirates$height, na.rm=TRUE)
0.02*209

scatter.smooth(
  
  pirates$height, # x-coordinates
  pirates$weight, # y-coordinates
  main = 'My first scatterplot of pirate data!',
  xlab = 'Height (in cm)',   # x-axis label
  ylab = 'Weight (in kg)',   # y-axis label
  pch = 16,                  # Filled circles
  col = gray(.0, .1))        # Transparent gray
  













