#----------- dplyr package -------------------------------------------------------------------

# ------------------------------------Using pipe operator %>% -----------------------------
# Syntax: dataset Name %>% function

library(dplyr)
titanic_df <- read.csv(file.choose(), header = T, stringsAsFactors = T)
head(titanic_df)
names(titanic_df)

class(titanic_df$Age)
titanic_no_age_na_df <- filter(titanic_df, !is.na(Age))

# Sort by Age, using dplyr's arrange()
titanic_sort_Age_df <- arrange(titanic_no_age_na_df, Age)
head(titanic_sort_Age_df)

titanic_sort_Age_df <- arrange(titanic_no_age_na_df, desc(Age))
head(titanic_sort_Age_df)

# Sort using pipe operator %>%
titanic_no_age_na_df %>% arrange(Age) %>% head() # Ascending order
t <- titanic_sort_Age_df %>% arrange(desc(Age)) %>% head() # Descending order
t

# Using %>% and select()
titanic_sub_df <- titanicDf
titanic_sub_df %>% select(Age, Sex)

# Now combining multiple operations using pipe %>%
titanic_sub_df <- titanicDf
titanic_sub_df %>% select(Age, Sex, Fare) %>% arrange(Age, desc(Fare)) %>% head()

# filter() function, dplyr package
head(mtcars)

# filter cars with only 6 cylinder
cyl_6_df <- filter(mtcars, cyl == 6) # or use pipe as below
mtcars %>% filter(cyl == 6) %>% head() # filter using pipe

# filter rows with mpg > 18 and sort by mpg in ascending order
mpg_18_plus <- arrange(filter(mtcars, mpg > 18), mpg) # sort by mpg, arrange() and filter() combined
head(mpg_18_plus)
mtcars %>% filter(mpg > 18) %>% arrange(mpg) %>% head() # same results using pipe

# filter only cyl is 4 or 6 and sort them by mpg and cyl ascending
cyl_4_or_6 <- arrange(filter(mtcars, cyl == 6 | cyl == 4), mpg, cyl)
head(cyl_4_or_6)
mtcars %>% filter(cyl == 4 | cyl == 6) %>% arrange(mpg, cyl) %>% head() # Same result using pipe operator
cyl_4_or_6 <- arrange(filter(mtcars, cyl %in% c(4,6)), mpg, cyl) # Using %in% operator
head(cyl_4_or_6)

cyl_not_4_6 <- filter(mtcars, !cyl %in% c(4,6)) # filter all cars with cyl other than 4 and 6
mtcars %>% filter(!cyl %in% c(4,6)) %>% head(10) # Same result using pipe operator

cyl_6_1 <- filter(mtcars, cyl == 6 & am == 1) # filter all cars with cyl == 6 and am == 1
cyl_6_1


# -------------------------- summarise() function ---------------------------------------------

summarise(mtcars, mean(mpg)) # Average of mpg
summarise(mtcars, mean_mpg = mean(mpg)) # Alias for mean(mpg)
# Find mean, median
summarise(mtcars, mean_mpg = mean(mpg), mean_disp = mean(disp), sd_mpg = sd(mpg), median_mpg = median(mpg))
# summarise_at() function, used to aggregate variables based on select helper functions (starts with, ends with, contains ...)
# Summarise all variables containing letter 'p'
summarise_at(mtcars, vars(contains('p')), funs(mean, median))

# summarise_if(), used to filter rows to summarise based on conditions
summarise_if(mtcars, is.numeric, mean)

# summarise_all(), used to summarise all variables on the functions specified
summarise_all(mtcars, funs(mean, median))

#---------------------------------- select() function -----------------------------------

names(mtcars)
# select(dataset name, col1[,col2, col3, coln])
# You can also change the order of the columns - they appear in the order thay are selected
mtcars_sub <- select(mtcars, mpg, cyl, hp, wt, am) # Selection by column name
mtcars_sub <- select(mtcars, 1,2,3,6,8,10) # Selection by column index/position
head(mtcars_sub)

# Drop columns, use -ve sign
mtcars_drop <- select(mtcars, -qsec, -vs) # or as below
mtcars_drop <- select(mtcars, -c(qsec, vs))
mtcars_drop <- select(mtcars, -1,-2,-3,-5,-10) 
mtcars_drop <- select(mtcars, -c(1,2,5,8))

# Select range of columns - by name and by index/position
mtcars_sub <- select(mtcars, mpg:hp)
mtcars_sub <- select(mtcars, 1:3, 10:11)
head(mtcars_sub,2)

# Drop columns with -ve sign, by name and by index/position
mtcars_sub <- select(mtcars, -(4:9), 1:3)
mtcars_sub <- select(mtcars, -(wt:vs), 1:3)

#----------------- rename() function ------------------------------------------
mtcars_sub <- rename(mtcars, 'weight' = wt)
head(mtcars_sub)

#----------------- filter() function ------------------------------------------
# Slice the dataset by rows; syntax: filter(dataset, condition)
head(mtcars)
mtcars_sub <- filter(mtcars, (mpg > 21 & cyl > 4) | (mpg > 18 & cyl >= 4))
mtcars_sub <- mtcars[(mtcars$mpg > 21 & mtcars$cyl >= 4),] # Use dataset_name$column_name notation

#------------------------ slice() function -----------------------------------
# Can be used to extract specific ranges of rows using slice(); syntax: slice(dataset_name, row_no1[:row_no2])

mtcars_row_20 <- slice(mtcars, 20)
mtcars_1_to_20 <- slice(mtcars, 1:20)

#----------------------- tibble::rownames_to_column function ----------------------------------

# Check if the dataset contains rownames
rownames(mtcars)
head(mtcars) # the first column is the rowname column; it is currently un-named

#mtcars <- add_rownames(mtcars, 'car_names') # Depricated, use the below function
tibble::rownames_to_column(mtcars, var='car_names')
# Using pipe operator
mtcars %>% tibble::rownames_to_column(var='car_names') %>% head(2)

#------------------------ mutate() and transmute() functions --------------------------------

# Use mutate() function when you want to create a new column which is a function of existing column(s)
mtcars_expnd <- mutate(mtcars, 'cost_per_mile' = 35/mpg) # Assuming it costs 35/gallon
head(mtcars_expnd)

# Use of if_else() clause within mutate() function
mtcars_expnd <- mutate(mtcars_expnd, car_review = if_else(cost_per_mile < 1.65, 'GOOD', 'AVERAGE'))
head(mtcars_expnd)

# transmute() is used to drop all other columns except the newly created column
mtcars_new_col_only <- transmute(mtcars, 'cost/mile' = 35/mpg)

#------------------ if_else() vs ifelse() --------------------------------------------

# if_else() is from dplyr package while ifelse() is from base package
# 1. if_else() is fine-tuned and is highly performant
# 2. if_else() can handle missing values
# 3. if_else() retains the data type

# Replicate rows to beef-up the dataset to run a test on ifelse() and if_else() functions
mtcars_repl_rows <- as.data.frame(sapply(mtcars_expnd, rep.int, times = 100000)) # Rows [1] 3200000      13
dim(mtcars_repl_rows)
head(mtcars_repl_rows)

# Got unexpected results ... ifelse() performed better than if_else() !!!!!!!
system.time(ifelse(mtcars_repl_rows$cyl >= 6 & mtcars_repl_rows$mpg >= 21 & mtcars_repl_rows$cost_per_mile < 1.65, 'H_EF', 'L_EF'))
# Performance of ifelse()
'''
   user  system elapsed 
   0.04    0.02    0.06 
'''
system.time(if_else(mtcars_repl_rows$cyl >= 6 & mtcars_repl_rows$mpg >= 21 & mtcars_repl_rows$cost_per_mile < 1.65, 'H_EF', 'L_EF', 'MISSING'))
# Performance of if_else()
'''
   user  system elapsed 
   0.12    0.03    0.15
'''

# if_else() retains the dataset
coin_toss <- factor(sample(c('H', 'T'), 5, replace = T))
class(coin_toss) # "factor"
class(ifelse(coin_toss %in% c('H'), coin_toss, factor(NA))) # "integer", not a factor which is the origunal data type
class(if_else(coin_toss %in% c('H'), coin_toss, factor(NA))) # "factor", retains the original data type

#----------------------- distinct() and arrange() functions ------------------------------------------------

# syntax: distinct(dataset_name, column[, column2, column_n])
# Running distinct on one column
mtcars_uni_cyl <- distinct(mtcars, cyl)
# Running distinct on multiple columns, combination of all the column values determines the uniqueness
mtcars_uni_combi <- distinct(mtcars, cyl, gear)

# arrange() function is used to sort the data 
# syntax: arrange(dataset_name, column[, column2, column_n])

mtcars_sort_cyl <- arrange(mtcars, cyl)
mtcars_sort_multui_col <- arrange(mtcars, cyl, desc(mpg)) # First sort on cyl and then on mpg in descending order

#-------------- summarise() and group_by() functions -----------------------------------------

# summarise() function is used to generate the summary statistics
summarise(mtcars, 
          count = n(), 
          mean(mpg, na.rm = T), 
          sd_mpg = sd(mpg, na.rm = T), 
          median_mpg = median(mpg, na.rm = T), 
          q3rd = quantile(mpg, .75))

str(mtcars)
# Lets mutate to create cost_per_mile feature, based on which we can have another feature recommendations
mtcars_expnd <- mutate(mtcars, cost_per_mile = 35/mpg)
head(mtcars_expnd)
# mtcars_expnd1 <- mtcars %>% mutate(cost_per_mile = 35/mpg) -- Another way to do it using pipe operator
mtcars_expnd <- mutate(mtcars_expnd, recommendation = if_else(cost_per_mile < 1.65, 'GOOD', 'AVERAGE', 'MISSING'))

'''
factor coersion on the feature is not required for grouping, group_by can work on character data type as well
class(mtcars_expnd$recommendation) # "character"
# Convert recommendation into a factor
mtcars_expnd$recommendation <- as.factor(mtcars_expnd$recommendation) # "factor"
'''

mtcars_expnd_grp_recomm <- group_by(mtcars_expnd, recommendation) # recommendation is of type "character"
summarise(mtcars_expnd_grp_recomm, 
          count = n(), 
          mean(mpg, na.rm = T), 
          sd_mpg = sd(mpg, na.rm = T), 
          median_mpg = median(mpg, na.rm = T), 
          q3rd = quantile(mpg, .75))

'''
o/p
  recommendation count `mean(mpg, na.rm = T)` sd_mpg median_mpg  q3rd
  <chr>          <int>                  <dbl>  <dbl>      <dbl> <dbl>
1 AVERAGE           20                   16.4   3.03       16.1  18.8
2 GOOD              12                   26.2   4.56       25.2  30.4
'''

data("AirPassengers")
head(AirPassengers)
View(AirPassengers)
dim(AirPassengers)

# Create a data.frame, AirPassengers dataset contains a single column which needs to be converted
# into rows and columns
air_passengers <- data.frame(
                              matrix(AirPassengers, 
                                     ncol = 12, 
                                     byrow = T, 
                                     dimnames = list(as.character(1:12), month.abb)
                                    )
                            )
head(air_passengers)
dim(air_passengers)

# Add an additional year column into air_passengers dataset
air_passengers$year <- as.character(1949:1960)
# year column got added as the last column, make it first
library(dplyr)
air_passengers <- select(air_passengers, year, everything()) # Another way is to use the pipe operator as below
air_passengers <- air_passengers %>% select(year, everything())

# Now use summarise_all() function to get max for all variables (max values for each month/column)
summarise_all(air_passengers, max)
'''
o/p
  year Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
  1960 417 391 419 461 472 535 622 606 508 461 390 432
'''
# year above is a character type, lets omit the feature and run a summary only for numeric variables
# For this use summarise_if() function, the below code provides max flights for each month 
summarise_if(air_passengers, is.numeric, funs(max)) # Another way using pipe operator
air_passengers %>% summarise_if(is.numeric, max)

# Maximum flights for each year, row summary
air_passengers_months <- air_passengers[,sapply(air_passengers, is.numeric)] # create a dataframe with only numeric variables
cbind(air_passengers, max_passengers_for_the_year = (apply(air_passengers_months, 1, max)))

# Another way with mutate
mutate(air_passengers, max_flights_year = apply(air_passengers[,sapply(air_passengers, is.numeric)], 1, max))


# --------------- https://stackoverflow.com/questions/12864867/row-based-summary-calculations ---------
# Row based summary calculations

# Advanced - creating a dataframe
set.seed(007)
X <- data.frame(matrix(sample(c(10:20, NA), 100, replace=TRUE), ncol=10))
sex <- sample(c('F', 'M'), 10, T)
reg <- sample(c('N', 'S', 'E', 'W'), 10, T)
DF <- cbind(sex, reg, X)
DF # this is your data.frame

# About t(), apply() and sapply() functions
# t() is for transpose
# syntax: apply(dataset_name, MARGIN, FUNCTION) MARGIN can be 1 (to apply the function on rows),
# 2 to apply function on columns and c(1,2) to apply the function on both rows and cols
# syntax: sapply(), apply Function to each element and return a matrix, vector or array

Stats <- function(x){
  Mean <- mean(x, na.rm=TRUE)
  SD <- sd(x, na.rm=TRUE)
  Min <- min(x, na.rm=TRUE)
  Max <- max(x, na.rm=TRUE)
  return(c(Mean=Mean, SD=SD, Min=Min, Max=Max))
}

# Drop non-numeric columns
newDF <- DF[,sapply(DF, is.numeric)]
DF <- cbind(DF, t(apply(newDF, 1, Stats)))
DF

#--------------------------- End - Row based summary calculations ----------------------------------

# Handling na
# https://stackoverflow.com/questions/4862178/remove-rows-with-nas-missing-values-in-data-frame

Auto <- read.csv(file.choose(), header = T, stringsAsFactors = T)
Auto.no.na.df <- Auto[complete.cases(Auto),] # omits na across the df
Auto.no.na <- na.omit(Auto) # omits na across the df
Auto.no.na <- Auto[complete.cases(Auto[,5:9]),] # Checks for na in specific columns, here 5 thru 9
Auto.na.rows <- Auto[!complete.cases(Auto),] # 0 rows, as no rows contain 'na'

# Using dplyr
Auto.no.na <- filter(Auto, !is.na(mpg))
Auto %>% filter(is.na(mpg))

plot(Auto$cylinders , Auto$mpg) 


