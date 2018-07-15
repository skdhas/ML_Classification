# Install 'EDAWR' package which contains all the datasets to be used in this practice
# The packsge contains these datasets - Storms, Cases, Pollution and Tb
install.packages('devtools')
devtools::install_github('rstudio/EDAWR')
library(EDAWR)
'''
A subset of data from the World Health Organization Global Tuberculosis Report.
A dataset with the estimated number of TB cases in France, Germany, and the United States for 
2011, 2012, and 2013.
http://www.who.int/tb/country/data/download/en/
'''
?cases
?storms
'''
Pollution data from the WHO, 2014. This dataset contains a subset of data from the Ambient Air Pollution 
Database, WHO, May 2014

A data frame with variables:
  city : Name of city: New York, London, or Beijing
  Size : Size of air particulate measured. Fine suspended particles smaller than 10 microns in diameter (large) and 2.5 microns in diameter (small).
  amount : The mean annual concentration of particles in milligrams per meter cubed (ug/m3)

Source
http://www.who.int/phe/health_topics/outdoorair/databases/cities/en/
'''
?pollution
'''
A subset of data from the World Health Organization Global Tuberculosis Report.

A dataset with the variables
  country
  year
  sex
  child : Number of new cases reported among people 0 - 14 years of age.
  adult : Number of new cases reported among people 15 - 64 years of age.
  elderly : Number of new cases reported among people over 64 years of age.

Source
http://www.who.int/tb/country/data/download/en/
'''
?tb

#============================================= tidyr ============================================
'''
What is tidy data?
  1. Each variable is saved in its own column
  2. Each observation is saved in its own row
  3. Each type of observation is stored in a single table

A package that reshapes the layout of tables.
Two main functions:
  1. gather()
  2. spread()
'''
install.packages('tidyr')
library(tidyr)
?gather
?spread

# Print cases data-frame
cases
'''
  country  2011  2012  2013
1      FR  7000  6900  7000
2      DE  5800  6000  6200
3      US 15000 14000 13000
'''
# Reshape the above into a tidy data-frame using gather() with three variables 'country', 'year' and 'n'
# gather(data-frame-to-reshape, 'new key column', 'value of new column', column-scope-to-collapse)
gather(cases, 'year', 'n', 2:4)
'''
  country year     n
1      FR 2011  7000
2      DE 2011  5800
3      US 2011 15000
4      FR 2012  6900
5      DE 2012  6000
6      US 2012 14000
7      FR 2013  7000
8      DE 2013  6200
9      US 2013 13000
'''
# Now how the pollution dataset will look tidy with 3 variables 'city', 'large', 'small'
pollution
'''
      city  size amount
1 New York large     23
2 New York small     14
3   London large     22
4   London small     16
5  Beijing large    121
6  Beijing small     56
'''
# Use spread() method to tidy pollution data-set

'''
spread()
Generates multiple columns from 2 columns
  1. Each unique value in key column becomes a column name
  2. Each value in the value column becomes a cell in the new columns

spread(data-frame-to-reshape, 'column to use for key', 'column to use for value')
'''
spread(pollution, 'size', 'amount')
'''
      city large small
1  Beijing   121    56
2   London    22    16
3 New York    23    14
'''

# separate() function - takes a single column and separates it into multiple columns
storms

'''
  storm    wind pressure date      
  <chr>   <int>    <int> <date>    
1 Alberto   110     1007 2000-08-03
2 Alex       45     1009 1998-07-27
3 Allison    65     1005 1995-06-03
4 Ana        40     1013 1997-06-30
5 Arlene     50     1010 1999-06-11
6 Arthur     45     1010 1996-06-17
'''
separate(storms, date, c('year', 'month', 'day'), sep = '-')

'''
  storm    wind pressure year  month day  
  <chr>   <int>    <int> <chr> <chr> <chr>
1 Alberto   110     1007 2000  08    03   
2 Alex       45     1009 1998  07    27   
3 Allison    65     1005 1995  06    03   
4 Ana        40     1013 1997  06    30   
5 Arlene     50     1010 1999  06    11   
6 Arthur     45     1010 1996  06    17
'''

# unite() function - unites columns into a single column

storms.separated <- separate(storms, date, c('year', 'month', 'day'), sep = '-')
stroms.united <- unite(storms.separated, 'DATE', year, month, day, sep = '-')

