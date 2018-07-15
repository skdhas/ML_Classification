#==================================== ggplot2 ===============================================
install.packages('ggplot2')

'''
http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html

Why ggplot2?
Advantages of ggplot2

1. consistent underlying grammar of graphics (Wilkinson, 2005)
2. plot specification at a high level of abstraction
3. very flexible
4. theme system for polishing plot appearance
5. mature and complete graphics system
6. many users, active mailing list

That said, there are some things you cannot (or should not) do With ggplot2:

  1. 3-dimensional graphics (see the rgl package)
  2. Graph-theory type graphs (nodes/edges layout; see the igraph package)
  3. Interactive graphics (see the ggvis package)

What Is The Grammar Of Graphics?

  The basic idea: independently specify plot building blocks and combine them to create just about 
  any kind of graphical display you want. Building blocks of a graph include:

  1. data
  2. aesthetic mapping
  3. geometric object
  4. statistical transformations
  5. scales
  6. coordinate system
  7. position adjustments
  8. faceting

ggplot2 VS Base Graphics
------------------------

  Compared to base graphics, ggplot2 (For simple graphics use base graphics)
  
  1. is more verbose (using or expressed in more words than are needed) for simple / canned graphics
  2. is less verbose (using or expressed in more words than are needed) for complex / custom graphics
  3. does not have methods (data should always be in a data.frame)
  4. uses a different system for adding plot elements

'''

# ================================ Geometric Objects And Aesthetics ==================================

'''
-------------------------------------------
Aesthetic Mapping (set with aes() function)
-------------------------------------------

In ggplot land aesthetic means “something you can see”. Examples include:

  position (i.e., on the x and y axes)
  color (“outside” color)
  fill (“inside” color)
  shape (of points)
  linetype
  size

Each type of geom accepts only a subset of all aesthetics–refer to the geom help pages to see what 
mappings each geom accepts. Aesthetic mappings are set with the aes() function.

-----------------------
Geometic Objects (geom)
-----------------------

Geometric objects are the actual marks we put on a plot. Examples include:

  points (geom_point, for scatter plots, dot plots, etc)
  lines (geom_line, for time series, trend lines, etc)
  boxplot (geom_boxplot, for, well, boxplots!)

A plot must have at least one geom; there is no upper limit. You can add a geom to a plot using the 
+ operator

You can get a list of available geometric objects using the code below:

  help.search("geom_", package = "ggplot2")

or simply type geom_<tab> in any good R IDE (such as Rstudio or ESS) to see a list of functions 
starting with geom_.

'''
# Read a sample dataset
housing <- read.csv('D:/Machine Learning/Data Hacks/Rgraphics/dataSets/landdata-states.csv')
head(housing[1:5])
head(housing)
fix(housing)

library(dplyr)
hp2001Q1 <- subset(housing, Date == 2001.25)
dim(hp2001Q1)
'''
Other ways to filter
hp2001Q1 <- filter(housing, Date == 2001.25)
hp2001Q1 <- housing %>% filter(Date == 2001.25)
hp2001Q1 <- housing[housing$Date == 2001.25]
'''
# ===================================== Points (Scatterplot) =======================================

library(ggplot2)
ggplot(
        hp2001Q1, 
        aes(y = Structure.Cost, x = Land.Value)) +
        geom_point()

# Base graphics
plot(log(hp2001Q1$Land.Value), hp2001Q1$Structure.Cost)
scatter.smooth(log(hp2001Q1$Land.Value), hp2001Q1$Structure.Cost)

#==================================== Lines (Prediction Line) ======================================
'''
A plot constructed with ggplot can have more than one geom. In that case the mappings 
established in the ggplot() call are plot defaults that can be added to or overridden. 
Our plot could use a regression line:
'''

plt1 <- ggplot(
              hp2001Q1,
              aes(y = Structure.Cost, x = log(Land.Value))) 

# Now build a model, predict and draw the regression line
hp2001Q1$pred.SC <- predict(lm(Structure.Cost~log(Land.Value), data = hp2001Q1))
plt1 + geom_point(aes(color = Home.Value)) +
geom_line(aes(y = pred.SC))

# ===================================== Smoothers =================================================
'''
Not all geometric objects are simple shapes – the smooth geom includes a line and a ribbon.
'''
plt1 + geom_point(aes(color = Home.Value)) + geom_smooth()

# =================================== Text (Label Points) ========================================
'''
Each geom accepts a particualar set of mappings – for example geom_text() accepts a labels mapping.
ggrepel - Provides text and label geoms for 'ggplot2' that help to avoid overlapping text labels. 
Labels repel away from each other and away from the data points.
'''
plt1 + geom_text(aes(label = State), size = 2)

install.packages('ggrepel')
library(ggrepel)
plt1 + 
  geom_point() + 
    geom_text_repel(aes(label = State, color = Home.Value), size = 3)

#=================================== Aesthetic Mapping VS Assignment =============================
'''
Note that 'variables are mapped to aesthetics with the aes() function, while fixed aesthetics are 
set outside the aes() call'. This sometimes leads to confusion, as in this example:
'''

plt1 +
  geom_point(aes(size = 2),# incorrect! 2 is not a variable (constant to be set outside aes())
             color="red") # this is fine -- all points red

# Mapping Variables To Other Aesthetics
# Other aesthetics are mapped in the same way as x and y in the previous example.

plt1 + 
  geom_point(aes(color = Home.Value, shape = region), size = 3) +
  geom_text_repel(aes(label = State, color = Home.Value), size = 3)

#======================================== Exercise I =============================================
'''
HDI : The Human Development Index is a composite statistic of life expectancy, education, 
and per capita income indicators, which are used to rank countries into four tiers of 
human development.

Definition: The Human Development Index (HDI) is a statistical tool used to measure a countrys 
overall achievement in its social and economic dimensions. The social and economic dimensions of 
a country are based on the health of people, their level of education attainment and 
their standard of living.

Original sources for these data are 
http://www.transparency.org/content/download/64476/1031428 
http://hdrstats.undp.org/en/indicators/display_cf_xls_indicator.cfm?indicator_id=103106&lang=en
These data consist of Human Development Index and Corruption Perception Index scores for 
several countries.

--------
EXERCISE
--------

1. Create a scatter plot with CPI on the x axis and HDI on the y axis.
2. Color the points blue.
3. Map the color of the the points to Region.
4. Make the points bigger by setting size to 2
5. Map the size of the points to HDI.Rank

'''
hdi.data <- read.csv('D:/Machine Learning/Data Hacks/Rgraphics/dataSets/EconomistData.csv')
head(hdi.data)
dim(hdi.data) # 173 6 
summary(hdi.data)

1. Create a scatter plot with CPI on the x axis and HDI on the y axis.

    hdi.plt <- ggplot(hdi.data,
                  aes(x = CPI, y = HDI)) +
                    geom_point()

2. Color the points blue.

    hdi.plt + geom_point(color = 'blue')
    
3. Map the color of the the points to Region.
    
    hdi.plt + geom_point(aes(color = Region))

4. Make the points bigger by setting size to 2

    hdi.plt + geom_point(aes(color = Region), size = 2)
    
5. Map the size of the points to HDI.Rank

    hdi.plt + geom_point(aes(color = Region, size = HDI.Rank))
    

#=============================== Statistical Transformations =====================================
'''
Some plot types (such as scatterplots) do not require transformations – each point is plotted at x 
and y coordinates equal to the original value. Other plots, such as boxplots, histograms, 
prediction lines etc. require statistical transformations:
     
  for a boxplot the y values must be transformed to the median and 1.5(IQR)
  for a smoother smother the y values must be transformed into predicted values

Each geom has a default statistic, but these can be changed. For example, the default statistic 
for geom_bar is stat_bin:
'''   
args(geom_histogram) # Find what stat the geom uses
args(stat_bin) # Find the arguments for that stat

args(geom_bar)
args(stat_count)

args(geom_smooth)
args(stat_smooth)

#======================= Setting Statistical Transformation Arguments ============================
'''
Arguments to stat_ functions can be passed through geom_ functions. This can be slightly annoying 
because in order to change it you have to first determine which stat the geom uses, then determine 
the arguments to that stat.

For example, here is the default histogram of Home.Value:
'''

# Default histogram of Home.Value
plt2 <- ggplot(housing,
               aes(x = Home.Value)) +
                  geom_histogram()

# Had trouble with the plot pane ... found a solution - to wrap ggplot() function inside print()
print(
    plt2 <- ggplot(housing,
               aes(x = Home.Value)) +
                  geom_histogram()
)

# To change the default stat we should first find the Stat the geom uses (here geom_histogram)
args(geom_histogram)
'''
function (mapping = NULL, data = NULL, stat = "bin", position = "stack", 
    ..., binwidth = NULL, bins = NULL, na.rm = FALSE, show.legend = NA, 
    inherit.aes = TRUE) 
'''
# Next find the arguments to the stat
args(stat_bin)
'''
function (mapping = NULL, data = NULL, geom = "bar", position = "stack", 
    ..., binwidth = NULL, bins = NULL, center = NULL, boundary = NULL, 
    breaks = NULL, closed = c("right", "left"), pad = FALSE, 
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
'''
# Knowing the above customize the geom_histogram() function
# We can change it by passing the binwidth argument to the stat_bin function
plt2 + geom_histogram(stat = 'bin', binwidth = 4000)

#============================ Changing The Statistical Transformation =======================
'''
Sometimes the default statistical transformation is not what you need. 
This is often the case with pre-summarized data:
'''
# Group housing by State and summarise Home.Value with mean
housing.state.grp <- aggregate(housing['Home.Value'], housing['State'], FUN = mean)
# Another way of aggregating using dplyr
#housing.state.grp <- group_by(housing, State) %>% summarise(mean(Home.Value)) 

# Now apply ggplot on the aggregated data
ggplot(housing.state.grp,
       aes(x = State, y = Home.Value)) +
          geom_bar()
'''
The above piece of code trying to plot an already aggregated data throws the below exception:

'Error: stat_count() must not be used with a y aesthetic.'

'What is the problem with the previous plot?'

Basically we take binned and summarized data and ask ggplot to bin and summarize 
it again (remember, geom_bar defaults to stat = stat_count); obviously this will not work. 
We can fix it by telling geom_bar to use a different statistical transformation function:

geom_bar(stat = 'identity')

'''
ggplot(housing.state.grp,
       aes(x = State, y = Home.Value)) +
          geom_bar(stat = 'identity')

#==================================== Exercise II ==========================================
1. Re-create a scatter plot with CPI on the x axis and HDI on the y axis 
   (as you did in the previous exercise).
2. Overlay a smoothing line on top of the scatter plot using geom_smooth.
3. Overlay a smoothing line on top of the scatter plot using geom_smooth, but use a 
   linear model for the predictions. Hint: see  ?stat_smooth.
4. Overlay a loess (method = 'loess') smoothling line on top of the scatter plot using geom_line. 
   Hint: change the statistical transformation.
5. BONUS: Overlay a smoothing line on top of the scatter plot using the default loess method, 
   but make it less smooth. Hint: see ?loess.

# Get the data-set ready
hdi.data <- read.csv('D:/Machine Learning/Data Hacks/Rgraphics/dataSets/EconomistData.csv')
head(hdi.data)
dim(hdi.data) # 173 6 
summary(hdi.data)

1. Re-create a scatter plot with CPI on the x axis and HDI on the y axis.

  plt3 <- ggplot(hdi.data,
          aes(x = CPI, y = HDI)) +
              geom_point()

2. Overlay a smoothing line on top of the scatter plot using geom_smooth.

  plt3 + geom_smooth()
  
3. Overlay a smoothing line on top of the scatter plot using geom_smooth, but use a 
   linear model for the predictions. Hint: see  ?stat_smooth.
   
   args(geom_smooth) # Find the stat argument for geom_smooth fn, it is stat = 'smooth', i.e. stat_smooth
   args(stat_smooth) # Now find the model args for stat_smooth fn
   ?stat_smooth # method - smoothing method (function) to use, eg. "lm", "glm", "gam", "loess", "rlm".
  
   plt3 + geom_smooth(method = lm)
   
4. Overlay a loess (method = 'loess') smoothling line on top of the scatter plot using geom_line. 
   Hint: change the statistical transformation.
   
   # stat_smooth statistical fn has method as one of its arguments which can take 'loess' as a value.
   # however the default statistical function of geom_line is 'identity' which does not have method
   # as it s argument. Hence we need a statistical transformation for geom_line()
   args(geom_line)
   ?stat_identity
   
   plt3 + geom_line(stat = 'smooth', method = 'loess')
   
5. BONUS: Overlay a smoothing line on top of the scatter plot using the 'default' loess method, 
   but make it less smooth. Hint: see ?loess.

   plt3 + geom_smooth(span = .5) # method = 'loess' is assumed as it is default for stat_smooth which
   # is the default stat function for geom_smooth.
   
#========================================= Scales ====================================================
Scales: Controlling Aesthetic Mapping
   
Aesthetic mapping (i.e., with aes()) only says that a variable should be mapped to an aesthetic. 
It doesn’t say how that should happen. For example, when mapping a variable to shape with aes(shape = x)
you don’t say what shapes should be used. Similarly, aes(color = z) doesn’t say what colors should be 
used. Describing what colors/shapes/sizes etc. to use is done by modifying the corresponding scale. 

In ggplot2 scales include

position
color and fill
size
shape
line type

Scales are modified with a series of functions using a scale_<aesthetic>_<type> naming scheme. 
Try typing scale_<tab> to see a list of scale modification functions.

Common Scale Arguments
----------------------
  
The following arguments are common to most scales in ggplot2:

  name: the first argument gives the axis or legend title
  limits: the minimum and maximum of the scale
  breaks: the points along the scale where labels should appear
  labels: the labels that appear at each break

Specific scale functions may have additional arguments; for example, the 'scale_color_continuous' 
function has arguments 'low' and 'high' for setting the colors at the low and high end of the scale.

# Start by constructing a dotplot showing the distribution of home values by Date and State.

  print(
        plt4 <- ggplot(housing,
                  aes(x = State, y = Home.Price.Index)) +
                    theme(legend.position = 'top',
                          axis.text = element_text(size = 5)) 
  )
  
  # Use jitter to avoid overplotting.
  print(
        plt5 <- plt4 +
                  geom_point(aes(color = Date),
                             size = 1.5,
                             alpha = 0.5,
                             position = position_jitter(width = 0.25, height = 0))
                             #position = position_jitterdodge(jitter.width = 0.25, jitter.height = 0, dodge.width = 0.75))
  )
  
  # Now modify the breaks for the x axis and color scales
  
  print(
        plt6 <- plt5 + 
                  scale_x_discrete(name = 'State Abbreviation') +
                    scale_color_continuous(name = 'Year', 
                                           labels = c('76','94','13'),
                                           breaks = c(1976, 1994, 2013))
  )
 
# Next change the low and high values to blue and red: 
 
print( 
      plt7 <- plt5 +
                scale_x_discrete(name = 'State Abbreviation') +
                  scale_color_continuous(name = '',
                                         breaks = c(1976, 1994, 2013),
                                         labels = c('76', '94', '13'),
                                         low = 'black',
                                         high = 'dark orange')
)
  
library(scales) # for muted() fn
print(
      plt8 <- plt5 +
                scale_x_discrete(name = 'State Abbreviation') +
                  scale_color_continuous(name = '',
                                         breaks = c(1976, 1994, 2013),
                                         labels = c('76', '94', '13'),
                                         low = muted('blue'),
                                         high = muted('maroon'))
)

#================================== Exercise III ===================================
1. Create a scatter plot with CPI on the x axis and HDI on the y axis. Color the 
   points to indicate region. 
   
2. Modify the x, y, and color scales so that they have 
   more easily-understood names (e.g., spell out “Human development Index” 
   instead of “HDI”).
   
3. Modify the color scale to use specific values of your choosing. 
   Hint: see ?scale_color_manual.

# Get the data-set ready
hdi.data <- read.csv('D:/Machine Learning/Data Hacks/Rgraphics/dataSets/EconomistData.csv')
head(hdi.data)
dim(hdi.data) # 173 6 
summary(hdi.data)

1. Create a scatter plot with CPI on the x axis and HDI on the y axis. 
   Color the points to indicate region.
   
  print(
         plt9 <- ggplot(hdi.data,
                          aes(x = CPI, y = HDI)) +
                            geom_point(aes(color = Region))
  )

2. Modify the x, y, and color scales so that they have more easily-understood 
   names (e.g., spell out “Human development Index” instead of “HDI”). 

  print(
          plt10 <- plt9 +
                      scale_y_continuous(name = 'Human Development Index')
  )
  
3. Modify the color scale to use specific values of your choosing. 
   Hint: see ?scale_color_manual.
   
   # Find the levels for the variable mentioned in the aes() fn
   levels(hdi.data$Region)
   # It's recommended to use a named vector, had some issues following just a vector convention
   # color_codes <- c('1' = 'darkgreen', '2' = 'yellow', '3' = 'blue', '4' = 'green', '5' = 'gray', '6' = 'red')
   color_codes <- c("#24576D", "#099DD7", "#28AADC", "#FF67FF", "#F2583F", "#96503F")

   print(
          plt11 <- ggplot(hdi.data,
                          aes(x = CPI, y = HDI, color = Region)) +
                            geom_point() +
                              scale_x_continuous(name = 'Corruption Perception Index') +
                                scale_y_continuous(name = 'Human Development Index') +
                                  scale_color_manual(name = 'Regions of the World',
                                                     values = color_codes)
   )
  
#===================================== Faceting ===============================================
1. Faceting is ggplot2 parlance for 'small multiples'
2. The idea is to create 'separate graphs for subsets of data'
3. ggplot2 offers two functions for creating small multiples:
  
    1. facet_wrap(): define subsets as the levels of a single grouping variable
    2. facet_grid(): define subsets as the crossing of two grouping variables
    
4. Facilitates 'comparison among plots', not just of geoms within a plot 

# What is the trend in housing prices in each state?
# Start by using a technique we already know–map State to color:
There are two problems here–there in the below graph are too many states to distinguish each one 
by color, and the lines obscure one another.
    
  plt10 <- ggplot(housing,
                  aes(x = Date, y = Home.Value)) 
  
  print(
        plt10 + geom_line(aes(color = State))
  )
  
# Faceting to the rescue
We can remedy the deficiencies of the previous plot by faceting by state rather than mapping 
state to color.

Note: facet_grid() also exists for faceting in 2d

  plt10 + geom_line() +
            facet_wrap(~State, ncol = 10) +
              # Use theme to modify facet axis, theme is discussed in the next part
              theme(axis.text.x = element_text(size = 3))
  
#========================================= Themes =================================================
The ggplot2 theme system handles 'non-data plot elements' such as

  Axis labels
  Plot background
  Facet label backround
  Legend appearance
            
There are multiple built-in themes, some of them are:
  
  theme_gray (this is the default theme)
  theme_bw
  theme_classic
  
  plt10 + geom_line() +
            facet_wrap(~State, ncol = 10) +
              theme_light() +
              # Use theme to modify facet axis, theme is discussed in the next part
              theme(axis.text.x = element_text(size = 4.5))
  
    plt10 + geom_line() +
            facet_wrap(~State, ncol = 10) +
              theme_linedraw() +
              # Use theme to modify facet - axis, background
              theme(axis.text.x = element_text(size = 4.5),
                    # Alter graph axis
                    text = element_text(color = 'turquoise'),
                    # Facet background  
                    panel.background = element_rect(fill = 'pink'),
                    strip.background = element_rect(fill = muted('red')))
    
#=================================== Creating and saving new themes =============================

my.theme <- theme_bw() + 
              theme(
                  # Entire background
                  plot.background = element_rect(size = 1, color = 'blue', fill = 'black'),
                  # Text for graph
                  text = element_text(size = 12, color = 'ivory', family = 'serif'),
                  # Facet x and y axis
                  axis.text.x = element_text(color = 'purple', size = 5),
                  axis.text.y = element_text(color = 'red'),
                  # Facet panel and title strip
                  panel.background = element_rect(fill = 'pink'),
                  strip.background = element_rect(fill = muted('red'))
              )
                    
# Now use the customized theme

    plt10 + geom_line() +
      facet_wrap(~State, ncol = 10) +
        my.theme
          
  
#-------------------------------- Rough Work --------------------------------------------

   values = c("#24576D",
                                                                "#099DD7",
                                                                "#28AADC",
                                                                "#FF67FF",
                                                                "#F2583F",
                                                                "#96503F")
   
sum(is.na(hdi.data$Region))
?factor
factor(hdi.data$Region)
   
# To visualize plots 
dev.cur()
dev.off()
print.eval = TRUE
print(ggplot(housing,
               aes(x = Home.Value)))

print(ggplot(housing,
               aes(x = Home.Value)) +
                  geom_histogram())

summary(housing)
