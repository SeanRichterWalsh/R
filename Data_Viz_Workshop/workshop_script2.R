# R Data Visualisation Workshop using ggplot2 and dygraphs
# Author: Sean Walsh
# Contact: sw23993@my.open.ac.uk


###########################
## PART 1: ggplot2 tutorial
###########################

# What is ggplot2? The Grammar of Graphics is based on three core components: a dataframe, aesthetic mapping, and geometric shapes for the mapping.
# Install (if necessary) and load the packages and data needed for this workshop:

# Load packages (knitr and rmarkdown should be installed by default)
library(ggplot2)
library(dplyr)
library(dygraphs)
library(ggcorrplot)
library(readxl)
library(xts)
library(networkD3)
library(viridis) # Beautiful colour scales


# Package developers
citation("ggplot2")
citation("dplyr")
citation("dygraphs")
citation("ggcorrplot")
citation("readxl")
citation("xts")
citation("networkD3")
citation("viridis")
citation("rmarkdown")
citation("knitr")


## DATASETS (we will use 3: diamonds, churn and timeseries)

# Note code writing best practice: always use whitespace and tidy indentation
# Hadley Wickham's style guide: http://style.tidyverse.org/ 
data("diamonds")
?diamonds

# I am using absolute paths to my home directory here so you will need to adjust this for your own machine later
churn <- read_excel("~/test_dfXL.xlsx", 
                    sheet = 1,
                    col_names = T)

timeseries <- read_excel("~/test_dfXL.xlsx",
                         sheet = 2,
                         col_names = T)


## VISUALISING UNIVARIATE DATA

# The best graphical methods for visualising univariate distributions tend to be histograms, density plots and index plots. 
# We want to investigate the shape of the data and also identify any potential anomalies or outliers. 
# Knowing the shape of data samples is highly important when making statistical inferences, performing statistical modelling and 
# testing hypotheses. These plot types are the bread and butter of data scientists and statisticians.

# Basic histogram (R sets the binwidth by default)
# binwidth = range / n desired bins
ggplot(data = diamonds, 
       aes(x = price)) +
  geom_histogram()

# Base R version of the histogram ("off the shelf")
hist(diamonds$price)

# Natural log (ln) transform the x-axis variable (an example of coercion to a more normal distribution)
# Transformations can be done inside the ggplot specification
ggplot(data = diamonds, 
       aes(x = log(price))) +
  geom_histogram()

# Arithmetic operations can also be performed inside ggplot 
ggplot(data = diamonds, 
       aes(x = price / carat)) +
  geom_histogram(bins = 25) +
  xlab("price per carat ($)")

# Basic density plot (plots the probability density function pdf)
ggplot(data = diamonds, 
       aes(x = price)) +
  geom_density(adjust = 0.5) # The kernel smoothness can be adjusted using the adjust argument

# Which is the same as
density(diamonds$price) %>% 
  plot()

# Base R version
plot(density(diamonds$price),
     main = "Probability density function\n of diamond price",
     xlab = "price ($)")

# Index plots are useful for spotting outliers or anomalies
plot(churn$LIFETIME) # Random distribution with constant variance

plot(diamonds$depth,
     col = if_else(diamonds$depth > 75 | diamonds$depth < 50, "red", "black"),
     main = "Diamond depth index plot",
     ylab = "depth (%)") # Mostly constant but with some outliers in terms of diamond depth

# So far we have been considering univariate countinuous data but we can also have categorical data in univariate analysis
# Categorical data in R are termed factors and can be ordinal or nominal

# We can use a barplot to visualise the frequency of the ordered factor cut in these data
ggplot(data = diamonds,
       aes(x = cut)) +
  geom_bar()

# We can also build a radar plot and customise a little
ggplot(data = diamonds,
       aes(x = cut)) +
  geom_bar(fill = "#00306A",
           colour = "white") +
  coord_polar() +
  theme_minimal() +  # Different look/theme
  theme(text = element_text(face = "bold",
                            family = "AvantGarde")) 
 

## VISUALISING BIVARIATE DATA

# When exploring bivariate data, typical visualisations used are scatter plots, column plots and boxplots.
# Boxplots are great for comparing the distributions of different samples but also for a single covariate

# Basic scatter plot
ggplot(data = diamonds, 
       aes(x = carat,
           y = price)) +
  geom_point()

# Overplotting can be solved using a transparency setting and jitter (random noise)
# Warning! Jitter will slightly alter the actual values when plotting so it is useful for identifying patterns only
ggplot(data = diamonds, 
       aes(x = carat,
           y = price)) +
  geom_jitter(alpha = 0.2)

# Boxplots to compare different samples
ggplot(data = diamonds, 
       aes(x = cut, # Categorical data on x-axis
           y = price)) +
  geom_boxplot()

# Boxplots without outliers (outlier definition: [Q1 - 1.5 * IQR, Q3 + 1.5 * IQR])
ggplot(data = diamonds, 
       aes(x = cut,
           y = price)) +
  geom_boxplot(outlier.shape = NA,
               width = 0.25) +
  theme_bw()

# Violin plot (merges density plots with boxplots)
ggplot(data = diamonds, 
       aes(x = cut,
           y = price)) +
  geom_violin(fill = "#6CAEE0",
              adjust = 0.5) +
  coord_flip()

# Column plot from a dplyr pipeline
diamonds %>%
  group_by(cut) %>%
  summarise(median_price = median(price, na.rm = T)) %>% # Median is preferred to mean as the distribution is heavily skewed (see univariate section)
  ungroup() %>%
  ggplot(aes(x = cut,
             y = median_price)) +
  geom_col(width = 0.75,
           fill = "#6CAEE0",
           colour = "black") +
  geom_hline(yintercept = 0)

# Line plot from a dplyr pipeline
churn %>%
  select(START_DATE, 
         RATING) %>%
  mutate(START_MNTH = as.yearmon(START_DATE)) %>%
  mutate(START_MNTH = as.POSIXct(START_MNTH)) %>%
  group_by(START_MNTH, 
           RATING) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  ggplot(aes(x = START_MNTH,
             y = total,
             colour = RATING)) +
  geom_line()

# Correlation plot
diamonds %>%
  select(1, 5, 6:10) %>%
  cor() %>%
  ggcorrplot(hc.order = T,
             title = "Pearson correlation between continuous variables",
             legend.title = "P",
             method = "square")


# Making publication-ready plots with additional ggplot2 functions
?theme_bw

# Scatterplot with additional information on diamond cut
ggplot(data = diamonds, 
       aes(x = carat,
           y = sqrt(price),
           colour = cut)) +
  geom_jitter(alpha = 0.5) +
  xlab("carat") +
  ylab("sqrt(price)") +
  scale_colour_discrete(name = " ") +
  ggtitle("Linear relationship between diamond price and carat",
          subtitle = "Source: diamonds dataset from the ggplot2 package") +
  theme_minimal()

# It's a bit difficult to distinguish the colours so let's facet wrap
ggplot(data = diamonds, 
       aes(x = carat,
           y = sqrt(price))) +
  geom_jitter(alpha = 0.5) +
  facet_wrap(~cut, scales = "free_y") +
  geom_smooth(method = "lm") +
  ggtitle("Linear relationship between diamond price and carat with regression",
          subtitle = "Source: diamonds dataset from the ggplot2 package") +
  theme_bw()

# Same y-axis scale for comparison of gradients (rate of change)
ggplot(data = diamonds, 
       aes(x = carat,
           y = sqrt(price))) +
  geom_jitter(alpha = 0.5) +
  facet_wrap(~cut) +
  geom_smooth(method = "lm") +
  ggtitle("Linear relationship between diamond price and carat with regression",
          subtitle = "Source: diamonds dataset from the ggplot2 package") +
  theme_bw()

# Line plot with modifications
# Fit loess (locally estimates statistical smoothers) curves to the data
churn %>%
  select(START_DATE, 
         RATING) %>%
  filter(RATING != "NONE") %>%
  mutate(START_MNTH = as.yearmon(START_DATE)) %>%
  mutate(START_MNTH = as.POSIXct(START_MNTH)) %>%
  group_by(START_MNTH, 
           RATING) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  ggplot(aes(x = START_MNTH,
             y = total,
             colour = RATING,
             shape = RATING)) +
  geom_point() +
  geom_smooth(se = T) +
  ylab("total sign-ups") +
  xlab(" ") +
  ggtitle("New customer sign-ups (2013-2018)") +
  theme_bw() +
  facet_wrap(~RATING,
             scales = "free_y")

# Flipping axes
ggplot(data = diamonds, 
       aes(x = cut,
           y = price)) +
  geom_boxplot(outlier.shape = NA,
               width = 0.25,
               fill = "#6CAEE0") +
  xlab(" ") +
  ggtitle("Distribution of diamond prices by cut",
          subtitle = "Source: diamonds dataset from the ggplot2 package") +
  coord_flip() +
  theme_bw()

# Colouring columns based on a threshold criterion
churn$RATING <- factor(churn$RATING, 
                       ordered = T,
                       levels = c("F","E","D","C","B","A"))

churn %>%
  filter(CHURNED == 1 & RATING != "NONE") %>%
  mutate(Mean_lifetime = mean(LIFETIME, na.rm = T),
         Deviation = LIFETIME - Mean_lifetime) %>%
  group_by(RATING) %>%
  summarise(Mean_deviation = mean(Deviation, na.rm = T)) %>%
  ungroup() %>%
  ggplot(aes(x = RATING,
             y = Mean_deviation,
             fill = Mean_deviation < 0)) +
  geom_col(colour = "black",
           width = 0.8) +
  geom_hline(yintercept = 0) +
  coord_flip()

# A better example from:
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
data("mtcars")  # load data
mtcars$`car name` <- rownames(mtcars)  # create new column for car names
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # compute normalized mpg
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # above / below avg flag
mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`)  # convert to factor to retain sorted order in plot.

# Diverging Barcharts
ggplot(mtcars, aes(x = `car name`, 
                   y = mpg_z, 
                   label = mpg_z)) + 
  geom_bar(stat = 'identity', 
           aes(fill = mpg_type), 
           width = 0.5)  +
  scale_fill_manual(name = "Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00306A", "below"="#6CAEE0")) + 
  labs(subtitle = "Normalised mileage from 'mtcars'", 
       title = "Diverging Bars") + 
  coord_flip() +
  theme_bw() +
  theme(text = element_text(face = "bold"))

# Facetting with radar plots
ggplot(data = diamonds,
       aes(x = color,
           fill = color)) +
  geom_bar(colour = "black") +
  xlab(" ") +
  coord_polar() +
  facet_wrap(~cut) +
  theme_bw() +
  theme(text = element_text(face = "bold",
                            family = "courier")) +
  ggtitle("Diamond color frequency by cut",
          subtitle = "Source: diamonds dataset from the ggplot2 package")
  

## BONUS MATERIAL: RAINCLOUD PLOTS AND SANKEY DIAGRAMS

# Raincloud plots combine multiple geoms into one pretty data visualisation: a very novel data visualisation
# geom_density, geom_point, geom_violin and geom_boxplot can all be present in a raincloud plot
# The "clouds" represent the probability density function pdf of a univariate distribution while the "rain" shows the distribution of the underlying data


# Create a function using the following R script courtesy of Dr. Micah Allen
# If you think that you will use raincloud plots a lot then I advise saving the functions and the ggproto object created in this script
source("~/RAINCLOUDS/raincloud_function.R")

# Basic raincloud plot
ggplot(data = diamonds,
       aes(x = cut,
           y = price)) +
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0),
                   adjust = 2) +
  geom_point(position = position_jitter(width = 0.15), 
             size = 0.25) +
  ylab("price ($)") +
  xlab(" ") +
  ggtitle("Raincloud plot of diamond price by cut",
          subtitle = "Source: diamonds dataset from the ggplot2 package") +
  coord_flip() 

# Add some colour and customise a little
ggplot(data = diamonds,
       aes(x = cut,
           y = price,
           fill = cut)) +
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0),
                   adjust = 2) +
  geom_point(position = position_jitter(width = 0.15), 
             size = 0.25,
             alpha = 0.5) +
  ylab("price ($)") +
  xlab(" ") +
  ggtitle("Raincloud plot of diamond price by cut",
          subtitle = "Source: diamonds dataset from the ggplot2 package") +
  coord_flip() +
  guides(fill = F) +
  theme_minimal() +
  theme(text = element_text(face = "bold",
                            family = "Bookman"))

# Adjust the smoothing kernel and transparency
ggplot(data = diamonds,
       aes(x = cut,
           y = price,
           fill = cut)) +
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0),
                   adjust = 0.2) +
  geom_point(position = position_jitter(width = 0.15), 
             size = 0.25,
             alpha = 0.2) +
  ylab("price ($)") +
  xlab(" ") +
  ggtitle("Raincloud plot of diamond price by cut",
          subtitle = "Source: diamonds dataset from the ggplot2 package") +
  coord_flip() +
  guides(fill = F) +
  theme_minimal() +
  theme(text = element_text(face = "bold",
                            family = "Bookman"))

# Facet wrapping 
ggplot(data = diamonds,
       aes(x = cut,
           y = price,
           fill = cut)) +
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0),
                   adjust = 0.2) +
  geom_point(position = position_jitter(width = 0.15), 
             size = 0.25,
             alpha = 0.5) +
  ylab("price ($)") +
  xlab(" ") +
  theme_minimal() +
  ggtitle("Raincloud plot of diamond price by cut and color",
          subtitle = "Source: diamonds dataset from the ggplot2 package") +
  coord_flip() +
  guides(fill = F) +
  facet_wrap(~color) +
  theme(text = element_text(face = "bold",
                            family = "AvantGarde"))

# Sankey diagram example
nodes = data.frame("name" = 
                     c("CALLSTAR", #0
                       "Switcher", #1
                       "Field Agents", #2 
                       "Online", #3
                       "Inbound", #4
                       "Outbound", #5
                       "Developer", #6
                       "Unavailable", #7
                       "Single product", #8
                       "Dual product", #9
                       "Triple product", #10
                       "Contract", #11
                       "0-1 year", #12
                       "1-3 year", #13
                       ">3 year", #14
                       "Zero traits", #15
                       "One traits", #16
                       "Two traits", #17
                       "Three traits", #18
                       "VIP")) #19

links = as.data.frame(matrix(c(
  0, 8, 15391,
  0, 9, 16782,
  0, 10, 18110,
  0, 11, 189,
  1, 8, 112,
  1, 9, 13845,
  1, 10, 4179,
  1, 11, 298,
  2, 8, 520,
  2, 9, 20360, 
  2, 10, 23209, 
  2, 11, 2900, 
  3, 8, 653,
  3, 9, 8281,
  3, 10, 5141,
  3, 11, 344,
  4, 8, 2817,
  4, 9, 5029,
  4, 10, 7529,
  4, 11, 32,
  5, 8, 37,
  5, 9, 209,
  5, 10, 1325,
  5, 11, 33,
  6, 8, 781,
  6, 9, 4706,
  6, 10, 4268,
  6, 11, 222,
  7, 8, 103603,
  7, 9, 99484,
  7, 10, 50956,
  7, 11, 55,
  8, 12, 11231,
  8, 13, 13179,
  8, 14, 100735,
  9, 12, 38657,
  9, 13, 41396,
  9, 14, 102526,
  10, 12, 33534,
  10, 13, 41336,
  10, 14, 58587,
  11, 12, 252,
  11, 13, 325,
  11, 14, 362,
  12, 15, 32084,
  12, 16, 22566,
  12, 17, 33371,
  12, 18, 4777,
  13, 15, 32084,
  13, 16, 47977,
  13, 17, 33371,
  13, 18, 66555,
  14, 15, 44168,
  14, 16, 37997,
  14, 17, 33371,
  14, 18, 4444,
  15, 19, 4994,
  16, 19, 22311,
  17, 19, 564,
  18, 19, 10198),
  byrow = T, 
  ncol = 3))

names(links) = c("source", "target", "value")

sankeyNetwork(Links = links, 
              Nodes = nodes,
              Source = "source", 
              Target = "target",
              Value = "value", 
              NodeID = "name",
              fontSize= 12,
              fontFamily = "Calibri",
              nodeWidth = 25)


####################################################
## PART 2: dygraphs interactive time-series plotting
####################################################

# dygraphs is an open-source JavaScript charting library. The R package provides an interface to dygraphs.
# Very clean interactive graphics which can be embedded beautifully into reports, webpages and dashboards.

m.xts <- xts(lead(timeseries$Male), timeseries$Date) 
f.xts <- xts(lead(timeseries$Female), timeseries$Date)

# Cbind together for plotting
all <- cbind(male = m.xts, female = f.xts)

# Interactive time-series dygraph 
# Time-slicer and embedded data are excellent features
dygraph(all)

# Note how it can be used with dplyr's pipe operator
dygraph(all) %>%
  dyRangeSelector()

# Modify
dygraph(all) %>%
  dyLegend(show = "always",
           labelsSeparateLines = F,
           width = 500) %>%
  dyOptions(drawGrid = T) %>%
  dySeries("male", color = "#6CAEE0", strokeWidth = 3) %>%
  dySeries("female", color = "#00306A", strokeWidth = 3) %>%
  dyAxis("y", label = "Population",
         valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
         axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
         axisLabelWidth = 70) %>%
  dyEvent("2013-05-01", "Recovery", 
          labelLoc = "bottom")


## Useful links:

# https://rstudio.github.io/dygraphs/
# R for Data Science
# citation("ggplot2")
# StackOverflow
# DataCamp
# R MeetUps (Dublin R, R Generation, Data Chats in a Pub, Data Scientists Ireland)
# https://www.htmlwidgets.org/
# https://micahallen.org/blog-neuroconscience/


