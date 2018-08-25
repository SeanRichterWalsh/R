# R Data Visualisation Workshop using ggplot2 and dygraphs
# 25 August 2018


##################
## PART 1: ggplot2 
##################

# What is ggplot2? The Grammar of Graphics is based on three core components: a dataframe, aesthetic mapping, and geometric shapes for the mapping.
# Install (if necessary) and load the packages and data needed for this workshop:

# Load packages (knitr and rmarkdown should be installed by default)
library(ggplot2)
library(dplyr)
library(dygraphs)
library(ggcorrplot)
library(readxl)
library(xts)


# Package developers
citation("ggplot2")
citation("dplyr")
citation("dygraphs")
citation("readxl")
citation("xts")
citation("rmarkdown")
citation("knitr")


# DATASETS

# Note code writing best practice: always use whitespace and tidy indentation
# http://style.tidyverse.org/ # Hadley Wickham's style guide
data("diamonds")
?diamonds

setwd("~/Desktop/Data_Viz_Workshop")

churn <- read_excel("test_dfXL.xlsx", 
                    sheet = 1)

timeseries <- read_excel("test_dfXL.xlsx",
                         sheet = 2)


# VISUALISING UNIVARIATE DATA

# The best graphical methods for visualising univariate distributions tend to be histograms, density plots and index plots. 
# We want to investigate the shape of the data and discover any potential anomalies or outliers. 
# Knowing the shape of data samples is highly important when making inferences, performing statistical modelling and 
# testing hypotheses. These plot types are the bread and butter of data scientists and statisticians.

# Basic histogram
ggplot(data = diamonds, 
       aes(x = price)) +
  geom_histogram()

# Base R version
hist(diamonds$price)

# Log transform the x-axis variable
ggplot(data = diamonds, 
       aes(x = log(price))) +
  geom_histogram()

# Arithmetic operations can be performed when assigning aesthetic mappings 
ggplot(data = diamonds, 
       aes(x = price / carat)) +
  geom_histogram()

# Basic density plot
ggplot(data = diamonds, 
       aes(x = price)) +
  geom_density()

# Base R version
plot(density(diamonds$price),
     main = "Probability density function\n of diamond price",
     xlab = "price ($)")

# Index plot
# Useful for spotting outliers or anomalies
plot(churn$LIFETIME)
plot(diamonds$depth,
     col = if_else(diamonds$depth > 75 | diamonds$depth < 50, "red", "black"),
     main = "Diamond depth index plot",
     ylab = "depth (%)")


# VISUALISING BIVARIATE DATA

# When exploring bivariate data, typical visualisations used are scatter plots, column plots and boxplots.
# Boxplots are great for comparing the distributions of different samples.

# Basic scatter plot
ggplot(data = diamonds, 
       aes(x = carat,
           y = price)) +
  geom_point()

# Overplotting can be solved using a transparency setting and jitter (random noise)
# Warning! Jitter will slightly alter the actual measure values so it is useful for identifying patterns only
ggplot(data = diamonds, 
       aes(x = carat,
           y = price)) +
  geom_point(alpha = 0.2)

# Boxplots to compare different samples
ggplot(data = diamonds, 
       aes(x = cut, # Categorical data on x-axis
           y = price)) +
  geom_boxplot()

# Boxplots without outliers
ggplot(data = diamonds, 
       aes(x = cut,
           y = price)) +
  geom_boxplot(outlier.shape = NA,
               width = 0.25) +
  theme_bw() # Different look

# Violin plot (merges density plots with boxplots)
ggplot(data = diamonds, 
       aes(x = cut,
           y = price)) +
  geom_violin(fill = "#6CAEE0") +
  coord_flip()

# Bar plot from a dplyr pipeline
diamonds %>%
  group_by(cut) %>%
  summarise(median_price = median(price, na.rm = T)) %>%
  ungroup() %>%
  ggplot(aes(x = cut,
             y = median_price)) +
  geom_col(width = 0.75,
           fill = "lavender",
           colour = "black") +
  geom_hline(yintercept = 0)

# Line plot from a dplyr pipeline
churn %>%
  select(START_DATE, RATING) %>%
  mutate(START_MNTH = as.yearmon(START_DATE)) %>%
  mutate(START_MNTH = as.POSIXct(START_MNTH)) %>%
  group_by(START_MNTH, RATING) %>%
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
  ggtitle("Linear relationship between diamond price and carat",
          subtitle = "Source: diamonds dataset from the ggplot2 package") +
  theme_bw()

# Same y-axis scale
ggplot(data = diamonds, 
       aes(x = carat,
           y = sqrt(price))) +
  geom_jitter(alpha = 0.5) +
  facet_wrap(~cut) +
  geom_smooth(method = "lm") +
  ggtitle("Linear relationship between diamond price and carat",
          subtitle = "Source: diamonds dataset from the ggplot2 package") +
  theme_bw()

# Line plot with modifications
# Fit loess (locally estimates statistical smoothers) curves to the data
churn %>%
  select(START_DATE, RATING) %>%
  filter(RATING != "NONE") %>%
  mutate(START_MNTH = as.yearmon(START_DATE)) %>%
  mutate(START_MNTH = as.POSIXct(START_MNTH)) %>%
  group_by(START_MNTH, RATING) %>%
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
ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_bar(stat='identity', aes(fill=mpg_type), width=.5)  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised mileage from 'mtcars'", 
       title= "Diverging Bars") + 
  coord_flip() +
  theme_bw()


########################################
## PART 2: dygraphs time-series plotting
########################################

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


###############################
## PART 3: R Markdown reporting
###############################

# Please use .Rmd file




# Useful links:

# https://rstudio.github.io/dygraphs/
# R for Data Science
# citation("ggplot2")
# StackOverflow
# DataCamp
# R MeetUps (Dublin R, R Generation, Data Chats in a Pub, Data Scientists Ireland)
# https://gb25.gbif-uat.org/en/programme/#friday-19-october-2018 
# https://www.htmlwidgets.org/















