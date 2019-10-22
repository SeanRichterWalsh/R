# One of the many strengths of using R for data analysis is the fact that you can keep a log (i.e. an R script)
# of each step you take in your analysis so that your work is reproducible. PBI is a decent tool for quick dashboard 
# development (given you have a tidy data frame for it to sit on) and makes an attempt at logging steps in the PowerQuery 
# tool but it is hard to follow and R scripts are much easier to tweak and reverse engineer.

# With R Studio, flexdashboard and HTML widgets we can merge the two worlds of statistics and web development to create 
# beautiful reports and dashboards. We can even automate these to render following a specified schedule on a server.

## SETUP ##
library(dplyr)
library(tidyr)

# Data import (the CSV I shared with you has already been cleaned)
fo_data <- read.csv("/home/swalsh/frenchopen_2019.csv",
                    stringsAsFactors = FALSE)

## QUICK DATA QUALITY CHECK AND EXPLORATION ##

# Location and ATP event code
summary(fo_data$ATP)
table(fo_data$Location, useNA = "always")
table(fo_data$Tournament, useNA = "always")

# Match dates
summary(fo_data$Date) # Date imported as a character vector by default using read.csv
head(fo_data$Date)
fo_data$Date <- as.Date(fo_data$Date, format = "%Y-%m-%d")
summary(fo_data$Date) 

# Tournament properties
table(fo_data$Series, useNA = "always")
table(fo_data$Court, useNA = "always")
table(fo_data$Surface, useNA = "always")
table(fo_data$Round, useNA = "always") # Round of 128 (x2)
table(fo_data$Best.of, useNA = "always")

# Player data
table(fo_data$Winner, useNA = "always")
table(fo_data$Loser, useNA = "always") 

# All players in the 2019 competition (could pass to wordcloud)
c(table(fo_data$Winner) %>% names(), 
  table(fo_data$Loser) %>% names()) %>% 
  unique() %>%
  sort()

# ATP ranking distribution prior to competition
summary(fo_data$WRank)

table(fo_data$LRank, useNA = "always") # Why an NA? Because we have a player in the data set with no ranking/ranking points.

fo_data %>%
  filter(is.na(LRank))

# ATP ranking points prior to competition
summary(fo_data$WPts)
table(fo_data$LPts, useNA = "always")

fo_data %>%
  filter(is.na(LPts))

# Match data
summary(fo_data$Wsets)
table(fo_data$Wsets, useNA = "always")

fo_data %>%
  filter(Wsets == 2)

summary(fo_data$Lsets)

table(fo_data$Comment, useNA = "always")

# All NAs make sense as not all matches went to 4 or 5 sets; plus non-ranking.
sapply(fo_data, 
       function(x){
         sum(is.na(x))
         }
       ) 

# First, I want to show you the dashboard that we are going to create using R.
# Okay. Let's move on to dashboard development. Restart your R session and open the FRENCH_OPEN_2019.Rmd



