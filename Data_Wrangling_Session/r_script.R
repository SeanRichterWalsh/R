# Load packages required for the session
library(dplyr)
library(forcats)
library(stringr)
library(tidyr)
library(ggplot2) 
library(gridExtra)


# Data wrangling, cleaning and preprocessing are synonymous terms for getting data ready for the extraction of insight
# This task takes up the majority of a data scientist's time but is often highly enjoyable (for me at least)
# Even more satisfying is when it is done and you can get a statistical result or build a statistical model

# Read in the food dataset from whichever working directory you have it stored in
# There are multiple ways to import data but I am working with a .csv file here
# In practice, you are likely to come across .csv, .txt, JSON, shapefiles, .xlsx and DWH connections, etc.
food <- read.csv("~/data_wrangling_session_20181204/food.csv",
                 stringsAsFactors = F,
                 header = T)

# Let's have a look at the variables that we are working with
# I use the dplyr package heavily in this script as it is a fundamental tool for data manipulation
glimpse(food)

# Duplicate ID codes present? No. All entries are unique
n_distinct(food$code)

# There is another useful function distinct() which removes any duplicated rows
food %>%
  distinct() %>%
  nrow() # I just want a count of distinct observations here so I use nrow() 

# Many of the variables are nutritional descriptors so let's not focus on these for the purposes of this analysis
# Create a chopped down version of the original dataset which contains variables of interest (note that this is not usually necessary
# but it makes for easier summarising and visualisation of summary information; a big monitor helps to avoid cluttering too!)
food_tidy <- food %>%
  select(code,
         created_datetime,
         main_category_en,
         main_category,
         cities,
         cities_tags,
         countries,
         countries_tags,
         countries_en,
         purchase_places,
         stores,
         ingredients_from_palm_oil_n,
         ingredients_from_palm_oil_tags)

?dplyr::select

# Above I used select() from the dplyr package. dplyr has some main verbs (select, filter, group_by, summarise, mutate, join
# and arrange) and I recommend that you become a proficient dplyr user. It is the package I use most often in practice. See a 
# post I did for R-bloggers which shows some of the main functions, with examples, in dplyr:

# https://www.r-bloggers.com/useful-dplyr-functions-wexamples/

?dplyr

# Inspect the data structure once more; the dataset is dominated by string data type (character)
glimpse(food_tidy)


# A column by column approach is usually necessary in practice and the work can be tedious 
# However, the end result is worth it! I will just go through some of our selected columns here

# Food ID code - we can leave it as character string data as we have no use for it other than DWH identification
class(food_tidy$code)

# created_datetime - let's convert to a proper date object (I use as.Date() but there are many alternatives e.g. lubridate package)
# Entries were made between 18/03/2012 and 11/01/2016
food_tidy$created_datetime <- as.Date(food_tidy$created_datetime)
?as.Date

summary(food_tidy$created_datetime) 

# main_category_en and main_category are proxies for one another; let's use the en version

# Which food categories are most frequent in these data?
# dplyr is used heavily in this next workflow (note the use of the pipe "%>%" operator)
food_tidy %>%
  group_by(main_category_en) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(25) %>%
  ggplot(aes(x = main_category_en,
             y = count)) +
  geom_col(fill = "forestgreen") +
  xlab(" ") +
  ylab("Frequency") +
  coord_flip() +
  theme(text = element_text(face = "bold",
                            family = "AvantGarde")) +
  ggtitle("Frequency of various food type categories",
          subtitle = "Source: https://www.datacamp.com")

# Order columns by frequency (ggplot2 orders alphabetically by default)
food_tidy %>%
  group_by(main_category_en) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(25) %>%
  ggplot(aes(x = reorder(main_category_en, # reorder this categorical variable
                         count, # by this continuous measure
                         FUN = sum), # and this function of the continuous measure
             y = count)) +
  geom_col(fill = "forestgreen") +
  xlab(" ") +
  ylab("Frequency") +
  coord_flip() +
  theme(text = element_text(face = "bold",
                            family = "AvantGarde")) +
  ggtitle("Frequency of various food type categories",
          subtitle = "Source: https://www.datacamp.com")


# Investigate the missing category observations (empty strings "") that make up the mode (most frequent category)
# Relate to main_category: it turns out that there is no categorisation done here and we could do it as a follow-up but
# that would involve much more research into the ID codes and data source
food_tidy %>%
  select(main_category_en,
         main_category) %>%
  group_by(main_category_en,
           main_category) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))

# So for our purposes, let's just remove cases where no categorisation has been carried out
food_tidy <- food_tidy %>%
  filter(main_category_en != "") 

# Now we have 1,124 rows and 13 columns
dim(food_tidy)

# Let's clean up the countries_en variable 
summary(factor(food_tidy$countries_en))

# Use separate() from tidyr to obtain individual country information where there are more than one listed (e.g. Denmark,France,Germany)
# unite() is the complement of separate() so feel free to expore that function in your own time
food_tidy <- food_tidy %>%
  separate(countries_en, c("Country1", # I separate the countries_en string data into these three new columns
                           "Country2",
                           "Country3"),
           sep = ",") # I tell R to split the strings where a comma is found

summary(factor(food_tidy$Country1))

# Make Country1 a nominal factor now
food_tidy$Country1 <- factor(food_tidy$Country1)

# Inspect the cleaned Country1 variable
summary(food_tidy$Country1)

# There are just 24 observations where a second country is listed
food_tidy %>%
  filter(!is.na(Country2)) %>%
  pull(Country2) %>% # pull() is another very useful dplyr function and returns a vector unlike select()
  factor() %>%
  summary() %>%
  sum()

?pull()

# What about third country? Just one observation has a third country associated with it
food_tidy %>%
  filter(!is.na(Country3)) %>%
  pull(Country3) %>%
  factor() %>%
  summary() %>%
  sum()

# France makes up over 70% of all purchases in this dataset
summary(food_tidy$Country1) %>% 
  prop.table() %>% 
  round(3)

# Let's chop our dataset down to what we need to address:
#1) What food type categories are most prevalent in this dataset and which types are countries associated with?
#2) What food types tend to be associated with ingredients from palm oil?
food_tidy <- food_tidy %>%
  select(code,
         created_datetime,
         main_category_en,
         Country1,
         cities_tags,
         ingredients_from_palm_oil_n)

# Now is a good time to check missingness in our remaining variables
# This is a useful one-liner which counts NAs column-wise across a dataframe using sapply()
sapply(food_tidy, function(x) sum(is.na(x)))

# ingredients_from_palm_oil_n has 160 actual NAs which we cannot reasonably impute
table(food_tidy$ingredients_from_palm_oil_n, 
      useNA = "always")

# We can leave this variable as an integer data type but I would like to show an application of forcats()
# First, convert ingredients_from_palm_oil_n to a factor
food_tidy$ingredients_from_palm_oil_n <- factor(food_tidy$ingredients_from_palm_oil_n)
summary(food_tidy$ingredients_from_palm_oil_n)


# Now, I want to assign "Yes" to "1" values and "No" to "0" values
food_tidy$ingredients_from_palm_oil_n <- fct_collapse(food_tidy$ingredients_from_palm_oil_n,
                                                      Yes = "1",
                                                      No = "0")

summary(food_tidy$ingredients_from_palm_oil_n)

# Let's assign NAs to their own explicit level
food_tidy$ingredients_from_palm_oil_n <- fct_explicit_na(food_tidy$ingredients_from_palm_oil_n,
                                                         na_level = "Unknown")

summary(food_tidy$ingredients_from_palm_oil_n)

# For a brief tutorial on some of the main functions of forcats see another of my R-bloggers posts
# https://www.r-bloggers.com/cats-are-great-and-so-is-the-forcats-r-package/

?forcats


# This is an impressively uninteresting dataset so we are limited in terms of what to explore
# However, let's try and answer the questions posed earlier:

#1) What food type categories are most prevalent in this dataset and which types are countries associated with?

# Plant-based foods and beverages, beverages and sugary snacks are the most prevalent food types
# An argument could be made for collapsing beverages into plant-based foods and beverages but I think there is 
# a distinction here between plant-based juicies/smoothies and other beverages (incl. alcohol) so best to leave it
food_tidy %>%
  group_by(main_category_en) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(25) %>%
  ggplot(aes(x = reorder(main_category_en, # reorder this categorical variable
                         count, # by this continuous measure
                         FUN = sum), # and this function of the continuous measure
             y = count)) +
  geom_col(fill = "forestgreen") +
  xlab(" ") +
  ylab("Frequency") +
  coord_flip() +
  theme(text = element_text(face = "bold",
                            family = "AvantGarde")) +
  ggtitle("Frequency of various food type categories",
          subtitle = "Source: https://www.datacamp.com")

# We know France dominates this dataset so we expect to see this in the data visualisation
# Obviously, a greater variety of food types were purchased in France as it dominates the dataset
# The main finding here is that France dominates the dataset so it would be more useful to subset the 
# entire dataset to include only France and then carry out a more granular analysis on national distributions
# of food purchases and stores, etc.
food_tidy %>%
  group_by(Country1,
           main_category_en) %>%
  summarise(Total = n()) %>%
  filter(Total > 5) %>%
  ggplot(aes(x = Country1,
             y = Total,
             fill = main_category_en)) +
  geom_col(position = "dodge",
           colour = "black") +
  xlab(" ") +
  ylab("Total recorded purchases") +
  theme(text = element_text(face = "bold",
                            family = "AvantGarde")) +
  ggtitle("Main food type category of purchases by country",
          subtitle = "Source: https://www.datacamp.com")

# The source database was started by Stéphane Gigandet so this is perhaps why French data are so ubiquitous in food.csv
# https://world.openfoodfacts.org/who-we-are

#2) What food types tend to be associated with ingredients from palm oil?
food_tidy %>%
  filter(ingredients_from_palm_oil_n == "Yes") %>%
  group_by(main_category_en) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(5) %>%
  ggplot(aes(x = reorder(main_category_en, 
                         count, 
                         FUN = sum), 
             y = count)) +
  geom_col(fill = "forestgreen") +
  xlab(" ") +
  ylab("Frequency") +
  coord_flip() +
  theme(text = element_text(face = "bold",
                            family = "AvantGarde")) +
  ggtitle("Food type categories most associated with palm oil",
          subtitle = "Source: https://www.datacamp.com")

# It is no surprise that sugary snacks are the food type most associated with ingredients from palm oil (think Nutella)
# Many less superior vegan foods contain palm oil so this is not a surprise either
# Breakfast cereals, desserts and sugary waffles make up the remaining top 5 food types associated with ingredients from palm oil

  







