######### DS 4002: Exploratory Data Analysis & Pre-processing for Data Analysis #########

## Setting Up
setwd('/Users/jules/Desktop/') # Note: change working directory to your own
dataset <- read.csv('ALC_DAT.csv',header=T)
head(dataset)

## Install Packages
library(ggplot2)
library(dplyr)

## Descriptive Statistics
N <- nrow(dataset) # 624 observations
colnames(dataset) # 62 variables


#### What is the range of time our data encompasses? ####
# Available Years: [2000, 2005, 2010, 2015, 2019] --> remove years w/out data
dataset <- dataset[, -c(38:41,43:46,48:51,53:55,57)]

## Remove other unneeded columns
dataset <- dataset[, -c(1:12,22:28)]
dataset <- dataset[, -c(23:24)]


#### Which geographic areas have the most alcohol consumption data over time? ####
## View Countries
country_count <- table(dataset$Geographic.Area.Name)
country_count

## View Geographic Regions
region_count <- table(dataset$Parent.Geographic.Area.Name)
region_count

# Regions w/ most alcohol consumption data:
## AFRICA: 156 observations total
    # Eastern Africa: 51
    # Northern Africa: 15
    # Southern Africa: 15
    # Western Africa 48
    # Middle Africa: 27
    
## EUROPE: 120 observations total
    # Eastern Europe: 30
    # Northern Europe: 30
    # Western Europe: 21
    # Southern Europe: 39

## ASIA: 165 observations total
    # Southern Asia excluding India: 48
    # Central Asia: 15
    # Eastern Asia: 15
    # South-Eastern Asia: 33
    # Southern Asia: 3
    # Western Asia: 51


#### Pre-processing Steps ####
# Goal: create new dataset for avg alcohol intake per the following continents: Europe, Africa, Asia

## 1. Avg Alcohol Consumption- Europe 
European_Cont <- c("Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe") # define regions
dataset_Europe <- dataset %>% # create dataset for European countries
  filter(Parent.Geographic.Area.Name %in% European_Cont)

# Obtain continent's avg alcohol consumption per each available year 
avg_Eur2000 <- mean(dataset_Europe$Value.2000)
avg_Eur2005 <- mean(dataset_Europe$Value.2005)
avg_Eur2010 <- mean(dataset_Europe$Value.2010)
avg_Eur2015 <- mean(dataset_Europe$Value.2015)
avg_Eur2019 <- mean(dataset_Europe$Value.2019) 

## 2. Avg Alcohol Consumption- Africa
African_cont <- c("Western Africa", "Eastern Africa", "Middle Africa", "Northern Africa", "Southern Africa") # define regions
dataset_Africa <- dataset %>% # create dataset for African countries
  filter(Parent.Geographic.Area.Name %in% African_cont)

# Obtain continent's avg alcohol consumption per each available year 
avg_Af2000 <- mean(dataset_Africa$Value.2000)
avg_Af2005 <- mean(dataset_Africa$Value.2005)
avg_Af2010 <- mean(dataset_Africa$Value.2010)
avg_Af2015 <- mean(dataset_Africa$Value.2015)
avg_Af2019 <- mean(dataset_Africa$Value.2019) 

## 3. Avg Alcohol Consumption- Asia
Asia_Cont <- c("Central Asia","Southern Asia", "South-Eastern Asia", "Southern Asia (excluding India)", "Eastern Asia", "Western Asia") # define regions
dataset_Asia <- dataset %>% # create dataset for Asian countries
  filter(Parent.Geographic.Area.Name %in% Asia_Cont)

# Obtain continent's avg alcohol consumption per each available year 
avg_As2000 <- mean(dataset_Asia$Value.2000)
avg_As2005 <- mean(dataset_Asia$Value.2005)
avg_As2010 <- mean(dataset_Asia$Value.2010)
avg_As2015 <- mean(dataset_Asia$Value.2015)
avg_As2019 <- mean(dataset_Asia$Value.2019) 

## 4. Assign Each Continent's Mean Values to a Year Variable
yr_2000 <- c(avg_Eur2000, avg_Af2000, avg_As2000)
yr_2005 <- c(avg_Eur2005, avg_Af2005, avg_As2005)
yr_2010 <- c(avg_Eur2010, avg_Af2010, avg_As2010)
yr_2015 <- c(avg_Eur2015, avg_Af2015, avg_As2015)
yr_2019 <- c(avg_Eur2019, avg_Af2019, avg_As2019)

## 5. Create new dataset containing avg alcohol intake for Europe, Africa, and Asia
continents <- c("Europe", "Africa", "Asia")
dat <- data.frame(
  Continent = continents, 
  "2000" = yr_2000, 
  "2005" = yr_2005, 
  "2010" = yr_2010, 
  "2015" = yr_2015, 
  "2019" = yr_2019)

# 6. Output dataset and upload to github for analysis
write.csv(dat, "/Users/jules/Desktop/aggregated_alc_dat.csv", row.names = FALSE) 
# Note: change working directory to your own

#### How Have Consumption Patterns Changed Over Time? ####

## Alcohol intake by year
# a.) Restructure aggregated dataset to transform from wide to long 
years <- c("2000", "2000", "2000", # create year variable 
           "2005", "2005", "2005",
           "2010", "2010", "2010",
           "2015", "2015", "2015",
           "2019", "2019", "2019") 

cont_long <- c("Europe", "Africa", "Asia", # create continent variable (long format)
               "Europe", "Africa", "Asia",
               "Europe", "Africa", "Asia",
               "Europe", "Africa", "Asia",
               "Europe", "Africa", "Asia")

alc_total <- c(avg_Eur2000, avg_Af2000, avg_As2000, # create alc consumption variable
               avg_Eur2005, avg_Af2005, avg_As2005, 
               avg_Eur2010, avg_Af2010, avg_As2010,
               avg_Eur2015, avg_Af2015, avg_As2015,
               avg_Eur2019, avg_Af2019, avg_As2019)

# b.) create long data set
long_dat <- data.frame(
  "Year" = years,
  "Continent" = cont_long, #order: Europe, Africa, Asia
  "Alc" = alc_total # represents avg alcohol intake
  ) 

# c.) create visualizations
# bar plot
ggplot(long_dat, aes(x = Year, y = Alc, fill = Continent)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +  
  scale_fill_manual(values = c("Africa" = "#D81B60", "Asia" = "#FFC107", "Europe" = "#1E88E5")) + 
  labs(title = "Average Alcohol Consumption by Continent From 2000-2019",
       x = "Year", y = "Average Alcohol Consumption (L)") +
  theme_minimal() 

# line plot
ggplot(long_dat, aes(x = Year, y = Alc, linetype = Continent, color = Continent, group = Continent)) +
  geom_line(size = .9) +  
  geom_point(size = 3) +
  labs(title = "Alcohol Consumption Trend by Continent From 2000-2019", 
       x = "Year", y = "Alcohol Consumption (L)") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 20, by = 2), limits = c(0,12)) +
  scale_color_manual(values = c("Africa" = "#D81B60", "Asia" = "#FFC107", "Europe" = "#1E88E5")) 


# d.) Output long dataset and upload to github
write.csv(long_dat, "/Users/jules/Desktop/long_dat.csv", row.names = FALSE)
# Note: change working directory to your own

