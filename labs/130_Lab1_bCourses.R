
## Only need to run installation code once
install.packages("HMDHFDplus", repos = "https://cloud.r-project.org")
install.packages("TidyVerse")
install.packages("data.table")
install.packages("dplyr")
install.packages("scico")


# Remove items in environment
rm(list = ls())


## Call the packages you'll need

library(tidyverse)
library(data.table)
library(HMDHFDplus)
library(dplyr)
library(scico)

# Used fixed and not scientific notation in output
options(scipen = 999) 


# Get 1 x 1 Death rates
usa_m <- readHMDweb("USA", item = "Mx_1x1", username="insertyouremail", password="insertyourpassword", fixup=TRUE)

# Rename variable names
  usa_m$Death_Rate_Total <- usa_m$Total
  usa_m$Death_Rate_Female <- usa_m$Female
  usa_m$Death_Rate_Male <- usa_m$Male
  
# Drop the Open Interval variable
  usa_m <- select(usa_m, -OpenInterval)


# Get 1 x 1 Death counts
  
  
  # Rename variable names
  
  
  # Drop the Open Interval variable
  

# Get 1 x 1 Person Year counts
  
  
  # Rename variable names
  
  
  # Drop the Open Interval variable 
  
  
  

# Merge the Death Rates, Death Counts, and Person-Year Counts
  
usa <- usa_m %>%
  left_join(usa_d, by = c("Year", "Age")) %>%
  left_join(usa_n, by = c("Year", "Age"))


# Generate the death rates yourself from Preston pp 49. Box 3.1, Section B Step 1. 



# Use the "summary" (summarizes variables) and "cor" (correlates two variables) to check that your estimate matches the HMD estimates. 
# Note:the death rate values you've calculated should have highly similar summary statistics with the HMD values. They should have a correlation of approximately 1.0






# Let's take a look at how skewed the death rate distribution is. 

summary(usa$Death_Rate_Total)

ggplot(usa, aes(x = Death_Rate_Total)) +
  geom_histogram()

# Given the skew, let's visualize the logged death rates: ln(nMx)
usa$LogDeath_Rate_Total <- log(usa$Death_Rate_Total)

ggplot(usa, aes(x = LogDeath_Rate_Total)) +
  geom_histogram()


# Let's plot ages 0-100. Truncate the top 10 years. 

usa_truncated <- usa %>% 
  filter(Age<=100)


# Choropleth, or Lexis Surface plot

ggplot(usa_truncated, aes(x = Year, y = Age, fill = LogDeath_Rate_Total)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma") +
  labs(title = "Mortality Rates (logged) by Age and Year, United States",
       x = "Calendar Year",
       y = "Age", 
       fill = "ln(mortality rate)") +
  scale_x_continuous(breaks = seq(1935, 2025, by = 10))
theme_minimal()

# Save the last plot 
ggsave("mort_US_1.pdf", width = 6, height = 4, units = "in")


# Let's see if different scale_fill option makes some of the patterns even more clear.

ggplot(usa_truncated, aes(x = Year, y = Age, fill = LogDeath_Rate_Total)) +
  geom_tile() +
  scico::scale_fill_scico(palette = "vik") +
  labs(title = "Mortality Rates (logged) by Age and Year, United States",
       x = "Calendar Year",
       y = "Age", 
       fill = "ln(mortality rate)") +
  scale_x_continuous(breaks = seq(1935, 2025, by = 10))
  theme_minimal()

# Save the last plot 
  ggsave("mort_US_2.pdf", width = 6, height = 4, units = "in")




