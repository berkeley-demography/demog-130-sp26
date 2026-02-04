
## Only need to run installation code once
install.packages("HMDHFDplus")
install.packages("tidyverse")
install.packages("data.table")
install.packages("dplyr")
install.packages("scico")
install.packages("purrr")
install.packages("slider")
install.packages("viridis")



# Remove items in environment
rm(list = ls())


setwd("[set a path name here if you need it]")

## Call the packages you'll need

library(tidyverse)
library(data.table)
library(HMDHFDplus)
library(dplyr)
library(purrr)
library(slider)
library(viridis)

# Used fixed and not scientific notation in output
options(scipen = 999) 

usa_m <- readHMDweb("USA", item = "Mx_1x1", username="insertyouremail", password="insertyourpassword", fixup=TRUE)


# Write CSV files for students
usa_m <- read.csv("/Users/jnobles/Dropbox/Teaching/Berkeley/130/Labs/usa_m.csv", header = TRUE)


# Rename variables names
  usa_m$Death_Rate_Total <- usa_m$Total
  usa_m$Death_Rate_Female <- usa_m$Female
  usa_m$Death_Rate_Male <- usa_m$Male
  
# Drop the Open Interval variable
  usa_m <- select(usa_m, -OpenInterval)

# log the death rates: ln(nMx)
  usa_m$LogDeath_Rate_Total <- log(usa_m$Death_Rate_Total)


# Keep years 1933, 1950, 1965, 1980, 1995, 2005, 2019  
  
  usa_selectyears <- usa_m %>% 
    filter(Year==1933 | Year==1950 | Year==1965 | Year==1980 | Year==1995 | Year==2005 | Year==2019)
  
  # Keep ages less than 100 
  
  usa_select_lt100 <- usa_selectyears %>% 
    filter(Age<100)
  
  
 # Plot mortality rates over age by year 
  
  ggplot(usa_select_lt100, aes(x = Age, y = Death_Rate_Total, color = Year, group = Year)) +
    geom_line() +
    labs(
      title = "U.S. Mortality Rates by Age and Year",
      x = "Age",
      y = "Mx",
      color = "Year" # Customize the legend title
    ) +
    scale_color_continuous(       # default ggplot continuous gradient
      limits = c(1933, 2019),
      breaks = c(1933, 2019),     # only endpoints in legend
      labels = c("1933", "2019")
    ) +
    theme_minimal() 

  
  ggsave("Lab2_mort_us.pdf", width = 6, height = 4, units = "in")
  
  
  # Plot logged mortality rates over age by year 
  
  ggplot(usa_select_lt100, aes(x = Age, y = LogDeath_Rate_Total, color = Year, group = Year)) +
    geom_line() +
    labs(
      title = "U.S. Mortality Rates by Age and Year",
      x = "Age",
      y = "log(Mx)",
      color = "Year" # Customize the legend title
    ) +
    scale_color_continuous(       # default ggplot continuous gradient
      limits = c(1933, 2019),
      breaks = c(1933, 2019),     # only endpoints in legend
      labels = c("1933", "2019")
    ) +
    theme_minimal() 
  
  
  ggsave("Lab2_logmort_us.pdf", width = 6, height = 4, units = "in")
  
   
########################################
 # Question 2 Lifetables
########################################
  
  
  # Let's get the nax values from the HMD so that our lifetable estimates are more precise 
  
  usa_hmdlt <- readHMDweb("USA", item = "bltper_1x1", username="insertyouremail", password="insertyourpassword", fixup=TRUE)
  
  # Generate a new data frame that is one vector, nax
  usa_nax <- select(usa_hmdlt, Year, Age, ax)
  # For legibility below, rename ax nax
  usa_nax$nax <- usa_nax$ax
  
  
  # Keep years 1933, 1950, 1965, 1980, 1995, 2005, 2019 for the nax vector 
  
  usa_selectnax <- usa_nax %>% 
    filter(Year==1933 | Year==1950 | Year==1965 | Year==1980 | Year==1995 | Year==2005 | Year==2019)
  
  # for ease of interpretation, let's generate a new variable called mx that is the mortality rates
  
  usa_selectyears$nmx <- usa_selectyears$Death_Rate_Total
  
  # Merge the nax vector onto the death rates and create a new data frame for this question called usa_lt
  
  usa_lt <- usa_selectyears %>%
    left_join(usa_selectnax, by = c("Year", "Age")) 
  
  # Now let's add a column that calculates interval length. 
  # Note that I'm going to call it nx to not get it confused with internal R code n
  
  usa_lt <- usa_lt %>%  
    mutate(
      nx = Age - lag(Age), 
      nx = case_when(
          Age == 0 ~ 1,      # If Age is 100, make n = 1
          TRUE ~ nx             # Otherwise (TRUE), keep the original score value
        )
      )
      
  # Now let's calculate death probabilities from rates
  
  usa_lt <- usa_lt %>%  
    mutate(
      nqx = (nx*nmx) / (1+((nx-nax)*nmx))
    )
  
  
  
  # Now lets get a the lx and dx vector. Doing this in base R because it's more legible than using tidyverse.
  # Set the radix. Because we'll want the lx column to be survival probabilities, we'll scale everything to a radix of 1.0
  lx0 <- 1.0
  
  # Initialize lx and ndx 
  usa_lt <- usa_lt %>%
    group_by(Year) %>%
    mutate(
      lx = NA_real_,
      ndx = NA_real_
    )
  
  # Recursive calculation per Year
  for(yr in unique(usa_lt$Year)) {
    rows <- which(usa_lt$Year == yr)
    lx_vec <- numeric(length(rows))
    ndx_vec <- numeric(length(rows))
    
    # First row
    lx_vec[1] <- lx0
    ndx_vec[1] <- lx_vec[1] * usa_lt$nqx[rows[1]]
    
    # Remaining rows
    if(length(rows) > 1){
      for(i in 2:length(rows)) {
        lx_vec[i] <- lx_vec[i-1] - lx_vec[i-1] * usa_lt$nqx[rows[i-1]]
        ndx_vec[i] <- lx_vec[i] * usa_lt$nqx[rows[i]]
      }
    }
    
    # Assign back to data frame
    usa_lt$lx[rows] <- lx_vec
    usa_lt$ndx[rows] <- ndx_vec
  }
  
  
  # Now lets get Lx. Let's get the count of survivors in the same row and call it survivors. 
  
  usa_lt <- usa_lt %>%  
    mutate(
      survivors = lx-ndx
    )
  
  # Lx is the sum of (survivors x interval width) + (deaths x PY lived by people dying in the interval)
  usa_lt <- usa_lt %>%  
    mutate(
      Lx = (survivors * nx) + (ndx * nax)
    )
  
  
  
  # Now let's calculate Tx. One straightforward way to do this is to create a column that indicates how many rows in the Lx column we need to sum over. 
  
  usa_lt$nrowsLx <- 110 - usa_lt$Age  # e.g., at age 100, the Tx value sums over 10 rows of Lx
  
  
  # This is what we want: Tx[i] = sum(Lx[i:(i + nrowsLx[i] - 1)])  Here's the tidyverse version: 
  
  usa_lt <- usa_lt %>%
    group_by(Year) %>%
    mutate(
      row_id = row_number(),
      Tx = map2_dbl(row_id, nrowsLx, ~ {
        start <- .x
        end   <- min(.x + .y - 1, n())  # don’t go past group length
        sum(Lx[start:end])
      })
    ) %>%
    select(-row_id) %>%
    ungroup()
  
  # Calculate Life Expectancy
  
  usa_lt <- usa_lt %>%  
    mutate(
      ex = Tx / lx
    )
  
  
  # Plot survival probabilities by age and year 
  
  ggplot(usa_lt, aes(x = Age, y = lx, color = Year, group = Year)) +
    geom_line(linewidth = 0.5) +
    labs(
      title = "U.S. Survival Probabilities by Age and Year",
      x = "Age",
      y = "lx",
      color = "Year"
    ) +
    scale_color_continuous(       # default ggplot continuous gradient
      limits = c(1933, 2019),
      breaks = c(1933, 2019),     # only endpoints in legend
      labels = c("1933", "2019")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14),
      axis.title = element_text(size = 11),
      legend.title = element_text(size = 11)
    )
  
  
# Save the last plot 
  ggsave("Lab2_survival_us.pdf", width = 6, height = 4, units = "in")


  

