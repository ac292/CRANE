
# Loading Data

# Set working directory 
# C:\Users\alexm\OneDrive\Documents\CLASSES\VTPEH6270\Immunizations _K

setwd("C:/Users/alexm/REPOS/CRANE/VaccinationHesitancy/analysis")

# Load file in df "data_K" from excel file 
# downloaded from CDC "Vaccination_Coverage_and_Exemptions_among_Kindergartners_20260201"

data_K <- 
  read.csv("../data/Vaccination_Coverage_and_Exemptions_among_Kindergartners_20260201.csv",header=TRUE)

# The data set is not labeled using best practices (e.g., uses unusual characters like "..."). We clean the  formatting using the `janitor` package.

# install.packages("janitor")

library(janitor)
cleaned_data_K <- clean_names(data_K)

# Preview the cleaned data
names(cleaned_data_K)

# count the number of rows
nobs_data_K <- nrow(cleaned_data_K)

# what are the unique values in some of the character columns
unique(cleaned_data_K$geography_type)
unique(cleaned_data_K$survey_type)

# remove the columns geography_type, footnotes & survey_type

cleaned_data_K <- 
  cleaned_data_K[, !names(cleaned_data_K) %in% 
                   c("geography_type","footnotes","survey_type")]

# remove the data for cities and whole US

library(dplyr)
library(stringr)
cleaned_data_K <- cleaned_data_K %>%
  filter(
    !str_starts(geography, regex("TX-|NY-", ignore_case = TRUE)) &
      !str_detect(geography, regex("United States|U\\.S\\. Median", ignore_case = TRUE))
  )

# Create a new numerical column "year" to describe the 20XX-XX+1 school year as 20XX. 

# substract "_XX" from cleaned_data_K$school_year start at digit 1 end at digit 4
cleaned_data_K$year <- as.numeric(substr(cleaned_data_K$school_year, 1, 4))


# filter out data when non numerical

library(tidyverse)
data_numerical_K <- filter(cleaned_data_K, estimate !="Nreq") 
data_numerical_K <- filter(data_numerical_K, estimate !="NReq") 
data_numerical_K <- filter(data_numerical_K, estimate !="NR") 

# Class conversion
# convert estimate column as  as numeric
# installing the package dplyr to use the mutate function

library(dplyr)
data_numerical_K <- data_numerical_K %>%
  mutate(estimate = as.numeric(estimate))



# Subset the data to Idaho, that has the highest % of exemptions

library(tidyverse)
data_exemption_Idaho <- filter(data_numerical_K, vaccine_exemption == "Exemption" & 
                               geography =="Idaho") 


# plot the data for Idaho

library(ggplot2)
Idaho_plot <- ggplot(data_exemption_Idaho, 
                     aes(year,estimate, color=dose)) +
  geom_point(size=3, alpha=0.5) + 
  theme(plot.title = element_text(hjust=0.5, size=10)) + 
  theme(plot.subtitle = element_text(hjust=0.5, size=10)) +
  labs( title = "Vaccination Exemptions rates for Kindergartners",
        subtitle = "(CDC data for Idaho State)",
        x = "Year", 
        y = "Vaccination Exemption Rates (%) ",
        color = "Type of Exemption") +
  scale_x_continuous(breaks = c(2009, 2014, 2019, 2024), 
                     labels = c("2009", "2014", "2019", "2024")) + 
  geom_line()

# save plot to a output file
ggsave(
  filename = "../output/Immunization_Exemptions_Idaho.png",
  plot = Idaho_plot,
  width = 5.25,
  height = 3, 
  dpi =300
)

# find minimum value of estimate for non-medical exemptions for Idaho

data_exemption_Idaho_nonmed <- filter(data_exemption_Idaho, 
                                      dose =="Non-Medical Exemption") 
print(min(data_exemption_Idaho_nonmed$estimate))

# find maximum value of estimate for non-medical exemptions for Idaho

print(max(data_exemption_Idaho_nonmed$estimate))

# find minimum value of estimate for medical exemptions for Idaho

data_exemption_Idaho_med <- filter(data_exemption_Idaho, 
                                   dose =="Medical Exemption") 

# write a csv file with only Idaho data

data_Idaho <- subset(data_numerical_K, geography == "Idaho")
write.csv(data_Idaho, "../data/Immunizations_K_Idaho.csv", row.names=F)


 