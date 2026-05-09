
# Set working directory 
setwd("C:/Users/alexm/REPOS/CRANE/VaccinationHesitancy/analysis")

# Load file  "Immunization_K_Idaho" from excel file 
# subset extracted from CDC "Vaccination_Coverage_and_Exemptions_among_Kindergartners_20260201"
data_K <- 
  read.csv("../data/Immunizations_K_Idaho.csv",header=TRUE)

# read bibliography
library(RefManageR)
# refs <- readBib("ref.bib")

# remove the columns geography_type, footnotes & survey_type

cleaned_data_K <- 
  data_K[, !names(data_K) %in% 
                   c("geography_type","footnotes","survey_type")]

library(ggplot2)

data_polio <- subset(cleaned_data_K, vaccine_exemption == "Polio")

ggplot(data_polio,
       aes( x = year, y = population_size )) +
  geom_col(fill = "lightblue") + 
  labs( caption = "Figure 1. Population size surveyed for Polio immunization/exemption",
        x = "Year", 
        y = "Population size") +
  scale_x_continuous(breaks = c(2009, 2014, 2019, 2024), 
                     labels = c("2009", "2014", "2019", "2024")) 

# find years of study and mean population size over the years of study

min.year <- min(cleaned_data_K$school_year)
max.year <- max(cleaned_data_K$school_year)
mean.K.pop <- round(mean(data_polio$population_size),0)
print(mean.K.pop)


# subset the data for exemption data only
data_exemptions <- subset(cleaned_data_K, vaccine_exemption == "Exemption")

library(dplyr)

table_exemptions <- data_exemptions %>%
  group_by(school_year) %>%
  summarise(exemption = sum(number_of_exemptions, na.rm = TRUE))

# make a nice table for the data
library(knitr)

kable(table_exemptions,
      col.names = c("School Year", "Total Exemptions"),
      caption = "Total Exemptions by School Year")



# plot the data after installing the ggplot2 package
library(ggplot2)

Idaho_plot_exempt <- ggplot(data_exemptions,
       aes(year,estimate, color=dose)) +
  geom_point(size=3, alpha=0.5) + 
  theme(plot.title = element_text(hjust=0.5, size=10)) + 
  theme(plot.subtitle = element_text(hjust=0.5, size=10)) +
  labs( caption = "Figure 2. Vaccination exemptions rates for kindergartners",
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
  plot = Idaho_plot_exempt,
  width = 5.25,
  height = 3, 
  dpi =300
)


# find min and max of exemption rate
min_exemption <- min(subset(data_exemptions, dose == "Any Exemption")$estimate, na.rm = TRUE)
max_exemption <- max(subset(data_exemptions, dose == "Any Exemption")$estimate, na.rm = TRUE)

min_year <- (data_exemptions$year[which.min(subset(data_exemptions, dose == "Any Exemption")$estimate)])
max_year <- (data_exemptions$year[which.max(subset(data_exemptions, dose == "Any Exemption")$estimate)])



library(ggplot2)

data_vaccines <- subset(cleaned_data_K, vaccine_exemption != "Exemption")

Idaho_plot_vacc <- ggplot(data_vaccines,
       aes(year,estimate, color=vaccine_exemption)) +
  geom_point(size=3, alpha=0.5) + 
  theme(plot.title = element_text(hjust=0.5, size=10)) + 
  theme(plot.subtitle = element_text(hjust=0.5, size=10)) +
  labs( caption = "Figure 3. Vaccination rates for kindergartners",
        subtitle = "(CDC data for Idaho State)",
        x = "Year", 
        y = "Vaccination  Rates (%) ",
        color = "Type of Vaccine") +
  scale_x_continuous(breaks = c(2009, 2014, 2019, 2024), 
                     labels = c("2009", "2014", "2019", "2024")) + 
  geom_line()

# save plot to a output file
ggsave(
  filename = "../output/Immunization_Vaccines_Idaho.png",
  plot = Idaho_plot_vacc,
  width = 5.25,
  height = 3, 
  dpi =300
)

# loading the data from https://hdpulse.nimhd.nih.gov and Idaho Board of Election

data_poverty <- read.csv("../data/IdahoCounties_Poverty.csv", skip = 4)
data_education <- read.csv("../data/IdahoCounties_Education.csv", skip = 5)
data_medianincome <- read.csv("../data/IdahoCounties_MedianIncome.csv", skip = 4)
data_languageisolation <- read.csv("../data/IdahoCounties_LanguageIsolation.csv", skip = 4)
data_population <- read.csv("../data/IdahoCounties_Population.csv")
data_voters <- read.csv("../data/2025IdahoVoters.csv", skip = 1)

# change class of Voter Counts to Integer
data_voters$CON <- as.integer(
  gsub(",", "", data_voters$CON))
data_voters$DEM <- as.integer(
  gsub(",", "", data_voters$DEM))
data_voters$LIB <- as.integer(
  gsub(",", "", data_voters$LIB))
data_voters$REP <- as.integer(
  gsub(",", "", data_voters$REP))
data_voters$UNA <- as.integer(
  gsub(",", "", data_voters$UNA))
data_voters$Total <- as.integer(
  gsub(",", "", data_voters$Total))



# sum voter registration for fringe parties
data_voters$FRINGE.PERCENT <- data_voters$CON.PERCENT + data_voters$LIB.PERCENT


library(ggplot2)
ggplot(data_voters, aes(x = FRINGE.PERCENT)) +
  geom_histogram(bins = 10,
                 fill = "lightblue",
                 color = "black") +
  labs(caption = "Figure 4. Distribution of counties with fringe voters", 
       x = "% Registered as Fringe Voters",
       y = "number of counties")

# county with Max Fringe Voters - which county has the highest value?
max(data_voters$FRINGE.PERCENT)
data_voters$COUNTY[which.max(data_voters$FRINGE.PERCENT)]
data_voters$FIPS[which.max(data_voters$FRINGE.PERCENT)]

library(dplyr)

merged_data <- data_poverty %>%
  full_join(data_education, by = "FIPS") %>%
  full_join(data_medianincome, by = "FIPS") %>%
  full_join(data_languageisolation, by = "FIPS") %>%
  full_join(data_population, by = "FIPS") %>%
  full_join(data_voters, by = "FIPS")
  
# rename columns
colnames(merged_data)[1] <- "county"
colnames(merged_data)[3] <- "below.poverty.percent"
colnames(merged_data)[7] <- "below.education.percent"
colnames(merged_data)[11] <- "median.income"
colnames(merged_data)[14] <- "language.isolation.percent"
colnames(merged_data)[19] <- "population.age.18.39"

head(merged_data$median.income)

# change class of median income from character to integer
merged_data$median.income <- as.integer(
  gsub(",", "", merged_data$median.income)
)

# create a categorical variable POP
merged_data$POP <- ifelse(merged_data$population.age.18.39 < 10000,
                                 "LOW",
                                 "HIGH")


# check that both distributions are normal

shapiro.test(subset(merged_data, POP == "LOW")$median.income)
shapiro.test(subset(merged_data, POP == "HIGH")$median.income)

# calculate Statistical values (mean, median, sd)- verify that sd are similar for both
library(dplyr)
merged_data %>%
  group_by(POP) %>%
  summarise(
    Mean = mean(median.income, na.rm = TRUE),
    Median = median(median.income, na.rm = TRUE),
    SD = sd(median.income, na.rm = TRUE)
  )
# run t-test
tt <- t.test(merged_data$median.income[merged_data$POP == "LOW"],
       merged_data$median.income[merged_data$POP == "HIGH"])

# display a table with the t-test result
# clean table display

library(knitr)
library(broom)

kable(tidy(tt), digits = 3,
      caption = "Independent Samples t-test Results")


# plotting median income for Idaho with 8 bins of income per county population density

library(ggplot2)

Idaho_plot_income <- ggplot(merged_data, aes(x = median.income, fill = POP)) +
  geom_histogram(bins = 8, alpha = 0.5, position = "dodge") +
  labs(caption = "Figure 5. Distribution of county median income",
       x = "Median Income",
       y = "Count",
       fill = "County Population")


# save plot to a output file
ggsave(
  filename = "../output/Hist_median_income_Idaho.png",
  plot = Idaho_plot_income,
  width = 5.25,
  height = 3, 
  dpi =300
)


# calculate correlation between Median Income and % below poverty. 
cor.test(merged_data$median.income, merged_data$below.poverty.percent)


library(ggplot2)
library(Hmisc)
library(dplyr)

# find min and max Poverty levels and which county has the highest poverty level
min(merged_data$below.poverty.percent)
max(merged_data$below.poverty.percent)
merged_data$COUNTY[which.max(merged_data$below.poverty.percent)]

# find the quartiles of the poverty levels observed
quantile(merged_data$below.poverty.percent)

# bins the below.poverty.percent variable in quartiles
merged_data <- merged_data %>%
  mutate(poverty_quartile = ntile(below.poverty.percent, 4))

merged_data$poverty_quartile <- factor(
  merged_data$poverty_quartile,
  levels = c(1, 2, 3, 4),
  labels = c("Lowest poverty",
             "Low-middle",
             "High-middle",
             "Highest poverty")
)

# plot county median income vs the 4 quartiles bins of percentage below poverty
barplot<- ggplot(merged_data, aes(x = poverty_quartile, y = median.income)) +
  stat_summary(fun = mean, geom = "bar", fill = "lightblue") +

 # SD error bars
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1),
               geom = "errorbar",
               width = 0.2) +
  
  # Scatter points (jittered)
  geom_jitter(width = 0.15,
              alpha = 0.5) +

   labs(y = "Averaged county median income",
       x = "Below poverty level quartiles", 
       caption= "Averaged median county income by poverty quartile")

# save plot to a output file
ggsave(
  filename = "../output/Averaged_median_income.vs.Below_poverty_quartiles.png",
  plot = barplot,
  width = 5.25,
  height = 3, 
  dpi =300
)

