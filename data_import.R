library(readr)
library(ggplot2)
library(dplyr)

vaccinations <- read_delim("data/vaccinations.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)
vaccinations_sample <- vaccinations[sample(nrow(vaccinations), 100000), ]

covid_hospitalizations <- read_delim("data/covid-hospitalizations.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)
covid_hospitalizations_sample <- hospitalizations[sample(nrow(covid_hospitalizations), 100000), ]

vaccinations$date <- as.Date(vaccinations$date)
covid_hospitalizations$date <- as.Date(covid_hospitalizations$date)

vaccinations_filtered <- vaccinations %>%
  filter(date < as.Date("2023-01-01"))

covid_hospitalizations_filtered <- covid_hospitalizations %>%
  filter(date < as.Date("2023-01-01"))
