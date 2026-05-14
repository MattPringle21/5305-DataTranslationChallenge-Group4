# Cleaning_GovExp.r

library(readxl)
library(stringr)
library(tidyverse)
library(tseries)

# BEA Table 1.1.6 provides real GDP and real government consumption expenditures and gross investment in chained 2017 dollars, which adjusts
# both series for inflation over time.
# Source https://apps.bea.gov/national/Release/XLS/Survey/Section1All_xls.xlsx

# Note run Cleaning_GDP.r first so we have the GDP_raw dataframe since it already has the data we need
year_row <- gdp_raw[8, ]
govExp_row  <- gdp_raw %>% filter(...1 == 22)

govExp_long <- data.frame(
  YEAR = as.numeric(unlist(year_row[4:ncol(year_row)])),
  GOVEXP  = as.numeric(gsub(",", "", unlist(govExp_row[4:ncol(govExp_row)])))
)

govExp_long <- govExp_long %>%
  filter(!is.na(YEAR), !is.na(GOVEXP)) %>%
  arrange(YEAR)

# Merge government data and GDP data 
gov_data <- gdp_long %>%
  inner_join(
    govExp_long,
    by = "YEAR"
  )

# Calculate Government Share of GDP as percentage
gov_data <- gov_data %>%
  mutate(
    GOV_SHARE = 100 * GOVEXP / GDP
  )

plot(
  gov_data$YEAR,
  gov_data$GOV_SHARE,
  type = "l",
  main = "Government Expenditure Share of GDP",
  xlab = "Year",
  ylab = "Percent of GDP"
)


adf.test(na.omit(gov_data$GOV_SHARE))
