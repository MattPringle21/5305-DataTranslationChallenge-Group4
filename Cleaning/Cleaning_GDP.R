# Cleaning_GDP.r
library(readxl)
library(stringr)
library(tidyverse)
library(tseries)

# BEA (1929- 2025)  Table 1.1.6 provides real GDP in chained 2017 dollars, which adjusts for inflation over time
# Source https://apps.bea.gov/national/Release/XLS/Survey/Section1All_xls.xlsx
gdp_raw <- read_excel(
  "Section1All_xls.xlsx",
  sheet = "T10106-A",
  col_names = FALSE
)

year_row <- gdp_raw[8, ]
gdp_row  <- gdp_raw %>% filter(...1 == 1)

gdp_long <- data.frame(
  YEAR = as.numeric(unlist(year_row[4:ncol(year_row)])),
  gdp  = as.numeric(gsub(",", "", unlist(gdp_row[4:ncol(gdp_row)])))
)

gdp_long <- gdp_long %>%
  filter(!is.na(YEAR), !is.na(gdp)) %>%
  arrange(YEAR)

gdp_long <- gdp_long %>%
  mutate(
    Lgdp = log(gdp),
    DLgdp = c(NA, diff(Lgdp))
  )

plot(
  gdp_long$YEAR,
  gdp_long$gdp,
  type = "l",
  main = "Real GDP",
  xlab = "Year",
  ylab = "Millions of Chained 2017 Dollars"
)

plot(
  gdp_long$YEAR,
  gdp_long$DLgdp,
  type = "l",
  main = "Differenced Log Real GDP",
  xlab = "Year",
  ylab = "Differenced Log GDP"
)

######### Government Expenditure as a share of GDP #############
# Same source as GDP
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
gdp_data <- gdp_long %>%
  inner_join(
    govExp_long,
    by = "YEAR"
  )

# Calculate Government Share of GDP as percentage
gdp_data <- gov_data %>%
  mutate(
    gov_gdp = 100 * GOVEXP / gdp
  )

plot(
  gdp_data$YEAR,
  gdp_data$gov_gdp,
  type = "l",
  main = "Government Expenditure Share of GDP",
  xlab = "Year",
  ylab = "Percent of GDP"
)


adf.test(na.omit(gdp_data$gov_gdp))

gdp_data <- gdp_data %>%
  mutate(
    D_gov_gdp = c(NA, diff(gov_gdp))
  )


plot(
  gdp_data$YEAR,
  gdp_data$D_gov_gdp,
  type = "l",
  main = "Differenced Government Expenditure Share of GDP",
  xlab = "Year",
  ylab = "Differenced Percent of GDP"
)

adf.test(na.omit(gdp_data$D_gov_gdp))


######### Services as a Share of GDP #############
# Source: BEA (1929- 2025) Table 1.1.5. Gross Domestic Product (Millions of Dollars)
serv_raw <- read_excel(
  "Section1All_xls.xlsx",
  sheet = "T10105-A",
  col_names = FALSE
)

year_row <- serv_raw[8, ]
gdp_row <- serv_raw %>% filter(...1 == 1)
serv_row  <- serv_raw %>% filter(...1 == 6)

serv_long <- data.frame(
  YEAR = as.numeric(unlist(year_row[4:ncol(year_row)])),
  GDP  = as.numeric(gsub(",", "", unlist(gdp_row[4:ncol(gdp_row)]))),
  SERVICES  = as.numeric(gsub(",", "", unlist(serv_row[4:ncol(serv_row)])))
)

serv_long <- serv_long %>%
  filter(!is.na(YEAR), !is.na(GDP), !is.na(SERVICES)) %>%
  arrange(YEAR)

serv_long <- serv_long %>% 
  mutate(
    serve_gdp = 100 * SERVICES / GDP,
    D_serve_gdp = c(NA, diff(serve_gdp))
  )

plot(
  serv_long$YEAR,
  serv_long$serve_gdp,
  type = "l",
  main = "Services Share of GDP",
  xlab = "Year",
  ylab = "Percent of GDP"
)

plot(
  serv_long$YEAR,
  serv_long$D_serve_gdp,
  type = "l",
  main = "Differenced Services Share of GDP",
  xlab = "Year",
  ylab = "Differenced Percent of GDP"
)

adf.test(na.omit(serv_long$serve_gdp))
adf.test(na.omit(serv_long$D_serve_gdp))