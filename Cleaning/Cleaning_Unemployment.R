# Cleaning_Unemployment.r

library(readxl)
library(tidyverse)
library(tseries)


# Unemployment Labor Force Statistics from BLS (Seasonally adjusted)
# Source: https://data.bls.gov/dataViewer/view/timeseries/LNS14000000

# Import unemployment data
unemp_raw <- read_excel(
  "Unemployment_BLS.xlsx",
  skip = 17
)

# Rename columns
names(unemp_raw) <- c(
  "YEAR",
  "PERIOD",
  "LABEL",
  "UNEMPRATE"
)

unemp_data <- unemp_raw %>%
  select(YEAR, PERIOD, UNEMPRATE) %>%
  mutate(
    YEAR = as.numeric(YEAR),
    unemp = as.numeric(UNEMPRATE)
  )

# Create annual average unemployment rate
unemp_annual <- unemp_data %>%
  group_by(YEAR) %>%
  summarize(
    unemp = mean(unemp, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(YEAR)

View(unemp_annual)

# Plot annual unemployment
plot(
  unemp_annual$YEAR,
  unemp_annual$unemp,
  type = "l",
  main = "Annual US Unemployment Rate",
  xlab = "Year",
  ylab = "Unemployment Rate (%)"
)

# Stationarity test
adf.test(na.omit(unemp_annual$unemp))

##############################################
##### Male Unemployment ##############

# Source: BLS Labor Force Statistics from the Current Population Survey

male_unemp_raw <- read_excel(
  "Unemployment_Male_BLS.xlsx",
  skip = 12
)

# Rename columns
names(male_unemp_raw) <- c(
  "YEAR",
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

# Convert wide monthly data to long format
male_unemp_long <- male_unemp_raw %>%
  pivot_longer(
    cols = Jan:Dec,
    names_to = "MONTH",
    values_to = "MALE_UNEMPRATE"
  ) %>%
  mutate(
    YEAR = as.numeric(YEAR),
    m_unemp = as.numeric(MALE_UNEMPRATE)
  )

# Create annual average male unemployment rate
male_unemp_annual <- male_unemp_long %>%
  group_by(YEAR) %>%
  summarize(
    m_unemp = mean(m_unemp, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(YEAR)

View(male_unemp_annual)

# Plot annual male unemployment
plot(
  male_unemp_annual$YEAR,
  male_unemp_annual$m_unemp,
  type = "l",
  main = "Annual Male Unemployment Rate",
  xlab = "Year",
  ylab = "Male Unemployment Rate (%)"
)

# Stationarity test
adf.test(na.omit(male_unemp_annual$m_unemp))

