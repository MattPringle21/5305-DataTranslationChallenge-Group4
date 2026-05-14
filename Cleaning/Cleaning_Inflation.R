# Cleaning_Inflation.r

library(readxl)
library(tidyverse)
library(tseries)


# Consumer Price Index for All Urban Consumers from BLS
# aggregates monthly CPI data to annual CPI. Then calculate inflation = 100*ln(CPI)
# https://data.bls.gov/dataViewer/view/timeseries/CUUR0000SA0
cpi_raw <- read_excel(
  "CPI_BLSxlsx.xlsx",
  skip = 10
)

# Rename columns
names(cpi_raw) <- c(
  "YEAR",
  "PERIOD",
  "LABEL",
  "CPI",
  "NET_CHANGE"
)

cpi_data <- cpi_raw %>%
  select(YEAR, PERIOD, CPI) %>%
  mutate(
    YEAR = as.numeric(YEAR),
    CPI = as.numeric(CPI)
  )

# Annualize and calculate inflation
cpi_annual <- cpi_data %>%
  group_by(YEAR) %>%
  summarize(CPI = mean(CPI, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(YEAR) %>%
  mutate(
    infl = 100 * c(NA, diff(log(CPI)))
  )

plot(
  cpi_annual$YEAR,
  cpi_annual$infl,
  type = "l",
  lwd = 2,
  main = "Calculated US Inflation Rate",
  xlab = "Year",
  ylab = "Inflation (%)"
)

abline(h = 0, lty = 2)


adf.test(na.omit(cpi_annual$infl))




