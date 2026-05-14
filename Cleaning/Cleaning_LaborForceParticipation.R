# Cleaning_LaborForceParticipation.R

library(readxl)
library(stringr)
library(tidyverse)
library(tseries)

# Labor Force Participation from BLS 
# Source: BLS (1948-2026) https://data.bls.gov/dataViewer/view/timeseries/LNS14000000

# Population participation
lfpr_raw <- read_excel(
  "Participation_Rate_Population.xlsx",   # replace with your file name
  skip = 12
)

# Rename columns
names(lfpr_raw) <- c(
  "YEAR",
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

# Convert monthly columns to long format
lfpr_long <- lfpr_raw %>%
  pivot_longer(
    cols = Jan:Dec,
    names_to = "MONTH",
    values_to = "lfpr"
  ) %>%
  mutate(
    YEAR = as.numeric(YEAR),
    lfpr = as.numeric(lfpr)
  )

# Annual average LFPR
lfpr_annual <- lfpr_long %>%
  group_by(YEAR) %>%
  summarize(
    lfpr = mean(lfpr, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(YEAR)

#View(lfpr_annual)

# Plot
plot(
  lfpr_annual$YEAR,
  lfpr_annual$lfpr,
  type = "l",
  main = "Labor Force Participation Rate",
  xlab = "Year",
  ylab = "Participation Rate (%)"
)

# ADF test
adf.test(na.omit(lfpr_annual$lfpr))

# Use difference value
lfpr_annual <- lfpr_annual %>% mutate(
  D_lfpr = c(NA, diff(lfpr))
)

adf.test(na.omit(lfpr_annual$D_lfpr))


# Female force participation

female_lfpr_raw <- read_excel(
  "Participation_Rate_Female.xlsx",
  skip = 13
)

# Rename columns
names(female_lfpr_raw) <- c(
  "YEAR",
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

# Convert wide to long
female_lfpr_long <- female_lfpr_raw %>%
  pivot_longer(
    cols = Jan:Dec,
    names_to = "MONTH",
    values_to = "fem_lfpr"
  ) %>%
  mutate(
    YEAR = as.numeric(YEAR),
    fem_lfpr = as.numeric(fem_lfpr)
  )

# Annual averages
female_lfpr_annual <- female_lfpr_long %>%
  group_by(YEAR) %>%
  summarize(
    fem_lfpr = mean(fem_lfpr, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(YEAR) %>%
  mutate(
    D_fem_lfpr = c(NA, diff(fem_lfpr))
  )

#View(female_lfpr_annual)

# Plot
plot(
  female_lfpr_annual$YEAR,
  female_lfpr_annual$fem_lfpr,
  type = "l",
  main = "Female Labor Force Participation Rate",
  xlab = "Year",
  ylab = "Participation Rate (%)"
)

# ADF tests
adf.test(na.omit(female_lfpr_annual$fem_lfpr))
adf.test(na.omit(female_lfpr_annual$D_fem_lfpr))

