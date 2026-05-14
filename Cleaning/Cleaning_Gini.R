# Cleaning_Gini.r
library(readxl)
library(stringr)
library(tidyverse)
library(tseries)

# Gini index -----------------------------------
#source: BLS Income Data Table A-2 (1967-2024) 
#https://www.census.gov/data/tables/2025/demo/income-poverty/p60-286.html
gini_raw <- read_excel("CENSUS-GINI.xlsx")

colnames(gini_raw)[1] <- "YEAR"
colnames(gini_raw)[14] <- "GINI"

# Create new df with year and gini columns
gini_data <- data.frame(
  YEAR = gini_raw[[1]],
  GINI = gini_raw[[14]]
)

# Remove metadata headers
gini_data <- gini_data[-c(1:5), ]

# Handle footnotes in year column
gini_data <- gini_data %>%
  mutate(
    YEAR_RAW = as.character(YEAR),
    YEAR = as.numeric(str_extract(YEAR, "^\\d{4}")),
    has_footnote = str_detect(YEAR_RAW, "\\d{4}\\D+\\d+")
    )

gini_data %>%
  count(YEAR) %>%
  filter(n > 1)

gini_data %>%
  filter(duplicated(YEAR) | duplicated(YEAR, fromLast = TRUE)) %>%
  arrange(YEAR)

gini_data <- gini_data %>%
  group_by(YEAR) %>%
  arrange(has_footnote) %>%   # FALSE first, TRUE second
  slice(1) %>%
  ungroup()

gini_data$YEAR <- as.numeric(gini_data$YEAR)
gini_data$GINI <- as.numeric(gini_data$GINI)
gini_data <- na.omit(gini_data)

gini_data <- gini_data %>%
  select(YEAR, GINI)

# Final Clean gini save 
write.csv(
  gini_data,
  "gini_data.csv",
  row.names = FALSE
)


# Create time series
gini_ts <- ts(
  gini_data$GINI,
  start = min(gini_data$YEAR),
  frequency = 1
)

# Plot Gini index 
plot(gini_ts,
     type = "l",
     lwd = 2,
     col = "darkblue",
     main = "US Gini Index (1967–2024)",
     ylab = "Gini Index",
     xlab = "Year")
# Plot shows clear upward trend, likely nonstationary, changes noticed betwen 1990 and 2000, 
# slightly before 2010, and 2020

# Stationarity Tests
adf.test(gini_ts)
# Output:
  # data:  gini_ts
  # Dickey-Fuller = -1.1923, Lag order = 3, p-value = 0.9003
  # alternative hypothesis: stationary


# Difference the time series
dgini <- diff(gini_ts)

plot(dgini,
     main = "Differenced Gini Index (1967 - 2024)",
     ylab = "Δ Gini",
     xlab = "Year")

# Retest stationarity
adf.test(dgini)
  #data:  dgini
  #Dickey-Fuller = -4.3371, Lag order = 3, p-value = 0.01
  #alternative hypothesis: stationary 
  # This is now stationary, low P-value, we can reject the null hypothesis

# Examine autocorrelation
acf(dgini)
pacf(dgini)





