# Cleaning_EDU
library(readxl)
library(stringr)
library(tidyverse)
library(tseries)

# Years of School Completed by People 25 Years and Over, by Age and Sex: Selected Years 1940 to 2024
# Source: BLS TableA-1 - https://www.census.gov/data/tables/time-series/demo/educational-attainment/cps-historical-time-series.html


edu_raw <- read_excel(
  "CENSUS_Edu.xlsx",  
  skip = 3           
)

# Find section boundaries
both_start <- which(edu_raw$Year == "25 Years and Over, Both Sexes")
male_start <- which(edu_raw$Year == "25 Years and Over, Male")
female_start <- which(edu_raw$Year == "25 Years and Over, Female")
female_end <- which(edu_raw$Year == "25 to 34 Years, Both Sexes")

# Both Sexes Population
population <- edu_raw[(both_start + 1):(male_start - 1), ] %>%
  transmute(
    YEAR = as.numeric(Year),
    TOTAL = as.numeric(Total),
    HS_COUNT = as.numeric(`...6`),
    COLLEGE_COUNT = as.numeric(`...8`)
  ) %>%
  filter(!is.na(YEAR)) %>%
  mutate(
    hs = 100 * HS_COUNT / TOTAL,
    col = 100 * COLLEGE_COUNT / TOTAL
  ) %>%
  select(YEAR, hs, col) %>%
  arrange(YEAR)

# Female Population
female_population <- edu_raw[(female_start + 1):(female_end - 1), ] %>%
  transmute(
    YEAR = as.numeric(Year),
    TOTAL_FEM = as.numeric(Total),
    HS_FEM_COUNT = as.numeric(`...6`),
    COLLEGE_FEM_COUNT = as.numeric(`...8`)
  ) %>%
  filter(!is.na(YEAR)) %>%
  mutate(
    hs_fem = 100 * HS_FEM_COUNT / TOTAL_FEM,
    col_fem = 100 * COLLEGE_FEM_COUNT / TOTAL_FEM
  ) %>%
  select(YEAR, hs_fem, col_fem) %>%
  arrange(YEAR)

# Combine education variables
edu_data <- population %>%
  inner_join(female_population, by = "YEAR") %>%
  arrange(YEAR)


plot(
  edu_data$YEAR,
  edu_data$hs,
  type = "l",
  lwd = 2,
  col = "blue",
  ylim = range(
    c(
      edu_data$hs,
      edu_data$col,
      edu_data$hs_fem,
      edu_data$col_fem
    ),
    na.rm = TRUE
  ),
  main = "Educational Attainment Rates",
  xlab = "Year",
  ylab = "Percent of Population 25+"
)

# College attainment (all population)
lines(
  edu_data$YEAR,
  edu_data$col,
  lwd = 2,
  col = "red",
  lty = 1
)

# High school attainment (female)
lines(
  edu_data$YEAR,
  edu_data$hs_fem,
  lwd = 2,
  col = "blue",
  lty = 2
)

# College attainment (female)
lines(
  edu_data$YEAR,
  edu_data$col_fem,
  lwd = 2,
  col = "red",
  lty = 2
)

legend(
  "topleft",
  legend = c(
    "HS",
    "HS Female",
    "College",
    
    "College Female"
  ),
  col = c("blue", "blue","red",  "red"),
  lty = c(1, 2, 1, 2),
  lwd = 2
)

# ADF Tests

adf.test(edu_data$col)

adf.test(edu_data$col_fem)

adf.test(edu_data$hs)

adf.test(edu_data$hs_fem)


# Difference Transformation
edu_data <- edu_data %>%
  arrange(YEAR) %>%
  mutate(
    D_col = c(NA, diff(col)),
    D_col_fem = c(NA, diff(col_fem)),
    D_hs = c(NA, diff(hs)),
    D_hs_fem = c(NA, diff(hs_fem))
  )

# Retest stationarity 
adf.test(na.omit(edu_data$D_col))

adf.test(na.omit(edu_data$D_col_fem))

adf.test(na.omit(edu_data$D_hs))

adf.test(na.omit(edu_data$D_hs_fem))

