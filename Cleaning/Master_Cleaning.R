# Master_Cleaning.r
# Run this to file which consolidates all the data cleaning from the other files.
# It does not produce any graphs so you will need to look into the separate files to see the plots

library(readxl)
library(stringr)
library(tidyverse)
library(tseries)


# 1. ################ Gini index time series #############################################################
#Cleaning_Gini.r
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


# Create time series
gini_ts <- ts(
  gini_data$GINI,
  start = min(gini_data$YEAR),
  frequency = 1
)


# 2. ################ GDP (gdp) #############################################################
# Cleaning_GDP.r
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
    L_gdp = log(gdp),
    D_L_gdp = c(NA, diff(L_gdp))
  )


# 3. ######### Government Expenditure as a share of GDP (gov_gdp) ##############################
# Cleaning_GDP.r
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
gdp_data <- gdp_data %>%
  mutate(
    gov_gdp = 100 * GOVEXP / gdp
  )

gdp_data <- gdp_data %>%
  mutate(
    D_gov_gdp = c(NA, diff(gov_gdp))
  )


# 4. ######### Services as a Share of GDP (serv_gdp) ###########################################
# Cleaning_GDP.r
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
    serv_gdp = 100 * SERVICES / GDP,
    D_serv_gdp = c(NA, diff(serv_gdp))
  )


# 5.######### Inflation (infl) ###########################################
#Cleaning_Inflation.r
# Consumer Price Index for All Urban Consumers from BLS (1967 - 2026)
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


# 6.######### Unemployment (unemp) ###########################################
# Cleaning_Unemployment.r
# Unemployment Labor Force Statistics from BLS (Seasonally adjusted) (1967-2026)
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


# 7. ######### Male Unemployment (m_unemp) ###########################################
# Cleaning_Unemployment.r
# Same as Unemployment data 
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


# 8. ######### Labor Force Participation (lfpr) ###########################################
# Cleaning_LaborForceParticipation.R
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


# Use difference value
lfpr_annual <- lfpr_annual %>% mutate(
  D_lfpr = c(NA, diff(lfpr))
)

adf.test(na.omit(lfpr_annual$D_lfpr))


# 9. ######### Female Labor Force Participation (fem_lfpr) ###########################################
# Cleaning_LaborForceParticipation.R
# Labor Force Participation from BLS 
# Source: BLS (1948-2026) https://data.bls.gov/dataViewer/view/timeseries/LNS14000000

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




# 10.  ######### Human Capital Attainment Variables (hs, col, fem_hs, fem_col) ###########################################
# Cleaning_EDU.r
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


# 11. ######### Merge Master Dataset ###########################################

master_data <- gini_data %>%
  
  # GDP
  left_join(
    gdp_long %>%
      select(YEAR, gdp, L_gdp, D_L_gdp),
    by = "YEAR"
  ) %>%
  
  # Government expenditure share
  left_join(
    gdp_data %>%
      select(YEAR, gov_gdp, D_gov_gdp),
    by = "YEAR"
  ) %>%
  
  # Services share of GDP
  left_join(
    serv_long %>%
      select(YEAR, serv_gdp, D_serv_gdp),
    by = "YEAR"
  ) %>%
  
  # Inflation
  left_join(
    cpi_annual %>%
      select(YEAR, infl),
    by = "YEAR"
  ) %>%
  
  # Unemployment
  left_join(
    unemp_annual %>%
      select(YEAR, unemp),
    by = "YEAR"
  ) %>%
  
  # Male unemployment
  left_join(
    male_unemp_annual %>%
      select(YEAR, m_unemp),
    by = "YEAR"
  ) %>%
  
  # Labor force participation
  left_join(
    lfpr_annual %>%
      select(YEAR, lfpr, D_lfpr),
    by = "YEAR"
  ) %>%
  
  # Female labor force participation
  left_join(
    female_lfpr_annual %>%
      select(YEAR, fem_lfpr, D_fem_lfpr),
    by = "YEAR"
  ) %>%
  
  # Education variables
  left_join(
    edu_data,
    by = "YEAR"
  ) %>%
  
  arrange(YEAR)



# Save final master dataset
write.csv(
  master_data,
  "master_data.csv",
  row.names = FALSE
)
