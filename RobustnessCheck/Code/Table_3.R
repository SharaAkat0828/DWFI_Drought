
pacman::p_load(
  pacman,
  stars, # spatiotemporal data handling
  terra, # raster data handling
  raster, # raster data handling
  sf, # vector data handling
  dplyr, # data wrangling
  tidyverse, 
  fixest,
  parallel,
  foreach,
  doParallel,
  cubelyr,
  tigris,
  zoo,
  SCI,
  spei,
  purrr,
  datatable,
  ncdf4,
  stringr, # string manipulation
  lubridate, # dates handling
  data.table, # data wrangling
  tidyr, # reshape
  tidyUSDA, # download USDA NASS data
  keyring, # API key management
  FedData, # download Daymet data
  daymetr, # download Daymet data
  ggplot2, # make maps
  tmap, # make maps
  future.apply, # parallel processing
  CropScapeR, # download CDL data
  prism, # download PRISM data
  exactextractr # extract raster values to sf
)

rm(list = ls()) 

#**********************************************************************************************************************************************
## Drought and Weather DATA
#**********************************************************************************************************************************************

drought_final <- readRDS("/Users/sharaakat/Dropbox/akat_shara/DWFI_drought/data/SPEI_final.rds")

growing_season_data <- drought_final %>%
  filter(month >= 3 & month <= 10)  # Select only March to October

# Aggregate to yearly level
yearly_drought_data <- growing_season_data %>%
  group_by(year, STATE_NAME, NAME) %>%  # Group by year, state, and county
  summarize(
    avg_tmin = mean(avg_tmin, na.rm = TRUE),  # Average of minimum temperature
    avg_tmax = mean(avg_tmax, na.rm = TRUE),  # Average of maximum temperature
    avg_tmean = mean(avg_tmean, na.rm = TRUE),  # Average of mean temperature
    total_ppt = sum(sum_ppt, na.rm = TRUE),  # Total precipitation
    total_D = sum(monthly_D, na.rm = TRUE),  # Total drought index
    total_GDD = sum(monthly_GDD, na.rm = TRUE),  # Total Growing Degree Days
    total_EDD = sum(monthly_EDD, na.rm = TRUE)  # Total Extreme Degree Days
  ) %>% 
  rename(state = STATE_NAME,
         county = NAME) %>% 
  dplyr::select(year, state, county, total_D, total_GDD, total_EDD)

yearly_drought_data$state <- tolower(yearly_drought_data$state)
yearly_drought_data$county <- tolower(yearly_drought_data$county)

#**********************************************************************************************************************************************
## HAY PRODUCTION (lbs) DATA
#**********************************************************************************************************************************************

state_names <- c(
  AL = "Alabama", AK = "Alaska", AZ = "Arizona", AR = "Arkansas", CA = "California", 
  CO = "Colorado", CT = "Connecticut", DE = "Delaware", FL = "Florida", GA = "Georgia", 
  HI = "Hawaii", ID = "Idaho", IL = "Illinois", IN = "Indiana", IA = "Iowa", 
  KS = "Kansas", KY = "Kentucky", LA = "Louisiana", ME = "Maine", MD = "Maryland", 
  MA = "Massachusetts", MI = "Michigan", MN = "Minnesota", MS = "Mississippi", 
  MO = "Missouri", MT = "Montana", NE = "Nebraska", NV = "Nevada", NH = "New Hampshire", 
  NJ = "New Jersey", NM = "New Mexico", NY = "New York", NC = "North Carolina", 
  ND = "North Dakota", OH = "Ohio", OK = "Oklahoma", OR = "Oregon", PA = "Pennsylvania", 
  RI = "Rhode Island", SC = "South Carolina", SD = "South Dakota", TN = "Tennessee", 
  TX = "Texas", UT = "Utah", VT = "Vermont", VA = "Virginia", WA = "Washington", 
  WV = "West Virginia", WI = "Wisconsin", WY = "Wyoming"
)

# All hat stocks, 1000 tones
hay_all_stocks_1000t <- readxl::read_xlsx("/Users/sharaakat/Dropbox/akat_shara/DWFI_drought/data/StateHaySupplies.xlsx", sheet = 2, skip = 2)

hay_all_stocks_1000t_2 <- pivot_longer(hay_all_stocks_1000t, cols = -year, names_to = "state", values_to = "hay_stock_1000t") %>% 
  mutate(state = state_names[state])

hay_all_stocks_1000t_2$state <- tolower(hay_all_stocks_1000t_2$state)

hay_all_stocks_1000t_3 <- hay_all_stocks_1000t_2 %>%
  group_by(state) %>%
  mutate(hay_stock_1000t = lead(hay_stock_1000t, n = 1)) %>%
  ungroup() 

hay_all_stocks_1000t_national <- hay_all_stocks_1000t_3 %>% 
  group_by(year) %>% 
  summarize(hay_stock_1000t = mean(hay_stock_1000t, na.rm = TRUE))


#**********************************************************************************************************************************************
## JOIN ALL
#**********************************************************************************************************************************************

# GET stocking rate data
SR_data <- readRDS("/Users/sharaakat/Dropbox/akat_shara/DWFI_drought/manuscript/Review of Drought Paper/Results/Data/SR_data.rds")
joined_drought_hay <- left_join(yearly_drought_data, hay_all_stocks_1000t_national, by = c("year")) 

regression_data <- left_join(joined_drought_hay, SR_data, by = c("year", "state", "county")) 
regression_data <- regression_data[!is.infinite(regression_data$stock_rate), ]

reg_0 <- feols(log(stock_rate) ~ total_D + total_GDD + total_EDD + hay_stock_1000t + hay_stock_1000t^2| county + state, data = regression_data)

#**********************************************************************************************************************************************
## VISUALIZE LAGGED EFFECTS -  Table 3
#**********************************************************************************************************************************************

# get lagged D values
regression_data_lagged <- regression_data %>%
  arrange(state, county, year) %>% # Ensure data is sorted by state, county, and year
  group_by(state, county) %>% # Group by state and county
  mutate(
    drought_lag1 = lag(total_D, 1), # 1-year lag
    drought_lag2 = lag(total_D, 2), # 2-year lag
    drought_lag3 = lag(total_D, 3), # 3-year lag
    drought_lag4 = lag(total_D, 4), # 4-year lag
    drought_lag5 = lag(total_D, 5)  # 5-year lag
  ) %>%
  ungroup() %>% 
  na.omit()

# ar_model <- ar.ols(ts(regression_data_lagged$total_D %>%  na.omit()), order.max = 10, demean = FALSE, intercept = TRUE) 

#### 1 LAG #####
pca <- prcomp(regression_data_lagged[, c("total_D", "drought_lag1")], scale. = TRUE)
regression_data_lagged$pca1 <- pca$x[, 1]

reg_1 <- feols(log(stock_rate) ~ pca1 + total_GDD + total_EDD + hay_stock_1000t + hay_stock_1000t^2| county + state, data = regression_data_lagged)
reg_1
#### 2 LAGS #####
pca <- prcomp(regression_data_lagged[, c("total_D", "drought_lag1", "drought_lag2")], scale. = TRUE)
regression_data_lagged$pca1 <- pca$x[, 1]

reg_2 <- feols(log(stock_rate) ~ pca1 + total_GDD + total_EDD + hay_stock_1000t + hay_stock_1000t^2| county + state, data = regression_data_lagged)
reg_2

#### 3 LAGS #####
pca <- prcomp(regression_data_lagged[, c("total_D", "drought_lag1", "drought_lag2", "drought_lag3")], scale. = TRUE)
regression_data_lagged$pca1 <- pca$x[, 1]

reg_3 <- feols(log(stock_rate) ~ pca1 + total_GDD + total_EDD + hay_stock_1000t + hay_stock_1000t^2| county + state, data = regression_data_lagged)
reg_3

#### 4 LAGS #####
pca <- prcomp(regression_data_lagged[, c("total_D", "drought_lag1", "drought_lag2", "drought_lag3", "drought_lag4")], scale. = TRUE)
regression_data_lagged$pca1 <- pca$x[, 1]

reg_4 <- feols(log(stock_rate) ~ pca1 + total_GDD + total_EDD + hay_stock_1000t + hay_stock_1000t^2| county + state, data = regression_data_lagged)
reg_4

#### 5 LAGS #####
pca <- prcomp(regression_data_lagged[, c("total_D", "drought_lag1", "drought_lag2", "drought_lag3", "drought_lag4", "drought_lag5")], scale. = TRUE)
regression_data_lagged$pca1 <- pca$x[, 1]

reg_5 <- feols(log(stock_rate) ~ pca1 + total_GDD + total_EDD + hay_stock_1000t + hay_stock_1000t^2| county + state, data = regression_data_lagged)
reg_5







