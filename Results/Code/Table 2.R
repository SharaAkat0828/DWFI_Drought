
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

# setting USDA-NASS key
usdarnass::nass_set_key(key = "79F68508-887F-3423-A3EE-F60AB7DFB3AE")
setwd("/Users/sharaakat/Dropbox/akat_shara/DWFI_drought/GitHub")

#**********************************************************************************************************************************************
## Retrieving PRISM data: “ppt” (precipitation), “tmean” (mean temperature), “tmin” (minimum temperature), and “tmax” (maximum temperature) 
## AND Joining all 
#**********************************************************************************************************************************************

####### SAMPLE OF CODE USED to OBTAIN VARIABLES #########

# US_counties <- tigris::counties(state = "US", cb = TRUE) %>% 
#   st_as_sf()
# 
# # 1981:1991; 1992:2002; 2003:2012; 2013:2020
# month_year_data <- CJ(month = 1:12, year = 2006)
# US_counties_data <- data.table()
# 
# temp_base <- 12  # base temperature for GDD calculation
# temp_up <- 31  # threshold temperature for EDD calculation
# 
# get_saved_PRISM_DT <- function(i, month_year_data, US_counties) {
#   # i = 1
#   temp_year <- month_year_data[i, year]
#   temp_month <- month_year_data[i, month]
#   
#   file_path_ppt <- sprintf("/Volumes/Extreme SSD/UNL/DWFI/data/PRISM_MONTHLY/PRISM_ppt_y%d_m%d.rds", temp_year, temp_month)
#   file_path_tmean <- sprintf("/Volumes/Extreme SSD/UNL/DWFI/data/PRISM_MONTHLY/PRISM_tmean_y%d_m%d.rds", temp_year, temp_month)
#   file_path_tmax <- sprintf("/Volumes/Extreme SSD/UNL/DWFI/data/PRISM_MONTHLY/PRISM_tmax_y%d_m%d.rds", temp_year, temp_month)
#   file_path_tmin <- sprintf("/Volumes/Extreme SSD/UNL/DWFI/data/PRISM_MONTHLY/PRISM_tmin_y%d_m%d.rds", temp_year, temp_month)
#   
#   cat("Year", temp_year, ", month", temp_month, "\n")
#   
#   saved_ppt <- readRDS(file_path_ppt)
#   saved_tmean <- readRDS(file_path_tmean)
#   saved_tmax <- readRDS(file_path_tmax)
#   saved_tmin <- readRDS(file_path_tmin)
#   
#   US_counties_sf <- st_transform(US_counties, st_crs(saved_ppt))
#   
#   a_ppt <- as.data.table(terra::extract(as(saved_ppt, "SpatRaster"), vect(US_counties_sf)))
#   a_tmean <- as.data.table(terra::extract(as(saved_tmean, "SpatRaster"), vect(US_counties_sf)))
#   a_tmax <- as.data.table(terra::extract(as(saved_tmax, "SpatRaster"), vect(US_counties_sf)))
#   a_tmin <- as.data.table(terra::extract(as(saved_tmin, "SpatRaster"), vect(US_counties_sf)))
#   
#   a_ppt_long <- melt(a_ppt, id.vars = "ID", variable.name = "Date", value.name = "ppt") %>%
#     .[, .(ppt = mean(ppt, na.rm = TRUE)), by = .(ID, Date)]
#   
#   # a_ppt_long <- melt(setDT(a_ppt), id.vars = "ID", variable.name = "Date", value.name = "ppt")[,
#   #      .(ppt = mean(ppt, na.rm = TRUE)), by = .(ID, Date)]
#   
#   a_tmean_long <- melt(a_tmean, id.vars = "ID", variable.name = "Date", value.name = "tmean") %>%
#     .[, .(tmean = mean(tmean, na.rm = TRUE)), by = .(ID, Date)]
#   a_tmax_long <- melt(a_tmax, id.vars = "ID", variable.name = "Date", value.name = "tmax") %>%
#     .[, .(tmax = mean(tmax, na.rm = TRUE)), by = .(ID, Date)]
#   a_tmin_long <- melt(a_tmin, id.vars = "ID", variable.name = "Date", value.name = "tmin") %>%
#     .[, .(tmin = mean(tmin, na.rm = TRUE)), by = .(ID, Date)]
#   
#   setkey(a_ppt_long, ID, Date)
#   setkey(a_tmean_long, ID, Date)
#   setkey(a_tmax_long, ID, Date)
#   setkey(a_tmin_long, ID, Date)
#   
#   # Joining the data.tables
#   a_long <- merge(a_ppt_long, a_tmean_long, by = c("ID", "Date"), all = TRUE)
#   a_long <- merge(a_long, a_tmax_long, by = c("ID", "Date"), all = TRUE)
#   a_long <- merge(a_long, a_tmin_long, by = c("ID", "Date"), all = TRUE)
#   
#   a_long[, Date := as.Date(gsub("date", "", Date))]
#   a_long[, `:=`(month = month(Date), year = year(Date))]
#   
#   US_counties_v_1 <- st_as_sf(vect(US_counties_sf)) %>%
#     mutate(ID = seq_len(nrow(.))) %>% 
#     as.data.table()
#   
#   US_counties_v_1 <- merge(US_counties_v_1, a_long, by = "ID", all.x = TRUE)
#   US_counties_v_1 <- US_counties_v_1[, .(
#     tmin = mean(tmin, na.rm = TRUE),
#     tmax = mean(tmax, na.rm = TRUE),
#     tmean = mean(tmean, na.rm = TRUE),
#     ppt = sum(ppt, na.rm = TRUE)
#   ), by = .(year, month, NAME, STATE_NAME)]
#   
#   
#   US_counties_v_1[, month_heat_index := (tmean / 5) ^ 1.514]
#   
#   US_counties_v_2 <- US_counties_v_1[, .(annual_heat_index = sum(month_heat_index, na.rm = TRUE)), by = .(year, NAME, STATE_NAME)]
#   US_counties_v_2[, a := 675 * 10^(-9) * annual_heat_index^3 - 771 * 10^(-7) * annual_heat_index^2 + 1792 * 10^(-5) * annual_heat_index + 0.49239]
#   
#   joined_weather_SPEI <- merge(US_counties_v_1, US_counties_v_2, by = c("year", "NAME", "STATE_NAME"), all.x = TRUE)
#   joined_weather_SPEI[, PET := ifelse(tmean <= 0, 0, 16 * ((10 * tmean) / annual_heat_index) ^ a)]
#   joined_weather_SPEI[, D := ppt - PET]
#   
#   joined_weather_SPEI[, Tavg := (tmax + tmin) / 2]
#   joined_weather_SPEI[, GDD := fifelse(Tavg < temp_base, 0, 
#                                        fifelse(Tavg > temp_up, temp_up - temp_base, 
#                                                Tavg - temp_base))]
#   
#   joined_weather_SPEI[, EDD := fifelse(tmax > temp_up, tmax - temp_up, 0)]
#   
#   joined_weather <- joined_weather_SPEI[, .(
#     avg_tmin = tmin,
#     avg_tmax = tmax,
#     avg_tmean = tmean,
#     sum_ppt = ppt,
#     monthly_D = sum(D, na.rm = TRUE),
#     monthly_GDD = sum(GDD, na.rm = TRUE),
#     monthly_EDD = sum(EDD, na.rm = TRUE)
#   ), by = .(year, month, NAME, STATE_NAME)]
#   
#   return(joined_weather)
# }
# 
# merged_data_1981_2020 <- data.frame()
# merged_data_1981_2020 <- lapply(
#   1:nrow(month_year_data),
#   function(x) get_saved_PRISM_DT(x, month_year_data, US_counties)
# )
# 
# combined_1981_2020 <- do.call(rbind, merged_data_1981_2020)
# 
# saveRDS(
#   combined_1981_2020,
#   paste0("/Volumes/Extreme SSD/UNL/DWFI /data/PRISM_MONTHLY_COMBINED_10_31/", "combined_1981_2020.rds")
# )


#**********************************************************************************************************************************************
## Drought and Weather DATA
#**********************************************************************************************************************************************

drought_final <- readRDS("/Results/Data/SPEI_final.rds")

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
hay_all_stocks_1000t <- readxl::read_xlsx("/Results/Data/StateHaySupplies.xlsx", sheet = 2, skip = 2)

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
SR_data <- readRDS("/Results/Data/SR_data.rds")
joined_drought_hay <- left_join(yearly_drought_data, hay_all_stocks_1000t_national, by = c("year")) 

regression_data <- left_join(SR_data, joined_drought_hay, by = c("year", "state", "county")) %>% 
  na.omit()

regression_data <- regression_data[!is.infinite(regression_data$stock_rate), ] %>%
  na.omit()

#**********************************************************************************************************************************************
## RUN REGRESSION -  TABLE 2
#**********************************************************************************************************************************************

reg_1 <- feols(log(stock_rate) ~ total_D| county + state + year, data = regression_data)
reg_1

reg_2 <- feols(log(stock_rate) ~ total_D + total_GDD | county + state + year, data = regression_data)
reg_2


reg_3 <- feols(log(stock_rate) ~ total_D + total_GDD + total_EDD | county + state + year, data = regression_data)
reg_3


reg_4 <- feols(log(stock_rate) ~ total_D + total_GDD + total_EDD + hay_stock_1000t | county + state, data = regression_data)
reg_4


reg_5 <- feols(log(stock_rate) ~ total_D + total_GDD + total_EDD + hay_stock_1000t + hay_stock_1000t^2| county + state, data = regression_data)
reg_5
















