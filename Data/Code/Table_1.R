
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
  exactextractr, # extract raster values to sf
  flextable
)

rm(list = ls()) 

# setting USDA-NASS key
usdarnass::nass_set_key(key = "79F68508-887F-3423-A3EE-F60AB7DFB3AE")

# change WD here:
setwd("/Users/sharaakat/Dropbox/akat_shara/DWFI_drought/GitHub")
      
#**********************************************************************************************************************************************
## Pastureland DATA
#**********************************************************************************************************************************************

## PASTURE LAND - from USDA NASS quickStats
pasture_land_total <- usdarnass::nass_data(source_desc = "CENSUS", sector_desc = "ECONOMICS", group_desc = "FARMS & LAND & ASSETS",
                                      commodity_desc = "AG LAND", statisticcat_desc = "AREA", short_desc = "AG LAND, PASTURELAND - ACRES",
                                      domain_desc = "TOTAL", agg_level_desc = "COUNTY", year = "1990<=") %>% 
  dplyr::select(., state_name, county_name, year, Value) 

pasture_land_total <- rename(pasture_land_total, state = state_name)
pasture_land_total <- rename(pasture_land_total, county = county_name)
pasture_land_total <- rename(pasture_land_total, pastureland_all_types = Value)


# PASTURE LAND - from PDFs
pasture_data_PDF <- readxl::read_xlsx('/Data/Data/pasture_data_FINAL.xlsx')


# Join all PASTURELAND data
pasture_data_ALL <- rbind(pasture_data_PDF, pasture_land_total)
pasture_data_ALL$year <- as.numeric(as.character(pasture_data_ALL$year))
pasture_data_ALL$pastureland_all_types <- gsub(",", "", pasture_data_ALL$pastureland_all_types)
pasture_data_ALL$pastureland_all_types <- as.numeric(as.character(pasture_data_ALL$pastureland_all_types))
pasture_data_ALL$state <- tolower(pasture_data_ALL$state)
pasture_data_ALL$county <- tolower(pasture_data_ALL$county)

#**********************************************************************************************************************************************
## Cattle Inventory DATA (End of December)
#**********************************************************************************************************************************************

# CATTLE INV - from USDA NASS quickStats
beef_inv_total <- usdarnass::nass_data(source_desc = "CENSUS", sector_desc = "ANIMALS & PRODUCTS", group_desc = "LIVESTOCK",
                                      commodity_desc = "CATTLE", statisticcat_desc = "INVENTORY", short_desc = "CATTLE, COWS, BEEF - INVENTORY",
                                      domain_desc = "TOTAL",  agg_level_desc = "COUNTY", year = "1990<=") %>% 
  dplyr::select(., state_name, county_name, year, Value) %>% 
  rename(beef_inv = Value,
         state = state_name,
         county = county_name)


# CATTLE INV - from PDFs
beef_inv_PDF <- readxl::read_xlsx('/Data/Data/county_beefinv.xlsx')


# Join all CATTLE INV. data
beef_inv <- rbind(beef_inv_PDF, beef_inv_total)
beef_inv$year <- as.numeric(as.character(beef_inv$year))
beef_inv$beef_inv <- gsub(",", "", beef_inv$beef_inv)
beef_inv$beef_inv <- as.numeric(as.character(beef_inv$beef_inv))
beef_inv$state <- tolower(beef_inv$state)
beef_inv$county <- tolower(beef_inv$county)

merged_ALL <- left_join(pasture_data_ALL, beef_inv, by = c("year", "state", "county")) 



#**********************************************************************************************************************************************
## US regional data
#**********************************************************************************************************************************************

state_to_region <- function(state_name) {
  state_name <- tolower(state_name)  # Convert state name to lowercase for case-insensitive matching
  ifelse(state_name %in% c("michigan", "wisconsin", "minnesota"), "Lake States",
         ifelse(state_name %in% c("iowa", "illinois", "indiana", "ohio", "missouri"), "Corn Belt",
                ifelse(state_name %in% c("north dakota", "south dakota", "nebraska", "kansas"), "Northern Plains",
                       ifelse(state_name %in% c("oklahoma", "texas"), "Southern Plains",
                              ifelse(state_name %in% c("arkansas", "louisiana", "mississippi"), "Delta States",
                                     ifelse(state_name %in% c("alabama", "south carolina", "georgia", "florida"), "Southeast",
                                            ifelse(state_name %in% c("west virginia", "kentucky", "virginia", "tennessee", "north carolina"), "Appalachia",
                                                   ifelse(state_name %in% c("maine", "new hampshire", "vermont", "massachusetts", "rhode island", "connecticut", 
                                                                            "new york", "new jersey", "pennsylvania", "delaware", "maryland"), "Northeast",
                                                          ifelse(state_name %in% c("montana", "idaho", "wyoming", "colorado", "utah", "nevada", "arizona", "new mexico"), "Mountain",
                                                                 ifelse(state_name %in% c("washington", "oregon", "california"), "Pacific", NA))))))))))
  
}

# Create ANNUAL pastureland data - REGIONAL
pasture_data_ALL_region <- pasture_data_ALL %>% 
  mutate(region = state_to_region(state)) %>% 
  group_by(year, region) %>%
  summarise(
    total_pastureland_all_types = sum(pastureland_all_types, na.rm = TRUE)
  )


# Create ANNUAL cattle inventory data - REGIONAL
beef_inv_total_region <- beef_inv %>% 
  mutate(region = state_to_region(state)) %>% 
  group_by(year, region) %>%
  summarise(
    beef_inv = sum(beef_inv, na.rm = TRUE)
  )

# JOIN
merged_ALL_regional <- left_join(pasture_data_ALL_region, beef_inv_total_region, by = c("year", "region")) 

#**********************************************************************************************************************************************
## Cattle weight (pounds) DATA
#**********************************************************************************************************************************************

dressing_pct <- 0.55
cattle_weight <- readxl::read_xlsx("/Data/Data/catsltr.xlsx", sheet = 2, skip = 3) %>% 
  dplyr::select(1, 18) %>% 
  `colnames<-`(c("date", "dressed_wt")) %>% 
  dplyr::mutate(live_wt = dressed_wt/dressing_pct,
                year = year(date), month = month(date)) %>% 
  group_by(year) %>% 
  summarize(mean_live_wt = mean(live_wt, na.rm = TRUE))


#**********************************************************************************************************************************************
## Stocking Rate - ESTIMATION & Table 1 (Regional)
#**********************************************************************************************************************************************

AU <- 1000
grazing_months <- 5 

# SAVE RDS file of SR for future use
merged_ALL_SR <- left_join(merged_ALL, cattle_weight, by = c("year")) %>% 
  mutate(stock_rate = (beef_inv * mean_live_wt/AU * grazing_months)/pastureland_all_types) %>% 
  dplyr::filter(year != 1978) %>% 
  dplyr::select(year, state, county, stock_rate) %>% 
  na.omit()

saveRDS(merged_ALL_SR, file = "/Data/Data/SR_data.rds")


merged_ALL_regional_SR <- left_join(merged_ALL_regional, cattle_weight, by = c("year")) %>% 
  mutate(stock_rate = (beef_inv * mean_live_wt/AU * grazing_months)/total_pastureland_all_types) %>% 
  dplyr::select(year, region, stock_rate) %>% 
  dplyr::filter(year != 1978) %>% 
  na.omit()


# Table 1
merged_ALL_regional_SR <- merged_ALL_regional_SR %>%
  mutate(decade = floor(year / 10) * 10)  # Group years into decades (e.g., 1980s, 1990s)

# Summarize by region and decade
summary_table <- merged_ALL_regional_SR %>%
  group_by(region, decade) %>%
  summarize(
    avg_stock_rate = mean(stock_rate, na.rm = TRUE),  # Calculate average stocking rate
    .groups = "drop"
  )

summary_table_wide <- summary_table %>%
  pivot_wider(
    names_from = decade,
    values_from = avg_stock_rate,
    names_prefix = "Decade_"
  )

summary_table_wide <- summary_table_wide %>%
  mutate(
    change_1990_1980 = ((Decade_1990 - Decade_1980) / Decade_1980) * 100,
    change_2000_1990 = ((Decade_2000 - Decade_1990) / Decade_1990) * 100,
    change_2010_2000 = ((Decade_2010 - Decade_2000) / Decade_2000) * 100
  )

# Calculate national average
national_avg <- merged_ALL_regional_SR %>%
  group_by(decade = floor(year / 10) * 10) %>%
  summarize(
    avg_stock_rate = mean(stock_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = decade,
    values_from = avg_stock_rate,
    names_prefix = "Decade_"
  ) %>%
  mutate(
    change_1990_1980 = ((Decade_1990 - Decade_1980) / Decade_1980) * 100,
    change_2000_1990 = ((Decade_2000 - Decade_1990) / Decade_1990) * 100,
    change_2010_2000 = ((Decade_2010 - Decade_2000) / Decade_2000) * 100
  )

# Add a "region" label for national average
national_avg <- national_avg %>%
  mutate(region = "National Avg")

# Append national average to the summary table
final_table <- bind_rows(summary_table_wide, national_avg) %>% 
  mutate(across(where(is.numeric), ~ round(., 2))) 
ft <- flextable(data = final_table)


