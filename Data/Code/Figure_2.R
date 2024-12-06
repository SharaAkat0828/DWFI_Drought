
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
## Pastureland DATA
#**********************************************************************************************************************************************

## PASTURE LAND - from USDA NASS quickStats
pasture_land_total <- usdarnass::nass_data(source_desc = "CENSUS", sector_desc = "ECONOMICS", group_desc = "FARMS & LAND & ASSETS",
                                      commodity_desc = "AG LAND", statisticcat_desc = "AREA", short_desc = "AG LAND, PASTURELAND - ACRES",
                                      domain_desc = "TOTAL", agg_level_desc = "NATIONAL", year = "1990<=") %>% 
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


# Create ANNUAL pastureland data - ALL US
pasture_data_ALL_US <- pasture_data_ALL %>% 
  group_by(year) %>%
  summarise(
    total_pastureland_all_types = sum(pastureland_all_types, na.rm = TRUE)
  )


#**********************************************************************************************************************************************
## Cattle Inventory DATA
#**********************************************************************************************************************************************

# CATTLE INV - from USDA NASS quickStats
beef_inv_total <- usdarnass::nass_data(source_desc = "CENSUS", sector_desc = "ANIMALS & PRODUCTS", group_desc = "LIVESTOCK",
                                      commodity_desc = "CATTLE", statisticcat_desc = "INVENTORY", short_desc = "CATTLE, COWS, BEEF - INVENTORY",
                                      domain_desc = "TOTAL",  agg_level_desc = "NATIONAL", year = "1990<=") %>% 
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


# Create ANNUAL cattle inventory data - ALL US
beef_inv_total_US <- beef_inv %>% 
  group_by(year) %>%
  summarise(
    beef_inv = sum(beef_inv, na.rm = TRUE)
  )

#**********************************************************************************************************************************************
## Figure 2
#**********************************************************************************************************************************************

# Figure 2
merged_ALL_national <- left_join(pasture_data_ALL_US, beef_inv_total_US, by = c("year"))

merged_ALL_national$beef_inv <- merged_ALL_national$beef_inv/1000000
merged_ALL_national$total_pastureland_all_types <- merged_ALL_national$total_pastureland_all_types/1000000
scaling_factor <- max(merged_ALL_national$total_pastureland_all_types) / max(merged_ALL_national$beef_inv)


ggplot() +
  geom_line(data = merged_ALL_national, aes(x = year, y = beef_inv, color = "Beef cow inventory, head")) +  # Assign legend label
  geom_line(data = merged_ALL_national, aes(x = year, y = total_pastureland_all_types / scaling_factor, color = "Pastureland (all), acres")) +  # Assign legend label and scaling factor
  xlab("Year") + 
  theme_bw() + 
  labs(title = "") + 
  # ylab("Steer, live finishing weight (lbs)") +
  scale_x_continuous(breaks = seq(min(merged_ALL_national$year), max(merged_ALL_national$year), by = 5)) +  # Show year as numeric on x-axis
  scale_y_continuous(
    name = "Beef cow inventory, head (mln)",
    sec.axis = sec_axis(~ . * scaling_factor, name = "Pastureland (all), acres (mln)")  # Multiply back the scaled beef_head for the secondary y-axis
  ) +
  scale_color_manual(values = c("Beef cow inventory, head" = "black", "Pastureland (all), acres" = "red"),  # Manually assign colors
                     name = "") +  # Legend title
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
        axis.title.y = element_text(size = 10),
        axis.title.y.right = element_text(size = 10),  
        axis.text.y = element_text(size = 10),
        legend.position = c(0.8, 0.8),  # Position the legend inside the plot at coordinates (0.2, 0.8)
        legend.background = element_rect(fill = alpha('white', 0.5)),
        legend.text = element_text(size = 12),  # Set legend text size
        legend.title = element_text(size = 14))   # Make legend background semi-transparent



