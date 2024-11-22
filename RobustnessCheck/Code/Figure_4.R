
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

setwd("/Volumes/Extreme SSD/UNL/DWFI/projection")

#**********************************************************************************************************************************************
## 1. Retrieving datasets - MACA
#**********************************************************************************************************************************************

# #**********************************************************************************************************************************************
# ## 1.1. pr DATA
# #**********************************************************************************************************************************************
# 
# # 4.5, pr: 2006-2099, daily
# urls <- c(
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_pr_bcc-csm1-1_r1i1p1_rcp45_2006_2025_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_pr_bcc-csm1-1_r1i1p1_rcp45_2026_2045_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_pr_bcc-csm1-1_r1i1p1_rcp45_2046_2065_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_pr_bcc-csm1-1_r1i1p1_rcp45_2066_2085_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_pr_bcc-csm1-1_r1i1p1_rcp45_2086_2099_CONUS_monthly.nc"
# )
# 
# 
# dir.create("./data", showWarnings = FALSE)
# 
# # Loop to download and extract each file
# for (url in urls) {
#   # Extract year range from the URL for naming the file
#   year_range <- gsub(".*_([0-9]{4}_[0-9]{4})_.*", "\\1", url)
#   
#   # Set destination file name
#   destfile <- paste0("./data/pr4.5", year_range, ".nc")
#   
#   # Increase timeout in case of large files
#   options(timeout = max(600, getOption("timeout")))
#   
#   # Download the file
#   download.file(url, destfile = destfile, method = "auto", mode = "wb")
#   
#   # Load the .nc file into a raster object
#   assign(paste0("pr4.5", year_range), terra::rast(destfile))
#   
#   # Print confirmation of download and extraction
#   print(paste("Downloaded and loaded:", destfile))
# }
# 
# 
# 
# 
# # 8.5, pr: 2006-2099, daily
# urls <- c(
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_pr_bcc-csm1-1_r1i1p1_rcp85_2006_2025_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_pr_bcc-csm1-1_r1i1p1_rcp85_2026_2045_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_pr_bcc-csm1-1_r1i1p1_rcp85_2046_2065_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_pr_bcc-csm1-1_r1i1p1_rcp85_2066_2085_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_pr_bcc-csm1-1_r1i1p1_rcp85_2086_2099_CONUS_monthly.nc"
# )
# 
# 
# dir.create("./data", showWarnings = FALSE)
# 
# # Loop to download and extract each file
# for (url in urls) {
#   # Extract year range from the URL for naming the file
#   year_range <- gsub(".*_([0-9]{4}_[0-9]{4})_.*", "\\1", url)
#   
#   # Set destination file name
#   destfile <- paste0("./data/pr8.5_", year_range, ".nc")
#   
#   # Increase timeout in case of large files
#   options(timeout = max(600, getOption("timeout")))
#   
#   # Download the file
#   download.file(url, destfile = destfile, method = "auto", mode = "wb")
#   
#   # Load the .nc file into a raster object
#   assign(paste0("pr8.5_", year_range), terra::rast(destfile))
#   
#   # Print confirmation of download and extraction
#   print(paste("Downloaded and loaded:", destfile))
# }
# 
# 
# 
# #**********************************************************************************************************************************************
# ## 1.2. tmin DATA
# #**********************************************************************************************************************************************
# 
# 
# # 4.5, tmin: 2006-2099, daily
# urls <- c(
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_tasmin_bcc-csm1-1_r1i1p1_rcp45_2006_2025_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_tasmin_bcc-csm1-1_r1i1p1_rcp45_2026_2045_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_tasmin_bcc-csm1-1_r1i1p1_rcp45_2046_2065_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_tasmin_bcc-csm1-1_r1i1p1_rcp45_2066_2085_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_tasmin_bcc-csm1-1_r1i1p1_rcp45_2086_2099_CONUS_monthly.nc"
# )
# 
# dir.create("./data", showWarnings = FALSE)
# 
# # Loop to download and extract each file
# for (url in urls) {
#   # Extract year range from the URL for naming the file
#   year_range <- gsub(".*_([0-9]{4}_[0-9]{4})_.*", "\\1", url)
#   
#   # Set destination file name
#   destfile <- paste0("./data/tmin4.5_", year_range, ".nc")
#   
#   # Increase timeout in case of large files
#   options(timeout = max(600, getOption("timeout")))
#   
#   # Download the file
#   download.file(url, destfile = destfile, method = "auto", mode = "wb")
#   
#   # Load the .nc file into a raster object
#   assign(paste0("tmin4.5_", year_range), terra::rast(destfile))
#   
#   # Print confirmation of download and extraction
#   print(paste("Downloaded and loaded:", destfile))
# }
# 
# 
# 
# # 8.5, tmin: 2006-2099, daily
# urls <- c(
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_tasmin_bcc-csm1-1_r1i1p1_rcp85_2006_2025_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_tasmin_bcc-csm1-1_r1i1p1_rcp85_2026_2045_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_tasmin_bcc-csm1-1_r1i1p1_rcp85_2046_2065_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_tasmin_bcc-csm1-1_r1i1p1_rcp85_2066_2085_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_tasmin_bcc-csm1-1_r1i1p1_rcp85_2086_2099_CONUS_monthly.nc"
# )
# 
# dir.create("./data", showWarnings = FALSE)
# 
# # Loop to download and extract each file
# for (url in urls) {
#   # Extract year range from the URL for naming the file
#   year_range <- gsub(".*_([0-9]{4}_[0-9]{4})_.*", "\\1", url)
#   
#   # Set destination file name
#   destfile <- paste0("./data/tmin8.5_", year_range, ".nc")
#   
#   # Increase timeout in case of large files
#   options(timeout = max(600, getOption("timeout")))
#   
#   # Download the file
#   download.file(url, destfile = destfile, method = "auto", mode = "wb")
#   
#   # Load the .nc file into a raster object
#   assign(paste0("tmin8.5_", year_range), terra::rast(destfile))
#   
#   # Print confirmation of download and extraction
#   print(paste("Downloaded and loaded:", destfile))
# }
# 
# 
# 
# #**********************************************************************************************************************************************
# ## 1.3. tmax DATA
# #**********************************************************************************************************************************************
# 
# 
# # 4.5, tmax: 2006-2099, daily
# urls_tmax <- c(
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_tasmax_bcc-csm1-1_r1i1p1_rcp45_2006_2025_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_tasmax_bcc-csm1-1_r1i1p1_rcp45_2026_2045_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_tasmax_bcc-csm1-1_r1i1p1_rcp45_2046_2065_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_tasmax_bcc-csm1-1_r1i1p1_rcp45_2066_2085_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_tasmax_bcc-csm1-1_r1i1p1_rcp45_2086_2099_CONUS_monthly.nc"
# )
# 
# dir.create("./data", showWarnings = FALSE)
# 
# # Loop to download and extract each file
# for (url in urls) {
#   # Extract year range from the URL for naming the file
#   year_range <- gsub(".*_([0-9]{4}_[0-9]{4})_.*", "\\1", url)
#   
#   # Set destination file name
#   destfile <- paste0("./data/tmax4.5_", year_range, ".nc")
#   
#   # Increase timeout in case of large files
#   options(timeout = max(600, getOption("timeout")))
#   
#   # Download the file
#   download.file(url, destfile = destfile, method = "auto", mode = "wb")
#   
#   # Load the .nc file into a raster object
#   assign(paste0("tmax4.5_", year_range), terra::rast(destfile))
#   
#   # Print confirmation of download and extraction
#   print(paste("Downloaded and loaded:", destfile))
# }
# 
# 
# 
# 
# # 8.5, tmax: 2006-2099, daily
# urls_tmax <- c(
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_tasmax_bcc-csm1-1_r1i1p1_rcp85_2006_2025_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_tasmax_bcc-csm1-1_r1i1p1_rcp85_2026_2045_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_tasmax_bcc-csm1-1_r1i1p1_rcp85_2046_2065_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_tasmax_bcc-csm1-1_r1i1p1_rcp85_2066_2085_CONUS_monthly.nc",
#   "http://thredds.northwestknowledge.net:8080/thredds/fileServer/NWCSC_INTEGRATED_SCENARIOS_ALL_CLIMATE/macav2livneh/bcc-csm1-1/macav2livneh_tasmax_bcc-csm1-1_r1i1p1_rcp85_2086_2099_CONUS_monthly.nc"
# )
# 
# dir.create("./data", showWarnings = FALSE)
# 
# # Loop to download and extract each file
# for (url in urls) {
#   # Extract year range from the URL for naming the file
#   year_range <- gsub(".*_([0-9]{4}_[0-9]{4})_.*", "\\1", url)
#   
#   # Set destination file name
#   destfile <- paste0("./data/tmax8.5_", year_range, ".nc")
#   
#   # Increase timeout in case of large files
#   options(timeout = max(600, getOption("timeout")))
#   
#   # Download the file
#   download.file(url, destfile = destfile, method = "auto", mode = "wb")
#   
#   # Load the .nc file into a raster object
#   assign(paste0("tmax8.5_", year_range), terra::rast(destfile))
#   
#   # Print confirmation of download and extraction
#   print(paste("Downloaded and loaded:", destfile))
# }
# 

#**********************************************************************************************************************************************
## 2. Estimating Variables   
#**********************************************************************************************************************************************

# temp_base <- 12  # base temperature for GDD calculation
# temp_up <- 31  # threshold temperature for EDD calculation
# 
# # Load US counties
# US_counties <- tigris::counties(state = "US", cb = TRUE) %>%
#   st_as_sf()
# 
# # 2006, 2026, 2046, 2066, 2086
# month_year_data <- CJ(year = 2086)
# data_dir <- "/Volumes/Extreme SSD/UNL/DWFI/projection/data"
# 
# # Define file paths for 4.5 and 8.5 scenarios
# file_paths <- list(
#   `4.5` = list(
#     pr = list.files(path = data_dir, pattern = "pr4.5.*\\.nc$", full.names = TRUE),
#     tmin = list.files(path = data_dir, pattern = "tmin4.5.*\\.nc$", full.names = TRUE),
#     tmax = list.files(path = data_dir, pattern = "tmax4.5.*\\.nc$", full.names = TRUE)
#   ),
#   `8.5` = list(
#     pr = list.files(path = data_dir, pattern = "pr8.5.*\\.nc$", full.names = TRUE),
#     tmin = list.files(path = data_dir, pattern = "tmin8.5.*\\.nc$", full.names = TRUE),
#     tmax = list.files(path = data_dir, pattern = "tmax8.5.*\\.nc$", full.names = TRUE)
#   )
# )
# 
# # Function to process one month of data
# process_monthly_data <- function(year, scenario, file_paths, US_counties) {
#   # Load corresponding files for the scenario and year range
#   pr_file <- file_paths[[scenario]]$pr[grepl(sprintf("%d", year), file_paths[[scenario]]$pr)]
#   tmin_file <- file_paths[[scenario]]$tmin[grepl(sprintf("%d", year), file_paths[[scenario]]$tmin)]
#   tmax_file <- file_paths[[scenario]]$tmax[grepl(sprintf("%d", year), file_paths[[scenario]]$tmax)]
#   
#   # Check if all files exist
#   if (length(pr_file) == 0 || length(tmin_file) == 0 || length(tmax_file) == 0) {
#     message(sprintf("Skipping %d-%02d for %s scenario due to missing files.", year, month, scenario))
#     return(NULL)
#   }
#   
#   # Load raster data
#   pr_raster <- rast(pr_file)
#   tmin_raster <- rast(tmin_file)
#   tmax_raster <- rast(tmax_file)
#   
#   names(pr_raster) <- time(pr_raster)
#   names(tmin_raster) <- time(tmin_raster)
#   names(tmax_raster) <- time(tmax_raster)
#   
#   # Transform counties to match CRS
#   US_counties_sf <- st_transform(US_counties, crs(pr_raster))
#   
#   # Extract data for each variable
#   pr_data <- as.data.table(terra::extract(pr_raster, vect(US_counties_sf)))
#   tmin_data <- as.data.table(terra::extract(tmin_raster, vect(US_counties_sf)))
#   tmax_data <- as.data.table(terra::extract(tmax_raster, vect(US_counties_sf)))
#   
#   # Reshape and aggregate data
#   pr_data_ID <- melt(pr_data, id.vars = "ID", variable.name = "Date", value.name = "ppt") %>% 
#     .[, .(ppt = mean(ppt, na.rm = TRUE)), by = .(ID, Date)]
#   
#   tmin_data_ID <- melt(tmin_data, id.vars = "ID", variable.name = "Date", value.name = "tmin") %>% 
#     .[, .(tmin = mean(tmin, na.rm = TRUE)), by = .(ID, Date)]
#   
#   tmax_data_ID <- melt(tmax_data, id.vars = "ID", variable.name = "Date", value.name = "tmax") %>% 
#     .[, .(tmax = mean(tmax, na.rm = TRUE)), by = .(ID, Date)]
#   
#   # Merge the data
#   weather_data <- merge(pr_data_ID, tmin_data_ID, by = c("ID", "Date"))
#   weather_data <- merge(weather_data, tmax_data_ID, by = c("ID", "Date"))
#   
#   # Add year and month
#   weather_data[, year := year(Date)]
#   weather_data[, month := month(Date)]
#   
#   US_counties_v_1 <- st_as_sf(vect(US_counties_sf)) %>%
#     mutate(ID = seq_len(nrow(.))) %>% 
#     as.data.table()
#   
#   US_counties_v_1 <- merge(US_counties_v_1, weather_data, by = "ID", all.x = TRUE)
#   US_counties_v_1[, tmax := tmax - 273.15]
#   US_counties_v_1[, tmin := tmin - 273.15]
#   
# 
#   US_counties_v_1[, Tavg := (tmax + tmin) / 2]
#   US_counties_v_1[, month_heat_index := (Tavg / 5) ^ 1.514]
#   
#   US_counties_v_2 <- US_counties_v_1[, .(annual_heat_index = sum(month_heat_index, na.rm = TRUE)), by = .(year, NAME, STATE_NAME)]
#   US_counties_v_2[, a := 675 * 10^(-9) * annual_heat_index^3 - 771 * 10^(-7) * annual_heat_index^2 + 1792 * 10^(-5) * annual_heat_index + 0.49239]
#   
#   joined_weather_SPEI <- merge(US_counties_v_1, US_counties_v_2, by = c("year", "NAME", "STATE_NAME"), all.x = TRUE)
#   joined_weather_SPEI[, PET := ifelse(Tavg <= 0, 0, 16 * ((10 * Tavg) / annual_heat_index) ^ a)]
#   joined_weather_SPEI[, D := ppt - PET]
#   
#   joined_weather_SPEI[, Tavg := (tmax + tmin) / 2]
#   joined_weather_SPEI[, GDD := fifelse(Tavg < temp_base, 0, 
#                                        fifelse(Tavg > temp_up, temp_up - temp_base, 
#                                                Tavg - temp_base))]
#   
#   joined_weather_SPEI[, EDD := fifelse(tmax > temp_up, tmax - temp_up, 0)]
#   
#   print(sprintf("Done with year: %d", year))
#   
#   joined_weather <- joined_weather_SPEI[, .(
#     avg_tmin = tmin,
#     avg_tmax = tmax,
#     avg_tmean = Tavg,
#     sum_ppt = ppt,
#     monthly_D = sum(D, na.rm = TRUE),
#     monthly_GDD = sum(GDD, na.rm = TRUE),
#     monthly_EDD = sum(EDD, na.rm = TRUE)
#   ), by = .(year, month, NAME, STATE_NAME)]
#   
#   return(joined_weather)
# }
# 
# # Process all data
# results <- list()
# for (scenario in names(file_paths)) {
#   scenario_results <- lapply(1:nrow(month_year_data), function(i) {
#     year <- month_year_data[i, year]
#     process_monthly_data(year, scenario, file_paths, US_counties)
#   })
#   results[[scenario]] <- do.call(rbind, scenario_results)
# }
# 
# # Save results
# saveRDS(results$`4.5`, file.path(data_dir, "2086_scenario_4.5_results.rds"))
# saveRDS(results$`8.5`, file.path(data_dir, "2086_scenario_8.5_results.rds"))
# 

#**********************************************************************************************************************************************
## 3. Combining datasets  
#**********************************************************************************************************************************************

# # 4.5
# drought_2006_45 <- readRDS("/Volumes/Extreme SSD/UNL/DWFI/projection/data/2006_scenario_4.5_results.rds")
# drought_2026_45 <- readRDS("/Volumes/Extreme SSD/UNL/DWFI/projection/data/2026_scenario_4.5_results.rds")
# drought_2046_45 <- readRDS("/Volumes/Extreme SSD/UNL/DWFI/projection/data/2046_scenario_4.5_results.rds")
# drought_2066_45 <- readRDS("/Volumes/Extreme SSD/UNL/DWFI/projection/data/2066_scenario_4.5_results.rds")
# drought_2086_45 <- readRDS("/Volumes/Extreme SSD/UNL/DWFI/projection/data/2086_scenario_4.5_results.rds")
# 
# drought_data_4.5 <- rbind(drought_2006_45, drought_2026_45, 
#                           drought_2046_45, drought_2066_45, drought_2086_45)
# saveRDS(drought_data_4.5, file.path(data_dir, "drought_4.5_ALL.rds"))
# 
# 
# 
# # 8.5
# drought_2006_85 <- readRDS("/Volumes/Extreme SSD/UNL/DWFI/projection/data/2006_scenario_8.5_results.rds")
# drought_2026_85 <- readRDS("/Volumes/Extreme SSD/UNL/DWFI/projection/data/2026_scenario_8.5_results.rds")
# drought_2046_85 <- readRDS("/Volumes/Extreme SSD/UNL/DWFI/projection/data/2046_scenario_8.5_results.rds")
# drought_2066_85 <- readRDS("/Volumes/Extreme SSD/UNL/DWFI/projection/data/2066_scenario_8.5_results.rds")
# drought_2086_85 <- readRDS("/Volumes/Extreme SSD/UNL/DWFI/projection/data/2086_scenario_8.5_results.rds")
# 
# drought_data_8.5 <- rbind(drought_2006_85, drought_2026_85, 
#                           drought_2046_85, drought_2066_85, drought_2086_85)
# saveRDS(drought_data_8.5, file.path(data_dir, "drought_8.5_ALL.rds"))


#**********************************************************************************************************************************************
## 4. Projected Hay Stocks
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

hay_all_stocks_1000t_national <- tibble(
    year = 2020:2025,
    hay_stock_1000t = c(1646, 1495, 1598)
  )

# Calculate the last 5-year average of non-NA values
last_5_years_avg <- hay_all_stocks_1000t_national %>%
  filter(!is.na(hay_stock_1000t)) %>%
  tail(5) %>%
  summarise(avg = mean(hay_stock_1000t, na.rm = TRUE)) %>%
  pull(avg)

# Create a sequence of missing years (2023 to 2099)
missing_years <- tibble(
  year = 2023:2099,
  hay_stock_1000t = last_5_years_avg
)

# Combine the original data with the missing years
hay_all_stocks_1000t_national_projected <- hay_all_stocks_1000t_national %>%
  bind_rows(missing_years) %>%
  arrange(year)


#**********************************************************************************************************************************************
## 5. Projected Stockign Density
#**********************************************************************************************************************************************

# INITIAL MODEL
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
joined_drought_hay <- left_join(yearly_drought_data, hay_all_stocks_1000t_national, by = c("year")) 

regression_data <- left_join(SR_data, joined_drought_hay, by = c("year", "state", "county")) %>% 
  na.omit()
regression_data <- regression_data[!is.infinite(regression_data$stock_rate), ] %>%
  na.omit()

reg_1 <- feols(log(stock_rate) ~ total_D + total_GDD + total_EDD + hay_stock_1000t + hay_stock_1000t^2| county + state, data = regression_data)

coefficients <- coef(reg_1)
coef_total_D <- as.numeric(coefficients["total_D"])
coef_total_GDD <- as.numeric(coefficients["total_GDD"])
coef_total_EDD <- as.numeric(coefficients["total_EDD"])
coef_hay_stock <- as.numeric(coefficients["hay_stock_1000t"])
coef_hay_stock_sq <- as.numeric(coefficients["I(hay_stock_1000t^2)"])


# 4.5
drought_data_4.5 <- readRDS("/Volumes/Extreme SSD/UNL/DWFI/projection/data/drought_4.5_ALL.rds")
growing_season_data <- drought_data_4.5 %>%
  filter(month >= 3 & month <= 10)  # Select only March to October

# Aggregate to yearly level
yearly_drought_data_projected <- growing_season_data %>%
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

yearly_drought_data_projected$state <- tolower(yearly_drought_data_projected$state)
yearly_drought_data_projected$county <- tolower(yearly_drought_data_projected$county)

joined_drought_hay_projected <- left_join(yearly_drought_data_projected, hay_all_stocks_1000t_national_projected, by = c("year")) 


Projected_SR_45 <- joined_drought_hay_projected %>%
  mutate(
    hay_stock_1000t_sq = hay_stock_1000t^2,  # Square term for hay_stock_1000t
    log_stock_rate = 
      coef_total_D * total_D +
      coef_total_GDD * total_GDD +
      coef_total_EDD * total_EDD +
      coef_hay_stock * hay_stock_1000t +
      coef_hay_stock_sq * hay_stock_1000t_sq,
    projected_stock_rate = exp(log_stock_rate)  # Exponentiate to get the stock rate
  )

# 8.5
drought_data_8.5 <- readRDS("/Volumes/Extreme SSD/UNL/DWFI/projection/data/drought_8.5_ALL.rds")
growing_season_data <- drought_data_8.5 %>%
  filter(month >= 3 & month <= 10)  # Select only March to October

# Aggregate to yearly level
yearly_drought_data_projected <- growing_season_data %>%
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

yearly_drought_data_projected$state <- tolower(yearly_drought_data_projected$state)
yearly_drought_data_projected$county <- tolower(yearly_drought_data_projected$county)

joined_drought_hay_projected <- left_join(yearly_drought_data_projected, hay_all_stocks_1000t_national_projected, by = c("year")) 


Projected_SR_85 <- joined_drought_hay_projected %>%
  mutate(
    hay_stock_1000t_sq = hay_stock_1000t^2,  # Square term for hay_stock_1000t
    log_stock_rate = 
      coef_total_D * total_D +
      coef_total_GDD * total_GDD +
      coef_total_EDD * total_EDD +
      coef_hay_stock * hay_stock_1000t +
      coef_hay_stock_sq * hay_stock_1000t_sq,
    projected_stock_rate = exp(log_stock_rate)  # Exponentiate to get the stock rate
  )



# FIGURE 4
library(ggplot2)
library(dplyr)

# Calculate yearly means for both scenarios 
yearly_data_45 <- Projected_SR_45 %>%
  group_by(year) %>%
  summarise(mean_projected_stock_rate = mean(projected_stock_rate, na.rm = TRUE)) %>%
  mutate(scenario = "RCP 4.5")  %>% 
  dplyr::filter(year >= 2020)

yearly_data_85 <- Projected_SR_85 %>%
  group_by(year) %>%
  summarise(mean_projected_stock_rate = mean(projected_stock_rate, na.rm = TRUE)) %>%
  mutate(scenario = "RCP 8.5")  %>% 
  dplyr::filter(year >= 2020)

# Combine the datasets
combined_data <- bind_rows(yearly_data_45, yearly_data_85)

# Plot using ggplot2
ggplot(data = combined_data, aes(x = year, y = mean_projected_stock_rate, color = scenario)) +
  geom_line(size = 1) +  # Line plot for each scenario
  geom_point(size = 2) +  # Points for each year
  labs(
    x = "Year",
    y = "Projected Stock Rate",
    color = "Scenario"
  ) +
  scale_color_manual(values = c("RCP 4.5" = "blue", "RCP 8.5" = "red")) +  # Set colors for each scenario
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )









