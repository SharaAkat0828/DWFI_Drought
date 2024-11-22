

pacman::p_load(
  pacman,
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

begin <- "1950-01-15" %>% as.Date()
end <- Sys.Date()
report_time_analysis <- paste0(begin, ":", end)



# outweight
OUTWT_USDA_quantity <- usdampr::mpr_request(slugIDs = "2477", report_time = report_time_analysis)$Detail %>% 
  tidyr::separate(., selling_basis_description, c("selling_type", "selling_transportation")) %>% 
  dplyr::filter(., class_description == "STEER", grade_description == "Total all grades", selling_type == "LIVE", selling_transportation == "FOB") %>%
  dplyr::mutate(., date = {report_date %m-% months(1)}, year = {year(date)}, month = {month(date)}) %>%
  dplyr::select(., date, year, month, report_title, class_description, grade_description, selling_type, selling_transportation, head_count, weight_range_avg, weighted_avg_price) %>%
  dplyr::arrange(., year, month) %>% dplyr::mutate(., steer_live_OTWT = weight_range_avg) %>% 
  dplyr::select(., year, month, steer_live_OTWT) %>% 
  group_by(year, month) %>% 
  summarize(., steer_live_OTWT = mean(steer_live_OTWT))

OUTWT_USDA_quantity <- OUTWT_USDA_quantity %>% 
  dplyr::mutate(., marketed_date = make_date(year, month, 15),
                purchased_date = {marketed_date %m-% months(4)},
                year_purch = {year(purchased_date)}, 
                month_purch = {month(purchased_date)}) %>% 
  na.omit()


# number of head
slaughter_head <- usdarnass::nass_data(source_desc = "SURVEY", sector_desc = "ANIMALS & PRODUCTS", group_desc = "LIVESTOCK",
                                       commodity_desc = "CATTLE",
                                       short_desc = "CATTLE, COWS, BEEF - INVENTORY", 
                                       agg_level_desc = "NATIONAL", year = "1990=<") %>% 
  dplyr::select(.,  year = year, month = end_code, beef_head = Value) %>% 
  dplyr::mutate(., year = {year %>% as.numeric()}, month = {month %>% as.numeric()}, 
                beef_head = {as.numeric(gsub(",","",beef_head))}) %>% 
  dplyr::filter(month == 1) %>%
  dplyr::mutate(beef_head = dplyr::lag(beef_head)) %>% na.omit()

slaughter_head <- slaughter_head %>% 
  dplyr::mutate(., marketed_date = make_date(year, month, 15),
                purchased_date = {marketed_date %m-% months(4)},
                year_purch = {year(purchased_date)}, 
                month_purch = {month(purchased_date)}) %>% 
  na.omit()


# Filter both datasets to start from the minimum common date
# Find the minimum common date
min_date <- max(min(OUTWT_USDA_quantity$marketed_date), min(slaughter_head$marketed_date))
OUTWT_USDA_quantity_filtered <- OUTWT_USDA_quantity %>% 
  dplyr::filter(marketed_date >= min_date) %>% 
  group_by (year) %>% 
  summarize(steer_live_OTWT_annual = mean(steer_live_OTWT, na.rm = TRUE))

slaughter_head_filtered <- slaughter_head %>%
  dplyr::filter(marketed_date >= min_date) %>% 
  dplyr::mutate(beef_head_2 = beef_head/1000)
scaling_factor <- 23.07692


# Figure 1
ggplot() +
  geom_line(data = OUTWT_USDA_quantity_filtered, aes(x = year, y = steer_live_OTWT_annual, color = "Steer, live weight")) +  # Assign legend label
  geom_line(data = slaughter_head_filtered, aes(x = year, y = beef_head_2 / scaling_factor, color = "Beef cow, head")) +  # Assign legend label and scaling factor
  xlab("Year") + 
  theme_bw() + 
  labs(title = "") + 
  ylab("Steer, live finishing weight (lbs)") +
  scale_y_continuous(
    name = "Steer, live finishing weight (lbs)",
    sec.axis = sec_axis(~ . * scaling_factor, name = "Beef cow, head count (1'000)")  # Multiply back the scaled beef_head for the secondary y-axis
  ) +
  scale_color_manual(values = c("Steer, live weight" = "black", "Beef cow, head" = "red"),  # Manually assign colors
                     name = "") +  # Legend title
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
        axis.title.y = element_text(size = 10),
        axis.title.y.right = element_text(size = 10),  
        axis.text.y = element_text(size = 10),
        legend.position = c(0.2, 0.9),  # Position the legend inside the plot at coordinates (0.8, 0.8)
        legend.background = element_rect(fill = alpha('white', 0.5)),
        legend.text = element_text(size = 12),  # Set legend text size
        legend.title = element_text(size = 14))   # Make legend background semi-transparent




