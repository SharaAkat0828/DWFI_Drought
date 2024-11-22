#Load Libraries
library("dplyr")         # cleans/organizes the data
library("reshape2")      # shape the data
library("lubridate")     # manipulation with dates and time
library("readstata13")   # reads STATA files
library("ggplot2")       # creates graphs
library("latex2exp")     # so we can label tables and graphs using latex type setting
library("gridExtra")     # allows multiple ggplot's to be gridded onto a single page
library("plm")           # enables panel model estimation
library("lmtest")        # allows clustered SEs

rm(list = ls()) 

#**********************************************************************************************************************************************
## Hay Yield -  DATA
#**********************************************************************************************************************************************

# ANNUAL TONS/ACRE (KANSAS, DOUGLAS COUNTY)
hay_yield <- read.csv("/Users/sharaakat/Dropbox/akat_shara/DWFI_drought/manuscript/Review of Drought Paper/RobustnessCheck/Data/HAY_historical.csv") %>% 
  dplyr::select(Year, Value) %>% 
  rename(DryYield = Value) 

hay_yield <- hay_yield %>% 
  mutate(Trend = seq(1, nrow(hay_yield), 1),
         ly = log(DryYield))

#**********************************************************************************************************************************************
## Weather -  DATA
#**********************************************************************************************************************************************

# Create weather data
weather <- readstata13::read.dta13("/Users/sharaakat/Dropbox/akat_shara/DWFI_drought/manuscript/Review of Drought Paper/RobustnessCheck/Data/Weather_fips20191.dta")

weather <- weather %>% 
  dplyr::mutate(., 
                Year = {dateNum %>% lubridate::year(.)},
                Mon  = {dateNum %>% lubridate::month(.)},
                Day  = {dateNum %>% lubridate::day(.)})

# March to October Growing Season - example for a single day
weather <- weather %>% 
  dplyr::filter(., 
                Mon >= 3, Mon <= 10)


#merge in grid weights
weather <- weather %>% dplyr::mutate(., tAvg = (tMax + tMin)/2)

weather_list_temp <- list
weather_list_temp <- lapply(seq(0,45,1), function(i){
  
   w <- weather %>%
    dplyr::mutate(!!paste0("time", i, "C") := dplyr::case_when( tMax <= i ~ 0,
                                                                i <= tMin ~ 1,
                                                                TRUE ~ acos((2*i - tMax - tMin)/(tMax - tMin))/base::pi)) 
    w %>% dplyr::select(., !!paste0("time", i, "C")) 
})

weather <- weather_list_temp %>% do.call("cbind",.) %>% cbind(weather, .) #combine binned weather data with weather temp data generated


weather_list_exp <- list
weather_list_exp <- lapply(seq(from = 0, to = 44, by = 1), function(j){
  
  w <- weather %>%
    dplyr::mutate(., !!paste0("exp", j) := get(paste0("time", j, "C")) - get(paste0("time", j+1, "C")))
    w %>% dplyr::select(., !!paste0("exp", j)) 
})

weather <- weather_list_exp %>% do.call("cbind", .) %>% cbind(weather, .)

# degree days above some threshold
weather_list_dday <- list
weather_list_dday <- lapply(seq(from = 0, to = 45, by = 1), function(k){
  
  w <- weather %>%
    dplyr::mutate(tAvg = (tMin + tMax)/2,
                  tempSave = acos((2*k - tMax - tMin)/(tMax - tMin)),
                  !!paste0("dday", k, "C") := dplyr::case_when( tMax <= k ~ 0,
                                                                k <= tMin ~ tAvg - k,
                                                                TRUE ~ ( (tAvg - k)*(tempSave) + (tMax - tMin)*sin(tempSave)/2)/base::pi))
  
  w %>% dplyr::select(., !!paste0("dday", k, "C"))
})

weather <- weather_list_dday %>% do.call("cbind", .) %>% cbind(weather, .)
rm(weather_list_dday, weather_list_exp, weather_list_temp)

#collapse by aggregation level, weighted average across grids;
# temp_weather_mean <- weather %>% 
#   group_by(dateNum, Year) %>%
#   summarise_at(.tbl = ., .vars = vars(tMax, tMin, tAvg), funs(weighted.mean(x = ., w = cropArea)))

temp_weather_mean <- weather %>% 
  group_by(dateNum, Year) %>%
  summarise_at(.tbl = ., .vars = vars(tMax, tMin, tAvg), funs(mean(x = ., na.rm = TRUE)))

# temp_agg_mean <- weather %>%
#   group_by(dateNum, Year) %>%
#   summarise_at(.tbl = ., .vars = vars(prec, time0C:time45C, exp0:exp44, dday0C:dday45C), funs(weighted.mean(x = ., w = cropArea)))

temp_agg_mean <- weather %>%
  group_by(dateNum, Year) %>%
  summarise_at(.tbl = ., .vars = vars(prec, time0C:time45C, exp0:exp44, dday0C:dday45C), funs(mean(x = ., na.rm = TRUE)))


temp_weather_year <- temp_weather_mean %>% 
  group_by(Year) %>%
  summarise_at(.tbl = ., .vars = vars(tMin, tMax, tAvg), funs(mean(.)))

temp_agg_year <- temp_agg_mean %>%
  group_by(Year) %>%
  summarise_at(.tbl = ., .vars = vars(prec, exp0:exp44, dday0C:dday45C), funs(sum(.)))


#create master-level weather dataset
weatherData <- temp_weather_year %>% dplyr::left_join(x = ., y = temp_agg_year, by = c("Year"))


#merge yield and weather data
MergedYieldWeather <- hay_yield %>% dplyr::inner_join(x = ., y = weatherData, by =c("Year")) %>%
  dplyr::mutate(., Year = as.factor(Year))

#create bins which are 3C wide
exp_bins_3C_list <- list()
exp_bins_3C_list <- lapply(seq(from = 0, to = 36, by = 3), function(b){
  #create exposure bins
  ip1 = b + 1
  ip2 = b + 2

  w <- MergedYieldWeather %>%
    dplyr::mutate(., !!paste("bin", b, ip2, sep = "_") := get(paste("exp", b, sep ="")) + get(paste("exp", ip1, sep ="")) + get(paste("exp", ip2, sep = "")))
  w %>% dplyr::select(., !!paste("bin", b, ip2, sep = "_"))
})

MergedYieldWeather <- exp_bins_3C_list %>% do.call("cbind", .) %>% cbind(MergedYieldWeather, .) %>%
  dplyr::mutate(., !!paste("bin", "39", "inf", sep = "_") := {dplyr::select(., exp39:exp44) %>% apply(.,1,sum)})

#estimate panel model clustered by Year
yield_lm_1 <- lm(formula = ly ~ poly(Trend, degree = 2) + poly(prec, degree = 2) + 
                   bin_0_2 + bin_3_5 + bin_6_8 + bin_9_11 + bin_12_14 + bin_15_17 + bin_18_20 + 
                   bin_21_23 + bin_24_26 + bin_27_29 + bin_30_32 + bin_33_35 + bin_36_38 + bin_39_inf,
                 data = MergedYieldWeather)
yield_lm_1$clse <- multiwayvcov::cluster.vcov(yield_lm_1, MergedYieldWeather$Year)
yield_lm_1_cluster_year_output <- lmtest::coeftest(yield_lm_1, yield_lm_1$clse) %>% round(.,4)

#Estimate the optimal knots (i.e. cut points) for a piecewise linear function
optimal_knots = list()
optimal_knots <- lapply(seq(from = 10, to = 20, by = 1), function(c1){
  lapply(seq(from = 29, to = 35, by = 1), function(c2){
    w <- MergedYieldWeather %>% dplyr::mutate(., !!paste("dday0", c1, sep = "_") := get("dday0C") - get(paste("dday", c1, "C", sep = "")),
                                              !!paste("dday", c1, c2, sep = "_") := get(paste("dday",c1, "C", sep = "")) - get(paste("dday", c2, "C", sep = "")),
                                              !!paste("dday", c2, "inf", sep = "_") := get(paste("dday",c2, "C", sep = ""))) %>% 
      dplyr::mutate(., Trend2 = {Trend^2}, prec2 = {prec^2}) %>%
      dplyr::select(., ly, Trend, Trend2, prec, prec2,
                    !!paste("dday0", c1, sep = "_"),
                    !!paste("dday", c1, c2, sep = "_"),
                    !!paste("dday", c2, "inf", sep = "_")) %>%
      lm(formula = ly ~ ., data = .) %>% summary %>% {.$r.squared}
    
    cbind(c1,c2,w)
    
  })
})

#create data.frame with cut values and Rsq
Rsq_10 <- lapply(1:11, function (i) {
  optimal_knots[[i]] %>% do.call(rbind,.) %>% matrix(., ncol = 3)
  }) %>% 
  do.call("rbind",.) %>% as.data.frame() %>% dplyr::rename(., cut1 = V1, cut2 = V2, Rsq = V3)

#find the max Rsq and return the cut values 
optimal_cut <- Rsq_10[which.max(Rsq_10$Rsq),]

#use these cut values to create bins -> estimate piecewise regression clustered at the year level
reg_data_preferred <- MergedYieldWeather %>% dplyr::mutate(., !!paste("dday", "0", optimal_cut$cut1, sep = "_") := get("dday0C") - get(paste("dday", optimal_cut$cut1, "C", sep = "")),
                                                           !!paste("dday", optimal_cut$cut1, optimal_cut$cut2, sep = "_") := get(paste("dday",optimal_cut$cut1, "C", sep = "")) - get(paste("dday", optimal_cut$cut2, "C", sep = "")),
                                                           !!paste("dday", optimal_cut$cut2, "inf", sep = "_") := get(paste("dday",optimal_cut$cut2, "C", sep = ""))) %>% 
  dplyr::mutate(., Trend2 = {Trend^2}, prec2 = {prec^2}) %>%
  dplyr::select(., ly, Trend, Trend2, prec, prec2,
                !!paste("dday", "0", optimal_cut$cut1, sep = "_"),
                !!paste("dday", optimal_cut$cut1, optimal_cut$cut2, sep = "_"),
                !!paste("dday", optimal_cut$cut2, "inf", sep = "_"))

yield_lm_1_optimal_cut <-  reg_data_preferred %>%
  lm(formula = ly ~ ., data = .)
yield_lm_1_optimal_cut$clse <- multiwayvcov::cluster.vcov(yield_lm_1_optimal_cut, MergedYieldWeather$Year)
yield_lm_1_optimal_cut_cluster_year_output <- lmtest::coeftest(yield_lm_1_optimal_cut, yield_lm_1_optimal_cut$clse)


#Piecewise linear marginal effect of temp
#Need to fix the way the SEmargeff is calculated
reg_data_preferred <- reg_data_preferred %>% dplyr::mutate(., 
                                     temp = seq(from = 0, to = nrow(.)-1, by = 1),
                                     I0 = 1, 
                                     I10 = case_when(temp < 10 ~ 0, TRUE ~ 1), 
                                     I30 = case_when(temp < 32 ~ 0, TRUE ~ 1),
                              margeff = (1-I10)*(yield_lm_1_optimal_cut_cluster_year_output[6,1]*(temp-0)) 
                              + (I10)*(1-I30)*(yield_lm_1_optimal_cut_cluster_year_output[6,1]*(10) + yield_lm_1_optimal_cut_cluster_year_output[7,1]*(temp-10))
                              + (I30)*(yield_lm_1_optimal_cut_cluster_year_output[6,1]*(10) + yield_lm_1_optimal_cut_cluster_year_output[7,1]*(30-10) + yield_lm_1_optimal_cut_cluster_year_output[8,1]*(temp-30)),
                              SEmargeff = 0.013,
                              Lower = margeff - 1.96*SEmargeff,
                              Upper = margeff + 1.96*SEmargeff)

# FIGURE 3
reg_data_preferred %>%
  dplyr::filter(., temp <= 45) %>%
  # dplyr::select(., temp, margeff, Lower, Upper) %>% 
  # reshape2::melt(., id = "temp") %>%
  ggplot(., aes(x = temp, y = margeff)) +
  geom_line() +
  geom_point() +
  scale_colour_discrete("") +
  scale_linetype_manual("", values=c(1,2,1,2)) +
  scale_shape_manual("", values=c(17,17,16,16)) + theme_classic() +
  xlab("Temperature (C)") +  ylab ("Hay Log-yield (tons/acre)") 




