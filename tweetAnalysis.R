library(rtweet)
library(tidyverse)
library(qdap)
library(tidytext)
library(tidyr)
library(dplyr)
library(ggpubr)
library(Metrics)
library(sp)
library(automap)
library(gstat)
library(ggmap)
library(ggplot2)
library(tsibble)
library(fable)
library(feasts)
library(SnowballC)
source("tweet_calculation_func.R")
source("info_cal_jidt_func.R")

# Register a Google API.
# ggmap_key <- "AIzaSyB055tAEERlsleH1Xf83-JqAa530V7roTk"
# register_google(key = ggmap_key, write = TRUE)

# name of Location
loc = "myanmar"
start_date_data <- as.Date("2020-06-01")
end_date_data <- as.Date("2020-07-22")

locCode = ifelse(loc == "delhi","DL",NA)

# List the files of csvs
list_of_files <- list.files(path = paste0("./tweetData/", loc, "/"), recursive = TRUE,
                            pattern = "\\.csv", 
                            full.names = TRUE)

tweet <- list_of_files %>%
  purrr::set_names(.) %>%
  map_df(.f = ~read_twitter_csv(file = .x), .id = "FileName") %>%
  select(user_id, status_id, created_at, text, retweet_count, coords_coords)

# subset of tweet: misinformed tweet
# misinform_tweet <- tweet %>%
#   filter(str_detect(text, "fake|misinformation|^''lie|false"))
# tweet <- misinform_tweet
#--

##---- 1. Added by Jie on May 19, extract volumn and sentiment from tweet ----
rt_vol_daily <- tweet_daily_vol_func(tweet)
# mean_stm_daily <- stm_labMT_daily_func(tweet)   # Function created by Jie
tweet_sentiment <- stm_labMT_daily_func_elroy(tweet) # Function created by Jie, but using Elroy's method.

mean_stm_daily <- tweet_sentiment %>%
  mutate(tweetDate = as.Date( strftime(day_created, format = "%Y-%m-%d"))) %>%
  group_by(tweetDate) %>%
  summarise(mean_daily_stm = mean(Sent, na.rm = TRUE)) %>%
  ungroup()

#-- For misinformation data filling
# rt_vol_daily <- data_fill_by_date(start_date_data,end_date_data,rt_vol_daily,0)
# mean_stm_daily <- data_fill_by_date(start_date_data,end_date_data,mean_stm_daily,5)
#--

vol_stm_daily <- cbind(rt_vol_daily,mean_daily_stm = mean_stm_daily[,2])

##---- end ----

##---- 2. Added by Jie on May 19, load epi-data online and save epi-data and tweet-data in csv files ----

if (loc == "delhi"){
  # India data from: https://www.kaggle.com/imdevskp/covid19-corona-virus-india-dataset
  # https://www.hindustantimes.com/coronavirus/state/delhi-coronavirus-updates-covid-19-pandemic-latest-news/
  # New Delhi: since Apr 18
  delhi_cases <- csv_epi_india_func("Delhi") %>%
    select(Date,newCases,activeCases) %>%
    filter(Date >= start_date_data-1)
  
  #-- fill the date without valid data --#
  hosp_omit_df <- data.frame(omit_date = as.Date(c("2020-06-26","2020-07-01","2020-07-04","2020-07-06","2020-07-18")),
                             omit_hosp = c(27657,27007,25940,25620,16711))
  
  day_seq <- seq(delhi_cases$Date[1],delhi_cases$Date[nrow(delhi_cases)],by = "day")
  df_data_add <- data.frame(Date=day_seq,c=0)
  df_data_filled <- merge(delhi_cases,df_data_add,by="Date",all=TRUE)
  df_data_filled$c <- NULL
  
  delhi_cases <-  df_data_filled %>%
    left_join(hosp_omit_df, by = c("Date" = "omit_date")) %>%
    mutate(activeCases = ifelse(is.na(activeCases), omit_hosp, activeCases)) %>%
    select(-omit_hosp)
  #--    --#
  
  delhi_city_population_rate <- 0.65
  daily_case_till_0625 <- delhi_cases$newCases[2:nrow(delhi_cases)][1:69]  # till 6.25
  daily_case_since_0626 <- diff(c(70390,73780,77240,80188,83077,85161,87360,89802,92175,94695,97200,99444,
                                  100823,102831,104864,107051,109140,110921,112494,113740,115346,116993,
                                  118645,120107,121582,122793,123747,125096))
  daily_case <- round(c(daily_case_till_0625,daily_case_since_0626)*delhi_city_population_rate)
  hospital <- round(delhi_cases$activeCases[2:nrow(delhi_cases)]*delhi_city_population_rate) # since 2020-4-18
  daily_new_hosp <- round(diff(delhi_cases$activeCases)*delhi_city_population_rate) # since 2020-4-18
  
  recordDate <- seq(start_date_data,start_date_data+length(daily_case)-1,by="day")
  
  # keep the length of tweet data identical to that of epi-data
  vol_stm_daily <- vol_stm_daily %>%
    filter(recordDate >= start_date_data & recordDate <= start_date_data+length(daily_case)-1)
} else if(loc == "mumbai") {
  # Mumbai in Maharashtra state, since Apr 18
  mumbai_cases <- csv_epi_india_func("Maharashtra") %>%
    select(Date,newCases,activeCases) %>%
    filter(Date >= start_date_data-1)
  
  #-- fill the date without valid data --#
  hosp_omit_df <- data.frame(omit_date = as.Date(c("2020-06-26","2020-07-01","2020-07-04","2020-07-06","2020-07-18")),
                             omit_hosp = c(65829,79075,86040,87681,126926))
  
  day_seq <- seq(mumbai_cases$Date[1],mumbai_cases$Date[nrow(mumbai_cases)],by = "day")
  df_data_add <- data.frame(Date=day_seq,c=0)
  df_data_filled <- merge(mumbai_cases,df_data_add,by="Date",all=TRUE)
  df_data_filled$c <- NULL
  
  mumbai_cases <-  df_data_filled %>%
    left_join(hosp_omit_df, by = c("Date" = "omit_date")) %>%
    mutate(activeCases = ifelse(is.na(activeCases), omit_hosp, activeCases)) %>%
    select(-omit_hosp)
  #--    --#
  
  mumbai_city_population_rate <- 0.2
  daily_case_till_0625 <- mumbai_cases$newCases[2:nrow(mumbai_cases)][1:69] # since 2020-4-18
  daily_case_since_0626 <- diff(c(142900,147741,152765,159133,164626,169883,174761,180298,186626,
                                  192990,200064,206619,211987,217121,223724,230599,238461,246600,
                                  254427,260924,267665,275640,284281,292589,300937,310455,318695,327031))
  daily_case <- round(c(daily_case_till_0625,daily_case_since_0626)*mumbai_city_population_rate)
  hospital <- round(mumbai_cases$activeCases[2:nrow(mumbai_cases)]*mumbai_city_population_rate) # since 2020-4-18
  daily_new_hosp <- round(diff(mumbai_cases$activeCases)*mumbai_city_population_rate) # since 2020-4-18
  
  recordDate <- seq(start_date_data,start_date_data+length(daily_case)-1,by="day")
  
  vol_stm_daily <- vol_stm_daily %>%
    filter(recordDate >= start_date_data & recordDate <= start_date_data+length(daily_case)-1)
} else if(loc == "jakarta") {
  # Jakarta data from: https://github.com/open-covid-19/data
  #                    https://corona.jakarta.go.id/en/data-pemantauan
  # data from Apr 18
  # daily_case_jkt
  # hospital_jkt: numbers of PDP in hospital + numbers of cases in ICU
  daily_case <- diff(c(2823,2902,3033,3112,3279,3399,3506,3605,3681,3746,3832,3950,4033,4138,
                       4283,4355,4417,4472,4641,4709,4775,4901,4958,5140,5195,5303,5437,5617,5679,
                       5795,5922,5996,6053,6150,6220,6316,6443,6561,6628,6689,6826,6929,7053,7151,
                       7272,7383,7459,7539,7600,7684,7786,7946,8037,8276,8423,8552,8628,8748,8863,
                       8968,9092,9209,9385,9525,9703,9830,9957,10123,10277,10472,10640,10853,10985,
                       11080,11276,11482,11677,11824,12039,12295,12526,12725,13069,13359,13598,13957,
                       14361,14639,14914,15173,15466,15707,16038,16351,16712,17153,17529))
  # hospital <- c(1373,1468,1476,1480,1486,1496,1499,860,871,885,903,945,969,982,
  #               997,994,1001,1015,1022,1034,1060,1065,1073,1103,1233,587,
  #               599,680,558,575,586,507,585,585,651,652,784,658,722,810,910,
  #               1005,1105,816,1134,1268,1281,1372,1526,1424,1486,1361,1387,1537,
  #               1438,1436,1143,1148,1199,1312,1211,1201,931,962,763,712,776,823,776,878,849,
  #               690,770,753,736,770,804,690,711,718,731,740,761,756,818,790,734,821,957,943,800) + # will no longer update
  hospital <- c(1727,1769,1839,1826,1935,1985,2010,1988,1947,1952,1950,2024,2002,2073,
      2151,2089,2062,2080,2146,2195,2196,2281,2312,2360,2258,1843,1833,
      1877,1900,1908,1932,1946,1936,1969,1955,1975,2006,2031,2044,2044,
      2034,2055,2007,1848,1823,1794,1743,1699,1670,1633,1635,1445,1448,
      1442,1427,1446,1424,1419,1368,1390,1416,1402,1377,1382,1340,1287,
      1310,1319,1349,1338,1330,1355,1322,1027,951,889,804,736,646,584,
      495,405,417,451,476,495,554,597,619,691,777,826,892,923,1026,1073,1193) # since 2020-4-17, Only ICU
  
  daily_new_hosp <- diff(hospital) # since 2020-4-18
  hospital <- hospital[2:length(hospital)] # since 2020-4-18
  
  recordDate <- seq(start_date_data,start_date_data+length(daily_case)-1,by="day")
  
  vol_stm_daily <- vol_stm_daily %>%
    filter(recordDate >= start_date_data & recordDate <= start_date_data+length(daily_case)-1)
} else if (loc == "bangkok"){
  # Bangkok data defined as 50% of the national data: https://www.worldometers.info/coronavirus/country/thailand/
  # data from Apr 18
  # daily_case_bkk: 50% of daily cases 
  # hospital_bkk: defined as 50% of active cases
  daily_case <- ceiling(c(33,32,27,19,15,13,15,53,15,9,7,9,7,6,6,3,
                          18,1,1,3,8,4,5,6,2,0,0,7,0,3,3,2,1,3,0,3,0,2,3,9,
                          11,11,1,4,1,1,1,17,1,2,8,7,2,4,0,4,5,1,0,0,0,6,5,1,1,3,5,1,1,4,0,0,7,2,2,
                          6,1,5,5,5,0,2,5,0,14,1,3,7,5,4,3,7,3,0,5,6)/2)
  hospital <- ceiling(c(964,899,790,746,655,425,359,314,309,277,270,232,228,213,187,180,176,
                        193,187,173,165,161,161,159,163,163,117,112,115,114,116,118,120,90,
                        84,71,68,63,57,59,66,63,74,59,61,60,59,58,75,73,75,82,88,90,86,
                        80,84,89,90,90,84,81,86,80,71,72,71,75,73,62,64,51,51,58,57,56,62,56,
                        61,61,65,65,65,59,57,70,71,72,78,82,83,85,92,95,96,92,98)/2) # since 2020-4-17
  
  daily_new_hosp <- diff(hospital) # since 2020-4-18
  hospital <- hospital[2:length(hospital)] # since 2020-4-18
  
  recordDate <- seq(start_date_data,start_date_data+length(daily_case)-1,by="day")
  
  vol_stm_daily <- vol_stm_daily %>%
    filter(recordDate >= start_date_data & recordDate <= start_date_data+length(daily_case)-1)
} else if (loc == "new york city"){
  start_date_data <- as.Date("2020-04-09")
  daily_case <- c(5041,4500,3710,2870,3301,4128,3865,3517,3574,2164,2347,3776,3054,3455,2841,2537)
  hospital <- c(1398,1312,1078,980,1204,1062,978,864,889,643,587,712,649,591,527,541)
  recordDate <- seq(start_date_data,start_date_data+length(daily_case)-1,by="day")
  vol_stm_daily <- vol_stm_daily %>%
    filter(recordDate <= start_date_data+length(daily_case)-1)
} else if (loc == "myanmar") {
  # data since 2020-06-01
  # data source: https://www.worldometers.info/coronavirus/country/myanmar/
  start_date_data <- as.Date("2020-06-01")
  daily_case <- c(4,4,1,3,0,4,2,2,2,2,12,1,0,0,1,0,0,1,23,1,3,0,2,1,0,0,0,6,0,0,4,1,2,7,0,3,0,1,2,
                  7,4,1,5,1,0,2,0,1,1,0,0,2)
  hospital <- c(80,84,83,82,82,79,78,80,79,81,77,89,90,88,88,81,77,71,70,88,85,84,84,82,79,76,72,71,
                75,72,71,75,75,63,67,66,65,65,61,59,64,64,64,69,70,65,63,62,61,59,57,55,57) # since 2020-05-31
  
  daily_new_hosp <- diff(hospital) # since 2020-6-1
  hospital <- hospital[2:length(hospital)] # since 2020-6-1
  
  recordDate <- seq(start_date_data,start_date_data+length(daily_case)-1,by="day")
  vol_stm_daily <- vol_stm_daily %>%
    filter(recordDate >= start_date_data & recordDate <= start_date_data+length(daily_case)-1)
}
epi_data <- data.frame(recordDate,daily_case,hospital,daily_new_hosp)
write.csv(epi_data,paste(loc,"-epi-data.csv",sep = ""))

# write the tweet table in csv files.
write.csv(vol_stm_daily,paste(loc,"-daily-vol-stm.csv",sep = ""))

week_tweet_data <- week_tweet_data_func(vol_stm_daily)
write.csv(week_tweet_data,paste(loc,"-week-vol-stm.csv",sep = ""))

tweet_cut <- tweet %>%
  filter(created_at >= start_date_data)
retweet_week <- retweet_vol_func(tweet_cut)
write.csv(retweet_week,paste(loc,"-week-retweet.csv",sep = ""))
# epi_data %>%
#   gather(key = "key", value = "value", -recordDate) %>%
#   filter(key %in% c("daily_case", "hospital")) %>%
#   ggplot(aes(x = recordDate, y = value, color = key)) +
#   # geom_point(size = 3) +
#   geom_line(size = 1) +
#   ylim(0,4000) +
#   xlab('Date') + ylab("Daily cases, hospitalizations") +
#   scale_color_manual(labels = c('Cases','Hospitalizations'),
#                      values=c('deepskyblue2','orange')) +
#   scale_x_date(date_breaks = '2 day', date_labels = "%b %d") +
#   theme(legend.position = 'top',
#         legend.direction = 'horizontal',
#         legend.margin = margin(t = 0,unit = 'cm'),
#         legend.text = element_text(hjust=0.5, vjust=0.5,size = 20),
#         axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 12)) +
#   tweetPlotTheme
# ggsave(paste(loc,"-daily-cases-hosp.eps",sep = ""), width = 10, height = 6.18)

##---- end ----

##---- 3. Added by Jie on Jun 1, mis-index calculation ----
# week_mis_indicator <- week_index_cal_func(start_date_data,vol_stm_daily,epi_data)
# week_mis_indicator_df <- data.frame(
#   week_no = seq(length(week_mis_indicator$te_stm_case_week)),
#   te_stm_case = as.numeric(week_mis_indicator$te_stm_case_week),
#   te_stm_hosp = as.numeric(week_mis_indicator$te_stm_hosp_week),
#   cc_stm_case = as.numeric(week_mis_indicator$cc_stm_case_week),
#   cc_stm_hosp = as.numeric(week_mis_indicator$cc_stm_hosp_week)
# )
# write.csv(week_mis_indicator_df,paste(loc,"-mis-week-indicator.csv",sep = ""))
# 
# epi_stm_vol_data <- cbind(epi_data,vol_stm_daily[,2:5])
# lm_case_stm <- lm(formula = daily_case ~ mean_daily_stm, data = epi_stm_vol_data)
# lm_hosp_stm <- lm(formula = daily_new_hosp ~ mean_daily_stm, data = epi_stm_vol_data)
# epi_stm_vol_data$predicted_mis_case <- round(lm_case_stm$coefficients[1] +
#   lm_case_stm$coefficients[2] * epi_stm_vol_data$mean_daily_stm)
# epi_stm_vol_data$predicted_mis_hosp <- round(lm_hosp_stm$coefficients[1] +
#   lm_hosp_stm$coefficients[2] * epi_stm_vol_data$mean_daily_stm)
# epi_stm_vol_data <- epi_stm_vol_data %>%
#   select(recordDate,daily_case,hospital,daily_new_hosp,predicted_mis_case,predicted_mis_hosp)
# week_mis_epi_data_df <- week_mis_epidata_func(epi_stm_vol_data)  # function only for mis-informed tweets.
# 
# week_mis_epi_data_df$gap_mis_case <- (week_mis_epi_data_df$pre_mis_case_week-week_mis_epi_data_df$daily_case_week)/week_mis_epi_data_df$daily_case_week
# week_mis_epi_data_df$gap_mis_hosp <- (week_mis_epi_data_df$pre_mis_hosp_week-week_mis_epi_data_df$daily_new_hosp_week)/week_mis_epi_data_df$daily_new_hosp_week
# 
# write.csv(week_mis_epi_data_df,paste(loc,"-mis-epi-week-pre-obs-gap.csv",sep = ""))

##---- end ----

##---- 4. Added by Jie on May 20, index calculation ----
# update for weekly index calculation by Jie on May 22

week_indicator <- week_index_cal_func(start_date_data,vol_stm_daily,epi_data)

week_indicator_df <- data.frame(
  week_no = seq(length(week_indicator$te_stm_case_week)),
  te_stm_case = as.numeric(week_indicator$te_stm_case_week),
  te_stm_hosp = as.numeric(week_indicator$te_stm_hosp_week),
  cc_stm_case = as.numeric(week_indicator$cc_stm_case_week),
  cc_stm_hosp = as.numeric(week_indicator$cc_stm_hosp_week),
  week_end_date = as.Date(unlist(week_indicator$week_end_date),origin = "1970-01-01")
)
write.csv(week_indicator_df,paste(loc,"-week-indicator.csv",sep = ""))
##---- end ----

##---- 5. Added by Jie on May 22, predicted cases and hp using Kriging ----
# Krige data

# predicted_epi_data <- week_krige_func(loc,start_date_data,tweet_sentiment,epi_data)

predicted_epi_data <- predict_arima_func(start_date_data,tweet_sentiment,epi_data)

pre_daily_case_hosp <- predict_arima_daily_func(start_date_data,tweet_sentiment,epi_data)
write.csv(pre_daily_case_hosp,paste(loc,"-pre-daily-case-hosp.csv",sep = ""))

week_krige_data_df_func(loc,start_date_data,tweet_sentiment,epi_data)

# predicted_epi_data <- week_krige_func_update(start_date_data,tweet_sentiment,epi_data)
# week_krige_data_df_func_update(loc,tweet_sentiment,epi_data)

##---- end ----

##---- 6. Visualization for validation ----

week_epi_data_df <- week_epi_data_func(epi_data)
# week_epi_data_df <- week_epidata_cumulative_func(epi_data)

predicted_epi_data_df <- data.frame(
  week_no = 2:(length(predicted_epi_data$predicted_case_week)+1),
  predicted_case = as.numeric(predicted_epi_data$predicted_case_week), # difference between using "<-" and "="
  predicted_hosp = as.numeric(predicted_epi_data$predicted_hosp_week),
  case_95_lower = as.numeric(predicted_epi_data$pre_case_lower),
  case_95_upper = as.numeric(predicted_epi_data$pre_case_upper),
  hosp_95_lower = as.numeric(predicted_epi_data$pre_hosp_lower),
  hosp_95_upper = as.numeric(predicted_epi_data$pre_hosp_upper)
)
epi_week_pre_obs <- full_join(week_epi_data_df,predicted_epi_data_df,by = "week_no")
epi_week_pre_obs$gap_case <- (epi_week_pre_obs$predicted_case-epi_week_pre_obs$daily_case_week)/epi_week_pre_obs$daily_case_week
epi_week_pre_obs$gap_hosp <- (epi_week_pre_obs$predicted_hosp-epi_week_pre_obs$daily_new_hosp_week)/epi_week_pre_obs$daily_new_hosp_week

write.csv(epi_week_pre_obs,paste(loc,"-epi-week-pre-obs-gap.csv",sep = ""))

epi_week_pre_obs %>%
  gather(key = "key", value = "value", -week_no) %>%
  filter(key %in% c("daily_case_week", "predicted_case")) %>%
  ggplot(aes(x = week_no, y = value, color = key)) +
  geom_point(size = 3) +
  geom_line() +
  xlab('Week (since Apr 18)') + ylab('Observed,predicted cases') +
  scale_color_manual(labels = c('observed','predicted'),
                     values=c('seagreen4','red')) +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0,unit = 'cm'),
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  tweetPlotTheme
ggsave(paste(loc,"-cases-preVSobs-cum.eps",sep = ""), width = 10, height = 6.18)

case_preVSobs <- epi_week_pre_obs %>%
  ggplot +
  aes(x = daily_case_week,y = predicted_case) +
  geom_point(size = 3,color = 'seagreen4') +
  geom_smooth(method = 'lm',formula = y~x,se = F,color = 'seagreen4') +
  labs(x = "Observed cases",y = "Predicted cases",title = paste("RMSE=",round(rmse_val,2))) +
  stat_regline_equation(aes(label =  ..eq.label..),formula = y~x, size = 5,
                        label.x = min(epi_week_pre_obs$daily_case_week),
                        label.y = max(epi_week_pre_obs$predicted_case,na.rm = T)) +
  # stat_regline_equation(aes(label =  ..eq.label..),formula = y~x, size = 5,
  #                       label.x = 6,
  #                       label.y = 12.5) +
  tweetPlotTheme
case_preVSobs
ggsave(paste(loc,"-case-preVSobs-Linear.eps",sep = ""), width = 10, height = 6.18)

y <- 96-0.59*epi_week_pre_obs$daily_case_week
rmse_val <- rmse(y[2:length(y)],epi_week_pre_obs$predicted_case[2:nrow(epi_week_pre_obs)])

epi_week_pre_obs %>%
  gather(key = "key", value = "value", -week_no) %>%
  filter(key %in% c("daily_hosp_week", "predicted_hosp")) %>%
  ggplot(aes(x = week_no, y = value, color = key)) +
  geom_point(size = 3) +
  geom_line() +
  xlab('Week (since Apr 18)') + ylab('Observed,predicted hospitalization') +
  scale_color_manual(labels = c('observed','predicted'),
                     values=c('seagreen4','red')) +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0,unit = 'cm'),
        legend.text = element_text(hjust=0.5, vjust=0.5,size = 20)) +
  tweetPlotTheme
ggsave(paste(loc,"-hosp-preVSobs-cum.eps",sep = ""), width = 10, height = 6.18)

hosp_preVSobs <- epi_week_pre_obs %>%
  ggplot +
  aes(x = daily_hosp_week,y = predicted_hosp) +
  geom_point(size = 3,color = 'seagreen4') +
  geom_smooth(method = 'lm',formula = y~x,se = F,color = 'seagreen4') +
  labs(x = "Observed hospitalizations",y = "Predicted hospitalizations",title = paste("RMSE=",round(rmse_val,2))) +
  # stat_regline_equation(aes(label =  ..eq.label..),formula = y~x, size = 5,
  #                       label.x = 250,
  #                       label.y = 310) +
  stat_regline_equation(aes(label =  ..eq.label..),formula = y~x, size = 5,
                        label.x = min(epi_week_pre_obs$daily_hosp_week),
                        label.y = max(epi_week_pre_obs$predicted_hosp,na.rm = T)) +
  tweetPlotTheme
hosp_preVSobs
ggsave(paste(loc,"-hosp-preVSobs-Linear.eps",sep = ""), width = 10, height = 6.18)

y <- -68+0.6*epi_week_pre_obs$daily_hosp_week
rmse_val <- rmse(y[2:length(y)],epi_week_pre_obs$predicted_hosp[2:nrow(epi_week_pre_obs)])

##---- end ----


# Plot theme
tweetPlotTheme <- theme(panel.background = element_blank(),
                        panel.grid.major = element_line(color = 'gray'),
                        panel.grid.minor = element_line(color = 'gray'),
                        axis.line = element_line(),
                        panel.border = element_rect(color = "black", fill=NA),
                        axis.title = element_text(size=20),
                        axis.text = element_text(size = 15),
                        plot.title = element_text(size=16))

# Time plot of tweets
ts_plot(tweet, "days") +
  geom_point() +
  labs(title = loc, x = "Days", y = "Number of Tweets") +
  tweetPlotTheme
  
# Unnest tweets
unnest_tweet <- tweet %>%
  # replace abbreviations and contractions
  mutate(text = replace_abbreviation(text) %>% 
           replace_symbol() %>%
           replace_contraction() %>%
           replace_ordinal() %>%
           replace_number()) %>%
  # tokenize, ie. separate a tweet text into its constituent elements
  unnest_tokens("word", "text", token = "tweets", 
                strip_punct = T, strip_url = T)

topBigrams <- tweet %>%
  mutate(text = rm_twitter_url(text) %>%
           str_remove_all(pattern = "[:punct:]") %>%
           str_remove_all(pattern = "[:digit:]")) %>%
  unnest_tokens("word", "text", token = "ngrams", n = 2) %>%
  separate(col = word, into = c("word1", "word2"), sep = " ") %>%
  filter(!(word1 %in% c(stop_words$word, "coronavirus", "covid19", 
                        "#covid19", "#coronavirus", "#covid2019", "amp", "covid", "-", "|", "19"))) %>%
  filter(!(word2 %in% c(stop_words$word, "coronavirus", "covid19", 
                        "#covid19", "#coronavirus", "#covid2019", "amp", "covid", "-", "|", "19"))) %>%
  unite(col = "pairs", c(word1, word2), sep = " ") %>%
  group_by("day_created" = strftime(created_at, format = "%Y-%m-%d"), pairs) %>%
  count() %>%
  ungroup() %>%
  group_by(day_created) %>%
  # change n to whatever number required
  top_n(n = 30, wt = n) %>%
  ungroup()

# top tweets and tweets
topTweets <- tweet %>%
  group_by("day_created" = strftime(created_at, format = "%Y-%m-%d")) %>%
  top_n(5,retweet_count) %>%
  ungroup()

write.csv(topTweets, "./data/topTweets.csv")

# sentiment valence
tweet_sentiment <- unnest_tweet %>%
  # determine sentiments of words
  inner_join(labMT) %>%
  # remove values in between 4 and 6 
  filter(!(4 < happiness_average & happiness_average < 6)) %>%
  ## count the number of positive and negative words per status per user
  group_by(user_id, status_id, coords_coords, 
           "day_created" = strftime(created_at, format = "%Y-%m-%d")) %>%
  summarise(Sent = mean(happiness_average, na.rm = T)) %>%
  ungroup() %>%
  separate(col = "coords_coords", into = c("lng", "lat"), sep = " ") %>%
  mutate(lng = as.numeric(lng),
         lat = as.numeric(lat),
         day_created = as.Date(day_created)) 

topBigramsSentiment <- topBigrams %>%
  separate(col = pairs, into = c("word1", "word2"), sep = " ") %>%
  inner_join(labMT, by = c("word1" = "word")) %>%
  inner_join(labMT, by = c("word2" = "word"), suffix = c("1","2")) %>%
  unite(col = "pairs", c(word1, word2), sep = " ") %>%
  group_by(day_created, pairs) %>%
  summarise(sentiment = (happiness_average1 + happiness_average2)/2) %>%
  ungroup()

tweet_sentiment %>%
  group_by(day_created) %>%
  summarise(meanSent = mean(Sent, na.rm = T)) %>%
  ungroup() %>%
  ggplot(aes(x = as.Date(day_created), y = meanSent, color = meanSent, group = 1)) +
  geom_line(size = 1, color = "black") +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(5.7, 5.9)) +
  labs(x = "Date", y = "Mean Daily Happiness", color = "Happiness",
       title = "New Delhi", subtitle = "Week ending 2020/05/02") +
  tweetPlotTheme

tweet_sentiment %>%
  group_by(day_created) %>%
  summarise(meanSent = mean(Sent, na.rm = T)) %>%
  ungroup() %>%
  mutate(delta_sentiment = order_by(day_created, diff(meanSent,differences = 1))) %>%
  ggplot(aes(x = as.Date(day_created), y = delta_sentiment, 
             color = delta_sentiment, group = 1)) +
  geom_line(size = 1, color = "black") +
  geom_point(size = 3) +
  labs(x = "Date", y = "Delta Positivity", color = "Delta Positivity",
       title = "New Delhi") +
  tweetPlotTheme

# emotion 
tweet_emotions <- unnest_tweet %>%
  inner_join(get_sentiments("nrc")) %>%
  group_by("day_created" = strftime(created_at, format = "%Y-%m-%d"),
           sentiment) %>%
  count() %>%
  ungroup()
  
topBigramsEmotions <- topBigrams %>%
  separate(col = pairs, into = c("word1", "word2"), sep = " ") %>%
  inner_join(y = get_sentiments("nrc"), by = c("word1"="word")) %>%
  inner_join(y = get_sentiments("nrc"), by = c("word2"="word"),suffix = c("1","2"))

emoColors <- c("anger" = "#fe0000", "anticipation" = "#fea853", "disgust" = "#ff54ff", 
                            "fear" = "#009600", "joy" = "#ffff54", "sadness" = "#5151ff", 
                            "surprise" = "#5abdff", "trust" = "#52ff4f")
  
tweet_emotions %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  ggplot(aes(x = as.Date(day_created), y = n, color = sentiment, group = sentiment)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = emoColors, 
                     labels = c("Anger",
                                "Anticipation",
                                "Disgust",
                                "Fear",
                                "Joy",
                                "Sadness",
                                "Surprise",
                                "Trust")) +
  labs(x = "Date", y = "Number of words", color = "Emotion",
       title = "New Delhi") +
  tweetPlotTheme

tweet_sentiment_plot <- tweet_sentiment %>%
  group_by(day_created) %>%
  summarise(nTweets = n_distinct(status_id),
            meanSent = mean(Sent, na.rm = T)) %>%
  ungroup() %>%
  ggplot(aes(x = as.Date(day_created), y = meanSent, color = nTweets, group = 1)) +
  geom_line(size = 1, color = "black") +
  geom_point(size = 3) +
  labs(x = "Date", y = "Mean Positivity", color = "Mean Daily Tweets",
       title = "New Delhi") +
  tweetPlotTheme

# Associations with variables
library("rJava")
.jinit()

# Change location of jar to match yours:
#  IMPORTANT -- If using the default below, make sure you have set the working directory
#   in R (e.g. with setwd()) to the location of this file (i.e. demos/r) !!
.jaddClassPath("./infodynamics-dist-1.5/infodynamics.jar")


teCal_jidt_knl_func <- function(srcArr, dstArr,histLen = 1L, width = 0.5){
  # histLen: 1L as an example; width: 0.5 as an example
  # Create a TE calculator and run it:
  teCalc<-.jnew("infodynamics/measures/continuous/kernel/TransferEntropyCalculatorKernel")
  .jcall(teCalc,"V","setProperty", "NORMALISE", "true") # Normalise the individual variables
  .jcall(teCalc,"V","initialise", histLen, width) # Use history length 1 (Schreiber k=1), kernel width of 0.5 normalised units
  .jcall(teCalc,"V","setObservations", srcArr, dstArr)
  # For copied source, should give something close to expected value for correlated Gaussians:
  result <- .jcall(teCalc,"D","computeAverageLocalOfObservations") # bit
  # result_nat <- result/log2(exp(1)) # bit to nat
  return(result)
}


hosp_data <- read.csv("https://raw.githubusercontent.com/imdevskp/covid-19-india-data/master/patients_data.csv") %>%
  filter(state_code == locCode, current_status == "Hospitalized") %>%
  group_by(date_announced) %>%
  count() %>%
  ungroup() %>%
  mutate(Date = lubridate::dmy(date_announced))

hosp_data2 <- read.csv("https://raw.githubusercontent.com/imdevskp/covid-19-india-data/master/complete.csv") %>%
  filter(`Name.of.State...UT` == "Delhi" )

hosp_weekly <- hosp_data %>%
  group_by("week" = lubridate::week(Date)) %>%
  summarise(hosp_week = sum(n)) %>%
  ungroup() %>%
  mutate(weekly_change = lag(order_by(week, diff(hosp_week)/hosp_week)))

hospBysent <- tweet_sentiment %>%
  mutate(Date = as.Date( strftime(day_created, format = "%Y-%m-%d"))) %>%
  left_join(hosp_data, by = "Date") %>%
  group_by(Date) %>%
  summarise(hosp = mean(n, na.rm = T)/22961, 
            meanSent = mean(Sent, na.rm = T)) %>%
  ungroup()



hospBysent_plot <- hospBysent  %>%
  ggplot(aes(x = hosp, y = meanSent)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_text(aes(label = as.character(strftime(Date, format = "%b-%d"))), 
            vjust = -.5) +
  # scale_y_continuous(limits = c(5.7, 5.9)) +
  # ggpmisc::stat_poly_eq(formula = y ~ x,
  #                     aes(label = paste0("\U025FA = ",
  #                                       stat(coef.ls)[[1]][[2]]
  #                   )
  #                  ),
  #                 size = 8,
  #                parse = F, label.y = "bottom", label.x = "right",
  #               output.type = "numeric"
  # geom = "debug", output.type = "numeric"
  # ) +
  geom_smooth(method = "lm", se = F, color = "darkgreen") +
  labs(x = "Hospitalizations", y = "Mean Daily Happiness") +
  tweetPlotTheme

hospPredictability <- hospBysent %>%
  group_by("week"  = lubridate::week(Date)) %>%
  summarise(te_posTOhosp = teCal_jidt_knl_func(srcArr = meanSent, dstArr = hosp, 1L, 0.5),
            cor_posTOhosp = cor(meanSent, hosp)) %>%
  ungroup() %>%
  mutate(delta_TE = lag(order_by(week, diff(te_posTOhosp)/te_posTOhosp)),
         delta_cor = lag(order_by(week, diff(cor_posTOhosp)/cor_posTOhosp)))

hospPredictability %>%
  ggplot(aes(x = week, y = delta_TE)) +
  geom_point() +
  geom_line()

cases <- read.csv("https://open-covid-19.github.io/data/data.csv") %>%
  filter(RegionName == "Delhi") %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(newCases = lag(order_by(Date, diff(Confirmed)))) %>%
  select(Date, Confirmed, newCases) %>%
  pivot_longer(cols = c(Confirmed, newCases))

cases_plot <- cases  %>%
  ggplot(aes(x = as.Date(Date), y = value, fill = name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "blue") ,
                    labels = c("New Cases", "Old Cases")) +
  labs(x = "Date", y = "No. of Cases", fill = "Type") +
  tweetPlotTheme

casesBysent <- tweet_sentiment %>%
  mutate(Date = as.Date( strftime(day_created, format = "%Y-%m-%d"))) %>%
  left_join(cases, by = "Date") %>%
  filter(name == "newCases") %>%
  group_by(Date) %>%
  summarise(newCases = mean(value, na.rm = T),
            meanSent = mean(Sent, na.rm = T)) %>%
  ungroup() %>%
  mutate(normNewCases = (newCases/21750000)*100000) 

casesPredictability <- casesBysent %>%
  group_by("week" = lubridate::week(Date)) %>%
  summarise(te_posTOcas = teCal_jidt_knl_func(srcArr = meanSent, dstArr = newCases),
            cor_posTOcas = cor(meanSent, newCases, method = "spearman")) %>%
  ungroup() %>%
  mutate(delta_TE = lag(order_by(week, diff(te_posTOcas, differences = 1))))

casesBysent_plot <- casesBysent %>%
  ggplot(aes(x = normNewCases, y = meanSent)) +
  geom_point(size = 4, color = "darkgreen") +
  #geom_text(aes(label = as.character(strftime(hospBysent$Date, format = "%b-%d"))), 
  #         vjust = -.5) +
  geom_smooth(method = "lm", se = F, color = "darkgreen") +
  labs(x = "Incidence Rates (per 100,000)", y = "Mean Daily Positivity") +
  tweetPlotTheme

growthBysent <- tweet_sentiment %>%
  mutate(Date = as.Date( strftime(day_created, format = "%Y-%m-%d"))) %>%
  left_join(cases_ndel, by = "Date") %>%
  pivot_wider(names_from = name, values_from = value) %>%
  group_by(Date) %>%
  summarise(growthRate = mean(newCases, na.rm = T)/mean(Total.Confirmed.cases, na.rm = T),
            meanSent = mean(Sent, na.rm = T)) %>%
  ungroup()

growthBysent_plot <- growthBysent  %>%
  ggplot(aes(x = growthRate, y = meanSent)) +
  geom_point(size = 3, color = "darkgreen") +
  geom_text(aes(label = as.character(strftime(hospBysent$Date, format = "%b-%d"))), 
            vjust = -.5) +
  scale_y_continuous(limits = c(5.7, 5.9)) +
  geom_smooth(method = "lm", se = F, color = "darkgreen") +
  labs(x = "Daily Growth Rate", y = "Mean Daily Happiness") +
  tweetPlotTheme

growPredictability <- growthBysent %>%
  group_by(lubridate::week(Date)) %>%
  summarise(te_posTOgrow = teCal_jidt_knl_func(srcArr = meanSent, dstArr = growthRate),
            cor_posTOgrow = cor(meanSent, growthRate, method = "spearman")) %>%
  ungroup() %>%
  mutate(delta_TE = lag(order_by(week, diff(te_posTOgrow, differences = 1))))

# Spatially Plot hospitals and sentiment
library(ggmap)
tweetSentMap <- tweet_sentiment %>%
  group_by(user_id, lat, lng) %>%
  summarise(meanSent = mean(Sent, na.rm = T)) %>%
  ungroup() 

tweet_loc <- get_googlemap("delhi")

tweet_map <- ggmap(tweet_loc) +
   geom_point(data = tweetSentMap,
             aes(x = as.numeric(lng),
                 y = as.numeric(lat),
                 color = meanSent),
             size = 4) +
  scale_color_gradient2(midpoint = 5) +
  labs(color = "Mean Sentiment", x = "Lon", y = "Lat")
tweet_map
# Krige data
loc_coords <- lookup_coords("new delhi")

p <- list(data.frame("x" = c(loc_coords[[2]][1], loc_coords[[2]][3]),
                     "y" = c(loc_coords[[2]][2], loc_coords[[2]][4])))

randomPoints <- data.frame("lng" = c(runif(10000, min = min(p[[1]][,1]), max = max(p[[1]][,1]))),
                           "lat" = c(runif(10000, min = min(p[[1]][,2]), max = max(p[[1]][,2])))) 

coordinates(randomPoints) <- ~lng+lat
randomGrid <- makegrid(randomPoints)
coordinates(randomGrid) <- ~x1+x2

library(sp)
library(automap)

tweet_sent_coords <- tweet_sentiment %>%
  left_join(hosp_data, by = c("day_created" = "Date")) %>%
  na.omit()

coordinates(tweet_sent_coords) <- ~lng+lat

lzn.kriged <- autoKrige(formula = Sent ~ 1, tweet_sent_coords, 
                         randomGrid
                        )

lzn.kriged.dataframe <- as.data.frame(lzn.kriged$krige_output) %>%
  rename("Sent" = var1.pred)

tweet_sentiment.dataframe <- as.data.frame(tweet_sent_coords)

coordinates(lzn.kriged.dataframe) <- ~x1+x2

lzn.kriged <- autoKrige(formula = n ~ Sent, 
                        input_data = tweet_sent_coords, 
                        new_data = lzn.kriged.dataframe)

lzn.kriged.dataframe.final <- as.data.frame(lzn.kriged$krige_output)

krigePlotMid = round(median(lzn.kriged.dataframe.final$var1.pred))

krige_plot <- tweet_loc %>%
  ggmap() +
  geom_contour_filled(data = lzn.kriged.dataframe.final,
                      aes(x = x1, y = x2, z = var1.pred), 
                      alpha = 0.8) + 
  labs(x = "Lon", y = "Lat", fill = "Hospitalization Rate") +
  theme_minimal()

library(ggpubr)

ggarrange(tweet_map, tweet_cases_plot, tweet_sentiment_plot,
          casesBysent_plot, growthBysent_plot, hospBysent_plot)
