library(rtweet)
library(tidyverse)
library(qdap)
library(tidytext)
library(tidyr)
library(dplyr)
library(ggpubr)
library(sp)
library(automap)
library(ggmap)
library(ggplot2)
source("tweet_calculation_func.R")
source("info_cal_jidt_func.R")

# Register a Google API.
# ggmap_key <- "AIzaSyB055tAEERlsleH1Xf83-JqAa530V7roTk"
# register_google(key = ggmap_key, write = TRUE)

# name of Location
loc = "bangkok"

locCode = ifelse(loc == "delhi",
                 "DL",
                 NA)

# List the files of csvs
list_of_files <- list.files(path = paste0("./tweetData/", loc, "/"), recursive = TRUE,
                            pattern = "\\.csv", 
                            full.names = TRUE)

# Read files and merge them into one file
tweet <- list_of_files %>%
  purrr::set_names(.) %>%
  map_df(.f = ~read_csv(file = .x), .id = "FileName") %>%
  select(user_id, status_id, created_at, text, retweet_count, coords_coords)

##---- 1. Added by Jie on May 19, extract volumn and sentiment from tweet ----
rt_vol_daily <- tweet_daily_vol_func(tweet)
# mean_stm_daily <- stm_labMT_daily_func(tweet)   # Function created by Jie
tweet_sentiment <- stm_labMT_daily_func_elroy(tweet) # Function created by Jie, but using Elroy's method.

mean_stm_daily <- tweet_sentiment %>%
  mutate(tweetDate = as.Date( strftime(day_created, format = "%Y-%m-%d"))) %>%
  group_by(tweetDate) %>%
  summarise(mean_daily_stm = mean(Sent, na.rm = TRUE)) %>%
  ungroup()

vol_stm_daily <- cbind(rt_vol_daily,mean_stm_daily[,2])
##---- end ----

##---- 2. Added by Jie on May 19, load epi-data online ----
if (loc == "delhi"){
  # India data from: https://www.kaggle.com/imdevskp/covid19-corona-virus-india-dataset
  # New Delhi: since Apr 18
  delhi_cases <- csv_epi_india_func("Delhi")
  daily_case <- delhi_cases$newCases[48:nrow(delhi_cases)]
  hospital <- delhi_cases$activeCases[48:nrow(delhi_cases)]
  recordDate <- seq(as.Date("2020-04-18"),delhi_cases$Date[nrow(delhi_cases)],by="day")
  
  # keep the length of tweet data identical to that of epi-data
  vol_stm_daily <- vol_stm_daily %>%
    filter(recordDate <= delhi_cases$Date[nrow(delhi_cases)])
} else if(loc == "mumbai") {
  # Mumbai in Maharashtra state, since Apr 18
  mumbai_cases <- csv_epi_india_func("Maharashtra")
  daily_case <- mumbai_cases$newCases[41:nrow(mumbai_cases)]
  hospital <- mumbai_cases$activeCases[41:nrow(mumbai_cases)]
  recordDate <- seq(as.Date("2020-04-18"),mumbai_cases$Date[nrow(mumbai_cases)],by="day")
  
  vol_stm_daily <- vol_stm_daily %>%
    filter(recordDate <= mumbai_cases$Date[nrow(mumbai_cases)])
} else if(loc == "jakarta") {
  # Jakarta data from: https://github.com/open-covid-19/data
  #                    https://corona.jakarta.go.id/en/data-pemantauan
  # data from Apr 18
  # daily_case_jkt
  # hospital_jkt: numbers of PDP in hospital + numbers of cases in ICU
  daily_case <- diff(c(2823,2902,3033,3112,3279,3399,3506,3605,3681,3746,3832,3950,4033,4138,
                       4283,4355,4417,4472,4641,4709,4775,4901,4958,5140,5195,5303,5437,5617,5679,
                       5795,5922,5996,6053,6150,6220,6316))
  hospital <- c(1468,1476,1480,1486,1496,1499,860,871,885,903,945,969,982,
                997,994,1001,1015,1022,1034,1060,1065,1073,1103,1233,587,
                599,680,558,575,586,507,585,585,651,652) + 
    c(1769,1839,1826,1935,1985,2010,1988,1947,1952,1950,2024,2002,2073,
      2151,2089,2062,2080,2146,2195,2196,2281,2312,2360,2258,1843,1833,
      1877,1900,1908,1932,1946,1936,1969,1955,1975)
  recordDate <- seq(as.Date("2020-04-18"),as.Date("2020-04-18")+length(daily_case)-1,by="day")
  
  vol_stm_daily <- vol_stm_daily %>%
    filter(recordDate <= as.Date("2020-04-18")+length(daily_case)-1)
} else if (loc == "bangkok"){
  # Bangkok data defined as 50% of the national data: https://www.worldometers.info/coronavirus/country/thailand/
  # data from Apr 18
  # daily_case_bkk: 50% of daily cases 
  # hospital_bkk: defined as 50% of active cases
  daily_case <- ceiling(c(33,32,27,19,15,13,15,53,15,9,7,9,7,6,6,3,
                          18,1,1,3,8,4,5,6,2,0,0,7,0,3,3,2,1,3,0)/2)
  hospital <- ceiling(c(899,790,746,655,425,359,314,309,277,270,232,228,213,187,180,176,
                        193,187,173,165,161,161,159,163,163,117,112,115,114,116,118,120,90,84,71)/2)
  recordDate <- seq(as.Date("2020-04-18"),as.Date("2020-04-18")+length(daily_case)-1,by="day")
  
  vol_stm_daily <- vol_stm_daily %>%
    filter(recordDate <= as.Date("2020-04-18")+length(daily_case)-1)
} else if (loc == "new york city"){
  daily_case <- c(5431,5753,5651,3853,3772,6359,6048,5560,5030,4495,3704,2866,3292,4127,
                  3866,3515,3572,2162,2347,3775,3055,3457,2843,2536,1594)
  hospital <- c(1537,1594,1595,1330,1316,1687,1563,1513,1391,1307,1071,968,1191,
                  1059,963,851,896,645,590,712,648,593,517,557,405)
  recordDate <- seq(as.Date("2020-04-01"),as.Date("2020-04-01")+length(daily_case)-1,by="day")
  vol_stm_daily <- vol_stm_daily %>%
    filter(recordDate <= as.Date("2020-04-01")+length(daily_case)-1)
}
epi_data <- data.frame(recordDate,daily_case,hospital)
##---- end ----

##---- 3. Added by Jie on May 20, index calculation ----
# update for weekly index calculation by Jie on May 22

week_indicator <- week_index_cal_func(as.Date("2020-04-18"),vol_stm_daily,epi_data)

week_indicator_df_bkk <- data.frame(
  week_no = seq(length(week_indicator$te_stm_case_week)),
  te_stm_case = as.numeric(week_indicator$te_stm_case_week),
  te_stm_hosp = as.numeric(week_indicator$te_stm_hosp_week),
  cc_stm_case = as.numeric(week_indicator$cc_stm_case_week),
  cc_stm_hosp = as.numeric(week_indicator$cc_stm_hosp_week)
)

##---- end ----

##---- 4. Added by Jie on May 22, predicted cases and hp using Kriging ----
# Krige data

predicted_epi_data <- week_krige_func(loc,as.Date("2020-04-18"),tweet_sentiment,epi_data)
week_krige_data_df_func(loc,as.Date("2020-04-18"),tweet_sentiment,epi_data)

##---- end ----

##---- 5. Visualization for validation ----

# week_epi_data_df <- week_epi_data_func(epi_data)
week_epi_data_df <- week_epidata_cumulative_func(epi_data)

predicted_epi_data_df <- data.frame(
  week_no = 2:(length(predicted_epi_data$predicted_case_week)+1),
  predicted_case = as.numeric(predicted_epi_data$predicted_case_week), # difference between using "<-" and "="
  predicted_hosp = as.numeric(predicted_epi_data$predicted_hosp_week)
)
epi_week_pre_obs <- full_join(week_epi_data_df,predicted_epi_data_df,by = "week_no")
epi_week_pre_obs$gap_case <- abs(epi_week_pre_obs$daily_case_week-epi_week_pre_obs$predicted_case)/epi_week_pre_obs$daily_case_week
epi_week_pre_obs$gap_hosp <- abs(epi_week_pre_obs$daily_hosp_week-epi_week_pre_obs$predicted_hosp)/epi_week_pre_obs$daily_hosp_week
write.csv(epi_week_pre_obs,"epi_week_pre_obs_gap.csv")

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
# ggsave("Mumbai-cases-preVSobs-cum.eps", width = 10, height = 6.18)

case_preVSobs <- epi_week_pre_obs %>%
  ggplot +
  aes(x = daily_case_week,y = predicted_case) +
  geom_point(size = 3,color = 'seagreen4') +
  geom_smooth(method = 'lm',formula = y~x,se = F,color = 'seagreen4') +
  labs(x = "Observed cases",y = "Predicted cases") +
  # ylim(5.5, 5.8) + #scale_x_continuous
  # xlim(0, 0.3) +
  # scale_x_date(date_breaks = '1 day', date_labels = "%b %d") +
  # scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M") +
  stat_regline_equation(aes(label =  ..eq.label..),formula = y~x, size = 5,
                        label.x = min(epi_week_pre_obs$daily_case_week),
                        label.y = max(epi_week_pre_obs$predicted_case,na.rm = T)) +
  tweetPlotTheme
case_preVSobs
ggsave("Bangkok-case-preVSobs-Linear.eps", width = 10, height = 6.18)

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
# ggsave("Mumbai-hosp-preVSobs-cum.eps", width = 10, height = 6.18)

hosp_preVSobs <- epi_week_pre_obs %>%
  ggplot +
  aes(x = daily_hosp_week,y = predicted_hosp) +
  geom_point(size = 3,color = 'seagreen4') +
  geom_smooth(method = 'lm',formula = y~x,se = F,color = 'seagreen4') +
  labs(x = "Observed hospitalizations",y = "Predicted hospitalizations") +
  # ylim(5.5, 5.8) + #scale_x_continuous
  # xlim(0, 0.3) +
  # scale_x_date(date_breaks = '1 day', date_labels = "%b %d") +
  # scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:%M") +
  
  # stat_regline_equation(aes(label =  ..eq.label..),formula = y~x, size = 5,
  #                       label.x = 250, 
  #                       label.y = 300) +
  stat_regline_equation(aes(label =  ..eq.label..),formula = y~x, size = 5,
                        label.x = min(epi_week_pre_obs$daily_hosp_week),
                        label.y = max(epi_week_pre_obs$predicted_hosp,na.rm = T)) +
  tweetPlotTheme
hosp_preVSobs
ggsave("Bangkok-hosp-preVSobs-Linear.eps", width = 10, height = 6.18)

##---- end ----


# Plot theme
tweetPlotTheme <- theme(panel.background = element_blank(),
                        panel.grid.major = element_line(color = 'gray'),
                        panel.grid.minor = element_line(color = 'gray'),
                        axis.line = element_line(),
                        panel.border = element_rect(color = "black", fill=NA),
                        axis.title = element_text(size=20),
                        axis.text = element_text(size = 15),
                        plot.title = element_text(size=20))

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
           replace_abbreviation() %>% 
           replace_symbol() %>%
           replace_contraction() %>%
           replace_ordinal() %>%
           replace_number()) %>%
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
  ggplot(aes(x = as.Date(day_created), y = nTweets, color = meanSent, group = 1)) +
  geom_line(size = 1, color = "black") +
  geom_point(size = 3) +
  labs(x = "Date", y = "Mean Daily Tweets", color = "Mean Happiness",
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

tweet_loc <- get_googlemap("new delhi", zoom = 12)

tweet_map <- ggmap(tweet_loc) +
   geom_point(data = tweet_tweetSentMap,
             aes(x = as.numeric(lng),
                 y = as.numeric(lat),
                 color = meanSent),
             size = 4) +
  scale_color_gradient2(midpoint = 5) +
  labs(color = "Mean Sentiment", x = "Lon", y = "Lat")

# Krige data
loc_coords <- lookup_coords("new delhi")

p <- list(data.frame("x" = c(loc_coords[[2]][1], loc_coords[[2]][3]),
                     "y" = c(loc_coords[[2]][2], loc_coords[[2]][4])))

randomPoints <- data.frame("lng" = c(runif(1000, min = min(p[[1]][,1]), max = max(p[[1]][,1]))),
                           "lat" = c(runif(1000, min = min(p[[1]][,2]), max = max(p[[1]][,2]))))

library(sp)
library(automap)

tweet_sent_coords <- tweet_sentiment %>%
  left_join(hosp_data, by = c("day_created" = "Date")) %>%
  na.omit() %>%
  filter(day_created < "2020-04-20")

coordinates(tweet_sent_coords) <- ~lng+lat

coordinates(randomPoints) <- ~lng+lat

lzn.kriged <- autoKrige(formula = Sent ~ 1, tweet_sent_coords, randomPoints) # simple Kriging

lzn.kriged.dataframe <- as.data.frame(lzn.kriged$krige_output) %>%
  rename("Sent" = var1.pred)

tweet_sentiment.dataframe <- as.data.frame(tweet_sent_coords)

coordinates(lzn.kriged.dataframe) <- ~lng+lat

lzn.kriged <- autoKrige(formula = n ~ Sent, 
                        input_data = tweet_sent_coords, 
                        new_data = lzn.kriged.dataframe)

lzn.kriged.dataframe.final <- as.data.frame(lzn.kriged$krige_output)

krigePlotMid = round(median(lzn.kriged.dataframe.final$var1.pred))

hosp_krige_plot <- tweet_loc %>%
  ggmap() +
  geom_point(data = lzn.kriged.dataframe.final,
             aes(x = lng, y = lat, color = (var1.pred))) +
  scale_color_gradient2(low = "blue", high = "red", midpoint = krigePlotMid, 
                        mid = "yellow") +
  labs(x = "Lon", y = "Lat", color = "Hospitalization Rate",alpha = "Positiveness") +
  theme_minimal()

library(ggpubr)

ggarrange(tweet_map, tweet_cases_plot, tweet_sentiment_plot,
          casesBysent_plot, growthBysent_plot, hospBysent_plot)
