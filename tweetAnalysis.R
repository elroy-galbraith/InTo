library(rtweet)
library(tidyverse)
library(qdap)
library(tidytext)
library(tidyr)

# name of Location
loc = "New Delhi"

locCode = ifelse(loc == "New Delhi",
                 "DL",
                 NA)

# List the files of csvs
list_of_files <- list.files(path = paste0("./tweetData/", loc, "/"), recursive = TRUE,
                            pattern = "\\.csv", 
                            full.names = TRUE)

# Read files and merge them into one file
tweet <- list_of_files %>%
  set_names(.) %>%
  map_df(.f = ~read_csv(file = .x), .id = "FileName") %>%
  select(user_id, status_id, created_at, text, retweet_count, coords_coords)

# Plot theme
tweetPlotTheme <- theme(panel.background = element_blank(),
                        panel.grid.major = element_line(color = 'gray'),
                        panel.grid.minor = element_line(color = 'gray'),
                        axis.line = element_line(),
                        panel.border = element_rect(color = "black", fill=NA),
                        axis.title = element_text(size=18),
                        axis.text = element_text(size = 15),
                        plot.title = element_text(size=18))

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
  summarise(te_posTOhosp = teCal_jidt_knl_func(srcArr = meanSent, dstArr = hosp),
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

cases_plot <- cases_ndel  %>%
  ggplot(aes(x = as.Date(Date), y = value, fill = name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "blue") ,
                    labels = c("New Cases", "Old Cases")) +
  labs(x = "Date", y = "No. of Cases", fill = "Type") +
  tweetPlotTheme

casesBysent <- tweet_sentiment %>%
  mutate(Date = as.Date( strftime(day_created, format = "%Y-%m-%d"))) %>%
  left_join(cases_ndel, by = "Date") %>%
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
  left_join(hosp_ndel, by = c("day_created" = "Date")) %>%
  na.omit() %>%
  filter(day_created < "2020-04-20")

coordinates(tweet_sent_coords) <- ~lng+lat

coordinates(randomPoints) <- ~lng+lat

lzn.kriged <- autoKrige(formula = Sent ~ 1, tweet_sent_coords, randomPoints)

lzn.kriged.dataframe <- as.data.frame(lzn.kriged$krige_output) %>%
  rename("Sent" = var1.pred)

tweet_sentiment.dataframe <- as.data.frame(tweet_sent_coords)

coordinates(lzn.kriged.dataframe) <- ~lng+lat

lzn.kriged <- autoKrige(formula = n ~ log(Sent), 
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
