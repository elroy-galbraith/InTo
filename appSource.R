library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggwordcloud)

library(leaflet)
library(plotly)
library(DT)

library(rtweet)
library(tidytext)
library(qdap)
library(qdapDictionaries)
library(htmltools)

# Convert to an SPDF
library(sp)
library(automap)
library(rgdal)
library(KernSmooth)

# Plot theme
tweetPlotTheme <- theme(panel.background = element_blank(),
                        panel.grid.major = element_line(color = 'gray'),
                        panel.grid.minor = element_line(color = 'gray'),
                        axis.line = element_line(),
                        panel.border = element_rect(color = "black", fill=NA),
                        axis.title = element_text(size=10),
                        axis.text = element_text(size = 10),
                        plot.title = element_text(size=10),
                        legend.background = element_blank(),
                        legend.text = element_text(size = 10),
                        legend.title = element_text(size = 10),
                        legend.position = "top",
                        legend.direction = "horizontal")

# colors of emotions based on Plutchik's colour wheel of emotion
emoColors <- c("anger" = "#fe0000", "anticipation" = "#fea853", "disgust" = "#ff54ff", 
               "fear" = "#009600", "joy" = "#ffff54", "sadness" = "#5151ff", 
               "surprise" = "#5abdff", "trust" = "#52ff4f")

# Import Data
# bangkok <- read_twitter_csv("./data/tweet_original_Bangkok_0507.csv")
 delhi <- read_twitter_csv("./data/tweet_Delhi.csv")
# jakarta <- read_twitter_csv("./data/tweet_original_Jakarta_0507.csv")
# mumbai <- read_twitter_csv("./data/tweet_original_Mumbai_0507.csv")
# 
# ## India covid data
# india_cases <- read.csv("./data/complete.csv")
# india_hosp <- read.csv("./data/patients_data.csv")

# Function to get cases data
get_cases_data <- function(loc){

  if(loc %in% c("New Delhi", "Mumbai")){

    india_cases %>%
      filter(`Name.of.State...UT` == "Delhi") %>%
      mutate(Date = as.Date(Date)) %>%
      arrange(Date) %>%
      mutate(newCases = Total.Confirmed.cases - lag(Total.Confirmed.cases)) %>%
      select(Date, Total.Confirmed.cases, newCases) %>%
      pivot_longer(cols = c(Total.Confirmed.cases, newCases)) %>%
      filter(name == "newCases")

  }

}

# Function to get hospitalization data
get_hosp_data <- function(loc){

  if(loc == "New Delhi"){

    india_hosp %>%
      filter(state_code == "DL", current_status == "Hospitalized") %>%
      group_by(date_announced) %>%
      count() %>%
      ungroup() %>%
      mutate(Date = dmy(date_announced))

  }

}

# Function to get tweet coordinates and sentiment
get_tweet_coords <- function(loc){

  loc %>%
    select(user_id, status_id, created_at, text, coords_coords) %>%
    tidyr::separate(col = "coords_coords", into = c("lng", "lat"), sep = " ") %>%
    # replace abbreviations and contractions
    mutate(text = replace_abbreviation(text) %>% # replace abbreviation
             replace_contraction()) %>%
    # tokenize, ie. separate a tweet text into its constituent elements
    unnest_tokens("word", "text", token = "tweets",
                  strip_punct = T, strip_url = T) %>%
    # determine sentiments of words
    inner_join(labMT) %>%
    # remove values in between 4 and 6
    filter(!(4 < happiness_average & happiness_average < 6)) %>%
    ## count the number of positive and negative words per status per user
    group_by(user_id, status_id, "lat" = as.numeric(lat), "lng" = as.numeric(lng),
             "day_created" = strftime(created_at, format = "%Y-%m-%d")) %>%
    summarise(Sent = mean(happiness_average, na.rm = T)) %>%
    ungroup() %>%
    group_by(day_created) %>%
    mutate(nTweets = n_distinct(status_id)) %>%
    ungroup() %>%
    filter(day_created >= input$dates[1], day_created <= input$dates[2])

}

# Function to get top bi-grams
get_bigrams <- function(loc){

  loc %>%
    filter(created_at >= input$dates[1], created_at <= input$dates[2]) %>%
    # top_frac(n = .2, wt = retweet_count) %>%
    mutate(text = rm_twitter_url(text) %>%
             rm_number()) %>%
    unnest_tokens("word", "text", token = "ngrams", n = 2) %>%
    separate(col = word, into = c("word1", "word2"), sep = " ") %>%
    filter(!(word1 %in% c(stop_words$word, "coronavirus", "covid19",
                          "#covid19", "#coronavirus", "#covid2019", "amp", "covid", "-", "|", "19"))) %>%
    filter(!(word2 %in% c(stop_words$word, "coronavirus", "covid19",
                          "#covid19", "#coronavirus", "#covid2019", "amp", "covid", "-", "|", "19"))) %>%
    unite(col = "pairs", c(word1, word2), sep = " ")  %>%
    group_by(pairs) %>%
    count() %>%
    ungroup() %>%
    # change n to whatever number required
    top_n(n = 10, wt = n) 

}

# Function to measure emotional content of top tweets
get_emotions <- function(loc){
  
  loc %>%
    filter(created_at >= input$dates[1], created_at <= input$dates[2]) %>%
    top_frac(n = .2, wt = retweet_count) %>%
    mutate(text = rm_twitter_url(text) %>%
           rm_number()) %>%
    unnest_tokens("pairs", "text", token = "ngrams", n = 2) %>%
    separate(pairs, c("word1", "word2")) %>%
    pivot_longer(c(word1, word2), names_to = "wordNum", values_to = "word") %>%
    inner_join(get_sentiments("nrc")) %>%
    filter(!(sentiment %in% c("positive", "negative"))) %>%
    group_by(sentiment) %>%
    count() %>%
    ungroup() 
  
}


# kriging
# get_krigin <- function(loc, coords){
#   
#   loc_coords <- lookup_coords(loc)
#   
#   p <- list(data.frame("x" = c(loc_coords[[2]][1], loc_coords[[2]][3]),
#                      "y" = c(loc_coords[[2]][2], loc_coords[[2]][4])))
#   
#   randomPoints <- data.frame("lng" = c(runif(10000, min = min(p[[1]][,1]), max = max(p[[1]][,1]))),
#                            "lat" = c(runif(10000, min = min(p[[1]][,2]), max = max(p[[1]][,2]))))
#   
#   coords <- dplyr::filter(coords, !is.na(lng))
#   
#   coordinates(coords) <- ~lng+lat
#   
#   coordinates(randomPoints) <- ~lng+lat
# 
#   lzn.kriged <- autoKrige(formula = Sent ~ 1, coords, randomPoints)
# 
#   # lzn.kriged.dataframe <- as.data.frame(lzn.kriged$krige_output) %>%
#   # rename("Sent" = var1.pred)
#   # 
#   # coordinates(lzn.kriged.dataframe) <- ~lng+lat
#   # 
#   # lzn.kriged <- autoKrige(formula = n ~ log(Sent), input_data = coords,
#   #                       lzn.kriged.dataframe, model = "Sph")
#   # 
#   # lzn.kriged.dataframe.final <- as.data.frame(lzn.kriged$krige_output)
# 
#   return(lzn.kriged)
# 
# }

# tweetCoord_Bangkok <- get_tweet_coords(bangkok)
# write.csv(tweetCoord_Bangkok, "tweetCoord_Bangkok.csv")
tweetCoord_Bang <-  read.csv("./data/tweetCoord_Bangkok.csv")

# tweetCoord_Delhi <- get_tweet_coords(delhi)
# write.csv(tweetCoord_Delhi, "tweetCoord_Delhi.csv")
tweetCoord_Del <-  read.csv("./data/tweetCoord_Delhi.csv")

# tweetCoord_Jak <- get_tweet_coords(jakarta)
# write.csv(tweetCoord_Jak, "tweetCoord_Jak.csv")
tweetCoord_Jak <-  read.csv("./data/tweetCoord_Jak.csv")

# tweetCoord_Mum <- get_tweet_coords(mumbai)
# write.csv(tweetCoord_Mum, "tweetCoord_Mum.csv")
tweetCoord_Mum <-  read.csv("./data/tweetCoord_Mum.csv")

# tweetBigrams_Bang <- get_bigrams(bangkok)
# write.csv(tweetBigrams_Bang, "tweetBigrams_Bang.csv")
tweetBigrams_Bang <-  read.csv("./data/tweetBigrams_Bang.csv")

# tweetBigrams_Del <- get_bigrams(delhi)
# write.csv(tweetBigrams_Del, "tweetBigrams_Del.csv")
tweetBigrams_Del <-  read.csv("./data/tweetBigrams_Del.csv")

# tweetBigrams_Jak <- get_bigrams(jakarta)
# write.csv(tweetBigrams_Jak, "tweetBigrams_Jak.csv")
tweetBigrams_Jak <-  read.csv("./data/tweetBigrams_Jak.csv")

# tweetBigrams_Mum <- get_bigrams(mumbai)
# write.csv(tweetBigrams_Mum, "tweetBigrams_Mum.csv")
tweetBigrams_Mum <-  read.csv("./data/tweetBigrams_Mum.csv")

# tweetEmo_Del <- get_emotions(delhi)
# write.csv(tweetEmo_Del, "tweetEmo_Del.csv")
tweetEmo_Del <-  read.csv("./data/tweetEmo_Del.csv")

# tweetEmo_Bang <- get_emotions(bangkok)
# write.csv(tweetEmo_Bang, "tweetEmo_Bang.csv")
tweetEmo_Bang <-  read.csv("./data/tweetEmo_Bang.csv")

# tweetEmo_Jak <- get_emotions(jakarta)
# write.csv(tweetEmo_Jak, "tweetEmo_Jak.csv")
tweetEmo_Jak <-  read.csv("./data/tweetEmo_Jak.csv")

# tweetEmo_Mum <- get_emotions(mumbai)
# write.csv(tweetEmo_Mum, "tweetEmo_Mum.csv")
tweetEmo_Mum <-  read.csv("./data/tweetEmo_Mum.csv")

# covidCases_Del <- get_cases_data("New Delhi")
# write.csv(covidCases_Del, "covidCases_Del.csv")
covidCases_Del <- read.csv("./data/covidCases_Del.csv") %>%
  mutate(Date = as.Date( strftime(Date, format = "%Y-%m-%d")))

# covidHosp_Del <- get_hosp_data("New Delhi")
# write.csv(covidHosp_Del, "covidHosp_Del.csv")
covidHosp_Del <- read.csv("./data/covidHosp_Del.csv") %>%
  mutate(Date = as.Date( strftime(Date, format = "%Y-%m-%d")))

# kriging data
kriging_Del <- read.csv("./data/kriging_Del.csv")

assocVals_Del <- read.csv("./data/assocVals_Del.csv")

tweetEmotions_del <- read.csv("./data/tweetEmotions_Ndel.csv")

topTweets <- read.csv("./data/topTweets.csv")

# helper function for making checkbox
shinyInput = function(FUN, len, id, ...) { 
  inputs = character(len) 
  for (i in seq_len(len)) { 
    inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...)) 
  } 
  inputs 
} 