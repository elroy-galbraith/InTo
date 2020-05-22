tweet_daily_vol_func <- function(rt_city){
  rt_city_daily <- rt_city %>%
    group_by("recordDate" = as.Date(strftime(created_at,format = "%Y-%m-%d"))) %>%
    count() %>%
    ungroup()
  return(rt_city_daily)
}

stm_afinn_daily_func <- function(rt_city){
  unnest_tweet <- rt_city %>%
    select(user_id,status_id,created_at,text) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
  
  ####----- Afinn Lexicon ------####
  sentiments <- get_sentiments('afinn')
  tweet_stm <- unnest_tweet %>%
    inner_join(sentiments) %>%
    # filter(!(value > -1 & value < 1 )) %>%
    group_by("tweetDate" = as.Date(strftime(created_at,format = "%Y-%m-%d")) , 
             status_id, user_id) %>%
    summarise(mean_stm = mean(value,na.rm = TRUE)) %>%
    ungroup()%>%
    group_by(tweetDate) %>%
    summarise(mean_daily_stm = mean(mean_stm, na.rm = T)) %>%
    ungroup()
  
  return(tweet_stm)
}

stm_labMT_daily_func <- function(rt_city){
  unnest_tweet <- rt_city %>%
    select(user_id,status_id,created_at,text) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
  
  ####----- Sentiment Database: labMT ------####
  # sentiments <- get_sentiments('afinn')
  data(labMT)
  force(labMT)
  tweet_stm <- unnest_tweet %>%
    inner_join(labMT,by = "word") %>%
    # filter(!(value > -1 &了 value < 1 )) %>%
    group_by("tweetDate" = as.Date(strftime(created_at,format = "%Y-%m-%d")) , 
             status_id, user_id) %>%
    summarise(mean_stm = mean(happiness_average,na.rm = TRUE)) %>%
    ungroup()%>%
    group_by(tweetDate) %>%
    summarise(mean_daily_stm = mean(mean_stm, na.rm = TRUE)) %>%
    ungroup()
  
  return(tweet_stm)
}

stm_labMT_daily_func_elroy <- function(tweet){
  # unnest_tweet <- rt_city %>%
  #   select(user_id,status_id,created_at,text) %>%
  #   unnest_tokens(word, text) %>%
  #   anti_join(stop_words)
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
  
  ####----- Sentiment Database: labMT ------####
  tweet_stm <- unnest_tweet %>%
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
  
  return(tweet_stm)
}


csv_epi_india_func <- function(city){
  india_cases <- read.csv("india_complete.csv") %>%
    filter(`Name.of.State...UT` == city) %>%    #  Delhi --> Delhi; Membai --> Maharashtra
    mutate(Date = as.Date(Date)) %>%
    arrange(Date) %>%
    mutate(newCases = Total.Confirmed.cases - lag(Total.Confirmed.cases)) %>%
    mutate(activeCases = Total.Confirmed.cases - Cured.Discharged.Migrated) %>%
    select(Date, Total.Confirmed.cases, Cured.Discharged.Migrated, newCases, activeCases)
  # pivot_longer(cols = c(Total.Confirmed.cases, newCases))
  
  return(india_cases)
}

data_period_func <- function(rt_data,start_date,end_date){
  rt_start_row_num <- which(rt_data$recordDate == start_date)
  rt_end_row_num <- which(rt_data$recordDate == end_date)
  rt_data_period <- rt_data[rt_start_row_num:rt_end_row_num,]
  
  return(rt_data_period)
}

normalization_func <- function(x){
  nml_x = (x-min(x))/(max(x)-min(x))
  return(nml_x)
}

week_krige_func <- function(geoloc,start_date,tweet_sent,epi_data){
  
  if (geoloc == "delhi"){
    geoloc <- "new delhi"
  }
  
  loc_coords <- lookup_coords(geoloc)  # "new delhi"
  p <- list(data.frame("x" = c(loc_coords[[2]][1], loc_coords[[2]][3]),
                       "y" = c(loc_coords[[2]][2], loc_coords[[2]][4])))
  randomPoints <- data.frame("lng" = c(runif(1000, min = min(p[[1]][,1]), max = max(p[[1]][,1]))),
                             "lat" = c(runif(1000, min = min(p[[1]][,2]), max = max(p[[1]][,2]))))
  
  no_week <- floor(nrow(epi_data)/7)
  
  predicted_case <- list()
  predicted_hosp <- list()
  
  tweet_sent_coords <- tweet_sent %>%
    left_join(epi_data, by = c("day_created" = "recordDate")) %>%
    na.omit()
  
  for (ii in 1:no_week) {
    end_date <- start_date + ii*7 - 1
    tweet_sent_coords_period <- tweet_sent_coords %>%
      filter(day_created >= start_date & day_created <= end_date)
    
    coordinates(tweet_sent_coords_period) <- ~lng+lat
    if (ii == 1){
      coordinates(randomPoints) <- ~lng+lat # Can not do this repeatedly
    }
    
    lzn.kriged.stm <- autoKrige(formula = Sent ~ 1, tweet_sent_coords_period, randomPoints) # simple Kriging
    lzn.kriged.stm.dataframe <- as.data.frame(lzn.kriged.stm$krige_output) %>%
      rename("Sent" = var1.pred)
    # tweet_sentiment.dataframe <- as.data.frame(tweet_sent_coords)
    
    coordinates(lzn.kriged.stm.dataframe) <- ~lng+lat
    
    lzn.kriged.case <- autoKrige(formula = daily_case ~ Sent, 
                                 input_data = tweet_sent_coords_period, 
                                 new_data = lzn.kriged.stm.dataframe)
    
    lzn.kriged.hosp <- autoKrige(formula = hospital ~ Sent, 
                                 input_data = tweet_sent_coords_period, 
                                 new_data = lzn.kriged.stm.dataframe)
    
    lzn.kriged.case.dataframe <- as.data.frame(lzn.kriged.case$krige_output)
    lzn.kriged.hosp.dataframe <- as.data.frame(lzn.kriged.hosp$krige_output)
    
    predicted_case[ii] = round(mean(lzn.kriged.case.dataframe$var1.pred))
    predicted_hosp[ii] = round(mean(lzn.kriged.hosp.dataframe$var1.pred))
  }
  
  result <- list(predicted_case_week = predicted_case, predicted_hosp_week = predicted_hosp)
}






