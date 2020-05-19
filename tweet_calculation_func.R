tweet_daily_vol_func <- function(rt_city){
  rt_city_daily <- rt_city %>%
    group_by("tweetDay" = as.Date(strftime(created_at,format = "%Y-%m-%d"))) %>%
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


