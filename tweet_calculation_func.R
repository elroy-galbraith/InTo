tweet_daily_vol_func <- function(rt_city){
  rt_city_daily <- rt_city %>%
    group_by("recordDate" = as.Date(strptime(created_at,format = "%Y-%m-%d"))) %>%
    summarise(tweetVol = n(),
              retweetVol = mean(retweet_count,na.rm = T),
              highest_rt_count = max(retweet_count,na.rm = T)) %>%
    ungroup()
  return(rt_city_daily)
}

retweet_vol_func <- function(rt_city){
  retweet_daily <- rt_city %>%
    group_by("recordDate" = as.Date(strptime(created_at,format = "%Y-%m-%d"))) %>%
    summarise(highest_rt_count = max(retweet_count,na.rm = T),
              poptweet = text[retweet_count == max(retweet_count,na.rm = T)]) %>%
    ungroup() %>%
    mutate(week_no = 1)
  
  start_date_rt <- retweet_daily$recordDate[1]
  # end_date_rt <- retweet_daily$recordDate[nrow(retweet_daily)]
  for (ii in 1:nrow(retweet_daily)) {
    date_rt <- retweet_daily$recordDate[ii]
    days <- as.numeric(date_rt - start_date_rt)
    retweet_daily$week_no[ii] <- floor(days/7) + 1
  }
  
  retweet_week <- retweet_daily %>%
    group_by(week_no) %>%
    summarise(highest_rt_count_week = max(highest_rt_count,na.rm = T),
              poptweet_week = poptweet[highest_rt_count == max(highest_rt_count,na.rm = T)]) %>%
    ungroup()
  
  return(retweet_week)
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
    left_join(labMT) %>%
    mutate(stemmed_word= ifelse(is.na(happiness_average),
                                wordStem(word, language = "en"),
                                "")) %>%
    left_join(labMT, by = c("stemmed_word" = "word")) %>%
    mutate(happiness_average = ifelse(is.na(happiness_average.x),
                               happiness_average.y,
                               happiness_average.x)) %>%
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
  
  # population_ratio <- 1
  
  if (geoloc == "delhi"){
    # geoloc <- "new delhi"
    # loc_coords <- lookup_coords(geoloc)
    load("Delhi_coords.RData")
  } else if (geoloc == "mumbai"){
    # geoloc <- "Bombay"
    # loc_coords <- lookup_coords(geoloc)
    load("mumbai_coords.RData")
    # load("Maharashtra_coords.RData")
  } else if(geoloc == "new york city"){
    load("nyc_coords.RData")
  } else if(geoloc == "myanmar"){
    load("myanmar.RData")
  } else{
    loc_coords <- lookup_coords(geoloc)
  }
  
  # loc_coords <- lookup_coords(geoloc)  # "new delhi"
  p <- list(data.frame("x" = c(loc_coords[[2]][1], loc_coords[[2]][3]),
                       "y" = c(loc_coords[[2]][2], loc_coords[[2]][4])))
  set.seed(1234)
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
    
    # ONLY for Myanmar, because the kriging prediction does not work for the 3 iteration.
    # if (ii == 3){
    #   tweet_sent_coords_period <- tweet_sent_coords %>%
    #     filter(day_created >= start_date & day_created <= end_date - 2)
    # }
    
    coordinates(tweet_sent_coords_period) <- ~lng+lat
    tweet_sent_coords_period <- tweet_sent_coords_period[which(!duplicated(tweet_sent_coords_period@coords)),]
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
  return(result)
} # has been updated as "week_krige_func_update"
week_krige_data_df_func <- function(geoloc,start_date,tweet_sent,epi_data){
  
  # save the kriging data in csv files.
  # population_ratio <- 1
  
  if (geoloc == "delhi"){
    # geoloc <- "new delhi"
    # loc_coords <- lookup_coords(geoloc)
    load("Delhi_coords.RData")
  } else if (geoloc == "mumbai"){
    # geoloc <- "Bombay"
    # loc_coords <- lookup_coords(geoloc)
    load("mumbai_coords.RData")
    # load("Maharashtra_coords.RData")
  } else if(geoloc == "new york city"){
    load("nyc_coords.RData")
  } else if(geoloc == "myanmar"){
    load("myanmar.RData")
  } else{
    loc_coords <- lookup_coords(geoloc)
  }
  
  # loc_coords <- lookup_coords(geoloc)  # "new delhi"
  p <- list(data.frame("x" = c(loc_coords[[2]][1], loc_coords[[2]][3]),
                       "y" = c(loc_coords[[2]][2], loc_coords[[2]][4])))
  set.seed(1234)
  randomPoints <- data.frame("lng" = c(runif(1000, min = min(p[[1]][,1]), max = max(p[[1]][,1]))),
                             "lat" = c(runif(1000, min = min(p[[1]][,2]), max = max(p[[1]][,2]))))
  
  tweet_sent_coords <- tweet_sent %>%
    left_join(epi_data, by = c("day_created" = "recordDate")) %>%
    na.omit()
  
  tweet_sent_coords_period <- tweet_sent_coords
  
  coordinates(tweet_sent_coords_period) <- ~lng+lat
  tweet_sent_coords_period <- tweet_sent_coords_period[which(!duplicated(tweet_sent_coords_period@coords)),]
  
  coordinates(randomPoints) <- ~lng+lat # Can not do this repeatedly
  
  lzn.kriged.stm <- autoKrige(formula = Sent ~ 1, 
                              input_data = tweet_sent_coords_period,
                              new_data = randomPoints) # simple Kriging
  lzn.kriged.stm.dataframe <- as.data.frame(lzn.kriged.stm$krige_output) %>%
    rename("Sent" = var1.pred)
  # tweet_sentiment.dataframe <- as.data.frame(tweet_sent_coords)
  
  coordinates(lzn.kriged.stm.dataframe) <- ~lng+lat
  
  lse.sh.fit.case.automap <- autofitVariogram(input_data = tweet_sent_coords_period, formula = daily_case ~ Sent)
  lzn.kriged.case <- autoKrige(formula = daily_case ~ Sent, 
                               input_data = tweet_sent_coords_period, 
                               new_data = lzn.kriged.stm.dataframe)
  lse.sh.fit.hosp.automap <- autofitVariogram(input_data = tweet_sent_coords_period, formula = hospital ~ Sent)
  lzn.kriged.hosp <- autoKrige(formula = hospital ~ Sent, 
                               input_data = tweet_sent_coords_period, 
                               new_data = lzn.kriged.stm.dataframe)
  
  lzn.kriged.case.dataframe <- as.data.frame(lzn.kriged.case$krige_output)
  lzn.kriged.hosp.dataframe <- as.data.frame(lzn.kriged.hosp$krige_output)
  
  png(filename = paste0(geoloc, "-hosp-semivariogram.png"))
  plot(lse.sh.fit.hosp.automap)
  dev.off()
  
  write.csv(lzn.kriged.case.dataframe,paste(geoloc,"-kriging-data-case.csv",sep = ""))
  write.csv(lzn.kriged.hosp.dataframe,paste(geoloc,"-kriging-data-hosp.csv",sep = ""))
  
} # has been updated as "week_krige_data_df_func_update"


week_krige_func_update <- function(start_date,tweet_sent,epi_data){
  
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
    
    lzn.kriged.case <- autoKrige(formula = daily_case ~ Sent, 
                                 input_data = tweet_sent_coords_period, 
                                 new_data = tweet_sent_coords_period)
    
    lzn.kriged.hosp <- autoKrige(formula = hospital ~ Sent, 
                                 input_data = tweet_sent_coords_period, 
                                 new_data = tweet_sent_coords_period)
    
    lzn.kriged.case.dataframe <- as.data.frame(lzn.kriged.case$krige_output)
    lzn.kriged.hosp.dataframe <- as.data.frame(lzn.kriged.hosp$krige_output)
    
    predicted_case[ii] = round(mean(lzn.kriged.case.dataframe$var1.pred))
    predicted_hosp[ii] = round(mean(lzn.kriged.hosp.dataframe$var1.pred))
  }
  
  result <- list(predicted_case_week = predicted_case, predicted_hosp_week = predicted_hosp)
  return(result)
}
week_krige_data_df_func_update <- function(loc,tweet_sent,epi_data){
  
  tweet_sent_coords <- tweet_sent %>%
    left_join(epi_data, by = c("day_created" = "recordDate")) %>%
    na.omit()
  
  tweet_sent_coords_period <- tweet_sent_coords
  
  coordinates(tweet_sent_coords_period) <- ~lng+lat
  
  lzn.kriged.case <- autoKrige(formula = daily_case ~ Sent, 
                               input_data = tweet_sent_coords_period, 
                               new_data = tweet_sent_coords_period)
  lzn.kriged.hosp <- autoKrige(formula = hospital ~ Sent, 
                               input_data = tweet_sent_coords_period, 
                               new_data = tweet_sent_coords_period)
  
  lzn.kriged.case.dataframe <- as.data.frame(lzn.kriged.case$krige_output)
  lzn.kriged.hosp.dataframe <- as.data.frame(lzn.kriged.hosp$krige_output)
  write.csv(lzn.kriged.case.dataframe,paste(loc,"-kriging-data-case.csv",sep = ""))
  write.csv(lzn.kriged.hosp.dataframe,paste(loc,"-kriging-data-hosp.csv",sep = ""))
}


week_epi_data_func <- function(epi_data){
 
  no_week <- ceiling(nrow(epi_data)/7)
  week_epi_data <- epi_data %>%
    mutate(week_no = 1)
  for (ii in 1:no_week) {
    start_index <- (ii-1)*7 + 1
    if(ii < no_week){
      end_index <- start_index + 6
    } else{
      end_index <- nrow(epi_data)
    }
    
    week_epi_data$week_no[start_index:end_index] <- ii
  }
  week_epi_data_df <- week_epi_data %>%
    group_by(week_no) %>%
    summarise(daily_case_week = round(mean(daily_case, na.rm = TRUE)),
              daily_hosp_week = round(mean(hospital,na.rm = TRUE))) %>%
    ungroup()
  
  return(week_epi_data_df)
}
week_tweet_data_func <- function(vol_stm_daily){
  
  no_week <- ceiling(nrow(vol_stm_daily)/7)
  
  week_tweet_data <- vol_stm_daily %>%
    mutate(week_no = 1)
  for (ii in 1:no_week) {
    start_index <- (ii-1)*7 + 1
    if(ii < no_week){
      end_index <- start_index + 6
    } else{
      end_index <- nrow(vol_stm_daily)
    }
    
    week_tweet_data$week_no[start_index:end_index] <- ii
  }
  week_tweet_data_df <- week_tweet_data %>%
    group_by(week_no) %>%
    summarise(tweetVol_week = round(mean(tweetVol, na.rm = TRUE)),
              retweetVol_week = round(mean(retweetVol, na.rm = TRUE)),
              stm_week = mean(mean_daily_stm,na.rm = TRUE),
              highest_rt_count_week = max(highest_rt_count,na.rm = T)) %>%
    ungroup()
  
  return(week_tweet_data_df)
}


week_epidata_cumulative_func <- function(epi_data){
  
  no_week <- ceiling(nrow(epi_data)/7)
  
  daily_case_week <- c()
  daily_hosp_week <- c()
  
  for (ii in 1:no_week) {
    if(ii < no_week){
      end_index <- ii * 7
    } else{
      end_index <- nrow(epi_data)
    }
    daily_case_week[ii] <- round(mean(epi_data$daily_case[1:end_index]))
    daily_hosp_week[ii] <- round(mean(epi_data$hospital[1:end_index]))
  }
  
  week_epidata_cumu_df <- data.frame(
    week_no = seq(no_week),
    daily_case_week,
    daily_hosp_week
  )
  
  return(week_epidata_cumu_df)
}

week_mis_cum_epidata_func <- function(mis_epi_data){
  
  no_week <- ceiling(nrow(mis_epi_data)/7)
  
  daily_case_week <- c()
  daily_hosp_week <- c()
  pre_mis_case_week <- c()
  pre_mis_hosp_week <- c()
  
  for (ii in 1:no_week) {
    if(ii < no_week){
      end_index <- ii * 7
    } else{
      end_index <- nrow(mis_epi_data)
    }
    daily_case_week[ii] <- round(mean(mis_epi_data$daily_case[1:end_index]))
    daily_hosp_week[ii] <- round(mean(mis_epi_data$hospital[1:end_index]))
    pre_mis_case_week[ii] <- round(mean(mis_epi_data$predicted_mis_case[1:end_index]))
    pre_mis_hosp_week[ii] <- round(mean(mis_epi_data$predicted_mis_hosp[1:end_index]))
  }
  
  
  
  week_case_hosp <- data.frame(
    week_no = seq(no_week),
    daily_case_week,
    daily_hosp_week
  )
  
  if ((nrow(mis_epi_data)/7 - no_week) != 0){
    pre_mis_case_week <- pre_mis_case_week[1:(length(pre_mis_case_week)-1)]
    pre_mis_hosp_week <- pre_mis_hosp_week[1:(length(pre_mis_hosp_week)-1)]
  }
  
  week_pre_mis_case_hosp <- data.frame(
    week_no = 2:(length(pre_mis_case_week)+1),
    pre_mis_case_week,
    pre_mis_hosp_week
  )
  
  week_mis_epidata_cumu_df <- full_join(week_case_hosp,week_pre_mis_case_hosp,by = "week_no")
  
  return(week_mis_epidata_cumu_df)
}


data_fill_by_date <- function(start_date,end_date,df_data,insert_val){
  colnames(df_data)[1] <- "recordDate"
  day_seq <- seq(start_date,end_date,by = "day")
  df_data_add <- data.frame(recordDate=day_seq,c=0)
  df_data_filled <- merge(df_data,df_data_add,by="recordDate",all=TRUE)
  df_data_filled$c <- NULL
  df_data_filled[is.na(df_data_filled)] <- insert_val
  
  return(df_data_filled)
}


predict_arima_func <- function(start_date,tweet_data,epi_data){
  
  no_week <- floor(nrow(epi_data)/7)
  
  week_pre_case <- list()
  week_pre_hosp <- list()
  week_pre_case_lower <- list()
  week_pre_case_upper <- list()
  week_pre_hosp_lower <- list()
  week_pre_hosp_upper <- list()
  
  for (ii in 1:no_week) {
    end_date <- start_date + ii*7 - 1
    
    tweet_data_weeks <- tweet_data %>%
      filter(day_created >= start_date & day_created <= end_date)
    epi_data_weeks <- epi_data %>%
      filter(recordDate >= start_date & recordDate <= end_date)
    
    ##-- Prediction for cumulative weekly data
    pos_case_hosp <- tweet_data_weeks %>%
      left_join(epi_data_weeks, by = c("day_created" = "recordDate")) %>%
      group_by(day_created) %>%
      summarise(Positivity = mean(Sent, na.rm = T),
                Case = mean(daily_case, na.rm = T),
                Hospitalization = mean(hospital, na.rm = T)) %>%
      ungroup() %>%
      as_tsibble(index = day_created)
    
    # max_case <- max(pos_case_hosp$Case)
    # max_hosp <- max(pos_case_hosp$Hospitalization)
    # pos_case_hosp$Case <- pos_case_hosp$Case/max_case
    # pos_case_hosp$Hospitalization <- pos_case_hosp$Hospitalization/max_hosp
    
    pos_model <- pos_case_hosp %>%
      model(pos.arima = ARIMA(Positivity ~ PDQ(0,0,0))) %>%
      forecast(h=7)
    
    pos_new_data <- as_tibble(pos_model) %>%
      select(day_created, "Positivity" = .mean)  %>%
      as_tsibble(index = day_created)
    
    pos_case_model <- pos_case_hosp %>%
      model(case.arima = ARIMA(Case ~ Positivity + pdq(d=2))) %>%
      forecast(new_data = pos_new_data)
    
    pos_hosp_model <- pos_case_hosp %>%
      model(hosp.arima = ARIMA(Hospitalization ~ Positivity + pdq(d=2))) %>%
      forecast(new_data = pos_new_data)
    
    pre_case_data <- pos_case_model %>%
      hilo() %>%
      unpack_hilo(`95%`) %>%
      select(day_created, .mean, `95%_lower`, `95%_upper`) %>%
      as_tibble()
    
    pre_hosp_data <- pos_hosp_model %>%
      hilo() %>%
      unpack_hilo(`95%`) %>%
      select(day_created, .mean, `95%_lower`, `95%_upper`) %>%
      as_tibble()
    
    week_pre_case[ii] <- round(mean(pre_case_data$.mean))
    week_pre_hosp[ii] <- round(mean(pre_hosp_data$.mean))
    
    week_pre_case_lower[ii] <- min(pre_case_data$`95%_lower`)
    week_pre_case_upper[ii] <- max(pre_case_data$`95%_upper`)
    week_pre_hosp_lower[ii] <- min(pre_hosp_data$`95%_lower`)
    week_pre_hosp_upper[ii] <- max(pre_hosp_data$`95%_upper`)
  }
  
  result <- list(predicted_case_week = week_pre_case,
                 predicted_hosp_week = week_pre_hosp,
                 pre_case_lower = week_pre_case_lower,
                 pre_case_upper = week_pre_case_upper,
                 pre_hosp_lower = week_pre_hosp_lower,
                 pre_hosp_upper = week_pre_hosp_upper)
  return(result)
}

