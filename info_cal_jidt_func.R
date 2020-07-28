library("rJava")
.jinit()

# Change location of jar to match yours:
#  IMPORTANT -- If using the default below, make sure you have set the working directory
#   in R (e.g. with setwd()) to the location of this file (i.e. demos/r) !!
.jaddClassPath("/home/lijie/twitterData/infodynamics-dist-1.5/infodynamics.jar")

teCal_jidt_ksg_func <- function(srcArr,dstArr,t_lag,k,histLen){
  # k: "4" as an example; histLen: 1L as an example
  # Create a TE calculator:
  teCalc<-.jnew("infodynamics/measures/continuous/kraskov/TransferEntropyCalculatorKraskov")
  .jcall(teCalc,"V","setProperty", "DELAY", t_lag) # specify the time lag for TE calculation
  .jcall(teCalc,"V","setProperty", "k", k) # Use Kraskov parameter K nearest points
  
  
  # Perform calculation with correlated source:
  .jcall(teCalc,"V","initialise", histLen) # Use history length 1 (Schreiber k=1)
  .jcall(teCalc,"V","setObservations", srcArr, dstArr)
  result <- .jcall(teCalc,"D","computeAverageLocalOfObservations") # nat
  result_bit <- log2(exp(1))*result    # nat to bit
  return(result_bit)
}

teCal_jidt_knl_func <- function(srcArr,dstArr,histLen,width){
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


miCal_jidt_func <- function(srcArr,dstArr){
  # different calculators JIDT provides (select one of them!):
  #  implementingClass <- "infodynamics/measures/continuous/kraskov/MutualInfoCalculatorMultiVariateKraskov1" # MI([1,2], [3,4]) = 0.36353
  implementingClass <- "infodynamics/measures/continuous/kernel/MutualInfoCalculatorMultiVariateKernel"
  #  implementingClass <- "infodynamics/measures/continuous/gaussian/MutualInfoCalculatorMultiVariateGaussian"
  # implementingClass <- "infodynamics/measures/continuous/kraskov/MutualInfoCalculatorMultiVariateKraskov1"
  miCalc<-.jnew(implementingClass)
  
  # a. Initialise the calculator to use the required number of
  #   dimensions for each variable:
  .jcall(miCalc,"V","initialise")
  
  # b. Supply the observations to compute the PDFs from:
  .jcall(miCalc,"V","setObservations",srcArr,dstArr)
  
  # c. Make the MI calculation:
  result <- .jcall(miCalc,"D","computeAverageLocalOfObservations")  # bit
  return(result)
}

miCal_jidt_func_lag <- function(srcArr,dstArr,time_lag){
  # time_lag: string -> "1" or "2"
  # different calculators JIDT provides (select one of them!):
  # implementingClass <- "infodynamics/measures/continuous/kraskov/MutualInfoCalculatorMultiVariateKraskov1" # MI([1,2], [3,4]) = 0.36353
  implementingClass <- "infodynamics/measures/continuous/kernel/MutualInfoCalculatorMultiVariateKernel"
  #  implementingClass <- "infodynamics/measures/continuous/gaussian/MutualInfoCalculatorMultiVariateGaussian"
  # implementingClass <- "infodynamics/measures/continuous/kraskov/MutualInfoCalculatorMultiVariateKraskov1"
  miCalc<-.jnew(implementingClass)
  
  .jcall(miCalc,"V","setProperty", "TIME_DIFF", time_lag)
  
  # a. Initialise the calculator to use the required number of
  #   dimensions for each variable:
  .jcall(miCalc,"V","initialise")
  
  # b. Supply the observations to compute the PDFs from:
  .jcall(miCalc,"V","setObservations",srcArr,dstArr)
  
  # c. Make the MI calculation:
  result <- .jcall(miCalc,"D","computeAverageLocalOfObservations")  # bit
  return(result)
}

opt_lag4mi_func <- function(srcArr,dstArr,max_lag){
  opt_lag <- 0
  opt_mi  <- miCal_jidt_func_lag(srcArr,dstArr,as.character(opt_lag))
  for (i in seq(max_lag)) {
    mi <- miCal_jidt_func_lag(srcArr,dstArr,as.character(i))
    if (is.na(mi) | is.na(opt_mi)){
      opt_lag <- 1
    }else if(mi>opt_mi){
      opt_mi <- mi
      opt_lag <- i
    } else {
      next
    }
  }
  if(opt_lag == 0){
    # since the time lag for TE calculaton can not be zero.
    # if zero, it reports an error saying "source index -1 out of bounds for double[]"
    opt_lag <- 1
  }
  result_list <- list(opt_lag = opt_lag,max_mi = opt_mi)
  return(result_list)
}

##------------an example for MI verification-----------
# datafile <- "/home/lijie/infodynamics-dist-1.5/demos/data/2coupledRandomCols-1.txt"
# data <- read.csv(datafile, header=FALSE, sep="")
# variable1 <- data[,1]
# variable2 <- data[,2]
# miCal_jidt_func(variable1,variable2)
# miCal_jidt_func_lag(variable1,variable2,'1')
# opt_lag4mi_func(variable1,variable2,5)
# teCal_jidt_knl_func(variable1,variable2,1L,0.2)

te_cal_opt_hist_func <- function(src_data,dst_data,max_hist,knl_width){
  te_value <- teCal_jidt_knl_func(src_data,dst_data,
                                  as.integer(opt_lag4mi_func(src_data,dst_data,max_hist)$opt_lag),
                                  knl_width)
  return(te_value)
}

te_cal_opt_lag_func <- function(src_data,dst_data,max_t_lag,k,histLen,knl_width){
  te_value <- try(teCal_jidt_ksg_func(src_data,dst_data,
                                  as.character(opt_lag4mi_func(src_data,dst_data,max_t_lag)$opt_lag),
                                  as.character(k),histLen),silent = T)
  if ("try-error" %in% class(te_value)){
    te_value <- teCal_jidt_knl_func(src_data,dst_data,histLen,knl_width)
  } else {
    print("TE is calculated by KSG with optimal lag.")
  }
  
  return(te_value)
}

week_index_cal_func <- function(start_date,tweet_data,epi_data){
  no_week <- ceiling(nrow(epi_data)/7)
  
  te_stm_case <- list()
  te_stm_hosp <- list()
  cc_stm_case <- list()
  cc_stm_hosp <- list()
  week_end_date_list <- list()
  
  for (ii in 1:no_week) {
    if(ii == no_week){
      end_date <- epi_data$recordDate[nrow(epi_data)]
    } else {
      end_date <- start_date + ii*7 - 1
    }
    
    week_end_date_list[ii] <- list(as.Date(end_date))
    
    twt_data_period <- tweet_data %>%
      filter(recordDate >= start_date & recordDate <= end_date)
    
    stm_period <- twt_data_period$mean_daily_stm
    stm_period_normal <- normalization_func(stm_period)
    
    epi_data_period <- epi_data %>%
      filter(recordDate >= start_date & recordDate <= end_date)
    daily_period <- epi_data_period$daily_case
    new_hospital_period <- epi_data_period$daily_new_hosp
    # hospital_period <- epi_data_period$hospital
    daily_period_normal <- normalization_func(daily_period)
    hospital_period_normal <- normalization_func(new_hospital_period)
    
    # max_hist <- 5L
    # knl_width <- 0.5
    # te_stm_case[ii] <- te_cal_opt_hist_func(stm_period_normal,daily_period_normal,max_hist,knl_width)
    # te_stm_hosp[ii] <- te_cal_opt_hist_func(stm_period_normal,hospital_period_normal,max_hist,knl_width)
    
    max_time_lag <- 5L
    hist_len <- 1L
    k_nearest <- 4L
    knl_width <- 0.5
    te_stm_case[ii] <- te_cal_opt_lag_func(stm_period_normal,daily_period_normal,max_time_lag,k_nearest,hist_len,knl_width)
    te_stm_hosp[ii] <- te_cal_opt_lag_func(stm_period_normal,hospital_period_normal,max_time_lag,k_nearest,hist_len,knl_width)
    
    cc_stm_case[ii] <- round(cor(stm_period_normal,daily_period_normal),3)
    cc_stm_hosp[ii] <- round(cor(stm_period_normal,hospital_period_normal),3)
  }
  result <- list(te_stm_case_week = te_stm_case, te_stm_hosp_week = te_stm_hosp,
                 cc_stm_case_week = cc_stm_case, cc_stm_hosp_week = cc_stm_hosp,
                 week_end_date = week_end_date_list)
}








