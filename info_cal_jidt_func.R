library("rJava")
.jinit()

# Change location of jar to match yours:
#  IMPORTANT -- If using the default below, make sure you have set the working directory
#   in R (e.g. with setwd()) to the location of this file (i.e. demos/r) !!
.jaddClassPath("/home/lijie/twitterData/infodynamics-dist-1.5/infodynamics.jar")

teCal_jidt_ksg_func <- function(srcArr,dstArr,k,histLen){
  # k: "4" as an example; histLen: 1L as an example
  # Create a TE calculator:
  teCalc<-.jnew("infodynamics/measures/continuous/kraskov/TransferEntropyCalculatorKraskov")
  .jcall(teCalc,"V","setProperty", "k", k) # Use Kraskov parameter K=4 for 4 nearest points
  
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
  #  implementingClass <- "infodynamics/measures/continuous/kraskov/MutualInfoCalculatorMultiVariateKraskov1" # MI([1,2], [3,4]) = 0.36353
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
    if(mi>opt_mi){
      opt_mi <- mi
      opt_lag <- i
    } else {
      next
    }
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
