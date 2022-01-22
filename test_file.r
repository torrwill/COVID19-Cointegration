# R file dedicated to testing snippets of code
# Author: Will Torres

#################################
# import data from PATH
# Rewrite to get from github folder in the future

# retrieve data from the csv files from PATH
covid_data <- list.files("C:/Users/torrw/Desktop/iCompBio/Syed_cointegration_covid19/county3RWM", pattern = ".csv", all.files = TRUE, full.names = TRUE)

# get list element for covid_data
csv_data <- lapply(covid_data, read.csv)

# Set the name of each list element to its respective file name.
# NOTE: full.names = FALSE to get only the file names, not the full path.
names(csv_data) <- gsub(".csv","", list.files("C:/Users/torrw/Desktop/iCompBio/Syed_cointegration_covid19/county3RWM", pattern = ".csv", all.files = TRUE, full.names = FALSE), fixed = TRUE)
names(csv_data) <- gsub(", ", "-", names(csv_data), fixed = TRUE)

# NOTE: any of the files can be referred to by covid_data[["City-State-US"]]

# Determine the amount of counties, 
# then run from a range of the number of counties. 
# There will be a variable, 
# called count that keeps track of the counties analyzed. 
# The program will run until that value of count is == length.
# -- Based on the current index, 
# a function will be called to set the csv variables. 
# For example, if the 1st index is Acadia, LA,
# then it will take that data and analyze it.

count <- 0
csv_length <- as.integer(length(csv_data))
csv_length


# Conditions
time_series_created = FALSE
systems_created = FALSE
lag_selected = FALSE
test_completed = FALSE
summaries_created = FALSE


# DailyCases were converted into time series data. 
# Since dates were not mentioned in the  Date format (shown as string)
# Thus general index were used (1 to 248)
# Declaring time series object
timeSeries_1 <- function(count) {
  data_frame <- csv_data[count]
  data_frame[is.na(data_frame)] = 0
  dailyCases_ts = ts(data_frame$dailyCases, start = 1, end=248, frequency = 1)
  return(dailyCases_ts)
} # test function

timeSeries_2 <- function(count) {
  data_frame <- csv_data[[count]]
  data_frame[is.na(data_frame)] = 0
  # structure for each function
  dailyCases_ts = ts(data_frame$dailyCases, 
                     start = 1, end=248, frequency = 1)
  
  # return value
  return(dailyCases_ts, airTemp_ts, RH_ts, Rt_ts, apple_driving_ts, Google_workplace_ts, Google_residential_ts)
} # test function


# Time series code
############################################################

# create a function for each time series;
# the result will create a time series object for every file.
# Since dates were not mentioned in the  Date format (shown as string)
# Thus general index were used (1 to 248)
# Declaring time series object

# dailyCases time series function
DC_timeSeries <- function(count) {
  data_frame <- csv_data[[count]]
  data_frame[is.na(data_frame)] = 0
  
  dailyCases_ts = ts(data_frame$dailyCases, 
                     start = 1, end=248, frequency = 1)
  
  return(dailyCases_ts)
}

#air_temp time series function
AT_timeSeries <- function(count) {
  data_frame <- csv_data[[count]]
  data_frame[is.na(data_frame)] = 0
  
  air_temp_ts = ts(data_frame$air_temp, 
                   start = 1, end=248, frequency = 1)
  
  return(air_temp_ts)
}

#RH time series function
RH_timeSeries <- function(count) {
  data_frame <- csv_data[[count]]
  data_frame[is.na(data_frame)] = 0
  
  RH_ts = ts(data_frame$RH, 
             start = 1, end=248, frequency = 1)
  
  return(RH_ts)
}

#Rt time series function
Rt_timeSeries <- function(count) {
  data_frame <- csv_data[[count]]
  data_frame[is.na(data_frame)] = 0
  
  Rt_ts = ts(data_frame$Rt, 
             start = 1, end=248, frequency = 1)
  
  return(Rt_ts)
}

#apple_driving time series function
AD_timeSeries <- function(count) {
  data_frame <- csv_data[[count]]
  data_frame[is.na(data_frame)] = 0
  
  apple_driving_ts = ts(data_frame$AppleDriving, 
                        start = 1, end=248, frequency = 1)
  
  return(apple_driving_ts)
}

#Google_workplace time series function
GW_timeSeries <- function(count) {
  data_frame <- csv_data[[count]]
  data_frame[is.na(data_frame)] = 0
  
  Google_workplace_ts = ts(data_frame$GoogleWorkplace, 
                           start = 1, end=248, frequency = 1)
  
  return(Google_workplace_ts)
}

#Google_residential time series function
GR_timeSeries <- function(count) {
  data_frame <- csv_data[[count]]
  data_frame[is.na(data_frame)] = 0
  
  Google_residential_ts <- ts(data_frame$GoogleResidential, 
                              start = 1, end=248, frequency = 1)
  
  return(Google_residential_ts)
}

# Results of Time-Series functions
DC_result <- lapply(seq_len(csv_length), DC_timeSeries)
AT_result <- lapply(seq_len(csv_length), AT_timeSeries)
RH_result <- lapply(seq_len(csv_length), RH_timeSeries)
Rt_result <- lapply(seq_len(csv_length), Rt_timeSeries)
AD_result <- lapply(seq_len(csv_length), AD_timeSeries)
GW_result <- lapply(seq_len(csv_length), GW_timeSeries)
GR_result <- lapply(seq_len(csv_length), GR_timeSeries)

get_df_name <- function(dataframe, count) {
  df.name <- names(dataframe[count])
  return(df.name)
}

# test time series in for loop
for (i in 1:csv_length) {
  count = i
  print(count)
  # time series creation
  if (time_series_created == TRUE) {  
    time_series_created = FALSE
    print(time_series_created)
    
  } else {
    
    dailyCases        <- DC_timeSeries(count)
    airTemp           <- AT_timeSeries(count)
    RH                <- RH_timeSeries(count)
    Rt                <- Rt_timeSeries(count)
    appleDriving      <- AD_timeSeries(count)
    GoogleWorkplace   <- GW_timeSeries(count)
    GoogleResidential <- GR_timeSeries(count)
    
    title <- get_df_name(csv_data, count)
    
    plot(dailyCases, main = title)
    plot(airTemp, main = title)
    plot(RH, main = title)
    plot(Rt, main = title)
    plot(appleDriving, main = title)
    plot(GoogleWorkplace, main = title)
    plot(GoogleResidential, main = title)
    
    time_series_created = TRUE
    print(time_series_created)
  } # if condition end
} # for-loop end

# System creation code
############################################################

# System Functions
################################################################################

# create a function that uses a for loop to set the systems:
# I.E., system[x] = cbind(Rt, [y])
#
# x = the index; system number
# y = the time series associated with the index
# For example, x = 1 --> y = airTemp

################################################################################

test_var = list()

for (i in paste0('test', c(1 : 5))) {
  test_var[[i]] <- merge(split[[i]])
}


test_function <- function (x, y) {cbind(x, y)}
system_1 <- mapply(test_function, RH, airTemp))

# compare : 
# Rt_ts : Reproduction No
# air_temp_ts : air temperature 

system1 = cbind(Rt, airTemp)
head(system1)

# compare : 
# Rt_ts : Reproduction No
# RH_ts : Relative humidity 

system2 = cbind(Rt, RH)
head(system2)

# compare : 
# Rt_ts : Reproduction No
# apple_driving_ts : Apple Driving 

system3 = cbind(Rt, appleDriving)
head(system3)

# compare : 
# Rt_ts : Reproduction No
# Google_workplace_ts : Google Workplace

system4 = cbind(Rt, GoogleWorkplace)
head(system4)

# compare : 
# Rt_ts : Reproduction No
# Google_residential_ts : Google Residential

system5 = cbind(Rt, GoogleResidential)
head(system5)

# Lag Selection Functions (using vars library)
################################################################################

# create a function that uses a loop to set the lag:
# I.E., lagselect[x] = VARselect([y], lag.max = 7, type = "const")
#       K_val[x] = strtoi(names(sort(summary(as.factor(lagselect[x]$selection)), decreasing=T)[1]))-1)
#       if statement: K_val[x] < 2 ...and so on
#
# x = the index; the selection number
# y = the system associated with the index

################################################################################
library(vars)

# lag select 1
# lag selection using VAR technique
lagselect1 = VARselect(system1, lag.max = 7, type = "const")
lagselect1

# lagselect1$selection   
# as.factor(lagselect1$selection)
# summary(as.factor(lagselect1$selection))
# sort(summary(as.factor(lagselect1$selection)), decreasing=T)[1]
# names(sort(summary(as.factor(lagselect1$selection)), decreasing=T)[1])

K_val1 =strtoi(names(sort(summary(as.factor(lagselect1$selection)), decreasing=T)[1]))-1
K_val1

# suggestion value
if (K_val1<2){
  K_val1=2
} 

# lag select 2
# lag = 7

# time-point 100 days    before 100 days  then forecast  next 100 day data 

lagselect2 = VARselect(system2, lag.max = 7, type = "const")
K_val2 =strtoi(names(sort(summary(as.factor(lagselect2$selection)), decreasing=T)[1]))-1

if (K_val2<2){
  K_val2=2
}
lagselect2
K_val2

# lag select 3

lagselect3 = VARselect(system3, lag.max = 7, type = "const")
K_val3 =strtoi(names(sort(summary(as.factor(lagselect3$selection)), decreasing=T)[1]))-1

if (K_val3<2){
  K_val3=2
}

K_val3

# lag select 4

lagselect4 = VARselect(system4, lag.max = 7, type = "const")
K_val4 =strtoi(names(sort(summary(as.factor(lagselect4$selection)), decreasing=T)[1]))-1

if (K_val4<2){
  K_val4=2
}

K_val4

# lag select 5
lagselect5 = VARselect(system5, lag.max = 7, type = "const")
#lagselect3$selection
K_val5 =strtoi(names(sort(summary(as.factor(lagselect5$selection)), decreasing=T)[1]))-1

if (K_val5<2){
  K_val5=2
}

K_val5

# test code
lagselect1 = VARselect(system1, lag.max = 7, type = "const")
K_val1 = strtoi(names(sort(summary(as.factor(lagselect1$selection)), decreasing=T)[1]))-1

# lag select 2
lagselect2 = VARselect(system2, lag.max = 7, type = "const")
K_val2 = strtoi(names(sort(summary(as.factor(lagselect2$selection)), decreasing=T)[1]))-1

# lag select 3
lagselect3 = VARselect(system3, lag.max = 7, type = "const")
K_val3 = strtoi(names(sort(summary(as.factor(lagselect3$selection)), decreasing=T)[1]))-1

# lag select 4
lagselect4 = VARselect(system4, lag.max = 7, type = "const")
K_val4 = strtoi(names(sort(summary(as.factor(lagselect4$selection)), decreasing=T)[1]))-1

# lag select 5
lagselect5 = VARselect(system5, lag.max = 7, type = "const")
#lagselect3$selection
K_val5 = strtoi(names(sort(summary(as.factor(lagselect5$selection)), decreasing=T)[1]))-1

k_vals <- c(K_val1, K_val2, K_val3, K_val4, K_val5)
print(k_vals)

set_k_vals <- function(k_vals) {
 
  kval_length = as.integer(length(k_vals))
  
  for (i in 1:kval_length) {
    if (k_vals[i] < 2) {
      k_vals[i] = 2
    }
  }
  return(k_vals)
}

k_vals <- set_k_vals(k_vals)

print(k_vals)


# coint test code
############################################################
# IMPORT LIBRARIES
library(urca)
library(vars)
library(ggplot2)
library("tseries")

systems <- c(system1, system2, system3, system4, system5)

# JOHANSEN TEST (trace) - TEST 1
test2_df <- data.frame(matrix(ncol = 9, nrow = 0))
x <- c("Location", "Start Date", "End Date", "Test Method", "Test Type", "Co-Efficient 1", "Co-Efficient 2", "Test Statistic", "P Value")
colnames(test2_df) <- x
test2_df

skip_to_next <- FALSE

johansen_error_handling_1 <- function(k_val){
  tryCatch(
    # This is what I want to do...
    {
      test1 = ca.jo(system1, type='trace', ecdet = 'const', K=k_vals[1])
      test2 = ca.jo(system2, type='trace', ecdet = 'const', K=k_vals[2])
      test3 = ca.jo(system3, type='trace', ecdet = 'const', K=k_vals[3])
      test4 = ca.jo(system4, type='trace', ecdet = 'const', K=k_vals[4])
      test5 = ca.jo(system5, type='trace', ecdet = 'const', K=k_vals[5])
      message("Everything went smoothly.")
    },
    # ... but if an error occurs, tell me what happened: 
    error=function(error_message) {
      message("And below is the error message from R:")
      message(error_message, "\n")
      skip_to_next <<- TRUE
      test2_df[nrow(test2_df) + 1,] = list("NA","NA","NA","NA","NA","NA","NA","NA","NA")
      return(test2_df)
    }
  )
}


if(skip_to_next) { next }     

johansen_error_handling_1(k_vals)

df <- data.frame(Name = c("Jon", "Bill", "Maria", "Tom", "Emma"),
                 Age = c(23,41,32,55,40)
)

test1 = ca.jo(system1, type='trace', ecdet = 'const', K=k_vals[1])
test2 = ca.jo(system2, type='trace', ecdet = 'const', K=k_vals[2])
test3 = ca.jo(system3, type='trace', ecdet = 'const', K=k_vals[3])
test4 = ca.jo(system4, type='trace', ecdet = 'const', K=k_vals[4])
test5 = ca.jo(system5, type='trace', ecdet = 'const', K=k_vals[5])
summary(test1)

# summary_str <- paste0(deparse(summary(test1)))
# test1_df <- (sapply(test1, summary))

# set cointegration weight variables
test1_coint_eq_var1 <- abs(test1@V[2,1])
test1_coint_eq_var2 <- abs(test1@V[3,1])

test2_coint_eq_var1 <- abs(test2@V[2,1])
test2_coint_eq_var2 <- abs(test2@V[3,1])

test3_coint_eq_var1 <- abs(test3@V[2,1])
test3_coint_eq_var2 <- abs(test3@V[3,1])

test4_coint_eq_var1 <- abs(test4@V[2,1])
test4_coint_eq_var2 <- abs(test4@V[3,1])

test5_coint_eq_var1 <- abs(test5@V[2,1])
test5_coint_eq_var2 <- abs(test5@V[3,1])

# declare cointegration weights
test1_w = Rt - test1_coint_eq_var1 * airTemp + test1_coint_eq_var2
test2_w = Rt - test2_coint_eq_var1 * RH + test2_coint_eq_var2
test3_w = Rt - test3_coint_eq_var1 * appleDriving + test3_coint_eq_var2
test4_w = Rt - test4_coint_eq_var1 * GoogleWorkplace + test4_coint_eq_var2
test5_w = Rt - test5_coint_eq_var1 * GoogleResidential + test5_coint_eq_var2

plot(test1_w, type="l", main = paste0(title," | Rt compared to airTemp"))
plot(test2_w, type="l", main = paste0(title," | Rt compared to RH"))
plot(test3_w, type="l", main = paste0(title," | Rt compared to appleDriving"))
plot(test4_w, type="l", main = paste0(title," | Rt compared to GoogleWorkplace"))
plot(test5_w, type="l", main = paste0(title," | Rt compared to GoogleResidential"))

# AUGMENTED DICKEY FULLER
adf_t1 <- adf.test(test1_w)
adf_t2 <- adf.test(test2_w)
adf_t3 <- adf.test(test3_w)
adf_t4 <- adf.test(test4_w)
adf_t5 <- adf.test(test5_w)

# JOHANSEN TEST (MaxEigen)

print("------Eigen value testing----")

test1ei = ca.jo(system1, type='eigen', ecdet = 'const', K=k_vals[1])
test2ei = ca.jo(system2, type='eigen', ecdet = 'const', K=k_vals[2])
test3ei = ca.jo(system3, type='eigen', ecdet = 'const', K=k_vals[3])
test4ei = ca.jo(system4, type='eigen', ecdet = 'const', K=k_vals[4])
test5ei = ca.jo(system5, type='eigen', ecdet = 'const', K=k_vals[5])

summary(test1ei)
summary(test2ei)
summary(test3ei)
summary(test4ei)
summary(test5ei)

##########
# send to dataframe? (example code)
test1_sum = summary(test1)
str(test1_sum)
test1_data <- cbind(test1_sum@teststat, test1_sum@cval)

test2_sum = summary(test2)
str(test2_sum)
test2_data <- cbind(test2_sum@teststat, test2_sum@cval)

test3_sum = summary(test3)
str(test3_sum)
test3_data <- cbind(test3_sum@teststat, test3_sum@cval)

test4_sum = summary(test4)
str(test4_sum)
test4_data <- cbind(test4_sum@teststat, test4_sum@cval)

test5_sum = summary(test5)
str(test5_sum)
test5_data <- cbind(test5_sum@teststat, test5_sum@cval)

test_summaries <- list(test1_data, test2_data, test3_data, test4_data, test5_data)
test_summaries
# example
#                    10pct  5pct  1pct
# r <= 1 | 26.642484 18.90 21.07 25.75
# r = 0  | 38.489175 24.78 27.14 32.14

# code that adds to df :)

# then send to workbook?
require(openxlsx)
list_of_datasets <- list("test1" = test_summaries)
write.xlsx(test_summaries, file = "writeXLSX2.xlsx")


#####
county = as.name(title)
data_csv = get(title,csv_data)
date_test = data_csv$date

start = data_csv$date[1]
end = data_csv$date[248]
test_name = test1@test.name
test_type = test1@type
test_coeff_1 = abs(test1@V[2,1])
test_coeff_2 = abs(test1@V[3,1])
test_stat = test1@teststat[2]
p_val = adf_t1$p.value


Location = c(title, title, title, title, title)
Start_Date = c(start, start, start, start, start)
End_Date = c(end, end, end, end, end)
Test_Method = c(test_name, test_name, test_name, test_name, test_name)
Test_Type = c(test_type, test_type, test_type, test_type, test_type)
Co_eff_1 = c(test_coeff_1, test_coeff_1, test_coeff_1, test_coeff_1, test_coeff_1)
Co_eff_2 = c(test_coeff_2, test_coeff_2, test_coeff_2, test_coeff_2, test_coeff_2)
Test_Stat = c(test_stat, test_stat, test_stat, test_stat, test_stat)
P_Val = c(p_val, p_val, p_val, p_val, p_val)

test_df <- data.frame(Location, Start_Date, End_Date, Test_Method, Test_Type, Co_eff_1, Co_eff_2, Test_Stat, P_Val)

test_df[nrow(test_df) + 1,] = list("NA","NA","NA","NA","NA","NA","NA","NA","NA")

exported_df[nrow(exported_df) + 1,] = list(title, start, end, test_name, test_type, test_coeff_1, test_coeff_2, test_stat, p_val)

#####
test2_df <- data.frame(matrix(ncol = 9, nrow = 0))
x <- c("Location", "Start Date", "End Date", "Test Method", "Test Type", "Co-Efficient 1", "Co-Efficient 2", "Test Statistic", "P Value")
colnames(test2_df) <- x
test2_df

test2_df[nrow(test2_df) + 1,] = list("NA","NA","NA","NA","NA","NA","NA","NA","NA")
#####

skip_to_next <- FALSE

# Note that print(b) fails since b doesn't exist

tryCatch(print(b), error = function(e) { skip_to_next <<- TRUE})

if(skip_to_next) { next }     