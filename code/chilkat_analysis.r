# Weir 1% rule
# Chilkat R. analysis

# ben.williams@alaska.gov
# 2019-03
# last updated by SEM: October 15, 2021
# Notes:
# 1. calculate end date of weir deployment
# 2. model tails of the runs, by year - Gompertz or logistic model
# 3. estimate the date required to operate through so that 95% of the escapement on average is observed
# 4. weir 1% rule - based upon 5,4, 3, or 2 days meeting the 1% requirement

# load ----
source('code/helper.r')
source('code/functions_Chilkat.r')
source('code/functions.r')

# globals ----
folder <- 'chilkat'

# data ----
# data inputs are date (mm/dd/yyyy) and weir count
read_csv('data/chilkat_weir_1971-2021.csv') %>% 
  filter(species=='Sockeye') %>%
  filter(year>2011) %>% # subset data for only the last ten years
  dplyr::select(date, count) -> chilkat

# run functions ----

# format data
f_clean_data(chilkat) -> df
df %>% 
  filter(!year >=1996 | !year <= 1998)-> df # for the analysis, include entire time series (a few years of missing data for the entire year)

# model gompertz function
f_gomp_model(df) -> model_gompertz
saveRDS(model_gompertz, paste0('output/', folder, '/gompertz_model.rda'))
readRDS(paste0('output/', folder, '/gompertz_model.rda')) -> model_gompertz

# model logistic function
f_logistic_model(df) -> model_logistic
saveRDS(model_logistic, paste0('output/', folder, '/logistic_model.rda'))
readRDS(paste0('output/', folder, '/logistic_model.rda')) -> model_logistic

# check model fits - did all models converge?
# if not may need to go into f_gomp_model and change the lower 
# and upper starting values isConv=TRUE
f_summary(model_gompertz)
f_summary(model_logistic)

# which model performs better
# >0.50 = model 1 (model_gompertz)
# <0.50 = model 2 (model_logistic)
f_deviance(model_gompertz, model_logistic) # check model fits - did all models converge?
# need to change all references to correct model (below) after 'best' model is chosen

# get parameters
f_params(model_gompertz) -> params # choose model based on deviance

# plot parameter fits - because why not?
f_param_plot(params)

# predict the model on a complete dataset
f_preds(df, model_gompertz) -> preds #preds.csv

# table of data cumsum (raw data) and fit_cumsum
f_table_output(preds) # total count of raw versus fitted; summary_table.csv

# plot of data cumsum (raw data) and fit_cumsum
tickryr <- data.frame(year = 2010:2025)# may need to adjust based on year_num
axisf <- tickr(tickryr, year, 1)
f_plot_output_chilkat(preds) # fitted_plot.png

# what is the minimum day that the weir should be in place?
# the 95th percentile of the Julian date that 95% of the modeled run has been observed in the last 10 years only - 
f_run_through(preds) -> run_through # run_through.csv file

# plot the predicted data and fits 
f_run95(preds, run_through) # outputs the 95% cumsum by year as a Julian date; run_95.csv
f_pred_plot(preds, run_through) # pred_plot.png
f_pred1999_plot_chilkat(preds, run_through) # pred_plot1990.png
f_pred2000_plot_chilkat(preds, run_through) # pred_plot1991.png
f_pred2010_plot_chilkat(preds, run_through) # pred_plot2001.png
f_pred2015_plot_chilkat(preds, run_through)
# dates the weirs would be removed based upon 1%  rule
# for 5,4,3, or 2 days
f_remove_dates(preds, run_through) -> remove_dates 
f_remove_dates_table(preds, run_through) # remove_dates_table.csv

# percent of the run that is caught at a given risk level
f_percent_missed (preds, remove_dates) # percent_missed.csv
f_run_caught(preds, remove_dates) # remove_dates_run_caught.csv

# plot of missed run and risk
f_risk_plot(preds, remove_dates) # remove_dates_risk_plot.png

# median, 25% and 75% quantiles of weir hard date
f_median_hard_date(remove_dates)

## global function implementation ##-----------------------------------------------------------------------------------------------------
# data ----
# data inputs are date (mm/dd/yyyy) and weir count
read_csv('data/chilkat_weir_1971-2021.csv') %>% 
  filter(species=='Sockeye') %>%
  filter(year>2011) %>% # subset data for only the last ten years
  dplyr::select(date, count) -> chilkat

# run functions ----

# format data
f_clean_data(chilkat) -> df
df %>% 
  filter(!year >=1996 | !year <= 1998)-> df # for the analysis, include entire time series (a few years of missing data for the entire year)

df %>%
  mutate (stream_name = julian) %>%
  dplyr::select(year, stream_name, count, date, Year, cumsum) %>%
  impute_global(., Year_column="year")

read_csv(paste0('output/', folder, '/EM_algorithm_model.csv'))  %>% 
  arrange(year, stream_name) %>%
  group_by(.,year) %>% 
  mutate(julian = stream_name,
         fit_run = count,
         fit_cumsum = cumsum(fit_run),
         Year = factor(year)) %>%
  dplyr::select(julian, year, Year, fit_run, fit_cumsum, Year) -> preds


df %>%
  dplyr::select(julian, year, count, cumsum) %>%
  merge(., preds) -> preds
write_csv(preds, paste0('output/', folder, '/preds.csv'))  

# table of data cumsum (raw data) and fit_cumsum
f_table_output(preds) # total count of raw versus fitted; summary_table.csv


# plot of data cumsum (raw data) and fit_cumsum
tickryr <- data.frame(year = 1970:2025) # may need to adjust based on year_num
axisf <- tickr(tickryr, year, 5)
f_plot_output_chilkat(preds) # fitted_plot.png

# what is the minimum day that the weir should be in place?
# the 95th percentile of the Julian date that 95% of the modeled run has been observed in the last 10 years only - 
f_run_through(preds) -> run_through # run_through.csv file

# plot the predicted data and fits 
f_run95(preds, run_through) # outputs the 95% cumsum by year as a Julian date; run_95.csv
f_pred_plot(preds, run_through) # pred_plot.png
f_pred1999_plot_chilkat(preds, run_through) # pred_plot1990.png
f_pred2000_plot_chilkat(preds, run_through) # pred_plot1991.png
f_pred2010_plot_chilkat(preds, run_through) # pred_plot2001.png
f_pred2015_plot_chilkat(preds, run_through)
# dates the weirs would be removed based upon 1%  rule
# for 5,4,3, or 2 days
f_remove_dates(preds, run_through) -> remove_dates 
f_remove_dates_table(preds, run_through) # remove_dates_table.csv

# percent of the run that is caught at a given risk level
f_percent_missed (preds, remove_dates) # percent_missed.csv
f_run_caught(preds, remove_dates) # remove_dates_run_caught.csv

# plot of missed run and risk
f_risk_plot(preds, remove_dates) # remove_dates_risk_plot.png

# median, 25% and 75% quantiles of weir hard date
f_median_hard_date(remove_dates)
