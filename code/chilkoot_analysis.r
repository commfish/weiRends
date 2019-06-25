# Weir 1% rule
# Chilkoot R. analysis

# ben.williams@alaska.gov
# 2019-03

# Notes:
# 1. calculate end date of weir deployment
# 2. model tails of the runs, by year - Gompertz model
# 3. estimate the date required to operate through
# so that 95% of the escapement on average is observed
# 4. weir 1% rule - based upon 5,4, 3, or 2 days meeting the 1% requirement

# load ----
source('code/helper.r')
source('code/functions.r')

# globals ----
folder <- 'chilkoot'

# data ----
# data inputs are date (mm/dd/yyyy) and weir count

read_csv('data/chilkoot_weir_1976-2018.csv') %>% 
  filter(Species=='Sockeye') %>% 
  dplyr::select(date, count) -> chilkoot

# run functions ----

# format data
f_clean_data(chilkoot) -> df

# model
#f_gomp_model(df) -> model
#saveRDS(model, paste0('output/', folder, '/model.rda'))
readRDS(paste0('output/', folder, '/model.rda')) -> model

# model logistic function
#f_logistic_model(df) -> model_logistic
#saveRDS(model_logistic, paste0('output/', folder, '/logistic_model.rda'))
readRDS(paste0('output/', folder, '/logistic_model.rda')) -> model_logistic

# check model fits - did all models converge?
# if not may need to go into f_gomp_model and change the lower 
# and upper starting values isConv=TRUE
f_summary(model)
f_summary(model_logistic)

# which model performs better
# >0.50 = model 1
# <0.50 = model 2
f_deviance(model, model_logistic) # check model fits - did all models converge?

# get parameters
f_params(model_logistic) -> params

# plot parameter fits - because why not?
f_param_plot(params)

# predict the model on a complete dataset
f_preds(df, model_logistic) -> preds

# what is the minimum day that the weir should be in place?
# the Julian date that 95% of the modeled run has been observed - on average
f_run_through(preds) -> run_through

# plot the predicted data
f_pred_plot(preds, run_through)

# plot the predicted data
f_pred_plot_decade(preds, run_through)

# dates the weirs would be removed based upon 1% and 0.05% rule
# for 5,4,3, or 2 days
f_remove_dates(preds, run_through) -> remove_dates 
f_remove_dates_05(preds, run_through) -> remove_dates_05 

# Percent of the run that is caught at a given risk level
f_run_caught(preds, remove_dates)
f_run_caught(preds, remove_dates_05)

# plot of missed run and risk
f_risk_plot(preds, remove_dates)
f_risk_plot(preds, remove_dates_05)

# Percent of risk at a given % of missed run
f_run_risk(preds, remove_dates)
f_run_risk(preds, remove_dates_05)

# Median, 25% and 75% quantiles of weir end date
f_median_end_date(remove_dates)
f_median_end_date(remove_dates_05)
