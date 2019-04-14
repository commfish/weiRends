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

# data ----
# data inputs are date (mm/dd/yyyy) and weir count

read_csv('egegik/data/egegik.csv') -> egegik

# run functions ----

# format data
f_clean_data(egegik) -> df

# model Gompertz function
f_gomp_model(df) -> model
saveRDS(model, 'egegik/output/gomp_model.rda')

# model logistic function
f_logistic_model(df) -> model_logistic
saveRDS(model_logistic, 'egegik/output/logistic_model.rda')

# check model fits - did all models converge?
# if not may need to go into f_gomp_model and change the lower 
# and upper starting values isConv=TRUE
f_summary(model)
f_summary(model_logistic)

# which model performs better
# >0.50 = model 1
# <0.50 = model 2
f_deviance(model, model_logistic)

# get parameters
f_params(model) -> params
write_csv(params, 'egegik/output/params.csv')

# plot parameter fits - because why not?
# do any look out of place?
f_param_plot(params)
ggsave("egegik/figs/param_plot.png", dpi = 100, height = 4, width = 6.5, units = "in") 

# predict the model on a complete dataset
f_preds(df, model) -> preds 
write_csv(params, 'egegik/output/preds.csv')

# plot the predicted data (for analyst analysis)
f_pred_plot(preds)

# decadeal plots of predicted data
f_pred_plot_decade(preds)
ggsave("egegik/figs/pred_plot.png", dpi = 100, height = 8.5, width = 6.5, units = "in") 


# what is the minimum day that the weir should be in place?
# the Julian date that 95% of the modeled run has been observed - on average
f_run_through(df, preds) -> run_through
run_through <- 198 # adjusted for two odd years
write(run_through, 'egegik/output/run_through')

# the date in a more informative format
f_real_day(run_through)  

# plot the predicted data w/ removal point
f_pred_plot95(preds, run_through)
ggsave("egegik/figs/pred_plot.png", dpi = 100, height = 5, width = 6.5, units = "in") 

# dates the weirs would be removed based upon 1% rule
# for 5,4,3, or 2 days
f_remove_dates(preds, run_through) -> remove_dates

# Percent of the run that is caught at a given risk level
f_run_caught(preds, remove_dates) %>% 
  write_csv(., 'egegik/output/run_caught.csv')

# plot of missed run and risk
f_risk_plot(preds, remove_dates)
ggsave("egegik/figs/run_risk_plot.png", dpi = 100, height = 5, width = 6.5, units = "in") 

# Percent of risk at a given % of missed run
f_run_risk(preds, remove_dates) %>% 
  write_csv(., 'egegik/output/run_risk.csv')

# Median, 25% and 75% quantiles of weir end date
f_median_end_date(remove_dates) %>% 
  write_csv(., 'egegik/output/median_end_date.csv')
