# Weir 1% rule
# Situk R. analysis

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

read_csv('data/situk_weir_1988-2018.csv') %>% 
  filter(species=='Sockeye') %>% 
  dplyr::select(date, count) -> situk

# run functions ----

# format data
f_clean_data(situk) -> df

# model
f_gomp_model(df) -> model

# model logistic function
f_logistic_model(df) -> model_logistic

# check model fits - did all models converge?
# if not may need to go into f_gomp_model and change the lower 
# and upper starting values isConv=TRUE
f_summary(model)
f_summary(model_logistic)

# which model performs better
# >0.50 = model 1
# <0.50 = model 2
f_deviance(model, model_logistic)# check model fits - did all models converge?

# get parameters
f_params(model) -> params

# plot parameter fits - because why not?
f_param_plot(params)
ggsave("figs/situk/param_plot.png", dpi = 100, height = 5, width = 7, units = "in") 

# predict the model on a complete dataset
f_preds(df, model) -> preds

# plot the predicted data
f_pred_plot( preds)
ggsave("figs/situk/preds_plot.png", dpi = 100, height = 5, width = 7, units = "in") 

# what is the minimum day that the weir should be in place?
# the Julian date that 95% of the modeled run has been observed - on average
f_run_through(df, preds) -> run_through

# the date in a more informative format
f_real_day(run_through)  

# dates the weirs would be removed based upon 1% rule
# for 5,4,3, or 2 days
f_remove_dates(preds, run_through) -> remove_dates

# Percent of the run that is caught at a given risk level
f_run_caught(preds, remove_dates)

# plot of missed run and risk
f_risk_plot(preds, remove_dates)
ggsave("figs/situk/risk_plot.png", dpi = 100, height = 5, width = 7, units = "in") 

# Percent of risk at a given % of missed run
f_run_risk(preds, remove_dates)

# Median, 25% and 75% quantiles of weir end date
f_median_end_date(remove_dates)
