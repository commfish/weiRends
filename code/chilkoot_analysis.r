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

read_csv('data/chilkoot_weir_1976-2018.csv') %>% 
  filter(Species=='Sockeye') %>% 
  dplyr::select(date, count) -> chilkoot

# run functions ----

# format data
f_clean_data(chilkoot) -> df

# model Gompertz function
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
f_deviance(model, model_logistic) -> dev # check model fits - did all models converge?
dev <-  as.data.frame(dev)
write.csv(dev, "data/processed/chilkoot_dev.csv")

# get parameters of preferred model
f_params(model_logistic) -> params

# plot parameter fits - because why not?
# do any look out of place?
f_param_plot(params)
ggsave("figs/chilkoot/param_plot.png", dpi = 100, height = 5, width = 7, units = "in") 

# predict the preferred model on a complete dataset
f_preds(df, model_logistic) -> preds 

# plot the predicted data
f_pred_plot(preds)
ggsave("figs/chilkoot/preds_plot.png", dpi = 100, height = 5, width = 7, units = "in") 

# plot in decadel scale so easier to read
preds %>%
  filter (year< 1990)-> year_subset
f_pred_plot(year_subset)
ggsave("figs/chilkoot/preds_plot_decadel.png", dpi = 100, height = 5, width = 7, units = "in") 

preds %>%
  filter (year > 1989 & year < 2000 )-> year_subset
f_pred_plot(year_subset)
ggsave("figs/chilkoot/preds_plot_deacadel2.png", dpi = 100, height = 5, width = 7, units = "in")

preds %>%
  filter (year > 1999 )-> year_subset
f_pred_plot(year_subset)
ggsave("figs/chilkoot/preds_plot_deacadel3.png", dpi = 100, height = 5, width = 7, units = "in") 

# what is the minimum day that the weir should be in place?
# the Julian date that 95% of the modeled run has been observed - on average
f_run_through(df, preds) -> run_through
run_through_chilkoot <-  as.data.frame(run_through)
write.csv(run_through_chilkoot, "data/processed/chilkoot_run_through.csv")

# the date in a more informative format
f_real_day(run_through) -> real_day 
real_day_chilkoot <-  as.data.frame(real_day)
write.csv(real_day_chilkoot, "data/processed/chilkoot_real_day.csv")

#add figure that shows the average 95% (dotted vertical line) and 95% julian date by year (star)
f_preds_plot95(preds, run_through) 
ggsave("figs/chilkoot/preds_plot95.png", dpi = 100, height = 5, width = 7, units = "in") 

# dates the weirs would be removed based upon 1% rule
# for 5,4,3, or 2 days
f_remove_dates(preds, run_through) -> remove_dates

# Percent of the run that is caught at a given risk level
f_run_caught(preds, remove_dates) -> run_caught_chilkoot
write.csv(run_caught_chilkoot, "data/processed/run_caught_chilkoot.csv")

# Number of years included in the percent of the run that is caught at a given risk level
f_run_caught_n(preds, remove_dates) -> run_caughtn_chilkoot
write.csv(run_caughtn_chilkoot, "data/processed/run_caughtn_chilkoot.csv")

# plot of missed run and risk (based on the output from f_run_caught)
f_risk_plot(preds, remove_dates)

# Percent of risk at a given % of missed run
f_run_risk(preds, remove_dates) -> run_risk_chilkoot
write.csv(run_risk_chilkoot, "data/processed/run_risk_chilkoot.csv")

# Median, 25% and 75% quantiles of weir end date
f_median_end_date(remove_dates) -> median_end_date_chilkoot
write.csv(median_end_date_chilkoot, "data/processed/median_end_date_chilkoot.csv")