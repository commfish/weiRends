## Initial request

Request 2019-01-23  
From: Steve Heinl  
Requests:  
    1.  model the tails of the run to quantify a hard ending date for weir operations – the date to which the weir would be required to be operated (e.g., would capture 95% of the escapement on average),  
    2. estimate % of counts missed (with 95% CI) if project was operated past the hard ending date until daily counts equaled less than 1% of cumulative count for 3, 4, or 5 days in a row.
    
## Weir 1% rule

Based upon seeing 1% of cumulative count for 2, 3, 4 or 5 days.

### Data format

Two columns:  
date (preferably in year-mm-dd format) and weir count data
date        count
2019-01-20  20

This is for a single species at a single weir. No other values or comments should be 
added to the file. It should preferably be in .csv format.    

### Analysis

All packages need to be loaded (see helper.r file) prior to running models.

There are a set of functions that need to be run in order:

1. f_clean_data = formats data for modeling
2. f_gomp_model = runs a gompertz model for each year
3. f_logistic_model = runs a logistic model for each year
4. f_summary = examine model outputs
5. f_deviance = compare gompertz and logistic models go forward with best fitting model
6. f_params = get parameter outputs 
7. f_param_plot = plot parameter values 
8. f_preds = predict the model
9. f_pred_plot = examine predictions
10. f_pred_plot = examine predictions by decade
11. f_run_through = the weir should be in place through this date
12. f_remove_dates = apply the 1% rule to determine when the weir would be pulled each year
13. f_remove_dates_05 = apply the 0.05% rule to determine when the weir would be pulled each year
14. f_run_caught = percent of the run that is caught at a given risk level
15. f_risk_plot = percent of missed run at a given level of risk (the inverse of # 14)
14. f_run_risk = percent of risk at a given percent of missed run
15. f_median_end_date = median, 25%, and 75% quantiles of weir removal dates (julian) for 1% or 0.05% rules

The five main functions are as follows:

*f_run_through*: For each year, this function filters the data to only include the cumulative sums that are less than or equal to 95% of the cumulative sum, and then determines the largest Julian date that corresponds to this. For example, the total cumulative sum for 1976 is 71,291 fish. The cumulative sum on Julian date 229 (67,546 fish) represents the cumulative sum that is less than or equal to $0.95 * 71,291 = 67,726$, and is the closest to 67,726. Next, this function summarizes this Julian date for each year (i.e., one Julian date is reported for each year; 229 for 1976 for example). Finally, this function determines the 0.95 quantile of these summarized Julian dates by year. This is reported as the run_through Julian date.

*f_remove_dates*: This function determines the Julian date that the weir should be removed based on a 1% rule for 2, 3, 4, or 5 days. This function first calculates the variables one_4, one_3, one_2, one_1, and one. These variables are based on different lags. Variable one_4 is lagged 4 days, variable one_3 is lagged three days, and so forth. For example, variable one_4 is the fitted run on day 147 divided by the cumulative sum on day 150. If this value is greater than 0.01 (i.e., 1%), then the variable one_4 is given a value of 1 for day 150; otherwise the variable is given a value of 0 for the particular Julian date. This is repeated for the other lag variables, including the variable one that has no lag. Next, variables two, three, four, and five (i.e., number of days) are created. Variable five is given a value of 1 if variables one_4 (lag of 4), one_3 (lag of 3), one_2 (lag of 2), one_1 (lag of 1), or one (no lag) are 1. Variable four is given a value of 1 if variables one_3, one_2, one_1, or one are 1. Variable three is given a value of 1 if variables one_2, one_1, or one are 1. Variable two is given a value of 1 if variables one_1, or one are 1. Next, by year, the data is filtered to only include Julian dates greater than or equal to the run through date (`r tbls_koot$run_through$end_date`) with a day value of one.            
    
*f_remove_dates_05*: This function is similar to the *f_remove_dates* function, but determines the Julian date that the weir should be removed based on a 0.05% rule for 2, 3, 4, or 5 days.

*f_run_caught*: For each year, this function first determines the difference variable which is equal to 1 - (the sum of the fitted run up to one day before the mean passage weir removal date (`r tbls_koot$run_through$end_date`)) divided by the maximum cumulative sum by year. Next, the % chance is calculated, by day, as 1 - the quantile (0.99, 0.95, 0.90, 0.80, 0.70, 0.60, 0.50) of the difference variable for each day (two, three, four, or five days). For example, the 0.95-quantile is equivalent to the 95-percentile and is such that 95% of the sample is below its value and 5% is above. This is done for both the 1% and the 0.05% rule.   

*f_run_risk*: This function determines the percent of the run that is caught at a given risk level (% chance) based upon the number of days the 1% rule is implemented for the Chilkoot River. For each year, this function first determines the difference variable which is equal to 1 - (the sum of the fitted run up to one day before the mean passage weir removal date (`r tbls_koot$run_through$date`)) divided by the maximum cumulative sum by year. Then the function fits a gamma distribution to this difference variable (i.e., the percent of the run missed based on the mean passage weir removal date (`r tbls_koot$run_through$date`). Using the rate and shape parameters from the gamma distribution fit, the inverse of the cumulative gamma distribution of the different bins is determined.    

*f_median_end_date*: This function determines the median, lower 25% quantile, upper 75% quantile of the weir removal dates based on the 1% or the 0.05% rule. 