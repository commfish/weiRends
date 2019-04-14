---
output:
  pdf_document: default
  html_document: default
---
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
2. f_gomp_model = runs a Gompertz model for each year
3. f_logistic_model = runs a logistic model for each year
4. f_summary = examine model outputs
5. f_deviance = compare Gompertz and logistic models and proceed with best fitting model; If deviance >0.50, then the Gompertz Model is preferred, and if deviance <0.50, then the logistic model is preferred
6. f_params = get parameter outputs 
7. f_param_plot = plot parameter values 
8. f_preds = predict the model
9. f_pred_plot = examine predictions
10. f_run_through = the weir should be in place through this date
11. f_real_day = change julian date to a date people can understand
12. f_preds_plot95 = examine predictions and 95% cut-off date
13. f_run_caught = percent of the run that is caught at a given risk level
14. f_run_caught_n = number of years included in the percent of the run that is caught at a given risk level
15. f_risk_plot = percent of missed run at a given level of risk (the inverse of # 13)
16. f_run_risk = percent of risk at a given percent of missed run
17. f_median_end_dates = median, 25%, and 75% quantiles of weir removal dates (julian) for 1% rules
