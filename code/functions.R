#https://github.com/padpadpadpad/nls.multstart
# last updated by SEM: October 15, 2021
f_clean_data <- function(data){
  
  data %>%
    drop_na %>%
    mutate(date = mdy(date),
           year = year(date),
           julian = yday(date), 
           Year = factor(year)) -> df
  
  expand.grid(year = min(df$year):max(df$year),
              julian = (min(df$julian)):(max(df$julian))) -> all_days 
  
  df %>% 
    group_by(year) %>%
    mutate(cumsum = cumsum(count)) %>% 
    left_join(all_days, .)
}

f_gomp_model <- function(data){
  
  data %>% 
    group_by(year) %>% 
    summarise(lim = max(cumsum, na.rm = T)) %>% 
    ungroup() %>% 
    summarise(low = min(lim),
              up = max(lim)) %>%
    as.data.frame()-> x
  
  
  data %>%
    group_by(.,year) %>%
    nest() %>%
    mutate(fit = purrr::map(data, ~ nls_multstart(cumsum ~ pop * (exp(-exp(-k * (julian - t)))),
                                                  data = .x,
                                                  iter = 500,
                                                  start_lower = c(pop = x$low, k = 0.1, t = 170),
                                                  start_upper = c(pop = x$up, k = 0.3, t = 200),
                                                  supp_errors = 'Y',
                                                  na.action = na.omit)))
}

f_logistic_model <- function(data){
  
  data %>% 
    group_by(year) %>% 
    summarise(lim = max(cumsum, na.rm = T)) %>% 
    ungroup() %>% 
    summarise(low = min(lim),
              up = max(lim)) -> x
  
  
  data %>%
    group_by(.,year) %>%
    nest() %>%
    mutate(fit = purrr::map(data, ~ nls_multstart(cumsum ~ pop / (1 + exp(-k * (julian - t))),
                                                  data = .x,
                                                  iter = 500,
                                                  start_lower = c(pop = x$low, k = 0.1, t = 180),
                                                  start_upper = c(pop = x$up, k = 0.3, t = 190),
                                                  supp_errors = 'Y',
                                                  na.action = na.omit)))
}

f_summary <- function(model){
  
  model %>%
    mutate(summary = map(fit, glance)) %>%
    unnest(summary)%>%
    dplyr::select(-data, -fit) %>% 
    as.data.frame
}

f_deviance <- function(model1, model2){
  sum((f_summary(model1)$deviance - 
         f_summary(model2)$deviance)<0) /length(unique(model1$year)) 
}

f_params <- function(model){
  
  model %>%
    mutate(., p = map(fit, tidy)) %>%
    unnest(p) %>%
    dplyr::select(-data, -fit) -> params
  
  model %>% 
    mutate(., cis = map(fit, confint2),
           cis = map(cis, data.frame)) %>%
    unnest(cis) %>%
    rename(., conf.low = X2.5.., conf.high = X97.5..) %>%
    group_by(., year) %>%
    mutate(., term = c('pop', 'k', 't')) %>%
    ungroup() %>%
    dplyr::select(-data, -fit)-> CI
  
  merge(params, CI, by = intersect(names(params), names(CI))) %T>% 
    write_csv(paste0('output/', folder, '/params.csv'))
  
}

f_param_plot <- function(params){
  
  params %>% 
    rename (Year = year) %>%
    rename (Estimate = estimate) %>%
    mutate(term = ifelse(term=='pop', 'p', term)) %>% 
    ggplot(aes(Year, Estimate)) +
    geom_point(alpha = .2) +
    geom_line(alpha = 0.5) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3) +
    facet_wrap(~term, scales = 'free_y') -> x
  print(x)
  ggsave(paste0('figs/', folder, "/param_plot.png"), x, dpi = 100, 
         height = 5, width = 6.5, units = "in") 
  
}

f_preds <- function(data, model){
  
  y = deparse(substitute(model))
  
  expand.grid(julian = min(data$julian):max(data$julian),
              year = unique(data$year)) -> x
  
  model %>%
    mutate(., p = map(fit, tidy)) %>%
    unnest(p) %>% 
    spread(term, estimate) %>% 
    group_by(.,year) %>% 
    summarise_all(mean, na.rm=TRUE) %>%
    left_join(x, .) %>% 
    left_join(data) %>% 
    arrange(year, julian) %>% 
    group_by(.,year) %>% 
    mutate(fit = case_when(y == 'model_gompertz' ~ pop * (exp(-exp(-k * (julian - t)))),
                           y == 'model_logistic' ~  pop / (1 + exp(-k * (julian - t)))),
           fit_count = round(c(fit[1], diff(fit)), 0),
           fit_run = ifelse(is.na(cumsum), fit_count, count),
           fit_cumsum = cumsum(fit_run),
           Year = factor(year)) %T>%
    write_csv(paste0('output/', folder, '/preds.csv'))
  
}

f_table_output <- function (preds){ # total raw count compared to fitted count 
  
  preds %>% 
    group_by(year) %>%
    summarise(count = max(cumsum, na.rm = T),
              fitted = max(fit_cumsum, na.rm = T)) %T>%
    write_csv(., paste0('output/', folder, '/summary_table.csv'))}



f_run_through <- function(preds, perc = 0.95, prob = 0.95){ #change perc to 0.90 for alt run
  
  preds %>% 
    group_by(year) %>%
    filter(fit_cumsum <= perc * max(fit_cumsum)) %>% # 95th percentile of last 10 years
    summarise(run_95 = max(julian)) %>% 
    ungroup %>%
    summarise(hard_date = round(quantile(run_95, prob),0)) %>% 
    ungroup() %>%
    as.data.frame() %>%
    mutate(date = as.Date(strptime(paste(year(Sys.Date()), hard_date, sep='-'), "%Y-%j"))) %T>% 
    write_csv(., paste0('output/', folder, '/run_through.csv'))
  
}

f_run95 <- function(preds, run_through, perc = 0.95){ #change perc to 0.90 for alt run
  run_through = run_through$hard_date
  
  preds %>%
    group_by(., year) %>%
    filter(fit_cumsum <= perc * max(fit_cumsum)) %>%
    summarise(run_95 = max(julian)) %>%
    write_csv(., paste0('output/', folder, '/run_95.csv'))
}

f_pred_plot <- function(preds, run_through, perc = 0.95){ #change perc to 0.90 for alt run
  run_through = run_through$hard_date
  max = max(preds$fit_cumsum) 
  preds %>%
    group_by(., year) %>%
    filter(fit_cumsum <= perc * max(fit_cumsum)) %>%
    summarise(run_95 = max(julian)) -> x
  preds %>% 
    group_by(., year) %>%
    left_join(x, .) %>% 
    mutate(julian95 = ifelse(julian == run_95, fit_cumsum, NA),
           alpha = ifelse(julian>run_95, 0.1, .2)) %>% 
    ggplot(aes(julian, fit_cumsum, color = Year, group = Year)) +
    geom_line(aes(alpha = alpha)) +
    geom_point(aes(y = cumsum), alpha = 0.2) +
    geom_point(aes(y = julian95, fill=Year), alpha = 0.90, pch = 21) +
    scale_y_continuous(limits = c(0, max * 1.1),
                       labels = scales::comma) +
    geom_vline(xintercept=run_through, lty = 3) +
    scale_alpha(guide = 'none') +
    xlab('\nJulian day') +
    ylab('Cumulative Escapement\n') -> x
  
  ggsave(paste0('figs/', folder, "/pred_plot.png"), plot = x, dpi = 100, 
         height = 5, width = 6.5, units = "in") 
  
  x
  
}

# f_pred_plot_decade <- function(preds, run_through){
#   run_through = run_through$hard_date
# 
#   preds %>%
#     group_by(., year) %>%
#     filter(fit_cumsum <= perc * max(fit_cumsum)) %>%
#     summarise(run_95 = max(julian)) -> x
# 
#   preds %>%
#     group_by(.,year) %>%
#     left_join(x, .) %>%
#     mutate(julian95 = ifelse(julian == run_95, fit_cumsum, NA),
#            alpha = ifelse(julian>run_95, 0.3, .8),
#            decade = year - year %% 10) %>%
#     ggplot(aes(julian, fit_cumsum , group = Year)) +
#     geom_line(aes(alpha = alpha)) +
#     geom_point(aes(y = cumsum ), alpha = 0.05) +
#     geom_point(aes(y = julian95 , fill=Year), alpha = 0.90, pch = 21) +
#     scale_y_continuous(labels = comma) +
#     geom_vline(xintercept=run_through, lty = 3) +
#     scale_alpha(guide = 'none') +
#     scale_fill_discrete(guide = 'none') +
#     xlab('\nJulian day') +
#     ylab('Cumulative Escapement\n') +
#     facet_wrap(~decade, dir = 'v') -> x
#   print(x)
# 
#   ggsave(paste0('figs/', folder, "/pred_plot_decade.png"), plot = x, dpi = 100,
#          height = 5, width = 6.5, units = "in")
# }
f_remove_dates <- function(preds, run_through){
  # removal date based upon 1% rules
  run_through = run_through$hard_date
  yrs = expand.grid(year = unique(preds$year),
                    days = c('one', 'two', 'three', 'four', 'five')) 
  
  preds %>%
    dplyr::select(year, julian, fit_run, fit_cumsum) %>% 
    group_by(year) %>% 
    mutate(one_5 = (lag(fit_run, 5) / (lag(fit_cumsum,6))),
           one_4 = (lag(fit_run, 4) / (lag(fit_cumsum,5))),
           one_3 = (lag(fit_run, 3) / (lag(fit_cumsum,4))),
           one_2 = (lag(fit_run, 2) / (lag(fit_cumsum,3))),
           one_1 =   (lag(fit_run,1) / (lag(fit_cumsum,2)))) %>% 
    mutate(one_5_rule = ifelse((lag(fit_run, 5) / (lag(fit_cumsum,6)))<0.01,1,0), # if <1%, then give it a 1
           one_4_rule = ifelse((lag(fit_run, 4) / (lag(fit_cumsum,5)))<0.01,1,0),
           one_3_rule = ifelse((lag(fit_run, 3) / (lag(fit_cumsum,4)))<0.01,1,0),
           one_2_rule = ifelse((lag(fit_run, 2) / (lag(fit_cumsum,3)))<0.01,1,0),
           one_1_rule =  ifelse((lag(fit_run,1) / (lag(fit_cumsum,2)))<0.01,1,0))%>% 
    filter((julian)>((round(run_through,0)))) %>% 
    mutate(five = ifelse(one_5_rule==1 & one_4_rule==1 & one_3_rule==1 & one_2_rule==1 & one_1_rule==1, 1,0),# determine JD of 5 days with <1%
           four = ifelse(one_4_rule==1 & one_3_rule==1 & one_2_rule==1 & one_1_rule==1, 1,0),
           three = ifelse(one_3_rule==1 & one_2_rule==1 & one_1_rule==1, 1,0),
           two = ifelse(one_2_rule==1 & one_1_rule==1, 1,0),
           one = ifelse(one_1_rule==1, 1,0)) %>% # if all < 1%, then give it a 1
    dplyr::select(year, julian, five, four, three, two, one)%>% 
    gather(days, value, -year, -julian) %>%  
    filter(julian >= run_through, value==1)%>% 
    left_join(yrs, .)%>% 
    mutate(julian = ifelse(julian < run_through, run_through, julian)) %>% 
    group_by(year, days) %>% 
    summarise(max = min(julian))
  
}
f_remove_dates_table <- function(preds, run_through){
  # removal date based upon 1% rules
  run_through = run_through$hard_date
  yrs = expand.grid(year = unique(preds$year),
                    days = c('one', 'two', 'three', 'four', 'five')) 
  
  preds %>%
    dplyr::select(year, julian, fit_run, fit_cumsum) %>% 
    group_by(year) %>% 
    mutate(one_5 = (lag(fit_run, 5) / (lag(fit_cumsum,6))),
           one_4 = (lag(fit_run, 4) / (lag(fit_cumsum,5))),
           one_3 = (lag(fit_run, 3) / (lag(fit_cumsum,4))),
           one_2 = (lag(fit_run, 2) / (lag(fit_cumsum,3))),
           one_1 =   (lag(fit_run,1) / (lag(fit_cumsum,2)))) %>% 
    mutate(one_5_rule = ifelse((lag(fit_run, 5) / (lag(fit_cumsum,6)))<0.01,1,0), # if <1%, then give it a 1
           one_4_rule = ifelse((lag(fit_run, 4) / (lag(fit_cumsum,5)))<0.01,1,0),
           one_3_rule = ifelse((lag(fit_run, 3) / (lag(fit_cumsum,4)))<0.01,1,0),
           one_2_rule = ifelse((lag(fit_run, 2) / (lag(fit_cumsum,3)))<0.01,1,0),
           one_1_rule =  ifelse((lag(fit_run,1) / (lag(fit_cumsum,2)))<0.01,1,0)) %>% 
    filter((julian)>((round(run_through,0)))) %>% 
    mutate(five = ifelse(one_5_rule==1 & one_4_rule==1 & one_3_rule==1 & one_2_rule==1 & one_1_rule==1, 1,0),# determine JD of 5 days with <1%
           four = ifelse(one_4_rule==1 & one_3_rule==1 & one_2_rule==1 & one_1_rule==1, 1,0),
           three = ifelse(one_3_rule==1 & one_2_rule==1 & one_1_rule==1, 1,0),
           two = ifelse(one_2_rule==1 & one_1_rule==1, 1,0),
           one = ifelse(one_1_rule==1, 1,0)) %>% # if all < 1%, then give it a 1
    dplyr::select(year, julian, five, four, three, two, one) %>% 
    gather(days, value, -year, -julian) %>%  
    filter(julian >= run_through, value==1) %>% 
    left_join(yrs, .)%>% 
    mutate(julian = ifelse(julian < run_through, run_through, julian)) %>% 
    group_by(year, days) %>% 
    summarise(max = min(julian)) %>% 
    write_csv(., paste0('output/', folder, '/remove_dates_table.csv'))
  
}
# f_remove_dates_05 <- function(preds, run_through){
#   # removal date based upon 0.05% rules
#   run_through = run_through$hard_date
#   
#   yrs = expand.grid(year = unique(preds$year),
#                     days = c('one', 'two', 'three', 'four', 'five')) 
# 
#   preds %>%
#     dplyr::select(year, julian, fit_run, fit_cumsum) %>% 
#     group_by(year) %>% 
#     mutate(one_5 = (lag(fit_run, 5) / (lag(fit_cumsum,6))),
#            one_4 = (lag(fit_run, 4) / (lag(fit_cumsum,5))),
#            one_3 = (lag(fit_run, 3) / (lag(fit_cumsum,4))),
#            one_2 = (lag(fit_run, 2) / (lag(fit_cumsum,3))),
#            one_1 =   (lag(fit_run,1) / (lag(fit_cumsum,2)))) %>% 
#     mutate(one_5_rule = ifelse((lag(fit_run, 5) / (lag(fit_cumsum,6)))<0.005,1,0), # if <0.05%, then give it a 1
#            one_4_rule = ifelse((lag(fit_run, 4) / (lag(fit_cumsum,5)))<0.005,1,0),
#            one_3_rule = ifelse((lag(fit_run, 3) / (lag(fit_cumsum,4)))<0.005,1,0),
#            one_2_rule = ifelse((lag(fit_run, 2) / (lag(fit_cumsum,3)))<0.005,1,0),
#            one_1_rule =  ifelse((lag(fit_run,1) / (lag(fit_cumsum,2)))<0.005,1,0)) %>% 
#     filter((julian)>((round(run_through,0))+5)) %>% 
#     mutate(five = ifelse(one_5_rule==1 & one_4_rule==1 & one_3_rule==1 & one_2_rule==1 & one_1_rule==1, 1,0),# determine JD of 5 days with <1%
#            four = ifelse(one_4_rule==1 & one_3_rule==1 & one_2_rule==1 & one_1_rule==1, 1,0),
#            three = ifelse(one_3_rule==1 & one_2_rule==1 & one_1_rule==1, 1,0),
#            two = ifelse(one_2_rule==1 & one_1_rule==1, 1,0),
#            one = ifelse(one_1_rule==1, 1,0)) %>% # if all < 0.05%, then give it a 1
#     dplyr::select(year, julian, five, four, three, two, one) %>% 
#     gather(days, value, -year, -julian) %>%  
#     filter(julian >= run_through, value==1) %>% 
#     left_join(yrs, .) %>% 
#     mutate(julian = ifelse(is.na(julian) | julian < run_through, run_through, julian)) %>% 
#     group_by(year, days) %>% 
#     summarise(min = min(julian))
# 
# } 
# need to work on this function and next

f_percent_missed <- function(preds, remove_dates){
  yrs = expand.grid(year = unique(preds$year),
                    days = c('one', 'two', 'three', 'four', 'five')) 
  y = deparse(substitute(remove_dates))
  
  preds %>% 
    dplyr::select(julian, year, fit_run, fit_cumsum) %>% 
    left_join(remove_dates) %>% 
    group_by(year) %>% 
    mutate(max_cumsum = max(fit_cumsum)) %>% 
    filter(julian < max) %>% 
    group_by(days, year) %>% 
    summarise(diff = (1 - (sum(fit_run) / mean(max_cumsum)))) %>% 
    group_by(days) %>% 
    left_join(yrs, .) %>% # join years so NA when no percent missed
    write_csv(., paste0('output/', folder, '/percent_missed.csv')) }


f_run_caught <- function(preds, remove_dates){
  
  y = deparse(substitute(remove_dates))
  
  preds %>% 
    dplyr::select(julian, year, fit_run, fit_cumsum) %>% 
    left_join(remove_dates) %>% 
    group_by(year) %>% 
    mutate(max_cumsum = max(fit_cumsum)) %>% 
    filter(julian < max) %>% 
    group_by(days, year) %>% 
    summarise(diff = (1 - (sum(fit_run) / mean(max_cumsum)))) %>% 
    group_by(days) %>% 
    summarise('99' = round(100 * (1 - quantile(diff, .99)), 1),
              '95' = round(100 * (1 - quantile(diff, .95)), 1),
              '90' = round(100 * (1 - quantile(diff, .90)), 1),
              '80' = round(100 * (1 - quantile(diff, .80)), 1),
              '70' = round(100 * (1 - quantile(diff, .70)), 1),
              '60' = round(100 * (1 - quantile(diff, .60)), 1),
              '50' = round(100 * (1 - quantile(diff, .50)), 1)) %>% 
    gather(`% Chance`, Percent, -days) %>% 
    ungroup %>% 
    mutate(days = factor(days, levels = c('one', 'two', 'three', 'four', 'five')),
           position = rep(1:7, each = length(unique(days)))) %>% 
    spread(days, Percent) %>% 
    arrange(position) %>% 
    dplyr::select(-position) %T>% 
    write_csv(., paste0('output/', folder,'/', y, '_run_caught.csv'))
  
}

f_risk_plot <- function(preds, remove_dates){
  
  y = deparse(substitute(remove_dates))
  z = ifelse(y =='remove_dates', '1% rule', '0.05% rule')
  
  preds %>% 
    left_join(remove_dates) %>% 
    group_by(days, year) %>% 
    mutate(sum_fit = sum(fit_run)) %>% 
    filter(julian<max) %>% 
    summarise(diff = mean(1 - (sum(fit_run) / mean(sum_fit)))) %>% 
    group_by(days) %>% 
    summarise('99' = round(100 * (quantile(diff, .99)), 1),
              '95' = round(100 * (quantile(diff, .95)), 1),
              '90' = round(100 * (quantile(diff, .90)), 1),
              '80' = round(100 * (quantile(diff, .80)), 1),
              '70' = round(100 * (quantile(diff, .70)), 1),
              '60' = round(100 * (quantile(diff, .60)), 1),
              '50' = round(100 * (quantile(diff, .50)), 1)) %>% 
    gather(`% Chance`, Percent, -days) %>% 
    mutate(days = factor(days, levels = c('one', 'two', 'three', 'four', 'five')),
           position = rep(1:7, each = length(unique(days)))) %>% 
    mutate(risk = rep(c(1, 5, 10, 20, 30, 40, 50), each = 5))%>% 
    ggplot(aes(risk, Percent, color = days, linetype = days)) +
    geom_line() +
    scale_color_brewer(palette = "Dark2") +
    xlab('% Risk') +
    ylab('% of missed run') +
    scale_y_continuous(limits = c(0, 8), breaks = c(0,1,2,3,4,5,6,7,8)) +
    expand_limits(y = 0) +
    ggtitle(z) -> x
  print(x)
  ggsave(paste0('figs/', folder,'/', y, "_risk_plot.png"), plot = x, dpi = 100, 
         height = 5, width = 6.5, units = "in") 
  
}

f_median_hard_date <- function(remove_dates, low = .25, high = .75){
  y = deparse(substitute(remove_dates))
  
  remove_dates %>% 
    na.omit() %>% # remove rows with NAs
    mutate(days = factor(days, levels = c('one', 'two', 'three', 'four', 'five'))) %>% 
    group_by(days) %>% 
    summarise(median = median(max),
              l_25 = round(quantile(max, low)),
              u_75 = round(quantile(max, high)),
              max = max(max)) %>% 
    mutate(date_median = as.Date(strptime(paste(year(Sys.Date()), median, sep='-'), "%Y-%j"))) %T>% 
    
    write_csv(., paste0('output/', folder,'/', y, '_median_end.csv'))
  
}

# Impute Data
# These functions were written by Justin Priest (ADF&G biologist; Douglas, AK)


# There are four functions here:
# impute_global() which imputes all NAs in all years iteratively
# impute_cohodefault() which imputes all NAs before year 2000 globally, then imputes annually
# impute_local() which imputes a 10-year rolling imputation (prev & following 5 years)
# impute_local_improved which is similar to impute_local() but accounts for early years better

impute_global <- function(dfname, Year_column="year", StreamName_column="stream_name",
                          outputname = "globalimpute", # Only used if Step 3 turned "on"
                          Count_column = "count"){
  ### SUMMARY: Global Impute ###
  # This creates a dataframe that imputes NA values.
  # This algorithm interpolates across rows and columns, following Blick
  # In essence, imputing across rows (years) and columns (streams) allows for an NA in
  #  a year/stream to be informed by typical counts and for that year AND stream
  # This function can be easily modified to auto-create a named dataframe
  
  # Make sure that all NAs are present (a missing row is NOT same as a row with an NA)
  
  
  ### EXAMPLE USAGE ###
  # impute_global(ktn_index, Year_column="year")
  
  # Step 1: Set up dataframe to impute
  require(dplyr)
  .test <- dfname %>% rename(year = Year_column, stream_name = StreamName_column, count = Count_column)
  .test <- .test %>% dplyr::select(year, stream_name, count)
  .test <- .test %>% mutate(imputed = is.na(count))
  
  # Step 2: Use multiplicative imputation as per Blick, in an iterative procedure
  j=1
  repeat{
    for(i in 1:nrow(.test)){
      .temprow = .test[i,]
      
      if(.temprow$imputed == TRUE){
        .sumyr = sum((.test %>% filter(year == .temprow$year) )$count, na.rm = TRUE)
        .sumrvr = sum((.test %>% filter(stream_name == .temprow$stream_name) )$count, na.rm = TRUE)
        .sumall = sum(.test$count, na.rm = TRUE)
        .test$count[i] = .sumyr * .sumrvr / .sumall
        # this interpolates across rows and columns
      }
    }
    j=j+1
    if(j>100){break} # repeat the above 100 times
  }
  #print(.test)
  
  # Optional Step 3: Auto create a dataframe with the correct name
  #assign(paste0((outputname), "_survey_imputed"), .test, envir = parent.frame() ) # use if you want a dynamic name
  imputedsurvey <- .test # use this if you want a static name
  write.csv(imputedsurvey, paste0('output/', folder, '/EM_algorithm_model.csv'))
}




impute_cohodefault <- function(dfname, Year_column="year", StreamName_column="stream_name",
                               Count_column = "count", outputname = "defaultcohoimputed"){
  ### SUMMARY: Impute 1987-2000 globally, then backwards impute each new year ###
  # This takes a dataframe with NA values and imputes missing data
  
  # This algorithm is the default method Leon used for imputation.
  # For all years pre-2000, impute globally (all values 1987-1999 inform imputed NAs)
  # Then for each following year calculate a new imputation looking backwards only
  # Leon imputed everything once ~2000 then annually would add the new years data with blanks
  # He ran the imputation on this new row which was informed by previous years' imputations
  # Once imputed, these values would go in the table and not be updated
  # Make sure that all NAs are present (a missing row is NOT same as a row with an NA)
  
  ### EXAMPLE USAGE ###
  # impute_cohodefault(ktn_index, Year_column="year", outputname = "ktn2020")
  
  
  # Step 1: Set up dataframe to impute
  require(dplyr)
  .test <- dfname %>% rename(year = Year_column, stream_name = StreamName_column, count = Count_column)
  .test <- .test %>% dplyr::select(year, stream_name, count)
  .test <- .test %>% mutate(imputed = is.na(count))
  
  # Step 2: Use multiplicative imputation as per Blick, in an iterative procedure
  
  j=1
  repeat{
    for(i in 1:nrow(.test %>% filter(year < 2000))){
      .temprow = .test[i,]
      if(.temprow$imputed == TRUE){
        .test_early <- .test %>% filter(year < 2000)
        .sumyr = sum((.test_early %>% filter(year == .temprow$year) )$count, na.rm = TRUE)
        .sumrvr = sum((.test_early %>% filter(stream_name == .temprow$stream_name) )$count, na.rm = TRUE)
        .sumall = sum(.test_early$count, na.rm = TRUE)
        .test$count[i] = .sumyr * .sumrvr / .sumall
      }
    }
    j=j+1
    if(j>100){break} # repeat the above 100 times. Needs to be iterative (imputing depends on other imputed values)
  }# end early
  
  j=1
  repeat{
    for(i in (nrow(.test %>% filter(year < 2000))+1):nrow(.test)){
      .temprow = .test[i,]
      if(.temprow$imputed == TRUE){
        .curryr <- .temprow$year
        .yr_range = .test %>% filter(year <= .curryr) #5 yrs before / after
        .sumyr = sum((.yr_range %>% filter(year == .temprow$year) )$count, na.rm = TRUE)
        .sumrvr = sum((.yr_range %>% filter(stream_name == .temprow$stream_name) )$count, na.rm = TRUE)
        .sumall = sum(.yr_range$count, na.rm = TRUE)
        .test$count[i] = .sumyr * .sumrvr / .sumall
        # this is multiplicative imputation as per Blick
      }
    }
    j=j+1
    if(j>100){break} # repeat the above 100 times. Needs to be iterative (imputing depends on other imputed values)
  }
  print(.test)
  # Step 3: Create the dataframe with the correct name
  # assign(paste0((outputname), "_survey_imputed"), .test, envir = parent.frame() ) # use if you want a dynamic name
  # imputedsurvey <- .test # use this if you want a static name
}



impute_local <- function(dfname, Year_column="year", StreamName_column="stream_name",
                         Count_column = "count"){
  ### SUMMARY: 10-yr Localized Imputation ###
  # This takes a dataframe with NA values and imputes missing data
  # This algorithm uses "local" imputation: only 5 years before and after impute a missing value
  # i.e., only using the preceding 5 years and following 5 years
  # Make sure that all NAs are present (a missing row is NOT same as a row with an NA)
  
  ### EXAMPLE USAGE ###
  # impute_local(ktn_index, Year_column="year")
  
  # Step 1: Set up dataframe to impute
  require(dplyr)
  .test <- dfname %>% rename(year = Year_column, stream_name = StreamName_column, count = Count_column)
  .test <- .test %>% dplyr::select(year, stream_name, count)
  .test <- .test %>% mutate(imputed = is.na(count))
  
  # Step 2: Use multiplicative imputation as per Blick, in an iterative procedure
  
  j=1
  repeat{
    for(i in 1:nrow(.test)){
      .temprow = .test[i,]
      
      if(.temprow$imputed == TRUE){
        .yr_range = .test %>% filter(between(year, .temprow$year - 5, .temprow$year + 5)) #5 yrs before / after
        .sumyr = sum((.yr_range %>% filter(year == .temprow$year) )$count, na.rm = TRUE)
        .sumrvr = sum((.yr_range %>% filter(stream_name == .temprow$stream_name) )$count, na.rm = TRUE)
        .sumall = sum(.yr_range$count, na.rm = TRUE)
        .test$count[i] = .sumyr * .sumrvr / .sumall
        # this is multiplicative imputation as per Blick
      }
    }
    j=j+1
    if(j>100){break} # repeat the above 100 times
  }
  print(.test)
  
  # Optional Step 3: Auto create a dataframe with the correct name
  # assign(paste0((outputname), "_survey_imputed"), .test, envir = parent.frame() ) # use if you want a dynamic name
  # imputedsurvey <- .test # use this if you want a static name
  
}



impute_local_improved <- function(dfname, Year_column="year", StreamName_column="stream_name",
                                  Count_column = "count"){
  ### SUMMARY: 10-yr Localized Imputation, improved ###
  # This takes a dataframe with NA values and imputes missing data
  # This algorithm uses "local" imputation: only 5 years before and after impute a missing value
  # i.e., only using the preceding 5 years and following 5 years
  # However this version adds a rule for early years (1987-1996) to use 10 next years (10 yr minimum)
  # Make sure that all NAs are present (a missing row is NOT same as a row with an NA)
  
  ### EXAMPLE USAGE ###
  # impute_local_improved(ktn_index, Year_column="year")
  
  # Step 1: Set up dataframe to impute
  require(dplyr)
  .test <- dfname %>% rename(year = Year_column, stream_name = StreamName_column, count = Count_column)
  .test <- .test %>% dplyr::select(year, stream_name, count)
  .test <- .test %>% mutate(imputed = is.na(count))
  
  # Step 2: Use multiplicative imputation as per Blick, in an iterative procedure
  
  j=1
  repeat{
    for(i in 1:nrow(.test)){
      .temprow = .test[i,]
      
      if(.test$year < 1997){
        if(.temprow$imputed == TRUE){
          .test_early <- .test %>% filter(year < 1997)
          .sumyr = sum((.test_early %>% filter(year == .temprow$year) )$count, na.rm = TRUE)
          .sumrvr = sum((.test_early %>% filter(stream_name == .temprow$stream_name) )$count, na.rm = TRUE)
          .sumall = sum(.test_early$count, na.rm = TRUE)
          .test$count[i] = .sumyr * .sumrvr / .sumall
        } # end early
      }}
    j=j+1
    if(j>50){break} # repeat the above 50 times. Needs to be iterative (imputing depends on other imputed values)
  }# end early
  
  j=1
  repeat{
    for(i in (nrow(.test %>% filter(year < 1997))+1):nrow(.test)){
      .temprow = .test[i,]
      if(.temprow$imputed == TRUE){
        .yr_range = .test %>% filter(between(year, .temprow$year - 5, .temprow$year + 5)) #5 yrs before / after
        .sumyr = sum((.yr_range %>% filter(year == .temprow$year) )$count, na.rm = TRUE)
        .sumrvr = sum((.yr_range %>% filter(stream_name == .temprow$stream_name) )$count, na.rm = TRUE)
        .sumall = sum(.yr_range$count, na.rm = TRUE)
        .test$count[i] = .sumyr * .sumrvr / .sumall
        # this is multiplicative imputation as per Blick
      }
    }
    j=j+1
    if(j>50){break} # repeat the above 50 times. Needs to be iterative (imputing depends on other imputed values)
  } # end late
  print(.test)
}

