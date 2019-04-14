f_clean_data <- function(data){
  
  data %>%
    drop_na %>%
    mutate(date = mdy(date),
           year = year(date),
           julian = yday(date), 
           Year = factor(year)) -> df
  
  expand.grid(year = min(df$year):max(df$year),
              julian = (min(df$julian) - 10):(max(df$julian) + 10)) -> all_days 
  df %>% 
    group_by(year) %>%
    mutate(cumsum = cumsum(count)) %>% 
    left_join(all_days, .)
}

f_gomp_model <- function(data){
  
  df %>% 
    group_by(year) %>% 
    summarise(lim = max(cumsum, na.rm = T)) %>% 
    ungroup() %>% 
    summarise(low = min(lim),
              up = max(lim)) -> x
    
  
  data %>%
    group_by(year) %>%
    nest() %>%
    mutate(fit = purrr::map(data, ~ nls_multstart(cumsum ~ pop * (exp(-exp(-k * (julian - t)))),
                                                  data = .,
                                                  iter = 500,
                                                  start_lower = c(pop = x$low, k = 0.1, t = 180),
                                                  start_upper = c(pop = x$up, k = 0.3, t = 190),
                                                  supp_errors = 'Y',
                                                  na.action = na.omit)))
}

f_logistic_model <- function(data){
  
  df %>% 
    group_by(year) %>% 
    summarise(lim = max(cumsum, na.rm = T)) %>% 
    ungroup() %>% 
    summarise(low = min(lim),
              up = max(lim)) -> x
  
  
  data %>%
    group_by(year) %>%
    nest() %>%
    mutate(fit = purrr::map(data, ~ nls_multstart(cumsum ~ pop / (1 + exp(-k * (julian - t))),
                                                  data = .,
                                                  iter = 500,
                                                  start_lower = c(pop = x$low, k = 0.1, t = 180),
                                                  start_upper = c(pop = x$up, k = 0.3, t = 190),
                                                  supp_errors = 'Y',
                                                  na.action = na.omit)))
}

f_summary <- function(model){
  
  model %>%
    unnest(fit %>% map(glance)) %>% 
    dplyr::select(-data, -fit) %>% 
    as.data.frame
}

f_deviance <- function(model1, model2){
  sum((f_summary(model1)$deviance - 
         f_summary(model2)$deviance)<0) /length(unique(model1$year)) 
}

f_params <- function(model){
  
  model %>%
    unnest(fit %>% map(tidy)) -> params
  
  model %>% 
    unnest(fit %>% map(~ confint2(.x) %>%
                         data.frame() %>%
                         rename(., conf.low = X2.5.., conf.high = X97.5..))) %>%
    group_by(., year) %>%
    mutate(., term = c('pop', 'k', 't')) %>%
    ungroup() -> CI
  
  merge(params, CI, by = intersect(names(params), names(CI))) %T>% 
    write_csv(paste0('output/', folder, '/params.csv'))
  
}

f_param_plot <- function(params){
  
  params %>% 
    ggplot(aes(year, estimate)) +
    geom_point(alpha = .2) +
    geom_line(alpha = 0.5) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3) +
    facet_wrap(~term, scales = 'free_y') -> x
  
  ggsave(paste0('figs/', folder, "/param_plot.png"), x, dpi = 100, height = 8.5, width = 6.5, units = "in") 
  
  x
  
}

f_preds <- function(data, model){
  
 y = deparse(substitute(model))
  
  expand.grid(julian = min(data$julian):max(data$julian),
              year = unique(data$year)) -> x
  
  model %>%
    unnest(fit %>%
             map(tidy)) %>% 
    spread(term, estimate) %>% 
    group_by(year) %>% 
    summarise_all(mean, na.rm=TRUE) %>%
    left_join(x, .) %>% 
    left_join(data) %>% 
    arrange(year, julian) %>% 
    group_by(year) %>% 
    mutate(fit = case_when(y == 'model' ~ pop * (exp(-exp(-k * (julian - t)))),
                           y == 'model_logistic' ~  pop / (1 + exp(-k * (julian - t)))),
           fit_count = round(c(fit[1], diff(fit)), 0),
           fit_run = ifelse(is.na(cumsum), fit_count, count),
           fit_cumsum = cumsum(fit_run),
           Year = factor(year)) %T>%
    write_csv(paste0('output/', folder, '/preds.csv'))
  
}

f_pred_plot <- function(preds, run_through){
  run_through = run_through$end_date
  
  preds %>%
    group_by(year) %>%
    filter(fit_cumsum <= 0.95 * max(fit_cumsum)) %>%
    summarise(run_95 = max(julian)) -> x

  preds %>% 
    group_by(year) %>%
    left_join(x, .) %>% 
    mutate(julian95 = ifelse(julian == run_95, fit_cumsum, NA),
           alpha = ifelse(julian>run_95, 0.3, .8)) %>% 
    ggplot(aes(julian, fit_cumsum, color = Year, group = Year)) +
    geom_line(aes(alpha = alpha)) +
    geom_point(aes(y = cumsum), alpha = 0.15) +
    geom_point(aes(y = julian95, fill=Year), alpha = 0.90, pch = 21) +
    scale_y_continuous(labels = comma) +
    geom_vline(xintercept=run_through, lty = 3) +
    scale_alpha(guide = 'none') +
    xlab('\nJulian date') +
    ylab('Cumulative Escapement\n') -> x
  
  ggsave(paste0('figs/', folder, "/pred_plot.png"), plot = x, dpi = 100, height = 8.5, width = 6.5, units = "in") 
  
  x
}

f_pred_plot_decade <- function(preds, run_through){
  run_through = run_through$end_date
  
  preds %>%
    group_by(year) %>%
    filter(fit_cumsum <= 0.95 * max(fit_cumsum)) %>%
    summarise(run_95 = max(julian)) -> x

  preds %>% 
    group_by(year) %>%
    left_join(x, .) %>% 
    mutate(julian95 = ifelse(julian == run_95, fit_cumsum, NA),
           alpha = ifelse(julian>run_95, 0.3, .8),
           decade = year - year %% 10) %>% 
    ggplot(aes(julian, fit_cumsum , group = Year)) +
    geom_line(aes(alpha = alpha)) +
    geom_point(aes(y = cumsum ), alpha = 0.05) +
    geom_point(aes(y = julian95 , fill=Year), alpha = 0.90, pch = 21) +
    geom_line() +
    geom_point(aes(y = cumsum), alpha= 0.30) +
    scale_y_continuous(labels = comma) +
    geom_vline(xintercept=run_through, lty = 3) +
    scale_alpha(guide = 'none') +
    scale_fill_discrete(guide = 'none') + 
    xlab('\nJulian date') +
    ylab('Cumulative Escapement\n') +
    facet_wrap(~decade, dir = 'v') -> x
  
  ggsave(paste0('figs/', folder, "/pred_plot_decade.png"), plot = x, dpi = 100, height = 8.5, width = 6.5, units = "in") 
  
  x

}

f_run_through <- function(data, preds){
  
  # modeled date to 95% of run
  preds %>%
    group_by(year) %>%
    filter(fit_cumsum <= 0.95 * max(fit_cumsum)) %>%
    summarise(run_95 = max(julian)) %>%
    ungroup %>%
    summarise(end_date = round(mean(run_95))) %>% 
    mutate(date = as.Date(strptime(paste(year(Sys.Date()), end_date, sep='-'), "%Y-%j"))) %T>% 
    write_csv(., paste0('output/', folder, '/run_through.csv'))
  # -> x
  
  # # observed date at 95% of run
  # data %>%
  #   group_by(year) %>%
  #   filter(cumsum <= max(cumsum, na.rm = T) * 0.95) %>%
  #   summarise(run_95 = max(julian)) %>%
  #   summarise(min(run_95)) %>% 
  #   ungroup %>%
  #   summarise(end_date = round(mean(run_95))) %>% .$end_date -> y
  # 
  # z <- round((x + y) / 2)
  # 
  # if(x - y >= 10) message( cat(paste('Warning: modeled weir removal date is >10 days later \n than actual weir removal date.
  #                              \nWeir is being removed too early \n will need to manually decrease the run_through date for reasonable results.
  #                                    \n Recommend run_through =', z)))
  # y
                          
}

f_remove_dates <- function(preds, run_through){
  # removal date based upon 1% rules
  run_through = run_through$end_date
  
  yrs = expand.grid(year = unique(preds$year),
               days = c('one', 'two', 'three', 'four', 'five')) 
    
  preds %>%
    dplyr::select(year, julian, fit_run, fit_cumsum) %>% 
    group_by(year) %>% 
    mutate(one_4 = ifelse((lag(fit_run, 4) / fit_cumsum) >= 0.01, 1, 0),
           one_3 = ifelse(lag(fit_run, 3) / fit_cumsum >=  0.01, 1, 0),
           one_2 = ifelse(lag(fit_run, 2) / fit_cumsum >=  0.01, 1, 0),
           one_1 = ifelse(lag(fit_run) / fit_cumsum >=  0.01, 1, 0),
           one = ifelse(fit_run / fit_cumsum >=  0.01, 1, 0),  
           five = ifelse(one_4==1 | one_3==1 | one_2==1 | one_1==1 | one==1, 1, 0),
           four = ifelse(one_3==1 | one_2==1 | one_1==1 | one==1, 1, 0),
           three = ifelse(one_2==1 | one_1==1 | one==1, 1, 0),
           two = ifelse(one_1==1 | one==1, 1, 0)) %>% 
    dplyr::select(year, julian, five, four, three, two, one) %>% 
    gather(days, value, -year, -julian) %>%  
    filter(julian >= run_through, value==1) %>% 
    left_join(yrs, .) %>% 
    mutate(julian = ifelse(is.na(julian) | julian < run_through, run_through, julian)) %>% 
    group_by(year, days) %>% 
    summarise(max = max(julian))
  
} 

f_remove_dates_05 <- function(preds, run_through){
  # removal date based upon 0.05% rules
  run_through = run_through$end_date
  
  yrs = expand.grid(year = unique(preds$year),
                    days = c('one', 'two', 'three', 'four', 'five')) 

  preds %>%
    dplyr::select(year, julian, fit_run, fit_cumsum) %>% 
    group_by(year) %>% 
    mutate(one_4 = ifelse((lag(fit_run, 4) / fit_cumsum) >= 0.005, 1, 0),
           one_3 = ifelse(lag(fit_run, 3) / fit_cumsum >=  0.005, 1, 0),
           one_2 = ifelse(lag(fit_run, 2) / fit_cumsum >=  0.005, 1, 0),
           one_1 = ifelse(lag(fit_run) / fit_cumsum >=  0.005, 1, 0),
           one = ifelse(fit_run / fit_cumsum >=  0.005, 1, 0),  
           five = ifelse(one_4==1 | one_3==1 | one_2==1 | one_1==1 | one==1, 1, 0),
           four = ifelse(one_3==1 | one_2==1 | one_1==1 | one==1, 1, 0),
           three = ifelse(one_2==1 | one_1==1 | one==1, 1, 0),
           two = ifelse(one_1==1 | one==1, 1, 0)) %>% 
    dplyr::select(year, julian, five, four, three, two, one) %>% 
    gather(days, value, -year, -julian) %>%  
    filter(julian >= run_through, value==1) %>% 
    left_join(yrs, .) %>% 
    mutate(julian = ifelse(is.na(julian) | julian < run_through, run_through, julian)) %>% 
    group_by(year, days) %>% 
    summarise(max = max(julian))
  
} 

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
    summarise('99' = round(100 * (1 - quantile(diff, .99))),
              '95' = round(100 * (1 - quantile(diff, .95))),
              '90' = round(100 * (1 - quantile(diff, .90))),
              '80' = round(100 * (1 - quantile(diff, .80))),
              '70' = round(100 * (1 - quantile(diff, .70))),
              '60' = round(100 * (1 - quantile(diff, .60))),
              '50' = round(100 * (1 - quantile(diff, .50)))) %>% 
    gather(`% Chance`, Percent, -days) %>% 
    ungroup %>% 
    mutate(days = factor(days, levels = c('one', 'two', 'three', 'four', 'five'))) %>% 
    mutate(position = rep(1:7, each = length(unique(days)))) %>% 
    spread(days, Percent) %>% 
    arrange(position) %>% 
    dplyr::select(-position) %T>% 
    write_csv(., paste0('output/', folder,'/', y, '_run_caught.csv'))
    
}

f_run_caught_n <- function(preds, remove_dates){
  
  y = deparse(substitute(remove_dates))
  z = ifelse(y == 'remove_dates', '1% rule', '0.05% rule')
  
  preds %>% 
    left_join(remove_dates) %>% 
    # group_by(days) %>% 
    # mutate(max = median(max)) %>% 
    group_by(days, year) %>% 
    mutate(sum_fit = max(fit_cumsum)) %>%
    filter(julian<=max) %>% 
    summarise(diff = mean(1 - (sum(fit_run) / mean(sum_fit)))) %>% 
    ungroup %>% 
    mutate(days = factor(days, levels = c('one', 'two', 'three', 'four', 'five'))) %>% 
    group_by(days) %>%
    summarise(count = n())}
  
f_risk_plot <- function(preds, remove_dates){
  
  y = deparse(substitute(remove_dates))
  z = ifelse(y =='remove_dates', '1% rule', '0.05% rule')
    
  preds %>% 
    left_join(remove_dates) %>% 
    group_by(days, year) %>% 
    mutate(sum_fit = sum(fit_run))%>% 
    filter(julian<=max) %>% 
    summarise(diff = mean(1 - (sum(fit_run) / mean(sum_fit)))) %>% 
    group_by(days) %>% 
    summarise('99' = (100 * (quantile(diff, .99))),
              '95' = (100 * (quantile(diff, .95))),
              '90' = (100 * (quantile(diff, .90))),
              '80' = (100 * (quantile(diff, .80))),
              '70' = (100 * (quantile(diff, .70))),
              '60' = (100 * (quantile(diff, .60))),
              '50' = (100 * (quantile(diff, .50)))) %>% 
    gather(`% Chance`, Percent, -days) %>% 
    mutate(position = rep(1:7, each = length(unique(days)))) %>% 
    mutate(risk = rep(c(1, 5, 10, 20, 30, 40, 50), each = 5)) %>% 
    ggplot(aes(risk, Percent, color = days)) +
    geom_line() +
    xlab('% Risk') +
    ylab('% of missed run') +
    expand_limits(y = 0) +
    ggtitle(z) -> x
  ggsave(paste0('figs/', folder,'/', y, "_risk_plot.png"), plot = x, dpi = 100, height = 8.5, width = 6.5, units = "in") 
  
  x

}

f_run_risk <- function(preds, remove_dates){
  
  y = deparse(substitute(remove_dates))
  
  # note: have to round "bins" due to 0.10 floating issue
  # see all computer programs for an example...
  expand.grid(days = c('five', 'four', 'three', 'two'),
              bins = round(seq(0.01, 0.5, 0.01), 2)) -> x
  
  preds %>% 
    left_join(remove_dates) %>% 
    group_by(days, year) %>% 
    mutate(mm = max(fit_cumsum)) %>% 
    filter(julian <= max) %>% 
    summarise(perc_missed = mean(1 - (sum(fit_run) / mean(mm)))) %>% 
      mutate(perc_missed = ifelse(perc_missed<0, 0, perc_missed)) %>% 
    group_by(days) %>% 
    filter(perc_missed>0) -> out
  
  if(nrow(out)==0){
    
    cat(paste("Missing more than 5% of the run regularly.", 
              'Decrease run_through date and rerun to get viable numbers.', sep="\n"))
  } else{
    
    out %>% 
      summarise(perc_missed = perc_missed %>% list) %>% 
      mutate(mod = map(perc_missed, ~fitdistr(.x, 'gamma'))) %>% 
      unnest(mod %>% map(tidy)) %>% 
      dplyr::select(-std.error) %>% 
      spread(term, estimate) %>% 
      left_join(x) %>% 
      mutate(gamma = 1 - pgamma(bins, shape, rate)) %>% 
      filter(bins %in% c(0.01, 0.05, 0.10, 0.2, 0.3, 0.4, 0.50)) %>% 
      dplyr::select(-rate, -shape) %>% 
      mutate(bins = case_when(bins == 0.01 ~ '1% run missed',
                              bins == 0.05 ~ '5% run missed',
                              bins == 0.10 ~ '10% run missed',
                              bins == 0.20 ~ '20% run missed',
                              bins == 0.30 ~ '30% run missed',
                              bins == 0.40 ~ '40% run missed',
                              bins == 0.50 ~ '50% run missed'),
             bins = factor(bins, levels = unique(bins)),
             gamma = round(gamma, 3) * 100) %>% 
      spread(days, gamma) %T>% 
      write_csv(., paste0('output/', folder,'/', y, '_run_risk.csv'))
  }
}

f_median_end_date <- function(remove_dates, low = .25, high = .75){
  y = deparse(substitute(remove_dates))
  
  remove_dates %>% 
    mutate(days = factor(days, levels = c('one', 'two', 'three', 'four', 'five'))) %>% 
    group_by(days) %>% 
    summarise(median = median(max),
              l_25 = quantile(max, low),
              u_75 = quantile(max, high)) %>% 
    mutate(date = as.Date(strptime(paste(year(Sys.Date()), median, sep='-'), "%Y-%j"))) %T>% 
    write_csv(., paste0('output/', folder,'/', y, '_median_end.csv'))

}


