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
    summarise(total_count = max(cumsum, na.rm = T),
              fitted = max(fit_cumsum, na.rm = T)) %T>%
    write_csv(., paste0('output/', folder, '/summary_table.csv'))}


f_plot_output <- function (preds){ # fitted cumulative sum versus raw and diff by year
  
max = max(preds$fit_cumsum) 

  preds %>% 
    group_by(year) %>%
    summarise(count = max(cumsum, na.rm = T),
              value = 'cumulative sum') -> x
  
  preds %>% 
    group_by(year) %>%
    summarise(count = max(fit_cumsum, na.rm = T),
              value = 'fitted cumulative sum') %>%
  rbind(.,x) %>%
    as.data.frame() %>% 
    ggplot() +
    geom_line(aes(x = year, y = count, group = value, lty = value, color = value)) +
    scale_colour_manual(values = c("grey80", "black")) +
    scale_x_continuous(limits = c(min(tickryr$year), max(tickryr$year)),
                       breaks = axisf$breaks, labels = axisf$labels) +
    scale_y_continuous(limits = c(0, max * 1.1),
                       labels = scales::comma) +
    geom_text(aes(x = 1985, y = 145000, label="A)"),family="Times New Roman", colour="black", size=4) +
    theme(legend.position = c(0.2, 0.85), legend.title = element_blank (),
          legend.text=element_text(size=12)) +
    xlab('\nYear') +
    ylab('Counts\n') -> plot1
  
plot1  

  preds %>% 
    group_by(year) %>%
    summarise(cumsum = max(cumsum, na.rm = T),
              fit_cumsum = max(fit_cumsum, na.rm = T)) %>%
    mutate(diff = fit_cumsum - cumsum) %>%
    as.data.frame() %>% 
    ggplot(., aes(x = year, y= diff)) +
    geom_bar(colour="grey50", stat="identity") +
    scale_x_continuous(limits = c(min(tickryr$year), max(tickryr$year)),
                       breaks = axisf$breaks, labels = axisf$labels) +
    scale_y_continuous(labels = scales::comma) +
    theme(legend.position = c(0.15, 0.85)) +
    theme ( legend.title = element_blank ()) +
    xlab('\nYear') +
    geom_text(aes(x = 1985, y = 27000, label="B)"),family="Times New Roman", colour="black", size=4) +
    ylab('Difference\n') -> plot2
  plot2
  cowplot::plot_grid(plot1, plot2,  align = "v", nrow = 2, ncol=1) 
  ggsave(paste0('figs/', folder, "/fitted_plot.png"), dpi = 500, height = 8, width = 9, units = "in")
}


f_run_through <- function(preds, perc = 0.95, prob = 0.95){ #change perc to 0.90 for alt run

  preds %>% 
    group_by(year) %>%
    filter(year>=year_num) %>% # only use the last 10 years of data
    filter(fit_cumsum <= perc * max(fit_cumsum)) %>% # 95th percentile of last 10 years
    summarise(run_95 = max(julian)) %>% 
    ungroup %>%
    summarise(end_date = round(quantile(run_95, prob),0)) %>% 
    ungroup() %>%
    as.data.frame() %>%
    mutate(date = as.Date(strptime(paste(year(Sys.Date()), end_date, sep='-'), "%Y-%j"))) %T>% 
    write_csv(., paste0('output/', folder, '/run_through.csv'))
  
}

f_run95 <- function(preds, run_through, perc = 0.95){ #change perc to 0.90 for alt run
  run_through = run_through$end_date
  
  preds %>%
    group_by(., year) %>%
    filter(fit_cumsum <= perc * max(fit_cumsum)) %>%
    summarise(run_95 = max(julian)) %>%
  write_csv(., paste0('output/', folder, '/run_95.csv'))
}

f_pred_plot <- function(preds, run_through, perc = 0.95){ #change perc to 0.90 for alt run
  run_through = run_through$end_date
  max = max(preds$fit_cumsum) 
  preds %>%
    filter(year>=year_num)%>%
    group_by(., year) %>%
    filter(fit_cumsum <= perc * max(fit_cumsum)) %>%
    summarise(run_95 = max(julian)) -> x
  preds %>% 
    filter(year>=year_num)%>%
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

f_pred1999_plot <- function(preds, run_through, perc = 0.95){ #change perc to 0.90 for alt run
  run_through = run_through$end_date

  preds %>%
    group_by(., year) %>%
    filter(fit_cumsum <= perc * max(fit_cumsum)) %>%
    summarise(run_95 = max(julian)) -> x

  preds %>%
    group_by(.,year) %>%
    left_join(x, .) %>%
    mutate(julian95 = ifelse(julian == run_95, fit_cumsum, NA),
           alpha = ifelse(julian>run_95, .6, .7),
           decade = year - year %% 1) %>%
    filter (year < 2000) %>%
    ggplot(aes(julian, fit_cumsum , group = Year)) +
    geom_line(aes(alpha = alpha)) +
    geom_point(aes(y = cumsum ), alpha = 0.1) +
    geom_point(aes(y = julian95 , fill=Year), alpha = 0.80, pch = 21, size =2) +
    scale_y_continuous(labels = comma) +
    scale_alpha(guide = 'none') +
    geom_vline(xintercept=run_through, lty = 3) +
    scale_fill_discrete(guide = 'none') +
    theme(legend.position="none") +
    xlab('\nJulian day') +
    ylab('Cumulative Escapement\n') +
    facet_wrap(~decade, dir = 'v') -> x
  print(x)

  ggsave(paste0('figs/', folder, "/pred_plot_1999year.png"), plot = x, dpi = 100,
         height = 10, width = 8.5, units = "in")
}

f_pred2000_plot <- function(preds, run_through){
  run_through = run_through$end_date

  preds %>%
    group_by(., year) %>%
    filter(fit_cumsum <= perc * max(fit_cumsum)) %>%
    summarise(run_95 = max(julian)) -> x

  preds %>%
    group_by(.,year) %>%
    left_join(x, .) %>%
    mutate(julian95 = ifelse(julian == run_95, fit_cumsum, NA),
           alpha = ifelse(julian>run_95, 0.6, .7),
           decade = year - year %% 1) %>%
    filter (year > 1999) %>%
    filter (year < 2012) %>%
    ggplot(aes(julian, fit_cumsum , group = Year)) +
    geom_line(aes(alpha = alpha)) +
    geom_point(aes(y = cumsum ), alpha = 0.1) +
    geom_point(aes(y = julian95 , fill=Year), alpha = 0.80, pch = 21, size =2) +
    scale_y_continuous(labels = comma) +
    scale_alpha(guide = 'none') +
    scale_fill_discrete(guide = 'none') +
    geom_vline(xintercept=run_through, lty = 3) +
    theme(legend.position="none") +
    xlab('\nJulian day') +
    ylab('Cumulative Escapement\n') +
    facet_wrap(~decade, dir = 'v') -> x
  print(x)

  ggsave(paste0('figs/', folder, "/pred_plot_2000year.png"), plot = x, dpi = 100,
         height = 10, width = 8.5, units = "in")
}

f_pred2010_plot <- function(preds, run_through){ #change perc to 0.90 for alt run
  run_through = run_through$end_date

  preds %>%
    group_by(., year) %>%
    filter(fit_cumsum <= perc * max(fit_cumsum)) %>%
    summarise(run_95 = max(julian)) -> x

  preds %>%
    group_by(.,year) %>%
    left_join(x, .) %>%
    mutate(julian95 = ifelse(julian == run_95, fit_cumsum, NA),
           alpha = ifelse(julian>run_95, 0.6, .7),
           decade = year - year %% 1) %>%
    filter (year > 2011) %>%
    ggplot(aes(julian, fit_cumsum , group = Year)) +
    geom_line(aes(alpha = alpha)) +
    geom_point(aes(y = cumsum ), alpha = 0.1) +
    geom_point(aes(y = julian95 , fill=Year), alpha = 0.80, pch = 21, size =2) +
    scale_y_continuous(labels = comma) +
    geom_vline(xintercept=run_through, lty = 3) +
    scale_alpha(guide = 'none') +
    scale_fill_discrete(guide = 'none') +
    theme(legend.position="none") +
    xlab('\nJulian day') +
    ylab('Cumulative Escapement\n') +
    facet_wrap(~decade, dir = 'v') -> x
  print(x)

  ggsave(paste0('figs/', folder, "/pred_plot_2010year.png"), plot = x, dpi = 100,
         height = 10, width = 8.5, units = "in")
}


# f_pred_plot_decade <- function(preds, run_through){
#   run_through = run_through$end_date
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
  run_through = run_through$end_date
  yrs = expand.grid(year = unique(preds$year),
                    days = c('one', 'two', 'three', 'four', 'five')) %>%
 filter(year>=year_num)
  
  preds %>%
    dplyr::select(year, julian, fit_run, fit_cumsum) %>% 
    group_by(year) %>% 
    filter(year>=year_num)%>%
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
    dplyr::select(year, julian, five, four, three, two, one) %>% 
    gather(days, value, -year, -julian) %>%  
    filter(julian >= run_through, value==1) %>% 
    left_join(yrs, .) %>% 
    mutate(julian = ifelse(is.na(julian) | julian < run_through, run_through, julian)) %>% 
    group_by(year, days) %>% 
    summarise(max = min(julian))
  
}
f_remove_dates_table <- function(preds, run_through){
  # removal date based upon 1% rules
  run_through = run_through$end_date
  yrs = expand.grid(year = unique(preds$year),
                    days = c('one', 'two', 'three', 'four', 'five')) %>%
    filter(year>=year_num)
  
  preds %>%
    dplyr::select(year, julian, fit_run, fit_cumsum) %>% 
    group_by(year) %>% 
    filter(year>=year_num)%>%
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
    left_join(yrs, .) %>% 
    mutate(julian = ifelse(is.na(julian) | julian < run_through, run_through, julian)) %>% 
    group_by(year, days) %>% 
    summarise(max = min(julian)) %>% 
    write_csv(., paste0('output/', folder, '/remove_dates_table.csv'))
  
}
# f_remove_dates_05 <- function(preds, run_through){
#   # removal date based upon 0.05% rules
#   run_through = run_through$end_date
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
  
  y = deparse(substitute(remove_dates))
  
  preds %>% 
    filter(year>=year_num)%>% 
    dplyr::select(julian, year, fit_run, fit_cumsum) %>% 
    left_join(remove_dates) %>% 
    group_by(year) %>% 
    mutate(max_cumsum = max(fit_cumsum)) %>% 
    filter(julian < max) %>% 
    group_by(days, year) %>% 
    summarise(diff = (1 - (sum(fit_run) / mean(max_cumsum)))) %>% 
    group_by(days) %>% 
    write_csv(., paste0('output/', folder, '/percent_missed.csv')) }

f_run_caught <- function(preds, remove_dates){
  
  y = deparse(substitute(remove_dates))
  
  preds %>% 
    filter(year>=year_num)%>% 
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
    filter(year>=year_num)%>% 
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

f_median_end_date <- function(remove_dates, low = .25, high = .75){
  y = deparse(substitute(remove_dates))
  
  remove_dates %>% 
    mutate(days = factor(days, levels = c('one', 'two', 'three', 'four', 'five'))) %>% 
    group_by(days) %>% 
    summarise(median = median(max),
              l_25 = round(quantile(max, low)),
              u_75 = round(quantile(max, high)),
              max = max(max)) %>% 
    mutate(date_median = as.Date(strptime(paste(year(Sys.Date()), median, sep='-'), "%Y-%j"))) %T>% 
    
    write_csv(., paste0('output/', folder,'/', y, '_median_end.csv'))

}


