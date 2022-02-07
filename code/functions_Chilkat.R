f_plot_output_chilkat <- function (preds){ # fitted cumulative sum versus raw and diff by year
  
max = max(preds$fit_cumsum) 
  
preds %>% 
  group_by(year) %>%
  summarise(count = max(cumsum, na.rm = T),
            value = 'cumulative sum') -> x
  
  preds %>% 
    group_by(year) %>%
    summarise(count = max(fit_cumsum, na.rm = T),
              value = 'fitted cumulative sum')%>%
    #mutate(count = ifelse(count == '-Inf', '', count))%>%
  rbind(.,x) %>%
    as.data.frame() %>% 
    ggplot() +
    geom_line(aes(x = year, y = count, group = value, lty = value, color = value)) +
    scale_colour_manual(values = c("grey80", "black")) +
    scale_x_continuous(limits = c(min(tickryr$year), max(tickryr$year)),
                       breaks = axisf$breaks, labels = axisf$labels) +
    scale_y_continuous(limits = c(0, max * 1.1),
                       labels = scales::comma) +
    geom_text(aes(x = 2010, y = 170000, label="A)"),family="Times New Roman", colour="black", size=4) +
    theme(legend.position = c(0.85, 0.9), legend.title = element_blank (),
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
    geom_text(aes(x = 2010, y = 23000, label="B)"),family="Times New Roman", colour="black", size=4) +
    ylab('Difference\n') -> plot2
  plot2
  cowplot::plot_grid(plot1, plot2,  align = "v", nrow = 2, ncol=1) 
  ggsave(paste0('figs/', folder, "/fitted_plot.png"), dpi = 500, height = 8, width = 9, units = "in")
}


f_pred1999_plot_chilkat <- function(preds, run_through, perc = 0.95){ #change perc to 0.90 for alt run
  run_through = run_through$hard_date

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
    filter (year < 1983) %>%
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

f_pred2000_plot_chilkat <- function(preds, run_through){
  run_through = run_through$hard_date

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
    filter (year > 1982) %>%
    filter (year < 1995) %>%
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

f_pred2010_plot_chilkat <- function(preds, run_through){ #change perc to 0.90 for alt run
  run_through = run_through$hard_date

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
    filter (year > 1994) %>%
    filter (year < 2010) %>%
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

f_pred2015_plot_chilkat <- function(preds, run_through){ #change perc to 0.90 for alt run
  run_through = run_through$hard_date
  
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
    filter (year > 2009) %>%
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
  
  ggsave(paste0('figs/', folder, "/pred_plot_2015year.png"), plot = x, dpi = 100,
         height = 10, width = 8.5, units = "in")
}




