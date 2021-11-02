# Summary file of useful functions that I've added over the years
# These are organized by "Sections"; click the button just below the script editor to see sections




#### Assign Stat Week ####
statweek <- function(x) {
  as.numeric(format(as.Date(x), "%U")) - as.numeric(format(as.Date(cut(x, "year")), "%U")) + 1
  # Function modified from:
  # https://stackoverflow.com/questions/17286983/calculate-statistical-week-starting-1st-january-as-used-in-fisheries-data
  # Example usage: dataframe %>% mutate(week = statweek(datecolumn))
}



#### Unsummarize Data ####
duplicaterows <- function(dataframename, duplicatecolname = "specimen_count", replacenaswithone = FALSE){
  # Use this function if you encounter already summarized data and need to split it into long format
  # For example, you might have ASL data that has a row with Sex=M, Length=687, Specimen Count=3
  # This means that there are 3 fish that are males all with the same length of 687
  # Often though, we will want to run stats on our data which require it to NOT be summarized
  # This function takes summarized data and adds rows based on the count column

  # Using replacenaswithone allows you to decide whether an NA in the count column were really a count of 1
  # Carefully use this because most often an NA is a true zero.

  require(tidyverse)

  # Make an index of which rows will be repeated, and how many times
  .dupcount <- dataframename %>% dplyr::select(duplicatecolname) %>% tibble::deframe()

  # NAs will normally make this fail. We can replace NAs though
  # This replaces NAs with 1. THIS IS A LARGE ASSUMPTION SO BE CAREFUL
  if(sum(is.na(.dupcount) > 0) && replacenaswithone == TRUE){
    .dupcount <- replace_na(.dupcount, 1)
  }

  # Now repeat this for every row to duplicate.
  # A specimen count of 1 will mean the row isn't duplicated; a count of 5, repeats the row 5 times
  dataframename[rep(1:nrow(dataframename), .dupcount), ] %>%
    dplyr::select(-duplicatecolname) # Removes the count row now that it is incorrect!

  # Use like so: duplicaterows(dataframename = newdf, duplicatecolname = "Number.of.Specimens")
  # Thanks to: https://stackoverflow.com/questions/29743691/duplicate-rows-in-a-data-frame-in-r
}



##### Summarize Proportion #####

count_pct <- function(df) {
  # https://stackoverflow.com/questions/24576515/relative-frequencies-proportions-with-dplyr
  return(
    df %>%
      tally %>%
      mutate(n_pct = 100*n/sum(n))
  )
}





#### Impute Data ####


# There are four functions here:
# impute_global() which imputes all NAs in all years iteratively
# impute_cohodefault() which imputes all NAs before year 2000 globally, then imputes annually
# impute_local() which imputes a 10-year rolling imputation (prev & following 5 years)
# impute_local_improved which is similar to impute_local() but accounts for early years better

impute_global <- function(dfname, Year_column="year", StreamName_column="stream_name",
                         outputname = "globalimpute", # Only used if Step 3 turned "on"
                         Count_column = "total_count"){
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
  .test <- dfname %>% rename(year = Year_column, stream_name = StreamName_column, total_count = Count_column)
  .test <- .test %>% dplyr::select(year, stream_name, total_count)
  .test <- .test %>% mutate(imputed = is.na(total_count))

  # Step 2: Use multiplicative imputation as per Blick, in an iterative procedure
  j=1
  repeat{
    for(i in 1:nrow(.test)){
      .temprow = .test[i,]

      if(.temprow$imputed == TRUE){
        .sumyr = sum((.test %>% filter(year == .temprow$year) )$total_count, na.rm = TRUE)
        .sumrvr = sum((.test %>% filter(stream_name == .temprow$stream_name) )$total_count, na.rm = TRUE)
        .sumall = sum(.test$total_count, na.rm = TRUE)
        .test$total_count[i] = .sumyr * .sumrvr / .sumall
        # this interpolates across rows and columns
      }
    }
    j=j+1
    if(j>100){break} # repeat the above 100 times
  }
  print(.test)

  # Optional Step 3: Auto create a dataframe with the correct name
  # #assign(paste0((outputname), "_survey_imputed"), .test, envir = parent.frame() ) # use if you want a dynamic name
  ## imputedsurvey <- .test # use this if you want a static name
}




impute_cohodefault <- function(dfname, Year_column="year", StreamName_column="stream_name",
                               Count_column = "total_count", outputname = "defaultcohoimputed"){
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
  .test <- dfname %>% rename(year = Year_column, stream_name = StreamName_column, total_count = Count_column)
  .test <- .test %>% dplyr::select(year, stream_name, total_count)
  .test <- .test %>% mutate(imputed = is.na(total_count))

  # Step 2: Use multiplicative imputation as per Blick, in an iterative procedure

  j=1
  repeat{
    for(i in 1:nrow(.test %>% filter(year < 2000))){
      .temprow = .test[i,]
      if(.temprow$imputed == TRUE){
        .test_early <- .test %>% filter(year < 2000)
        .sumyr = sum((.test_early %>% filter(year == .temprow$year) )$total_count, na.rm = TRUE)
        .sumrvr = sum((.test_early %>% filter(stream_name == .temprow$stream_name) )$total_count, na.rm = TRUE)
        .sumall = sum(.test_early$total_count, na.rm = TRUE)
        .test$total_count[i] = .sumyr * .sumrvr / .sumall
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
        .sumyr = sum((.yr_range %>% filter(year == .temprow$year) )$total_count, na.rm = TRUE)
        .sumrvr = sum((.yr_range %>% filter(stream_name == .temprow$stream_name) )$total_count, na.rm = TRUE)
        .sumall = sum(.yr_range$total_count, na.rm = TRUE)
        .test$total_count[i] = .sumyr * .sumrvr / .sumall
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
                         Count_column = "total_count"){
  ### SUMMARY: 10-yr Localized Imputation ###
  # This takes a dataframe with NA values and imputes missing data
  # This algorithm uses "local" imputation: only 5 years before and after impute a missing value
  # i.e., only using the preceding 5 years and following 5 years
  # Make sure that all NAs are present (a missing row is NOT same as a row with an NA)

  ### EXAMPLE USAGE ###
  # impute_local(ktn_index, Year_column="year")

  # Step 1: Set up dataframe to impute
  require(dplyr)
  .test <- dfname %>% rename(year = Year_column, stream_name = StreamName_column, total_count = Count_column)
  .test <- .test %>% dplyr::select(year, stream_name, total_count)
  .test <- .test %>% mutate(imputed = is.na(total_count))

  # Step 2: Use multiplicative imputation as per Blick, in an iterative procedure

  j=1
  repeat{
    for(i in 1:nrow(.test)){
      .temprow = .test[i,]

      if(.temprow$imputed == TRUE){
        .yr_range = .test %>% filter(between(year, .temprow$year - 5, .temprow$year + 5)) #5 yrs before / after
        .sumyr = sum((.yr_range %>% filter(year == .temprow$year) )$total_count, na.rm = TRUE)
        .sumrvr = sum((.yr_range %>% filter(stream_name == .temprow$stream_name) )$total_count, na.rm = TRUE)
        .sumall = sum(.yr_range$total_count, na.rm = TRUE)
        .test$total_count[i] = .sumyr * .sumrvr / .sumall
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
                                  Count_column = "total_count"){
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
  .test <- dfname %>% rename(year = Year_column, stream_name = StreamName_column, total_count = Count_column)
  .test <- .test %>% dplyr::select(year, stream_name, total_count)
  .test <- .test %>% mutate(imputed = is.na(total_count))

  # Step 2: Use multiplicative imputation as per Blick, in an iterative procedure

  j=1
  repeat{
    for(i in 1:nrow(.test)){
      .temprow = .test[i,]

      if(.test$year < 1997){
        if(.temprow$imputed == TRUE){
          .test_early <- .test %>% filter(year < 1997)
          .sumyr = sum((.test_early %>% filter(year == .temprow$year) )$total_count, na.rm = TRUE)
          .sumrvr = sum((.test_early %>% filter(stream_name == .temprow$stream_name) )$total_count, na.rm = TRUE)
          .sumall = sum(.test_early$total_count, na.rm = TRUE)
          .test$total_count[i] = .sumyr * .sumrvr / .sumall
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
        .sumyr = sum((.yr_range %>% filter(year == .temprow$year) )$total_count, na.rm = TRUE)
        .sumrvr = sum((.yr_range %>% filter(stream_name == .temprow$stream_name) )$total_count, na.rm = TRUE)
        .sumall = sum(.yr_range$total_count, na.rm = TRUE)
        .test$total_count[i] = .sumyr * .sumrvr / .sumall
        # this is multiplicative imputation as per Blick
      }
    }
    j=j+1
    if(j>50){break} # repeat the above 50 times. Needs to be iterative (imputing depends on other imputed values)
  } # end late
  print(.test)
}



##### Custom ggplot theme #####
theme_coho <- function(base_size = 12, base_family = "Times New Roman", rotate_text=TRUE){
  require(ggsidekick) # Need package ggsidekick for theme_sleek()
  require(extrafont)  # Need package extrafont for fonts. See pkg docs
  theme_sleek(base_size = base_size, base_family = base_family) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #axis.line = element_line(size = .5), panel.border = element_blank()# Optional to remove border
    ) +
    if(rotate_text==TRUE){
      theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))
    } else{
      theme(axis.text.x = element_text(angle = 0))
    }
}

# A better version of theme_coho() is theme_crisp() which doesn't require ggsidekick
theme_crisp <- function(base_size = 12, base_family = "Arial", rotate_text=TRUE, rmborder=FALSE) {
  # This is based heavily on Sean Anderson's theme_sleek from ggsidekick
  # https://github.com/seananderson/ggsidekick

  require(extrafont)  # Need package extrafont for fonts. See pkg docs

  half_line <- base_size/2
  theme_light(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, color = NA),
      strip.text.x = element_text(color = "gray30"),
      strip.text.y = element_text(color = "gray30"),
      axis.text = element_text(color = "gray30"),
      axis.title = element_text(color = "gray30"),
      legend.title = element_text(color = "gray30", size = rel(0.9)),
      panel.border = element_rect(fill = NA, color = "gray70", size = 1),
      legend.key.size = unit(0.9, "lines"),
      legend.text = element_text(size = rel(0.7), color = "gray30"),
      legend.key = element_rect(color = NA, fill = NA),
      legend.background = element_rect(color = NA, fill = NA),
      plot.title = element_text(color = "gray30", size = rel(1)),
      plot.subtitle = element_text(color = "gray30", size = rel(.85))
    ) +
    {if(rmborder==TRUE){
      theme(axis.line = element_line(size = 0.5, color = "gray70"),
            panel.border = element_blank())
    }
    else{
      theme()
    }} + # If modifying in future, need {} around entire if statement
    if(rotate_text==TRUE){
      theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1))
    } else{
      theme(axis.text.x = element_text(angle = 0))
    }
}





# # Depends on dplyr. Does not function currently!
# tickr <- function(
#   data, # dataframe
#   var, # column of interest
#   to # break point definition
#   ){
#   #tickr from Ben Williams and Jane Sullivan
#   #https://github.com/ben-williams/FNGr/
#   require(dplyr)
#
#   VAR <- enquo(var) # makes VAR a dynamic variable
#
#   data %>%
#     dplyr::filter(!is.na(!!VAR)) %>%
#     distinct(!!VAR) %>%
#     # mutate(labels = ifelse(!!VAR %in% seq(to * round(min(!!VAR) / to), max(!!VAR), to),
#     #                        !!VAR, "")) %>% # orig BW code
#     mutate(labels = seq(to * round(min(!!VAR) / to), max(!!VAR), to)) %>%
#     dplyr::select(breaks = UQ(VAR), labels)
# }
#
#
# yaxt <- tickr(mtcars, mpg, 2)
#
# # ggplot(mpg, aes(class, fill = class)) +
# #   geom_bar(color = "black") +
# #   theme_crisp(rmborder = FALSE, rotate_text = TRUE)
#
# ggplot(mtcars, aes(x = wt, y = mpg, colour = factor(gear))) +
#   geom_point() +
#   facet_wrap(~am) +
#   theme_sleek() +
#   scale_y_continuous(breaks = yaxt$breaks, labels = yaxt$labels)








#### Specific Figure Creation ####
## PSC Report Figure 5 ##
create_esc_fig <- function(dataframename = SEAK_escape, river = "Auke Creek",
                           setbreaks = c(0,500, 1000, 1500), minyear = 1980, blank_x = TRUE, ...) {
  require(tidyverse)
  require(scales)
  # This requires a dataframe with columns "River", "Year", "Escapement_Count",
  # "EscapementGoal_Lower", & "EscapementGoal_Upper"
  # Function theme_coho() must already be run
  # It was originally used in the 2020 PSC Coho Report for Figure 5

  # Example Use:
  # create_esc_fig(dataframename = SEAK_escape, "Ketchikan Survey Index",
  #                setbreaks = seq(from=0, to=18000, by=2000),
  #                minyear = 1987, blank_x = FALSE)

  dataframename %>%
    filter(River == river, Year >= minyear) %>%
    ggplot(aes(x = Year, y = Escapement_Count)) +
    geom_col(fill = "gray", color = "black") +
    geom_line(aes(y=EscapementGoal_Lower), size = 1) +
    geom_line(aes(y=EscapementGoal_Upper), size = 1) +
    expand_limits(x = 1980) +
    scale_x_continuous(breaks = seq(from = 1980, to = max(SEAK_escape$Year), by = 2)) +
    scale_y_continuous(labels = scales::comma, breaks = setbreaks) +
    labs(x = "", y = "") +
    theme_coho(base_family = "Arial") +
    if(blank_x==TRUE){
      theme(axis.text.x = element_blank(), axis.title.x = element_blank())
    } else{
      labs(x = "Year")
    }
}



# Harvest Figure ##
create_harvestfig <- function(dataframe = indic_totalrun, river = "Auke Creek",
                              setbreaks = c(0, 1000, 2000, 3000), minyear = 1982, blank_x = TRUE) {
  require(tidyverse)
  require(scales)
  # This requires a dataframe with columns "River", "Year", "Escapement_Count",
  # "EscapementGoal_Lower", & "EscapementGoal_Upper"
  # Function theme_coho() must already be run
  # It was originally used in the 2020 PSC Coho Report for Figures 9 & 10

  # Example Use:
  # create_harvestfig(dataframe = indic_totalrun, river = "Auke Creek", blank_x = TRUE,
  #                   setbreaks = seq(from=0, to=3000, by=500))

  dataframe %>%
    filter(River == river, Year >= minyear) %>%
    ggplot(aes(x=Year, y = Count, fill = Fishery)) +
    geom_col(color = "black", width = 0.7, size=0.5) +
    geom_line(aes(y=EscapementGoal_Lower), size = 1) +
    geom_line(aes(y=EscapementGoal_Upper), size = 1) +
    scale_x_continuous(breaks = seq(from=1982, to=max(SEAK_escape$Year), by = 2)) +
    scale_y_continuous(labels = scales::comma, breaks = setbreaks) +
    scale_fill_manual(values = c("white", "black", "gray")) +
    labs(y = "Number of Coho Salmon") +
    theme_coho(base_family = "Arial") +
    theme(legend.position="none") +
    if(blank_x==TRUE){
      theme(axis.text.x = element_blank(), axis.title.x = element_blank())
    } else{
      labs(x = "Year")
    }
}

###############################


addrowconditional <- function(dataframename, criteriacolumn = columnname, 
                              repeatcount1 = 1, repeatcount2 = 0,
                              criteria1 = filter1, criteria2 = NA, 
                              sort1 = column1, sort2 = NA){
  # Use this function if you need to add rows to a dataframe based on a certain criteria
  # For example, you might have rockfish data that had species "groups" which might 
  #   need to be partitioned into extra rows for which add'l catch can be assigned.
  # This function can specify which rows need to be duplicated ("criteria1")
  #   and then sort ("sort1") to make a more useful view
  
  require(tidyverse)
  
  # Note the use of the {{}} here! 
  .duprows1 <- dataframename %>% 
    filter({{criteriacolumn}} == criteria1) 
  
  .duprows2 <- dataframename %>% 
    filter({{criteriacolumn}} == criteria2) 
  
  .duprows1 <- .duprows1[rep(1:nrow(.duprows1), repeatcount1), ]
  .duprows2 <- .duprows2[rep(1:nrow(.duprows2), repeatcount2), ]
  
  .gooddata <- dataframename %>%
    filter({{criteriacolumn}} != criteria1 | {{criteriacolumn}} != criteria2) 
  
  .duprows1 %>% 
    full_join(.duprows2) %>%
    full_join(.gooddata) %>%
    arrange({{sort1}}, {{sort2}})
  
  # Example usage: 
  # addrowconditional(df, criteriacolumn = species, # 
  #                  repeatcount1 = 5, repeatcount2 = 10,
  #                  criteria1 = 168, criteria2 = 140, # these are species 168 and 140
  #                  sort1 = year, sort2 = species)
  # This would create duplicate rows 5 times for species 168 (red rockfishes),
  #  and 10 duplicate rows for species 140, then sort by columns year and species
  
}





