# stt.r -------------------------------------------------------------------
stockPriceTimeline <- function(table, days_back = 14){
  table %>% 
    tail(days_back) %>% 
    mutate(close_q25 = quantile(Close, .25),
           close_q5 = quantile(Close, .5),
           close_q75 = quantile(Close, .75),
           trend = lm(Close ~ date)$coefficients[2],
           mean = round(mean(Close),4),
           sd_perc = round(sd(Close)/mean*100,2),
           high_price = round(mean + sd(Close)*1.75,2),
           low_price = round(mean - sd(Close)*1.75,2),
           min = min(Close),
           max = max(Close),
           last_n_days = days_back) %>% 
    mutate_if(is.numeric, round, 4) %>% 
    dplyr::select(last_n_days, mean, close_q5, high_price, low_price,
                  sd_perc, trend) %>% 
    distinct() 
}


readYahooFinance <- function(stock){
  page <- read_html(
    paste('https://finance.yahoo.com/quote/', 
          stock, 
          '/financials?p=',
          stock, 
          sep = "")) 
  tmp_nodes <- page %>% 
    html_nodes(".fi-row")
  tmp = NULL
  for(i in tmp_nodes){
    r <- list(i %>% 
                html_nodes("[title],[data-test='fin-col']") %>% 
                html_text())
    
    tmp <- rbind(tmp,
                 as.data.frame(matrix(r[[1]], 
                                      ncol = length(r[[1]]), 
                                      byrow = TRUE), 
                               stringsAsFactors = FALSE))
  }
  
  matches <- str_match_all(page%>%
                             html_node('#Col1-3-Financials-Proxy')%>%
                             html_text(),
                           '\\d{1,2}/\\d{1,2}/\\d{4}')  
  
  headers <- c('Breakdown','TTM', matches[[1]][,1]) 
  names(tmp) <- headers
  tmp 
  # %>%
  #   mutate_all(str_remove_all, ",") %>%
  #   mutate_at(vars(-Breakdown), as.numeric)
}

renameSummary <- function(x){
  x %>% 
    rename(
      `Median price` = close_q5,
      `Trend direction - price change per day` = trend,
      `Mean price` = mean,
      `Price deviations [%]` = sd_perc,
      `High price level` = high_price,
      `Low price level` = low_price)
}
stt <- function(stock){
  #' @name stt Stock Timeseries Tables
  #' 
  #' @description Produces multiple tables describing timeseries. 
  #' Includes: 
  #' - data collection: 
  #'  - Alpha Vantage: stock prices, 
  #'  - Yahoo Finance: stock prices, financial statement, 
  #'  - Tiingo: stock prices;
  #' - data processing: 
  #'   - data cleaning, 
  #'   - data summarizing;
  #'
  #' @param stock list produced with getMamut() function
  #' Available stocks: Cisco, Docusign, Dropbox, Netflix, Slack, Zendesk, Zoom
  #'
  #' @return list 
  #'  @slot intraday 
  #'   @slot av xts object covering prices, interval, periodicity, time range
  #'  @slot intraday_df
  #'   @slot av av$intraday dataframed and names cleaned
  #'   @slot tiingo tiible covering prices, interval, periodicity, time range
  #'  @slot daily
  #'   @slot av xts object covering prices, interval, periodicity, time range
  #'  @slot daily_df
  #'   @slot av xts object covering prices, interval, periodicity, time range
  #'  @slot financial_statement
  #'   @slot yahoo data.frame income statement https://finance.yahoo.com/quote/$stock/financials?p=$stock
  #'  @slot summary
  #'   @slot periodic data.frame summary of stt$intraday_df$av table
  #'   @slot intraday data.frame summary of stt$intraday_df$av table
  #'   
  
  message("Creating stt list.")
  stt <- list()
  message("Collecting the data.")
  stt$intraday$av <- getSymbols.av(stock$descriptions$stock, 
                                   from = "2020-01-01", 
                                   periodicity = "intraday",
                                   interval = "60min", 
                                   output.size = "full",
                                   api.key = "WYS1Z0Z74MH19M1Y",
                                   auto.assign = FALSE)
  message("Alpha Vantage collected")
  stt$intraday_df$tiingo <- tq_get(stock$descriptions$stock,
                                   get = "tiingo.iex",
                                   from   = "2019-01-01",
                                   to     = Sys.Date(),
                                   resample_frequency = "60min")
  message("Tiingo data collected.")
  stt$intraday_df$av <- stt$intraday$av %>%
    as.data.frame() %>%
    setNames(., sub(".*\\.", "", colnames(.))) %>%
    mutate(Date_time = lubridate::as_datetime((rownames(.))))
  message("Alpha Vantage cleaned.")
  message(paste("Alpha Vantage:", nrow(stt$intraday$av), "rows"))
  message(paste("Tiingo", nrow(stt$intraday_df$tiingo), "rows"))
  # ** daily ----------------------------------------------------------------
  stt$daily$av <- getSymbols.av(stock$descriptions$stock, 
                                from = "2016-01-01", 
                                output.size = "full",
                                periodicity = "daily",
                                api.key = "WYS1Z0Z74MH19M1Y",
                                auto.assign = FALSE)
  stt$daily_df$av <- stt$daily$av %>%
    as.data.frame() %>%
    setNames(., sub(".*\\.", "", colnames(.))) %>%
    mutate(date = as.Date(rownames(.)))
  message("Daily data collected.")
  message(paste("Alpha Vantage:", nrow(stt$daily$av), "rows"))
  # * scrape ----------------------------------------------------------------
  # ** financial statement --------------------------------------------------
  stt$financial_statement$yahoo <- readYahooFinance(stock$descriptions$stock)
  message("Financial statement scraped from Yahoo Finances.")
  # summary stats -----------------------------------------------------------
  # * daily -----------------------------------------------------------------
  stt$summary$periodic <- stt$daily_df$av %>% 
    stockPriceTimeline(days_back = 3) %>% 
    rbind(stt$daily_df$av %>%
            stockPriceTimeline(days_back = 7)) %>% 
    rbind(stt$daily_df$av %>% 
            stockPriceTimeline(days_back = 14)) %>% 
    rbind(stt$daily_df$av %>% 
            stockPriceTimeline(days_back = 30)) %>% 
    rbind(stt$daily_df$av %>% 
            stockPriceTimeline(days_back = 60)) %>% 
    rbind(stt$daily_df$av %>% 
            stockPriceTimeline(days_back = 90)) %>% 
    mutate(close_q5 = as.numeric(close_q5),
      trend = round(as.numeric(trend),4)) 
  # * intraday --------------------------------------------------------------
  stt$summary$intraday <- stt$intraday_df$av %>% 
    mutate(Date = as.Date(substr(Date_time, 1, 10))) %>% 
    group_by(Date) %>% 
    summarise(mean = round(mean(Close), 4),
              close_q5 = quantile(Close, .5),
              high_price = round(mean + sd(Close)*1.75,2),
              low_price = round(mean - sd(Close)*1.75,2),
              sd_perc = round(sd(Close)/mean*100,2),
              trend = lm(Close ~ Date_time)$coefficients[2]) %>% 
    ungroup() %>% 
    arrange(desc(Date))%>% 
    mutate(close_q5 = as.numeric(close_q5),
      trend = round(as.numeric(trend),4))
  message(paste(stock$descriptions$name, "data is prepared."))
  return(stt)
}

# report ------------------------------------------------------------------
renameSummary2 <- function(x){
  x %>% 
    rename(`Median price` = close_q5,
           `Trend direction - price change per time interval [$]` = trend,
           `Mean price` = mean,
           `Price deviations [%]` = sd_perc,
           `High price level` = high_price,
           `Low price level` = low_price)
}



color_table <- function(table ,var){
  round(
    seq(
      255, 
      40, 
      length.out = length(
        quantile(
          table %>% 
            dplyr::select(var),
          probs = seq(.05, .95, .05), 
          na.rm = TRUE)) + 1), 
    0) %>% 
    {paste0("rgb(255,", ., ",", ., ")")}
}
color_table_points <- function(table ,var){
  quantile(
    table %>% 
      dplyr::select(var),
    probs = seq(.05, .95, .05), 
    na.rm = TRUE)
}


mmtRender <- list()
mmtRender$discoverData <- function(x){
  
  files <- list()
  files$getAll <- as_tibble(list.files("render data")) %>% 
    mutate(type = substr(value,
                         1, 
                         str_locate(value, "_")-1),
           dateTimeP = substr(value, 
                              str_locate(value, "_")+1, 
                              str_locate(value, ".rds")-1),
           date = substr(dateTimeP, 1, 10),
           time = substr(dateTimeP, 12, str_count(dateTimeP)),
           time = paste(substr(time, 1, 2), ":",
                        substr(time, 3, 4), ":",
                        substr(time, 5, 6), 
                        sep = ""),
           dateTime = as_datetime(paste(date, time)))
  
  files$render <- files$getAll %>% 
    filter(type == "mmtRprt") %>% 
    filter(dateTime == max(dateTime))
  
  files$mostRecentRDS <- paste(files$render$value)
  
  return(files)
  
  
}

runPackage <- function(x){
  suppressPackageStartupMessages(library(x,character.only = TRUE))
}


# model competition -------------------------------------------------------

# # Invoking all necessary scripts
# source("src/forecast/dataReading.R")
# source("src/forecast/utils.R")
# source("src/forecast/preprocessingFunctions.R")
# source("src/forecast/classesDefinitions.R")
# source("src/forecast/generateForecastGeneric.R")
# source("src/forecast/forecastEnsembleGeneric.R")
# source("src/forecast/pickBestModelGeneric.R")
# source("src/forecast/forecastBestModelGeneric.R")
# source("src/forecast/modelsDefinitions.R")
# source("src/forecast/htsRelatedFunctions.R")
# source("src/forecast/multivariateForecastGeneric.R")
# source("src/forecast/retrievalFunctionsGenerics.R")

is_outlier_density_based <- function(x, outlier_border = 100, k = 5, ...){
  
  #' @title Wrapper for RKOF function from package DDoutlier
  #' 
  #' @description This function detect outliers with  algorithm based on kernel 
  #'  density estimation. Based on analysis such an approach dealt the best with
  #'  abnormal values which appeared among accuracy measures.
  #'  
  #' @param x Vector containing values among which we want to detect outliers.
  #' @param outlier_border Border value; if the scores calculated with RKOF function will be
  #'  higher than it then observations corresponding to them will be marked as outliers.
  #' @param k The number of nearest neighbors to compare density estimation with
  #' @param ... Further arguments passed to the RKOF function.
  #' 
  #' @return Logical vector indicating whether specific value is outlier (TRUE) or not (FALSE).
  
  n <- length(x)
  
  # If there is only one value in a vector we assume that this is not an outlier
  if(n == 1){
    return(F)
  }
  
  # customizing k parameter for different group sizes
  if(n >= 10){
    k <- round((n/7))
  }
  
  # k parameter should be lesser than n and greater than 0
  if(k >= n){
    k <- n - 1
  }
  
  # Since MAPE distribution is positively skew we should use absoulte values of
  # input vector to avoid classifying some small negative values as outliers.
  out_values <- DDoutlier::RKOF(as.matrix(abs(x)), k)
  
  res <- out_values > outlier_border
  res <- replace_na(res, F)
  res
  
}

# getData -----------------------------------------------------------------
azureNLP_raw <-function(stock){ 
  data <- list()
  data$time_stamp <- as.POSIXlt(Sys.time(), tz = "GMT") %>% gsub(" ", "_", .) %>% gsub(":", "-", .)
  stock$startNLP <- paste("2020-01-01", "T00:00:00", sep = "")
  # end - now as recent
  stock$endNLP <- paste(Sys.Date(), "T23:59:59", sep = "")
  # query
  stock$query <- paste(
    '{
  "firm": "', toupper(stock$descriptions$stock), '" ,',
  '"id_message" : 1,
  "start_range" : "', stock$startNLP, '" ,',
  '"end_range" : "', stock$endNLP, ', "',
  '"CompanySign": "', toupper(stock$descriptions$stock), '"', 
  '}',
    sep = ""
  )
  
  stock$query <- paste(
    '{
  "firm": "', toupper(stock$descriptions$stock), '" ,',
    '"id_message" : 1,
  "start_range" : "', stock$startNLP, '" ,',
    '"end_range" : "', stock$endNLP, '"',
    '}',
    sep = ""
  )
  message(paste("Collecting", stock$descriptions$name, "data. Time range:",
                paste(as_datetime(stock$startNLP), 
                      as_datetime(stock$endNLP), 
                      sep = " - ")))
  data$request <- POST(
    "http://127.0.0.1:8888/get_sentiments_for_firm",
    content_type_json(),
    body = stock$query)
  message("Query prepared.")
  # data collection
  data$raw <- content(data$request) %>% 
    map_dfr(function(x)unlist(x))
  message("NLP data collected.")
  return(data)
}


azureNLP <- function(stock){
  data <- azureNLP_raw(stock)
  data$clean <- data$raw %>% 
    mutate(provider = str_remove_all(provider, "�"),
           title = str_remove_all(title, "�")) %>% 
    rename(dateTime = date) %>% 
    mutate(Date = as.Date(substr(dateTime, 1, 10)),
           dateTime = as_datetime(dateTime),
           Time = strftime(dateTime-7200, format="%H:%M:%S"),
           stock = settings$descriptions$name) %>% 
    mutate(provider = ifelse(is.na(provider), "unknown", provider),
           title = ifelse(is.na(title), "unknown", title))
  message("NLP cleaned.")
  return(data)
}




# Scraper -----------------------------------------------------------------

searchArticles <- function(cmpn, src,
                           dictionary = "scraper/providersMain.csv",
                           paste_req = TRUE,
                           modified_link = FALSE){
  
  dict <- read.csv(dictionary, stringsAsFactors = F) %>% 
    filter(tolower(stock) == tolower(cmpn),
           tolower(provider_name) == tolower(src))
  link <- dict$stock_articles
  
  if(modified_link != FALSE){
    link <- searchArticles.modifyPage(link, src, modified_link)
  }

  links <-  link %>%
    as.character() %>% 
    read_html() %>% 
    html_nodes(css = dict$css_links) %>%
    html_attr("href") %>% 
    na.omit()
  
  if(tolower(src) == "investing"){
    links <- ifelse(!str_detect(links, "/news/") & !str_detect(links, "/analysis/"), 
                     "", links) %>% 
      na.omit(.) %>% 
      as_tibble() %>% 
      filter(. != "") %>% 
      unlist()
  }
  
  if(paste_req == TRUE) links <- paste0(dict$provider_url, links)
  
  return(links)
}

searchArticles.modifyPage <- function(link, provider, toPage){
  
  if(tolower(provider) == "infosfera"){
    link <- str_replace_all(link, "/1\\?", paste0("/", toPage, "?"))
  }
  
  if(tolower(provider) == "investing"){
    link <- paste0(link, "/", toPage)
  }
  
  return(link)
}

singleArticle <- function(link, 
                          css_elements = list(
                          text,
                          time, 
                          author,
                          title
                          )){
  
  if(css_elements$text != "NULL"){
    text <- read_html(link) %>%
      html_nodes(css = css_elements$text) %>%
      html_text() %>%
      paste(collapse = " ")
  }else text <- " "
  
  if(css_elements$time != "NULL"){
    time <- read_html(link) %>%
      html_nodes(css = css_elements$time) %>%
      html_text() 
  }else time <- " "
  
  if(css_elements$author != "NULL"){
    author <- read_html(link) %>%
      html_nodes(css = css_elements$author) %>%
      html_text()
  }else author <- " "
  
  if(css_elements$title != "NULL"){
    title <- read_html(link) %>%
      html_nodes(css = css_elements$title) %>%
      html_text() 
  }else title <- " "
  
  if(length(time) > 1) time <- time[2]
  if(length(author) > 1) author <- paste(author, collapse = ", ")
    
  # bind all together
  table <- data.frame(
    title = title,
    url = link,
    date = time,
    author = author,
    text_content = text,
    label = "",
    preview = "",
    keywords = "",
    tagged_text_content = ""
  )
  
  return(table)
}

singleArticle.css <- function(source){
  css <- read.csv("scraper/providersCSS.csv")
  css %>% 
    filter(tolower(provider) == tolower(source)) %>% 
    as.list()
}

articlesMod.time <- function(table, 
                             source){
  
  # define required labels for each source
  
  # Infosfera 
  if(tolower(source) == "infosfera"){
    mod <- table$date
    tim <- paste0(substr(mod, 1, 5), ":00")
    dat <- as.Date(substr(mod, 7, str_count(mod)), format = '%d/%m/%Y')
    mod <- paste(dat, tim, sep = "T")
    table$date <- mod
  }
  
  if(tolower(source) == "investing"){
    mod <- table$date
    mod <- anytime::anydate(mod)
    table$date <- mod
  }
  
  return(table)
}

articlesMod.labels <- function(table, cmpn, src,
                               dictionary = "scraper/providersMain.csv"){

    table <- table %>% 
      mutate(source = src,
             provider_url = read.csv(dictionary) %>% 
               filter(tolower(stock) == tolower(cmpn),
                      tolower(provider_name) == tolower(src)) %>% 
               pull(stock_articles),
             CompanySign = read.csv(dictionary) %>% 
               filter(tolower(stock) == tolower(cmpn),
                      tolower(provider_name) == tolower(src)) %>% 
               pull(provider_name2),
             firm = cmpn) 
  
  return(table)
}

# dataProcessing ----------------------------------------------------------

dataEnhance.stt <- function(data, 
                            stt){
  data$full <- stt$summary$intraday %>% 
    left_join(stt$intraday_df$av %>% 
                mutate(Date = as.Date(substr(Date_time, 1, 10)))) %>% 
    janitor::clean_names() %>%
    mutate(dateTime = as.POSIXct(date_time)) %>% 
    dplyr::select(-date_time) %>% 
    left_join(data$clean) %>% 
    mutate(signal_up = ifelse(predicted_trend > 0, low_price, NA),
           signal_down = ifelse(predicted_trend < 0, high_price, NA)) 
  message( "NLP data joined with timeseries (Alpha Vantage).")
  return(data)
}

dataEnhance.modelTime <- function(data, 
                                  modelTime){
  
  message( "Data enhanced with timeseries models.")
  return(data)
}




postprocessMamut <- function(data, stt){
  
  data$signalTable <- data$full %>% 
    mutate(signal_up = as.numeric(signal_up),
           signal_down = as.numeric(signal_down)) %>% 
    dplyr::select(-Time) %>% 
    renameSummary() %>% 
    rename(`Signal up` = signal_up,
           `Signal down` = signal_down,
           Time = dateTime,
           `Signal provider` = provider
    ) 
  # table 2
  data$signalPast <- stt$daily_df$av %>% 
    left_join(data$clean %>% 
                mutate(date = as.Date(substr(dateTime, 1, 10)))) %>% 
    janitor::clean_names() %>% 
    mutate(signal_up = ifelse(predicted_trend > 0, 
                              round((open+low)/2,2), 
                              NA),
           signal_down = ifelse(predicted_trend < 0, 
                                round((open+hign)/2,2), 
                                NA)) %>% 
    mutate(signal_up = as.numeric(signal_up),
           signal_down = as.numeric(signal_down)) %>% 
    filter(date > "2019-01-01") 
  # table 3
  data$signalPast3 <- data$signalPast %>% 
    group_by(date) %>% 
    summarise(open = unique(open),
              high = unique(high),
              low = unique(low),
              close = unique(close),
              texts_scanned = n_distinct(na.omit(id_document)),
              signals_up = length(which(predicted_trend == 1)),
              signals_down = length(which(predicted_trend == -1)),
              signalPrice = round(mean(signal_up, na.rm = T),2),
              urls = n_distinct(na.omit(url)),
              providers = n_distinct(na.omit(provider)))%>% 
    ungroup()
  
  # modelTime$refitHorizonEnd <- modelTime$refit %>% 
  #   full_join(modelTime$refitForecastActuals) %>% 
  #   filter(!is.na(rankedForecast)) %>% 
  #   filter(.index == max(.index)) %>%  
  #   pull(.index)
  # modelTime$refitHorizonEndShort <- modelTime$refitShort %>% 
  #   full_join(modelTime$refitForecastActualsShort) %>% 
  #   filter(!is.na(rankedForecast)) %>% 
  #   filter(.index == max(.index)) %>%  
  #   pull(.index)
  
  # data$forecastSummary$tmp2 <- tibble(
  #   Method = "Long Memory Ensemble",
  #   `Forecasting period` = paste(modelTime$refitHorizonEnd - Sys.Date(), "days"),
  #   
  #   `Mean forecasted price change [$]` = modelTime$refit %>% 
  #     full_join(modelTime$refitForecastActuals) %>% 
  #     filter(!is.na(rankedForecast)) %>%
  #     filter(rankedForecast > 0) %>% 
  #     summarise(round(mean(rankedForecast))) %>% 
  #     pull(),
  #   
  #   `Highest forecasted price change [$]` = modelTime$refit %>% 
  #     full_join(modelTime$refitForecastActuals) %>% 
  #     filter(!is.na(rankedForecast)) %>% 
  #     filter(rankedForecast == max(rankedForecast)) %>%  
  #     pull(rankedForecast),
  #   
  #   `Lowest forecasted price change [$]` = modelTime$refit %>% 
  #     full_join(modelTime$refitForecastActuals) %>% 
  #     filter(!is.na(rankedForecast)) %>% 
  #     filter(rankedForecast == min(rankedForecast)) %>%  
  #     pull(rankedForecast)
  # )
  # 
  # data$forecastSummary$tmp <- rbind(data$forecastSummary$tmp, 
  #                                   data$forecastSummary$tmpModel2)
  # data$forecastSummary$tmp3 <- data$forecastSummary$tmp %>% 
  #   # rbind(data$forecastSummary$tmp2) %>% 
  #   rbind(tibble(
  #     Method = "Short Memory Ensemble",
  #     `Forecasting period` = paste(modelTime$refitHorizonEndShort - Sys.Date(), "days"),
  #     `Mean forecasted price change [$]` = modelTime$refitShort %>% 
  #       full_join(modelTime$refitForecastActualsShort) %>% 
  #       filter(!is.na(rankedForecast)) %>%
  #       filter(rankedForecast > 0) %>% 
  #       summarise(round(mean(rankedForecast))) %>% 
  #       pull(),
  #     
  #     `Highest forecasted price change [$]` = modelTime$refitShort %>% 
  #       full_join(modelTime$refitForecastActualsShort) %>% 
  #       filter(!is.na(rankedForecast)) %>% 
  #       filter(rankedForecast == max(rankedForecast)) %>%  
  #       pull(rankedForecast),
  #     
  #     `Lowest forecasted price change [$]` = modelTime$refitShort %>% 
  #       full_join(modelTime$refitForecastActualsShort) %>% 
  #       filter(!is.na(rankedForecast)) %>% 
  #       filter(rankedForecast == min(rankedForecast)) %>%  
  #       pull(rankedForecast)
  #   ))
  # 
  # data$forecastSummary$tmp4 <- data$forecastSummary$tmp3 %>% 
  #   mutate(`Forecasting period` = factor(`Forecasting period`))
  # data$forecastSummary$tmp4$`Lowest forecasted price change [$]` <- round(data$forecastSummary$tmp4$`Lowest forecasted price change [$]`, 2)
  # data$forecastSummary$tmp4$`Highest forecasted price change [$]` <- round(data$forecastSummary$tmp4$`Highest forecasted price change [$]`, 2)
  # data$forecastSummary$tmp4$`Mean forecasted price change [$]` <- round(data$forecastSummary$tmp4$`Mean forecasted price change [$]`, 2)
  # 
  # 
  return(list(data = data))
}

prepareMamut <- function(data, stt, stock){
  
  validation <- list()
  windowPrices <- stt$intraday_df$tiingo %>% 
    dplyr::select(date, close) %>% 
    mutate(date = as.character(date)) %>% 
    rename(windowOpen = date,
           priceSignal = close) %>% 
    mutate(priceValid = lead(priceSignal),
           windowClose = lead(windowOpen)) %>% 
    mutate(priceChange = priceValid - priceSignal)%>% 
    filter(complete.cases(.)) %>% 
    mutate(priceChangeAbs = abs(priceChange),
           outlierDens = is_outlier_density_based(priceChangeAbs))
  validation$windowPrices <- windowPrices
  response <- data$clean %>% 
  mutate(stock = unique(stock$descriptions$stock)) %>% 
    filter(important_keywords != "") %>% 
    rename(nlpID = id_document) %>% 
    mutate(signalTime = lubridate::round_date(dateTime, unit = minutes(x = 60))) %>% 
    mutate(impactTime = lubridate::round_date(dateTime, unit = minutes(x = 60))+3600) %>% 
    dplyr::select(nlpID, stock, predicted_trend, dateTime, signalTime, impactTime) %>% 
    mutate(t2  = as.numeric(substr(signalTime, 12, 13))) %>% 
    mutate(t3 = ifelse(t2 < 9 | t2 > 16, F, T),
           t4 = as.numeric(substr(impactTime, 12, 13))) %>% 
    mutate(nextday = ifelse(t4 < 17, F, T)) %>% 
    mutate(signalDate = as.Date(substr(signalTime, 1, 10)),
           impactDate = as.Date(substr(impactTime, 1, 10))) %>%
    mutate(impactDate = impactDate + nextday) %>% 
    mutate(windowOpen = ifelse(t3 == T, 
                               as.character(signalTime), 
                               ifelse(t2 < 9, 
                                      paste(signalDate - 1,"17:00:00"),
                                      paste(signalDate, "17:00:00"))),
           windowClose = ifelse(t3 == T, 
                                as.character(impactTime), 
                                paste(impactDate, "10:00:00"))) %>% 
    dplyr::select(nlpID, stock, predicted_trend, dateTime, windowOpen, windowClose) 
  
  tmp_price <- stt$intraday_df$tiingo %>% 
    dplyr::select(date, close) %>% 
    mutate(date = as.character(substr(date, 1, 10))) %>% 
    rename(windowClose = date,
           priceValid = close) %>% 
    group_by(windowClose) %>% 
    summarise(priceValid = priceValid[n()]) %>% 
    ungroup() %>% 
    mutate(day2Validation = lag(priceValid),
           day3Validation = lag(priceValid,2),
           priceValid2_lead = lead(priceValid))
  
  tmp_signal <- stt$intraday_df$tiingo %>% 
    dplyr::select(date, close) %>% 
    mutate(date = as.character(substr(date, 1, 10))) %>% 
    rename(windowClose = date,
           priceSignal = close) %>% 
    group_by(windowClose) %>% 
    summarise(priceSignal = priceSignal[n()]) %>% 
    ungroup()
  
  response2 <- response %>% 
    mutate(windowClose = as.character(substr(windowClose, 1, 10))) %>% 
    left_join(tmp_signal) %>% 
    left_join(tmp_price) %>% 
    mutate(priceChange = round((1.5*priceValid + 1.25*day2Validation + .25*day3Validation)/3,2)) %>% 
    mutate(priceChange = priceChange - priceSignal)
  
  response <- response %>% 
    filter(complete.cases(.))
  
  overallStats <- tibble(sd = sd(windowPrices$priceChange, na.rm = T),
                         sd_abs = abs(sd),
                         q8_abs = quantile(abs(windowPrices$priceChange), .8, na.rm = T))
  
  # validation summary
  validation$response <- response2 %>% 
    mutate(predicted_trend = as.numeric(predicted_trend)) %>% 
    ungroup() %>% 
    mutate(priceDirection = ifelse(priceChange > 0, 1, 
                                   ifelse(priceChange < 0, -1, 0)),
           priceChange = abs(priceChange),
           #outDensBas = is_outlier_density_based(priceChange),
           outQuant = ifelse(priceChange > overallStats$q8_abs, T, F),
           outSD = ifelse(priceChange > 2.5*overallStats$sd_abs, T, F),
           outSUM = outQuant + outSD,
           validatePrice = ifelse(outSUM >= 2, 1, 0),
           validatePrice = validatePrice * priceDirection) %>%
    mutate(comparision_t = predicted_trend == priceDirection,
           comparision_f = ifelse(predicted_trend == 0, F,
                                  predicted_trend == priceDirection)) %>% 
    mutate(score = predicted_trend * priceDirection * priceChange) %>% 
    dplyr::select(-nlpID) %>% 
    distinct() %>% 
    mutate(validNLP = ifelse((predicted_trend > 0 & score > 0) | (predicted_trend < 0 & score < 0), 
                             1, 
                             ifelse((predicted_trend > 0 & score < 0) | (predicted_trend < 0 & score > 0),
                                    -1,
                                    0))) %>% 
    dplyr::select(-comparision_t, -comparision_f)
  validation$summary <- tibble(
    true_signals = sum(validation$response$score > 0),
    false_signals = sum(validation$response$score < 0),
    ts_score = sum(validation$response$score[which(validation$response$score > 0)]),
    fs_score = sum(validation$response$score[which(validation$response$score < 0)])
    #,
    #perc_score = positive_score / (positive_score + abs(negative_score)) 
  )
  
  validation$daily <- validation$response %>% 
    mutate(date = as.Date(windowOpen)) %>% 
    group_by(date) %>% 
    summarise(signals = sum(abs(predicted_trend)),
              singal_mean = round(mean(predicted_trend),2),
              priceDirection_mean = round(mean(priceDirection),2),
              priceChange_mean = round(mean(priceChange),2),
              outSUM = sum(outSUM),
              validated_score = round(sum(score),2))
  
  if(nrow(validation$response) == 0){
    tmp_matrix <- matrix(1,ncol = ncol(validation$response)) %>% 
      as.data.frame() %>% 
      setNames(colnames(validation$response)) %>% 
      mutate_all(function(x) "no signals provided")
    
    validation$response <- rbind(
      validation$response,
      tmp_matrix)
      rm(tmp_matrix)
  }
  
  validation$all <- validation$windowPrices %>% 
    dplyr::select(-priceChange) %>% 
    left_join(validation$response %>% 
                dplyr::select(-windowClose,
                              -priceSignal, -priceValid))
  tmp_clean<-data$clean %>%
    dplyr::select(Date, Time, predicted_trend, title, provider, url, dateTime) %>% 
    arrange(desc(dateTime)) %>% 
    mutate(provider = factor(provider),
           predicted_trend = factor(predicted_trend)) %>% 
    rename(Signal = predicted_trend,
           `Text title` = title,
           `Text provider` = provider) %>% 
    dplyr::select(-dateTime)
  tmp_sp3 <- data$signalPast3 %>% 
    mutate(price_change = lead(close) - close,
           days = lead(date) - date,
           price_change_daily = round(price_change / as.numeric(days),2),
           price_change_perc = round(price_change/close*100,2)) 
  plot1 <- tmp_sp3 %>% 
    left_join(tmp_clean %>% 
                rename(date = Date) %>% 
                dplyr::select(-Time)) 
  
  tst2 <- plot1 %>% 
    mutate(Signal = as.numeric(as.character(Signal))) %>% 
    filter(!is.na(`Text provider`)) %>% 
    left_join(tmp_clean %>% 
                dplyr::select(Time, url)) %>% 
    dplyr::select(-open, -high, -low, -url) %>% 
    arrange(date, Time) %>% 
    mutate(n = 1) %>% 
    mutate(`Intraday score` = round((close - signalPrice)/signalPrice*100,2)*Signal,
           `Next day score` = round(((close+price_change) - signalPrice)/signalPrice*100,2)*Signal,
    ) %>% 
    mutate(`Provider signals` = cumsum(abs(Signal)),
           `Provider texts` = 1:n(),
           `Sum of intraday scores` = sapply(`Provider texts`, 
                                             function(`Provider texts`){ round(sum(`Intraday score`[1:`Provider texts`], na.rm = TRUE),2) 
                                             }
           ),
           `Sum of next day scores` = sapply(`Provider texts`, 
                                             function(`Provider texts`){ round(sum(`Next day score`[1:`Provider texts`], na.rm = TRUE),2)
                                             }
           )) %>% 
    ungroup() %>% 
    mutate(Signal = factor(Signal))%>% 
    mutate(`Intraday score` = ifelse(is.na(`Intraday score`), 0, `Intraday score`),
           `Next day score` = ifelse(is.na(`Next day score`), 0, `Next day score`)) %>% 
    mutate(`Sum of scores` = round((`Sum of intraday scores` + 
                                      `Sum of next day scores`)/2,4))
  
  tst3 <- tst2 %>% 
    left_join(tmp_clean %>% 
                rename(date = Date) %>% 
                dplyr::select(date, Time, url)) %>% 
    left_join(data$clean %>% dplyr::select(id_document, url)) %>% 
    dplyr::select(id_document, Signal, price_change_perc) %>% 
    mutate(Signal = as.numeric(as.character(Signal))) %>% 
    mutate(validation = ifelse(Signal*price_change_perc > 0, 1, 
                               ifelse(Signal*price_change_perc < 0, -1, 0))) %>% 
    dplyr::select(-price_change_perc) %>% 
    mutate(stock = settings$descriptions$name) %>% 
    rename(id = id_document,
           real_trend = validation,
           predicted_trend = Signal)
  
  # Save data ---------------------------------------------------------------
  # * local -----------------------------------------------------------------
  #  source("src/saveImage.r") 
  
  stock$render$dataStamp <- paste("mmtRprt_", 
                                     str_replace_all(str_replace_all(Sys.time(), " ", "-"), ":", ""), 
                                     sep = "")
  stock$render$reportName <- paste(stock$descriptions$name, Sys.Date(), sep = "_")
  stock$render$folderData <- paste("render/render data/",
                                   stock$render$dataStamp, ".rds", sep = "")
  stock$render$folderFileName <- paste("render/render data/",
                                       stock$render$reportName, ".rds", sep = "")
  
  data$signalPast2 <- data$signalPast[,2:6] %>% 
    dplyr::select(-volume) %>% 
    distinct() %>% 
    left_join(validation$daily)
  # table 4
  # mamutReportData <- list(data = data, 
  #                         settings = settings,
  #                         stats = stt,
  #                         forecast = forecastModels,
  #                         modelTime = modelTime,
  #                         xgbData = xgbData,
  #                         validation = validation)
  

  
  # saveRDS(mamutReportData, settings$render$folderData)
  
  tmp_formattable <- stt$summary$periodic %>% 
    renameSummary() %>% 
    rename(`Analyzed period: days back` = last_n_days)
  
  stock$imp_formatters$mean_price <-
    round(mean(tmp_formattable$`Mean price`, 2))
  stock$imp_formatters$median_price <-
    round(quantile(tmp_formattable$`Median price`, .5), 2)
  stock$imp_formatters$high_price <-
    round(quantile(tmp_formattable$`High price level`, .7), 2)
  stock$imp_formatters$low_price <-
    round(quantile(tmp_formattable$`Low price level`, .8), 2)
  stock$imp_formatters$sd_perce <-
    round(quantile(tmp_formattable$`Price deviations [%]`, .6), 2)
  
  stock$imp_formatters$mp <- 
    formatter("span", 
              style = x ~ style(
                font.weight = "bold", 
                color = ifelse(x > settings$imp_formatters$mean_price, "#7EDB76",
                               ifelse(x < settings$imp_formatters$mean_price, "#F94A2E", 
                                      "black"))))
  stock$imp_formatters$medp <- 
    formatter("span", 
              style = x ~ style(
                font.weight = "bold", 
                color = ifelse(x > settings$imp_formatters$median_price, "#7EDB76",
                               ifelse(x < settings$imp_formatters$median_price, "#F94A2E", 
                                      "black"))))
  stock$imp_formatters$hp <- 
    formatter("span", 
              style = x ~ style(
                font.weight = "bold", 
                color = ifelse(x > settings$imp_formatters$high_price, "#7EDB76",
                               ifelse(x < settings$imp_formatters$high_price, "#F94A2E", 
                                      "black"))))
  stock$imp_formatters$lp <- 
    formatter("span", 
              style = x ~ style(
                font.weight = "bold", 
                color = ifelse(x > settings$imp_formatters$low_price, "#7EDB76",
                               ifelse(x < settings$imp_formatters$low_price, "#F94A2E", 
                                      "black"))))
  stock$imp_formatters$sd <- 
    formatter("span", 
              style = x ~ style(
                font.weight = "bold", 
                color = ifelse(x > settings$imp_formatters$sd_perce, "#7EDB76",
                               ifelse(x < settings$imp_formatters$sd_perce, "#F94A2E", 
                                      "black"))))
  stock$imp_formatters$last_prices <- 
    formatter("span", 
              style = x ~ style(
                font.weight = "bold", 
                color = ifelse(x > stt$tiingo$intraday %>% 
                                 tail(21) %>% 
                                 dplyr::select(close) %>% 
                                 summarise(mean(close)), 
                               "#7EDB76",
                               ifelse(x < stt$tiingo$intraday %>% 
                                        tail(21) %>% 
                                        dplyr::select(close) %>%
                                        summarise(mean(close)), 
                                      "#F94A2E", 
                                      "black"))))
  # validation <- mmtData$validation
  tmp_sp3 <- data$signalPast3 %>% 
    mutate(price_change = lead(close) - close,
           days = lead(date) - date,
           price_change_daily = round(price_change / as.numeric(days),2),
           price_change_perc = round(price_change/close*100,2)) 
  tmp_clean<-data$clean %>%
    dplyr::select(Date, Time, predicted_trend, title, provider, url, dateTime) %>% 
    arrange(desc(dateTime)) %>% 
    mutate(provider = factor(provider),
           predicted_trend = factor(predicted_trend)) %>% 
    rename(Signal = predicted_trend,
           `Text title` = title,
           `Text provider` = provider) %>% 
    dplyr::select(-dateTime)
  # to add model general score
  # provider score
  
  tmp_clean2 <- tmp_clean
  
  validation$all_summary <- validation$all %>% 
    mutate(predicted_trend = as.numeric(predicted_trend),
           dateTime = as.Date(windowOpen)) %>% 
    left_join(data$clean %>% 
                dplyr::select(dateTime, predicted_trend, title, provider) %>% 
                mutate(predicted_trend = as.numeric(predicted_trend),
                       dateTime = as.Date(dateTime))) %>% 
    distinct() %>% 
    mutate(score = ifelse(is.na(score), 
                          round(priceValid - priceSignal,2),
                          score)) %>% 
    group_by(windowOpen) %>% 
    summarise(priceSignal = round(mean(priceSignal, na.rm = T),2),
              priceValid = round(mean(priceValid, na.rm = T),2),
              predicted_trend = round(mean(predicted_trend, na.rm = T),2),
              scoreValue = round(length(score[!is.na(score) & score != 0])/n(),2),
              score = round(mean(score, na.rm = T),2),
              texts = n_distinct(na.omit(title)),
              obs = n(),
              providers = n_distinct(na.omit(provider))) %>% 
    ungroup() %>% 
    mutate(predicted_trend = ifelse(is.na(predicted_trend),
                                    0, predicted_trend)) %>% 
    mutate(buySignal = ifelse(predicted_trend == 1, priceSignal, NA),
           sellSignal = ifelse(predicted_trend == -1, priceSignal, NA))
  
  plot1 <- tmp_sp3 %>% 
    left_join(tmp_clean %>% 
                rename(date = Date) %>% 
                dplyr::select(-Time)) 
  
  validation$all_daily <- validation$all %>% 
    rename(Time = windowOpen) %>% 
    mutate(Time = as_datetime(Time),
           buy = ifelse(predicted_trend == 1, priceSignal, NA),
           sell = ifelse(predicted_trend == -1, pricesSignal, NA),
           priceChange = priceValid - priceSignal,
           priceDirection = ifelse(priceChange > 0, 1, 
                                   ifelse(priceChange == 0, 0, -1)),
           validatedPrice = priceChange * priceDirection + priceSignal) %>% 
    filter(!is.na(priceChange)) %>% 
    mutate(date = as.Date(substr(Time, 1, 10))) %>% 
    group_by(date) %>% 
    summarise(priceSignal = round(mean(priceSignal),2),
              buySignal = round(mean(buy, na.rm = T),2),
              priceValid = round(mean(priceValid),2),
              nlp_score_mean = round(mean(predicted_trend),2),
              score_mean = round(mean(score, na.rm = T),2),
              n = n())
  
  dt1 <- plot1 %>%
    filter(Signal == 1 | Signal == -1) %>%
    arrange(desc(date)) %>%
    dplyr::select(
      -urls,
      -texts_scanned,
      -signals_up,
      -signals_down,
      -providers,
      -price_change,
      -days,
      -date,
      -`Text title`) %>% 
    left_join(data$clean %>% 
                dplyr::select(dateTime, url)) %>% 
    dplyr::select(Signal, `Text provider`,
                  everything()) %>% 
    mutate(Signal = as.numeric(as.character(Signal))) %>% 
    rename(`Validated change [$]` = price_change_daily,
           `Validated change [%]` = price_change_perc,
           Time = dateTime,
           `NLP Signal` = Signal,
           `NLP Signal price` = signalPrice)
  
  scores_fix <- plot1 %>% 
    mutate(Signal = as.numeric(as.character(Signal))) %>% 
    filter(!is.na(`Text provider`)) %>% 
    filter(Signal != 0) %>% 
    dplyr::select(date,`Text provider`, price_change, Signal) %>% 
    group_by(date) %>% 
    summarise(score = sum(price_change * Signal),
              predicited_trend = round(mean(Signal),2),
              price_change = round(mean(price_change),2),
              providers = n_distinct(`Text provider`)) %>% 
    ungroup() 
  
  scores_fix_p <- plot1 %>% 
    mutate(Signal = as.numeric(as.character(Signal))) %>% 
    filter(!is.na(`Text provider`)) %>% 
    filter(Signal != 0) %>% 
    dplyr::select(date,`Text provider`, price_change, Signal) %>% 
    group_by(date,`Text provider`) %>% 
    summarise(score = sum(price_change * Signal),
              predicited_trend = round(mean(Signal),2),
              price_change = round(mean(price_change),2)) %>% 
    ungroup() 
  
  dt2 <- dt1 %>% 
    filter(`NLP Signal` == 1 | `NLP Signal` == 1) %>% 
    mutate(`Signal validation` = as.numeric(`Validated change [$]` > 0)) %>% 
    group_by(`NLP Signal`, `Signal validation`) %>% 
    summarise(`Signals` = n(),
              `Validated price change [$]` = sum(`Validated change [$]`)) %>% 
    ungroup()
  
  minscoresifx <- scores_fix_p %>% 
    summarise(signals = n(),
              score_sum = sum(score),
              score_mean = round(mean(predicited_trend * price_change),2))
  
  predicted_price_change <- dt1$`Validated change [$]` %>% 
    sum() / dt1$`Validated change [$]` %>% abs() %>% 
    sum() * 100 
  predicted_price_change <- round(predicted_price_change, 2)
  
  scoring_prob <- sum(dt2$Signals[which(dt2$`NLP Signal` * dt2$`Validated price change [$]` > 0)]) / sum(dt2$Signals)
  
  # sum(scores_fix$price_change[scores_fix$price_change > 0])
  # abs(sum(scores_fix$price_change[scores_fix$price_change < 0]))
  
  predicted_price_change <- round(
    sum(scores_fix$price_change[scores_fix$price_change > 0]) / (sum(scores_fix$price_change[scores_fix$price_change > 0]) +abs(sum(scores_fix$price_change[scores_fix$price_change < 0])) )*100, 2
  )
  
  tmp_scores <- tmp_clean %>% 
    dplyr::select(-url)
  
  tst <- plot1 %>% 
    mutate(Signal = as.numeric(as.character(Signal))) %>% 
    filter(!is.na(`Text provider`)) %>% 
    left_join(tmp_clean %>% 
                dplyr::select(Time, url)) %>% 
    dplyr::select(-open, -high, -low, -url) %>% 
    arrange(date, Time) %>% 
    mutate(n = 1) %>% 
    mutate(`Intraday score` = round((close - signalPrice)/signalPrice*100,2)*Signal,
           `Next day score` = round(((close+price_change) - signalPrice)/signalPrice*100,2)*Signal,
    ) %>% 
    group_by(`Text provider`) %>% 
    mutate(`Provider signals` = cumsum(abs(Signal)),
           `Provider texts` = 1:n(),
           `Provider intraday mean score` = sapply(`Provider texts`, 
                                                   function(`Provider texts`){ round(sum(`Intraday score`[1:`Provider texts`], na.rm = TRUE),2) 
                                                   }
           ),
           `Provider next day mean score` = sapply(`Provider texts`, 
                                                   function(`Provider texts`){ round(sum(`Next day score`[1:`Provider texts`], na.rm = TRUE),2)
                                                   }
           )) %>% 
    ungroup() %>% 
    mutate(Signal = factor(Signal))%>% 
    mutate(`Intraday score` = ifelse(is.na(`Intraday score`), 0, `Intraday score`),
           `Next day score` = ifelse(is.na(`Next day score`), 0, `Next day score`)) %>% 
    mutate(`Provider intraday mean score` = round(`Provider intraday mean score` / 
                                                    `Provider signals`,2),
           `Provider next day mean score` = round(`Provider next day mean score`/ 
                                                    `Provider signals`,2)) %>% 
    mutate(`Mean score` = round((`Provider intraday mean score` + 
                                   `Provider next day mean score`)/2,4))
  tst2 <- plot1 %>% 
    mutate(Signal = as.numeric(as.character(Signal))) %>% 
    filter(!is.na(`Text provider`)) %>% 
    left_join(tmp_clean %>% 
                dplyr::select(Time, url)) %>% 
    dplyr::select(-open, -high, -low, -url) %>% 
    arrange(date, Time) %>% 
    mutate(n = 1) %>% 
    mutate(`Intraday score` = round((close - signalPrice)/signalPrice*100,2)*Signal,
           `Next day score` = round(((close+price_change) - signalPrice)/signalPrice*100,2)*Signal,
    ) %>% 
    mutate(`Provider signals` = cumsum(abs(Signal)),
           `Provider texts` = 1:n(),
           `Sum of intraday scores` = sapply(`Provider texts`, 
                                             function(`Provider texts`){ round(sum(`Intraday score`[1:`Provider texts`], na.rm = TRUE),2) 
                                             }
           ),
           `Sum of next day scores` = sapply(`Provider texts`, 
                                             function(`Provider texts`){ round(sum(`Next day score`[1:`Provider texts`], na.rm = TRUE),2)
                                             }
           )) %>% 
    ungroup() %>% 
    mutate(Signal = factor(Signal))%>% 
    mutate(`Intraday score` = ifelse(is.na(`Intraday score`), 0, `Intraday score`),
           `Next day score` = ifelse(is.na(`Next day score`), 0, `Next day score`)) %>% 
    mutate(`Sum of scores` = round((`Sum of intraday scores` + 
                                      `Sum of next day scores`)/2,4))
  
  tmp002 <- tibble(`XGB feature` = colnames(xgbData$predictions )[c(-1, -24, -26, -27)])
  
  tst_t <- tst %>% 
    group_by(`Text provider`) %>% 
    summarise(`Provider signals` = sum(`Provider signals`, na.rm = T),
              `Provider texts` = sum(`Provider texts`, na.rm = T)) %>% 
    ungroup()
  
  stock$imp_formatters$cc <- 
    formatter("span", 
              style = x ~ style(
                font.weight = "bold", 
                color = ifelse(x > dtt1$`Day close price`, "#7EDB76",
                               ifelse(x < dtt1$`Day close price`, "#F94A2E", 
                                      "black"))))
  stock$imp_formatters$cc2 <- 
    formatter("span", 
              style = x ~ style(
                font.weight = "bold", 
                color = ifelse(x > dtt1$`NLP Signal price`, "#7EDB76",
                               ifelse(x < dtt1$`NLP Signal price`, "#F94A2E", 
                                      "black"))))
  
  tst3 <- tst2 %>% 
    left_join(tmp_clean %>% 
                rename(date = Date) %>% 
                dplyr::select(date, Time, url)) %>% 
    left_join(data$clean %>% dplyr::select(id_document, url)) %>% 
    dplyr::select(id_document, Signal, price_change_perc) %>% 
    mutate(Signal = as.numeric(as.character(Signal))) %>% 
    mutate(validation = ifelse(Signal*price_change_perc > 0, 1, 
                               ifelse(Signal*price_change_perc < 0, -1, 0))) %>% 
    dplyr::select(-price_change_perc) %>% 
    mutate(stock = stock$descriptions$name) %>% 
    rename(id = id_document,
           real_trend = validation,
           predicted_trend = Signal)
  
  return(list(validation = validation,
              data = data,
              stock = stock,
              stt = stt,
              tst2 = tst2,
              tst3 = tst3,
              tmp_clean = tmp_clean,
              dt1 = dt1,
              predicted_price_change = predicted_price_change,
              tmp_sp3 = tmp_sp3,
              xgbData = xgbData,
              dt2 = dt2,
              scoring_prob = scoring_prob,
              scores_fix_p = scores_fix_p,
              tst = tst,
              tmp_formattable = tmp_formattable, 
              tmp002 = tmp002))
  
}

saveValidation <- function(tst3 = validatedMamut$tst3,
                           stock){
  
  val_dir <- paste0(str_remove_all(getwd(), "/src/R"), "/validation_data/")
  message(paste("Directory:", val_dir))
  sink(paste(val_dir, stock$descriptions$name, "_", Sys.Date(), ".txt", sep = ""))
  tst3 %>% 
    distinct() %>% 
    jsonlite::toJSON(pretty = T) %>% 
    cat() 
  sink()
  message(paste(stock$descriptions$name, "validated data saved to a file."))
}

