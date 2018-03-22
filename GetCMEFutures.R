# A function to get the electric and gas futures from CME 
# Reference: https://www.r-bloggers.com/using-rvest-to-scrape-an-html-table/

library(rvest)
library(tidyverse)
library(lubridate) 

month.2.number <- function(x) grep(paste("(?i)",x,sep=""),month.abb)


get.futures.table <- function(URL, XPATH, COLUMNS_KEEP, ROWS_DISCARD) {
  # URL describing where the target table lists 
  # XPATH string describing where the table is on the page, reference for hints on how to find this
  # COLUMNS_KEEP vector with columns numbers for the "Month", "Prior Settle", and "Updated"
  # ROWS_DISCARD number of rows at the top that are to be deleted 
  data.list <- URL %>%
    read_html() %>%
    html_nodes(xpath = XPATH) %>%
    html_table(fill = TRUE)
  good.data.frame <- data.list[[1]][ ,COLUMNS_KEEP] 
  last.row <- nrow(good.data.frame)
  good.data.frame <- good.data.frame %>% tail(last.row - ROWS_DISCARD) %>% 
    rename(Prior.Settle = !!names(.[2])) %>% 
    mutate(Prior.Settle = parse_number(Prior.Settle, na = c("", "-", "NA")),
           Updated = ifelse(Updated == "-", NA, Updated),
           Year = as.numeric(substring(Month, 5,8)),
           Month = substring(Month,1,3)) %>% 
    rowwise() %>% 
    mutate(Month = month.2.number(Month)) %>% 
    select(1,4,2,3)
  return(good.data.frame)
}




url.henry.hub.futures <- "http://www.cmegroup.com/trading/energy/natural-gas/natural-gas.html"
xpath.henry.hub.futures  <- '//*[@id="quotesFuturesProductTable1"]'
henry.hub.futures <- get.futures.table(url.henry.hub.futures,
                                       xpath.henry.hub.futures,
                                       c(1,6,12),
                                       1)

url.on.peak  <- "http://www.cmegroup.com/trading/energy/electricity/pjm-pseg-zone-peak-calendar-month-day-ahead-lmp-swap-futures.html"
xpath.on.peak  <- '//*[@id="quotesFuturesProductTable1"]'
pseg.on.peak.futures <- get.futures.table(url.on.peak, 
                                          xpath.on.peak,
                                          c(1,6,12),
                                          2)

url.off.peak <- "http://www.cmegroup.com/trading/energy/electricity/pjm-pseg-zone-off-peak-calendar-month-day-ahead-lmp-swap-futures.html"
xpath.off.peak <- '//*[@id="quotesFuturesProductTable1"]'
pseg.off.peak.futures <- get.futures.table(url.off.peak, 
                                           xpath.off.peak,
                                           c(1,5,11),
                                           2)
