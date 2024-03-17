
library(tidyverse)
library(janitor)
library(lubridate)
library(rvest)

# source : https://minfin.gov.ru/ru/perfomance/public_debt/internal/operations/ofz/auction

rm(list = ls())

auction_file = "OFZ_auction-full_tbl.csv"

ofz_url = "https://minfin.gov.ru/ru/perfomance/public_debt/internal/operations/ofz/auction?id_66=305849-rezultaty_provedennykh_auktsionov_po_razmeshcheniyu_gosudarstvennykh_tsennykh_bumag_v_2024_godu_na_07.03.2024"
  # "https://minfin.gov.ru/ru/perfomance/public_debt/internal/operations/ofz/auction?id_66=129477-rezultaty_provedennykh_auktsionov_po_razmeshcheniyu_gosudarstvennykh_tsennykh_bumag_v_2019_godu"
  # "https://minfin.gov.ru/ru/perfomance/public_debt/internal/operations/ofz/auction?id_66=129477-rezultaty_provedennykh_auktsionov_po_razmeshcheniyu_gosudarstvennykh_tsennykh_bumag_v_2019_godu"
  # "https://minfin.gov.ru/ru/perfomance/public_debt/internal/operations/ofz/auction?id_66=126516-rezultaty_provedennykh_auktsionov_po_razmeshcheniyu_gosudarstvennykh_tsennykh_bumag_v_2020_godu"
  # "https://minfin.gov.ru/ru/perfomance/public_debt/internal/operations/ofz/auction?id_66=135507-rezultaty_provedennykh_auktsionov_po_razmeshcheniyu_gosudarstvennykh_tsennykh_bumag_v_2021_godu"
  # "https://minfin.gov.ru/ru/perfomance/public_debt/internal/operations/ofz/auction?id_66=301249-rezultaty_provedennykh_auktsionov_po_razmeshcheniyu_gosudarstvennykh_tsennykh_bumag_v_2022_godu"
  # "https://minfin.gov.ru/ru/perfomance/public_debt/internal/operations/ofz/auction?id_66=132380-rezultaty_provedennykh_auktsionov_po_razmeshcheniyu_gosudarstvennykh_tsennykh_bumag_v_2023_godu"

ofz_html = read_html(ofz_url)

ofz_tbl = ofz_html  %>% 
  html_elements("table") %>%   
  html_table() %>% 
  .[[1]] 


colz_wt_names = ofz_tbl %>% names() %>% .[.!=""]

ofz_tbl = ofz_tbl %>% 
  select(colz_wt_names)

if(ncol(ofz_tbl)==15){
new_col_names <- c(
  "AUCTION DATE",
  "FORMAT",
  "RELEASE CODE",	
  "PAPER TYPE",
  "MATURITY DATE",
  "DAYS UNTIL MATURITY",
  "SUPPLY VOLUME (MILLION RUBLES)",	
  "CUT-OFF PRICE (% OF FACE VALUE)"	,
  "WEIGHTED AVERAGE PRICE (% OF FACE VALUE)",
  "YIELD AT CUT-OFF PRICE (% PER ANNUM)",
  "PROFITABILITY AT WEIGHTED AVERAGE PRICE (% PER ANNUM)",
  "TOTAL DEMAND AT PAR (MILLION RUBLES)",
  "PLACEMENT VOLUME AT PAR (MILLION RUBLES)",
  "REVENUE VOLUME (MILLION RUBLES)",
  "DEMAND SATISFACTION RATE AT AUCTION (12/11)"
) %>% tolower() 

temp_ofz_tbl <- 
  ofz_tbl %>% 
  slice(3:n()) %>% 
  `colnames<-`(new_col_names) %>% 
  janitor::clean_names() %>% 
  mutate(url = ofz_url) %>% 
  mutate_at( c(6:15) , funs(as.numeric(str_remove(., "\\s+"))) ) %>% 
  mutate_at(c(1,5), funs(as.Date(., "%Y-%m-%d"))) %>% 
  select(!"format")}

if(ncol(ofz_tbl)==14){
  new_col_names <- c(
    "AUCTION DATE",
    "RELEASE CODE",	
    "PAPER TYPE",
    "MATURITY DATE",
    "DAYS UNTIL MATURITY",
    "SUPPLY VOLUME (MILLION RUBLES)",	
    "CUT-OFF PRICE (% OF FACE VALUE)"	,
    "WEIGHTED AVERAGE PRICE (% OF FACE VALUE)",
    "YIELD AT CUT-OFF PRICE (% PER ANNUM)",
    "PROFITABILITY AT WEIGHTED AVERAGE PRICE (% PER ANNUM)",
    "TOTAL DEMAND AT PAR (MILLION RUBLES)",
    "PLACEMENT VOLUME AT PAR (MILLION RUBLES)",
    "REVENUE VOLUME (MILLION RUBLES)",
    "DEMAND SATISFACTION RATE AT AUCTION (12/11)"
  ) %>% tolower() 
  
  temp_ofz_tbl <- 
    ofz_tbl %>% 
    slice(3:n()) %>% 
    `colnames<-`(new_col_names) %>% 
    janitor::clean_names() %>% 
    mutate(url = ofz_url) %>% 
    mutate_at( c(5:14) , funs(as.numeric(str_remove(., "\\s+"))) ) %>% 
    mutate_at(c(1,4), funs(as.Date(., "%Y-%m-%d")))}
  
if (file.exists(auction_file)){
  old_auction_df <- read_csv(auction_file)
}else{
  old_auction_df <- temp_ofz_tbl
}


auction_df <- old_auction_df %>% 
  bind_rows(temp_ofz_tbl) %>% 
  distinct(url, release_code, maturity_date, paper_type, supply_volume_million_rubles, .keep_all = TRUE)

auction_df %>% write_csv(auction_file)
