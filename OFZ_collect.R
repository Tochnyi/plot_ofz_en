library(tidyverse)

# Copy announcement from the FinMin in the clipboard

clipbrd <-read.delim(pipe("pbpaste"))

date_of_auction <-  clipbrd %>% 
    names() %>% 
    str_extract("(?<=auction.on).{20}") %>% 
    str_split( "\\.") %>% 
    .[[1]] %>% 
    .[.!=""] %>% 
    .[1:3] %>% 
    glue::glue_collapse(" ") %>%  
    lubridate::mdy()

type_of_placement <- clipbrd %>% 
  names() %>% 
  str_extract("(?<=for.the.placement.of.).{6}") 
  
maturity_date <-   clipbrd %>% 
  names() %>% 
  str_extract("(?<=a.maturity.date.of.).{1,}") %>% 
  str_split( "\\.") %>% 
  .[[1]] %>% 
  .[.!=""] %>% 
  .[1:3] %>% 
  glue::glue_collapse(" ") %>%  
  lubridate::mdy()

placement_of_issue_nb <- clipbrd[1,1]  %>% str_remove_all(":")  %>% str_split(" ") %>% .[[1]] %>% nth(-1)

volume_of_demand <- clipbrd[3,1] %>% str_extract_all("([:digit:]|\\.){1,}| billion| million") %>% .[[1]] %>%  glue::glue_collapse( .)

placed_issue_volume <- clipbrd[4,1]  %>% str_extract_all("([:digit:]|\\.){1,}| billion| million") %>% .[[1]] %>%  glue::glue_collapse( .)

proceeds_from_the_placement <- clipbrd[5,1]  %>% str_extract_all("([:digit:]|\\.){1,}| billion| million") %>% .[[1]] %>%  glue::glue_collapse( .)

cut_off_price <- clipbrd[6,1]  %>% str_extract_all("([:digit:]|\\.){1,}|%") %>% .[[1]] %>%  glue::glue_collapse( .)

yield_at_the_cut_off_price <- clipbrd[7,1]  %>% str_extract_all("([:digit:]|\\.){1,}|%") %>% .[[1]] %>%  glue::glue_collapse( .)

weighted_average_price <- clipbrd[8,1]  %>% str_extract_all("([:digit:]|\\.){1,}|%") %>% .[[1]] %>%  glue::glue_collapse( .)

weighted_average_yield  <- clipbrd[9,1]  %>% str_extract_all("([:digit:]|\\.){1,}|%") %>% .[[1]] %>%  glue::glue_collapse( .)

temp_auction_df <- tibble(date_of_auction, type_of_placement, maturity_date, placement_of_issue_nb, volume_of_demand, placed_issue_volume , proceeds_from_the_placement, cut_off_price, yield_at_the_cut_off_price,
       weighted_average_price, weighted_average_yield)

# auction_df <- temp_auction_df
auction_df <- auction_df %>% bind_rows(temp_auction_df) %>% distinct(placement_of_issue_nb,date_of_auction,maturity_date, .keep_all = TRUE)

auction_df %>% write_csv("OFZ_auction.csv")


