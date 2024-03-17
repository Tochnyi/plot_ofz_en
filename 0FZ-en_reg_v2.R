library(tidyverse)
library(janitor)
library(lubridate)
library(tsibble)
library(fable)
library(ggplot2)
library(ggstream)
library(showtext)
library(ggtext)
library(png)


rm(list = ls())


### GETTING THE FONTS AND COLORS
font <- "Josefin Sans"
font2 <- "Open Sans"

# Use the font_add_google() function to load fonts from the web
font_add_google(family=font, font, db_cache = FALSE)
font_add_google(family=font2, font2, db_cache = FALSE)

sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = "Font Awesome 6 Brands-Regular-400.otf")
showtext::showtext_auto()


theme_set(theme_minimal(base_family = font2, base_size = 3))

bg <- "white"
txt_col <- "black"

showtext_auto(enable = TRUE)

ofz_init_df <- readr::read_csv("OFZ_auction.csv")

ofz_init_df %>% 
  filter( type_of_placement =="OFZ.IN") %>% View()


# Prep TOSCHNYI markings
twitter_icon <- "&#xf099"
twitter_username <- "@tochnyi"

social_caption <- glue::glue("**Data:** minfin.gov.ru<br>",
                             "<span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
  <span style='color: #000000'>{twitter_username}</span>"
)


long_logo <- readPNG(paste0(getwd(),'/Tochnyi_Logo_copy.png'))



Sys.setlocale(,"en_US")


new_col_names <- c(
  "auction_date",
  "type",
  "days_to_maturity",
  "yield",
  "demand_in_mil_rub") 

auction_file = "OFZ_auction-full_tbl.csv"


auction_df = readr::read_csv(auction_file)

ozf_types = auction_df$paper_type %>% unique() %>% .[c(1, 2, 4)]

new_auction_df <-
auction_df %>% 
  filter(!is.na(auction_date)) %>% 
  select( auction_date, paper_type, days_until_maturity, yield_at_cut_off_price_percent_per_annum, total_demand_at_par_million_rubles ) %>% 
  filter(!is.na(yield_at_cut_off_price_percent_per_annum))  %>% 
  `colnames<-`(new_col_names) %>% 
  arrange(auction_date) %>% 
  filter(type %in% ozf_types) %>% 
  filter( yield > 0) %>% 
  mutate( month = auction_date %>% format("%Y %b") %>% yearmonth() ) %>% 
  select(!auction_date) %>% 
  group_by(month, type ) %>% 
  mutate( sum_demand = sum(demand_in_mil_rub)  ) %>% 
  ungroup %>% 
  mutate( sub_yield = yield*(demand_in_mil_rub/sum_demand),
          sub_mat = days_to_maturity*(demand_in_mil_rub/sum_demand)) %>% 
  group_by(month, type ) %>% 
  summarise( yield = sum(sub_yield),
             maturity = sum(sub_mat),
             demand = max(sum_demand)) %>% 
  ungroup %>% 
  as_tsibble(key = type)

auction_lm <- new_auction_df %>%
  model(TSLM(yield ~ maturity + demand )) %>% fitted()

auction_lm %>% 
  filter( type == ozf_types[1]) %>%
  as_tibble() %>% 
  select(month, .fitted) %>% 
  ggplot(aes(x=month, y=.fitted, group=1)) +
  geom_line() +
  ggtitle(glue::glue("Yield of\nRussia's bond issues (OFZ_PD)") )+
  coord_cartesian(clip = "off") +
  xlab("") +
  ylab("%") +
  scale_y_continuous(breaks = seq(6, 12, by = 1)) +
  theme(
    plot.title = element_text(size=20, 
                              face="bold",
                              family=font, vjust = 1,
                              hjust = 0.5, lineheight=1.5,
                              margin= margin(0,0,30,0)),
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "lightgrey"),
    axis.line.x = element_line(linewidth = .75),
    axis.line.y = element_line(linewidth = .75),
    panel.grid = element_blank(),
    axis.title.y = element_text(size = 10),
    axis.text.y= element_text(color=txt_col, size=10),
    axis.text.x = element_text(color=txt_col, size=10,margin = margin(5,0,0,0)),
    plot.margin = margin(20,30,10,30),
    legend.position = "right",
    legend.title = element_text(size=10),
    legend.text = element_text(size=7),
    legend.background = element_rect(fill = "lightgray"),
    legend.box.margin = margin(0, 0, 0, 20),
    plot.caption = element_markdown(hjust=0, margin=margin(10,0,0,0), size=8, color=txt_col, lineheight = 1.2)
  )


