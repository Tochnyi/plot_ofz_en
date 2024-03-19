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


Sys.setlocale(,"en_US")

# Prep TOSCHNYI markings
twitter_icon <- "&#xf099"
twitter_username <- "@tochnyi"

social_caption <- glue::glue("<b>Data:</b> minfin.gov.ru + tradingeconomics.com<br>",
                             "<span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
  <span style='color: #000000'>{twitter_username}</span>"
)


# https://tradingeconomics.com/russia/consumer-price-index-cpi

cpi_df = readxl::read_xlsx("russia consumer price index_2018_2024.xlsx")

clean_cpi_df <- 
  cpi_df %>% 
  mutate(month = month %>% yearmonth(), 
         inflation = (cpi-lag(cpi, 12))/lag(cpi, 12)*100 ) %>% 
  select(!cpi) %>% 
  filter( !inflation %>%is.na() ) %>% 
  as_tsibble()


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


# auction_lm <- 
#   auction_lm %>% 
#   annotate("pointrange", x = yearmonth('202306'), y = 11, ymin = 11, ymax = 11,colour = "red", size = 2.5 ) +

# annotate("rect", xmin = 3, xmax = 4.2, ymin = 12, ymax = 21, alpha = .2)
# for the geom_area

auction_lm <- new_auction_df %>%
  inner_join(clean_cpi_df) %>% 
  model(TSLM(yield ~ maturity + demand + inflation)) %>% fitted()


#. https://stackoverflow.com/questions/71306740/set-upper-and-lower-limits-for-geom-area-in-ggplot2

plot <-
  auction_lm %>% 
  filter( type == ozf_types[1]) %>%
  as_tibble() %>% 
  select(month, .fitted, type) %>% 
  ggplot( aes(x=month, 
              y=.fitted, 
              group = type, 
              colour= type, 
              size = type, 
              linetype = type) ) +
  geom_area( fill = "#ec5454") + # #FFCC66 
  ggtitle(glue::glue("From the inside looking in"), subtitle = "Yield of Russian fixed-rate domestic goverment bonds\nnormalised for inflation, volume and maturity, in %" ) +
  coord_cartesian(ylim = c(5.6,11.3)) + #   # coord_cartesian(clip = "off") +
  scale_color_manual(values=c('#ec5454', 'darkorange')) + # 
  scale_size_manual(values = c(0.5, 0.5)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  xlab("") +
  ylab("") + #Computed yield (in %)
  scale_x_yearmonth( date_breaks = "1 year", date_labels = "%Y", expand =  c(0,0) ) +
  scale_y_continuous(breaks = seq(2, 15, by = 1),   position = "right", expand =  c(0,0) ) +
  labs(caption =   labs(caption = social_caption) ) +
  theme(
    
    #----PLOT
    plot.title = element_text(size=36, 
                              face="bold",
                              family=font, 
                              vjust = 1,
                              hjust = 0, lineheight=1.5,
                              margin= margin(0,0,10,0)),
    plot.subtitle = element_text(size=13, 
                                 face="italic",
                                 family=font, 
                                 vjust = 1,
                                 hjust = 0, lineheight=1,
                                 margin= margin(0,0,10,0)),
    plot.background = element_rect(fill = "#fffdaf"), ##ffe28a. #FFCC66
    plot.margin = margin(20,30,10,30),
    
    #-----PANEL
    panel.background = element_rect(fill = "#fffdaf",
                                    colour = "#fffdaf",
                                    size = 0.5, 
                                    linetype = "solid"),
    panel.grid.major.y = element_line(size = 0.85, 
                                      linetype = 'dotted',
                                      colour = "darkgrey"), 
    # panel.grid.minor = element_line(size = 0.25, linetype = 'dotted',
    #                                 colour = "lightgrey"),
    panel.grid = element_blank(),
    
    #-----AXIS
    axis.line.x = element_line(linewidth = .75),
    axis.line.y = element_line(linewidth = 0),
    axis.title.y = element_text(size = 10),
    axis.text.y= element_text(color=txt_col, size=10),
    axis.text.x = element_text(color=txt_col, size=10, margin = margin(5,0,0,0)),
    
    axis.text.y.right   = element_text(margin = margin(l = 0.15, unit = "cm")),
    
    
    #-----LEGEND
    legend.position = "none",
    legend.title = element_text(size=10),
    legend.text = element_text(size=7),
    legend.background = element_rect(fill = "lightgray"),
    legend.box.margin = margin(0, 0, 0, 20),
    plot.caption = element_textbox_simple(hjust=0, margin=margin(5,0,0,0), size=8, color=txt_col, lineheight = 1.2)
  )


plot 


short_logo <- readPNG(paste0(getwd(),'/Copy_of_Logo-Trans.png'))

plot2 = plot + 
  annotation_raster(short_logo, ymin = 5.8, ymax=6.15 , xmin = ymd(20230901),xmax = ymd(20231201))

plot2


y_out = lubridate::today() %>% year() %>% as.character()

output = paste0(getwd(),"plot_liquid_nfw_2109_to", y_out ,".pdf")

ggsave(filename = output,
       plot = plot2,
       height = 14,
       width = 14,
       units = "cm")

