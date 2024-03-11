library(tidyverse)
library(ggplot2)
library(ggstream)
library(showtext)
library(ggtext)
library(png)

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



Date_of_maturity <- c("2033-03-23","2031-09-17","2029-05-23","2026-09-16","2027-02-03","2022-12-7")
Price <- c(75.195,81.60, 79.60,90.01, 89.8, 99.98)
tibble(Date_of_maturity, Price) %>% 
  ggplot(aes(x=Date_of_maturity, y=Price, group=1)) +
  geom_line()

ofz_init_df <- readr::read_csv("/Users/ulyssekhatinskyi/Documents/TOCHNY/Research/OFZ/OFZ_auction.csv")

ofz_init_df %>% 
  filter( type_of_placement =="OFZ.IN") %>% View()
  
type_i = "OFZ.PD"

# Prep TOSCHNYI markings
twitter_icon <- "&#xf099"
twitter_username <- "@tochnyi"

social_caption <- glue::glue("**Data:** minfin.gov.ru<br>",
                             "<span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
  <span style='color: #000000'>{twitter_username}</span>"
)


long_logo <- readPNG(paste0(getwd(),'/Tochnyi_Logo_copy.png'))


# Maturity
ofz_init_df %>% 
  filter( type_of_placement ==type_i) %>% 
  rename("Proceeds" = "proceeds_from_the_placement") %>% 
  mutate(maturity = as.numeric(interval(date_of_auction, maturity_date) %/% years()),
         Proceeds = Proceeds %>% str_remove_all(" billion") %>% str_extract("([:digit:]{1,2}\\.[:digit:]{1,2})") %>% as.numeric())   %>% 
  ggplot(aes(x=date_of_auction, y=maturity, group=1)) +
  geom_point(aes(size=Proceeds)) +
  geom_smooth(se = FALSE) +
  ggtitle("Maturity and volume of\nRussia's bond issues (OFZ-PD)") +
  coord_cartesian(clip = "off") +
  xlab("") +
  ylab("Years") +
  scale_y_continuous(breaks = seq(2, 19, by = 2)) +
  labs(caption = social_caption) +
  scale_size_continuous(name = "Proceeds\n(in RUB bn)")+
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
  ) + 
  annotation_raster(long_logo, ymin = 19.8, ymax= 21.6, xmin = ymd(20221001),xmax = ymd(20221231))




ofz_init_df %>% 
  filter( type_of_placement ==type_i) %>% 
  View

# Average yield
ofz_init_df %>% 
  filter( type_of_placement ==type_i) %>% 
  mutate(weighted_average_yield_num = weighted_average_yield %>% str_remove_all("%") %>% str_extract("([:digit:]{1,2}\\.[:digit:]{1,2})") %>% as.numeric())   %>%
  ggplot(aes(x=date_of_auction, y=weighted_average_yield_num, group=1)) +
  geom_line()


ofz_init_df %>% 
  filter( type_of_placement ==type_i) %>% 
  mutate(weighted_average_yield_num = weighted_average_yield %>% str_remove_all("%") %>% str_extract("([:digit:]{1,2}\\.[:digit:]{1,2})") %>% as.numeric())   %>%
  max(.$weighted_average_yield_num)

# Volumes of demand
ofz_init_df %>% 
  filter( type_of_placement ==type_i) %>% 
  mutate(volume_of_demand = volume_of_demand %>% str_remove_all(" billion") %>% str_extract("([:digit:]{1,2}\\.[:digit:]{1,2})") %>% as.numeric(),
         month_of_auction = date_of_auction %>% lubridate::floor_date(unit = "months")
         )   %>% 
  group_by(month_of_auction) %>% 
  summarise( monthy_demand = sum(volume_of_demand)) %>% 
  ungroup() %>% 
  filter( month_of_auction != lubridate::today() %>% lubridate::floor_date(unit = "months")) %>% 
  ggplot(aes(x=month_of_auction, y=monthy_demand, group=1)) +
  geom_line()

# ofz_init_df %>% 
#   mutate(volume_of_demand = volume_of_demand %>% str_remove_all(" billion") %>% str_extract("([:digit:]{1,2}\\.[:digit:]{1,2})") %>% as.numeric(),
#          month_of_auction = date_of_auction %>% lubridate::floor_date(unit = "months"))   %>% 
#   group_by(month_of_auction) %>% 
#   summarise( monthy_demand = sum(volume_of_demand)) %>% 
#   ungroup() %>% 
#   filter( month_of_auction != lubridate::today() %>% lubridate::floor_date(unit = "months")) %>% 
#   ggplot(aes(x=month_of_auction, y=monthy_demand, group=type_of_placement)) +
#   geom_line()



# Number of auctions
ofz_init_df %>% 
  filter( type_of_placement ==type_i) %>% 
  mutate( month_of_auction = date_of_auction %>% lubridate::floor_date(unit = "months"))   %>% 
  group_by(month_of_auction) %>% 
  tally() %>% 
  ungroup() %>% 
  rename( monthy_auctions = n) %>% 
  filter( month_of_auction != lubridate::today() %>% lubridate::floor_date(unit = "months")) %>% 
  ggplot(aes(x=month_of_auction, y=monthy_auctions, group=1)) +
  geom_line()


