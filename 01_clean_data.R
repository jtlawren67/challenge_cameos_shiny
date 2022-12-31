library(tidyverse)


cameo <- readRDS('./data/ChallengeCameos.RDS') 

head(cameo)

#Data Cleaning
#1) Turn Price into a Numberic
#2) Get Rid of Any Rows where Price is NA (2/20 - 3/07 gets dropped because I didn't notice some stuff)
#2) Fix the Time when there was a sale (11-21)
#4) Fix when There was a change in the field to daying $XX+ after 3/20
#3) Drop the Time Field since its either 24hrs or Missing for Some People


unique(cameo$time)

cameo[is.na(cameo$time), ]
cameo[(cameo$time==""), ]

count(cameo, ds, name, sort = T) %>% 
  filter(n > 1) %>% 
  View()

cameo %>% filter(ds == '2022-03-07') %>% View()

cameo %>% filter(is.na(price)) %>% View()

cameo %>% filter(str_detect(price, '\\$\\d+$', negate = T)) %>% View()


cameo2 <- cameo %>% 
  select(-time) %>%
  filter(!is.na(price)) %>% 
  mutate(n = str_count(price),
    newprice = if_else(str_length(price) > 5,
                            as.numeric(str_extract(str_trim(price), '\\d+$')),
                            parse_number(price))
         ) %>%
  mutate(
    #Fix Some Name Collisions
    name = if_else(name == 'Paul Calafiore', 'Paulie Calafiore', name),
    name = if_else(name == 'Syrus From MTV', 'Syrus The Challenge AllStars', name),
    
  )

saveRDS(cameo2, file = "./data/cameo_clean.RDS")
