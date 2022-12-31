library(rvest)
library(tidyverse)
library(RSelenium)

setwd('~/data')

rD <- rsDriver(browser = "firefox", 
               port = 6768L, 
               #If Running RSelenium for the First Time, you can't have check =F
               #since you'll need to download the appropriate drivers
               check = F, 
               
)
remDr <- rD[["client"]]

#remDr$navigate("https://www.cameo.com/search?q=the%20challenge")
remDr$navigate('https://www.cameo.com/browse/reality-tv/mtv/the-challenge')
#If Needing a 2nd Page Run
#remDr$navigate('https://www.cameo.com/browse/reality-tv/mtv/the-challenge?nextToken=40')

Sys.sleep(10)

webElem <- remDr$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end"))

html_obj <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()

##Redoing on 10.20 After Cameo Changed Their Web Page
##Updating again on 12.5.2021

peoples <- html_obj %>% html_elements(css = '.css-1dbjc4n .r-knv0ih')


get_info <- function(x) {
  if(length( x %>% html_elements(css = '.css-1dbjc4n .r-18u37iz')) > 1){
  tibble(
    name = x %>% html_elements(css = '.r-135wba7') %>% html_text(),
    job = x %>% html_elements(css = '.r-6gpygo') %>% html_text(),
    price = x %>% html_elements(css = '.css-1dbjc4n .r-18u37iz') %>% .[[1]] %>% html_text(),
    time =  x %>% html_elements(css = '.css-1dbjc4n .r-18u37iz') %>% .[[2]] %>% html_text(),
    ds = Sys.Date()
  ) 
  } else if(length( x %>% html_elements(css = '.css-1dbjc4n .r-18u37iz')) == 1){
    tibble(
      name = x %>% html_elements(css = '.r-135wba7') %>% html_text(),
      job = x %>% html_elements(css = '.r-6gpygo') %>% html_text(),
      price = x %>% html_elements(css = '.css-1dbjc4n .r-18u37iz') %>% .[[1]] %>% html_text(),
      time =  NA_character_,
      ds = Sys.Date()
    ) 
  }
}

cameo <- map_dfr(peoples, get_info)


###Append RDS File
if(file.exists('ChallengeCameos.RDS')){
  dt <- readRDS('ChallengeCameos.RDS')
  dt <- rbind(dt, cameo) %>% distinct()
  saveRDS(dt, file = "ChallengeCameos.RDS")
} else {
  saveRDS(cameo, file = "ChallengeCameos.RDS")
}



remDr$close()
rD$server$stop()
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)


dt %>%
  group_by(ds) %>% 
  summarize(n = n_distinct(name)) %>%
  View()

##New Additions
newest <- dt %>% distinct(ds) %>% slice_max(ds, n = 1) %>% pull(ds) %>% unique()

next_newest <- dt %>% distinct(ds) %>% slice_max(ds, n = 2) %>% arrange(ds) %>% head(1) %>% pull(ds) 

setdiff(dt %>% filter(ds == newest) %>% pull(name),
        dt %>% filter(ds == next_newest) %>% pull(name)
        )

##Removals
setdiff(dt %>% filter(ds == next_newest) %>% pull(name),
        dt %>% filter(ds == newest) %>% pull(name)
)
