library(traits)
library(ggplot2)
library(lubridate)
library(dplyr)
library(knitr)

# do a query of BetyDB using Curtis' key

options(betydb_url = "https://terraref.ncsa.illinois.edu/bety/",
        betydb_api_version = 'beta',
        betydb_key = 'ejz7CFTOPT4kDAa5jDEiKD9ieajajNDzGZkLiFsd')


# get all of season 4 data 
# This query make take several hours
season_4 <- betydb_query(sitename  = "~Season 4",
                         limit     =  "none")
#season_6 <- betydb_query(sitename  = "~Season 6",
#                         limit     =  "none")

season_4_date <- season_4 %>%
  mutate(trans_date = with_tz(ymd_hms(raw_date), "America/Phoenix"))
#season_6_date <- season_6 %>%
#  mutate(trans_date = with_tz(ymd_hms(raw_date), "America/Phoenix"))

s4traits <- season_4 %>%
  distinct(trait, trait_description)
#kable(s4traits)

# convert to a wide-format dataframe; I don't know if this actually works very well
s4wide <- spread(season_4_date,trait,mean)
