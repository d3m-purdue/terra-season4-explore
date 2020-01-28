library(tidyverse)

# Convert sitename such as "MAC Field Scanner Season 4 Range 3 Column 3"
# into a data frame with columns "range" and "column"
fix_sitename <- function(x) {
  tibble(
    range = as.integer(gsub(".*Range ([0-9]+).*", "\\1", x)),
    column = as.integer(gsub(".*Column ([0-9]+)", "\\1", x)))
}

# Weather data
f <- "data/raw/terra_tabular/S4 Daily UA-MAC AZMET Weather Station.csv"
weather <- read_csv(f)

# Read all data files
ff <- list.files("data/raw/terra_tabular", full.names = TRUE,
  pattern = "_formatted")

dd <- map(ff, function(f) {
  d <- read_csv(f)
  bind_cols(fix_sitename(d$sitename), d) %>%
    select(-sitename)
})

# Some exploration...

# What are the unique days in the data?
days <- map(dd, ~ select(., day)) %>%
  bind_rows() %>%
  distinct() %>%
  arrange(day)
# There are 75 unique days, ranging from 12 to 133
# How do these map to the days in the weather data????

# Count number of distinct cultivar/range/column/day combinations
map(dd, ~ select(., cultivar, range, column, day) %>%
  distinct() %>%
  nrow())

# Count number of distinct cultivar/range/column/day/value combinations
map(dd, ~ select(., everything()) %>%
  distinct() %>%
  nrow())

# There are duplicates, but the variable is always the same
# value within each cultivar, range, column, day combination

# For a given dataset, collapse to one measurement per
# range, column, day, and cultivar. But count the number of
# measures for each combination and add that as a variable
prepare_data <- function(x) {
  nm <- setdiff(names(x), c("range", "column", "day", "cultivar"))
  xn <- x %>%
    group_by_all() %>%
    tally() %>%
    rename(!!paste0(nm, "_n") := n)

  nr1 <- xn %>%
    select(cultivar, range, column, day) %>%
    distinct() %>%
    nrow()
  nr2 <- nrow(xn)
  if (nr1 != nr2)
    stop("non-unique values")

  xn
}

# Prepare final dataset
# First, make data frame of all possible range/column/cultivar/day
dat <- map(dd, ~ select(., cultivar, range, column, day)) %>%
  bind_rows() %>%
  distinct() %>%
  arrange(cultivar, range, column, day)

# Now join all the datasets to this
dat <- left_join(dat, prepare_data(dd[[1]]))
dat <- left_join(dat, prepare_data(dd[[2]]))
dat <- left_join(dat, prepare_data(dd[[3]]))
dat <- left_join(dat, prepare_data(dd[[4]]))
dat <- left_join(dat, prepare_data(dd[[5]]))

saveRDS(dat, file = "output/s4_data.rds")








# df of all possible range/column/cultivar combinations
cultivars <- read_csv("data/raw/terra_tabular/cultivars_s4_2017.csv")
cultivars <- bind_cols(fix_sitename(cultivars$sitename), cultivars) %>%
  select(-sitename)

map(dd, ~ select(., cultivar, range, column, day)) %>%
  bind_rows() %>%
  distinct()

# There are cases where cultivar is NA in the data
map(dd, ~ colSums(is.na(.)))
map(dd, nrow)

dd[[1]] %>%
  group_by(cultivar, range, column, day) %>%
  tally() %>%
  arrange(-n) %>%
  filter(is.na(cultivar))

filter(dd[[1]], range == 2 & column == 2 & day == 12)
