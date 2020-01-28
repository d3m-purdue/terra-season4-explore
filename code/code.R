library(tidyverse)
load("data/raw/season4date.Rdata")
# load("season6date.Rdata")

x <- season_4_date
# x <- season_6_date

range(x$raw_date)

a <- filter(x, trait == "leaf_angle_mean")
length(unique(a$trans_date))
# 66

a <- filter(x, trait == "SPAD_880")
length(unique(a$trans_date))
# 1031

sort(sapply(x, function(a) length(unique(a))))

unique(x$result_type)
# "traits"
unique(x$city)
# "Maricopa"
unique(x$scientificname)
# "Sorghum bicolor"
unique(x$commonname)
# "sorghum"
unique(x$genus)
# "sorghum"
unique(x$species_id)
# 2588
unique(x$citation_id)
# 6000000008 6000000011         NA
unique(x$author)
# "Zongyang, Li"   "Newcomb, Maria" NA
unique(x$citation_year)
# 2016 2017   NA
unique(x$treatment)
# NA "BAP 2017, water-deficit stress Aug 1-14"
# "BAP 2017, water-deficit stress Aug 15-30"
unique(x$treatment_id) # redundant

x %>%
  filter(is.na(treatment_id)) %>%
  pull(trait) %>%
  unique()
# "canopy_height" "panicle_count" "panicle_volume" "panicle_surface_area"
# "surface_temperature" "leaf_angle_alpha" "leaf_angle_beta" "leaf_angle_chi"
# "leaf_angle_mean"

unique(x$year)
# 2017
unique(x$dateloc)
# "5.0"
unique(x$n)
# NA
unique(x$statname)
# ""
unique(x$stat)
# NA
unique(x$access_level)
# 2 4
head(unique(x$notes)) # keep
head(unique(x$entity)) # keep
head(x$view_url) # keep
head(x$edit_url) # redundant

head(x$trans_date)
head(x$date) # redundant
head(x$time) # redundant

# month is redundant...
unique(x$month)
tmp <- format(x$trans_date, "%m")
table(as.integer(tmp) == x$month)

traits <- select(x, trait, trait_description) %>% distinct()
treatments <- select(x, treatment_id, treatment) %>% distinct()
cultivars <- select(x, cultivar, cultivar_id) %>% distinct()
sites <- select(x, site_id, sitename, lat, lon) %>% distinct()

filter(sites,
  grepl("MAC Field Scanner Season 4 Range 36 Column 2", sitename))$lat
filter(sites,
  grepl("MAC Field Scanner Season 4 Range 36 Column 2", sitename))$lon
# There are many sites that have the same sitename except for an
# extra "E" or "W" at the end of the name, with same lat/lon

# these are the variables that have E/W measurements
filter(xx, grepl(" (E|W)$", sitename)) %>%
  pull(trait) %>%
  unique()

# all sites start the same
table(substr(sites$sitename, 1, 26))
tmp <- gsub("MAC Field Scanner Season 4 ", "", sites$sitename)
sites$range <- as.integer(gsub("Range ([0-9]+).*", "\\1", tmp))
sites$column <- as.integer(gsub(".*Column ([0-9]+).*", "\\1", tmp))
sites$dir <- trimws(gsub(".*Column [0-9]+(.*)", "\\1", tmp))

# size: 5.852761 11.226994
sites2 <- sites %>%
  mutate(size = ifelse(dir == "", 4, 2)) %>%
  arrange(dir)
ggplot(sites2, aes(lon, lat, color = dir)) +
  geom_point(alpha = 0.9, size = sites2$size) +
  ggthemes::scale_color_tableau(palette = "Tableau 10",
    name = "site suffix") +
  theme_minimal() +
  coord_fixed(ratio = 0.3620315)

xx <- mutate(xx, sitename2 = gsub(" (E|W)$", "", sitename))

# can we remove these if they are otherwise duplicates?

# view_url is just "https://terraref.ncsa.illinois.edu/bety/traits/{id}"

xx <- select(x, -checked, -result_type, -city, -scientificname,
  -commonname, -genus, -species_id, -author, -citation_year, -year,
  -dateloc, -n, -statname, -stat, -access_level, -month, -edit_url,
  -date, -time, -citation_id, -treatment, -trait_description,
  -cultivar_id, -raw_date, -view_url, -site_id)

# there are duplicates
xx %>%
  group_by(id) %>%
  tally() %>%
  filter(n > 1) %>%
  arrange(-n)

filter(xx, id == 6001938532)
filter(xx, id == 6001938532) %>% distinct()

xx <- distinct(xx)

# even still there are duplicates
xx %>%
  mutate(date = as.Date(trans_date)) %>%
  filter(sitename == "MAC Field Scanner Season 4 Range 10 Column 1" &
    date == "2017-07-18" &
    trait == "surface_temperature") %>%
  distinct()

xx %>%
  mutate(date = as.Date(trans_date)) %>%
  filter(sitename == "MAC Field Scanner Season 4 Range 10 Column 1" &
    date == "2017-07-18" &
    trait == "surface_temperature") %>%
  select(-id) %>%
  distinct()

# remove them
xx <- xx %>%
  group_by_at(setdiff(names(xx), "id")) %>%
  filter(row_number() == 1) %>%
  ungroup()


# s4 <- pivot_wider(xx,
#   names_from = "trait", values_from = c("mean", "units"))
# which(sapply(s4$mean_canopy_height, length) == 4)
# s4[83408, ]

trait_site_freq <- xx %>%
  group_by(trait, sitename, lat, lon) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(trait) %>%
  mutate(
    pct = n / sum(n),
    npoints = n()) %>%
  ungroup() %>%
  mutate(trait2 = fct_reorder(trait, npoints, mean))

tmp <- trait_site_freq %>%
  group_by(trait) %>%
  summarise(n = n())

filter(trait_site_freq, trait2 %in% levels(trait2)[1:39]) %>%
ggplot(aes(lon, lat, size = pct)) +
  geom_point(alpha = 0.7) +
  theme_bw() +
  scale_size_continuous(guide = FALSE) +
  facet_wrap(~ trait2, nrow = 3, ncol = 13) +
  coord_fixed(ratio = 0.3620315) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

filter(trait_site_freq, trait2 %in% levels(trait2)[40:77]) %>%
ggplot(aes(lon, lat, size = pct)) +
  geom_point(alpha = 0.7) +
  theme_bw() +
  scale_size_continuous(guide = FALSE) +
  facet_wrap(~ trait2, nrow = 3, ncol = 13) +
  coord_fixed(ratio = 0.3620315) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

filter(trait_site_freq, trait2 %in% tail(levels(trait2), 10)) %>%
ggplot(aes(lon, lat)) +
  geom_point(alpha = 0.7) +
  theme_bw() +
  scale_size_continuous(guide = FALSE) +
  facet_wrap(~ trait2, nrow = 2, ncol = 5) +
  coord_fixed(ratio = 0.3620315) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())

# 1:44 have visually identical grid sampling
# 45:52 have similar
# 53:58 have similar
# 59:62 have similar
# 63:64 have similar
# 65:74 have similar
# 75:76 have similar
# 77
trait_grid_types <- tibble(
  trait = levels(trait_site_freq$trait2),
  type = c(rep(1, 44), rep(2, 8), rep(3, 6), rep(4, 4),
    rep(5, 2), rep(6, 10), rep(7, 2), 7)
)

summ <- xx %>%
  group_by(trait) %>%
  summarise(
    start_date = min(trans_date),
    end_date = max(trans_date),
    n = length(unique(trans_date))
  ) %>%
  arrange(end_date) %>%
  mutate(
    color = as.character(start_date),
    trait = fct_reorder2(trait, start_date, end_date, function(x, y) {
      xx <- as.numeric(min(x))
      yy <- as.numeric(max(y))
      xx^2 + yy - xx
    })
  )

trait_grid_types$trait <- factor(trait_grid_types$trait,
  levels = levels(summ$trait))
summ <- left_join(summ, trait_grid_types, by = "trait")
summ$type2 <- paste0("(grid type ", summ$type, ")")

ggplot(summ, aes(x = trait,
  ymin = start_date, ymax = end_date, color = color)) +
  geom_linerange(size = 1) +
  geom_point(aes(y = start_date)) +
  geom_point(aes(y = end_date)) +
  geom_text(aes(y = end_date + 100000, label = n), hjust = 0) +
  geom_text(aes(y = end_date + 600000, label = type2), hjust = 0, color = "darkgray") +
  coord_flip() +
  ggthemes::scale_color_tableau(palette = "Tableau 20", guide = FALSE) +
  theme_minimal()

hand_traits <- levels(trait_site_freq$trait2)[1:44]

# what is the median coefficient of variation
# for each hand_trait / sitename combination?

xx %>%
  filter(trait %in% hand_traits) %>%
  group_by(trait, sitename) %>%
  summarise(n = n(), cv = sd(mean) / mean(mean)) %>%
  group_by(trait) %>%
  summarise(md = median(cv, na.rm = TRUE)) %>%
  arrange(md)

tmp <- filter(xx, trait == hand_traits[3])
tmp <- filter(xx, trait == "proximal_air_temperature")

ggplot(tmp, aes(trans_date, mean)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ sitename, nrow = 1)

tmp <- filter(xx, trait == "leaf_temperature")

ggplot(tmp, aes(trans_date, mean)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ sitename)

tmp <- filter(xx, trait == "seedling_emergence_rate")

ggplot(tmp, aes(trans_date, mean)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ sitename)

# ignore hand-calculated variables (type = 1)



# does every site have the same cultivar?
xx %>%
  mutate(sitename2 = gsub(" (E|W)$", "", sitename)) %>%
  group_by(sitename2) %>%
  summarise(n = length(unique(cultivar))) %>%
  arrange(-n)

# yes! so we should group by sitename2 and have a
# separate time series for each variable within each group

tmp <- xx %>%
  mutate(
    date = as.Date(trans_date),
    sitename2 = gsub(" (E|W)$", "", sitename)) %>%
  select(-trans_date) %>%
  filter(trait %in% c("surface_temperature", "canopy_height",
    "leaf_angle_mean", "leaf_angle_chi", "leaf_angle_beta",
    "leaf_angle_alpha", "panicle_volume", "panicle_surface_area",
    "panicle_count"))

dts <- sort(unique(tmp$date))

tmp %>%
  group_by(trait, date, sitename) %>%
  tally() %>%
  filter(n > 1) %>%
  arrange(-n)

tmp %>%
  filter(sitename == "MAC Field Scanner Season 4 Range 10 Column 11" &
    date == "2017-07-06" &
    trait == "canopy_height") %>%
  distinct() %>%
  data.frame()

tmp %>%
  group_by(trait, date) %>%
  summarise(n = n(), nsite = length(unique(sitename2))) %>%
  arrange(trait, date) %>%
  data.frame()

tmp %>%
  group_by(trait) %>%
  summarise(n = n(), nsite = length(unique(sitename2))) %>%
  arrange(-nsite) %>%
  data.frame()

tmp %>%
  group_by(trait, date, sitename2) %>%
  summarise(n = n(), umeas = length(unique(mean)), var = var(mean)) %>%
  filter(n > 1) %>%
  arrange(-var)

filter(tmp, trait == "canopy_height" &
  date == "2017-07-06" &
  sitename2 == "MAC Field Scanner Season 4 Range 22 Column 2")
# means are 102 and 360?
# ??


tmp %>%
  group_by(trait) %>%
  tally()

ggplot(tmp, aes(date, trait, color = trait)) +
  geom_point() +
  ggthemes::scale_color_tableau(palette = "Tableau 10", guide = FALSE) +
  theme_minimal()



#### features to use:
# planter_seed_drop: one-time measurement of count of seeds - measured densely
# seedling_emergence_rate: maybe average of the last 2-3 measurements


tmp1 <- filter(xx, trait == "seedling_emergence_rate") %>%
  arrange(trans_date) %>%
  group_by(sitename2) %>%
  summarise(mean_emergence = mean(tail(mean, 3)))
tmp2 <- filter(xx, trait == "aboveground_dry_biomass")

filter(tmp2, sitename %in% head(unique(tmp2$sitename), 10)) %>%
ggplot(aes(trans_date, mean)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ sitename, nrow = 1)

tmp3 <- left_join(tmp1, select(tmp2, sitename2, mean),
  by = "sitename2")

ggplot(tmp3, aes(mean_emergence, mean)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  scale_x_log10() +
  scale_y_log10()


install.packages("htmlTable")
install.packages("cowplot")
install.packages("officer")
install.packages("formattable")
install.packages("kableExtra")
remotes::install_local("__path_to_downloaded_.tar.gz__", dependencies = TRUE)
