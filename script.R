# Main scripts 

# Tasks - load contents from dropbox linkes

# Prereqs -----------------------------------------------------------------



rm(list = ls())

require(readr)
require(repmis)
require(tidyr)
require(dplyr)
require(stringr)
require(ggplot2)



# Comments & links from Gavin ---------------------------------------------


# Jon, the RData you asked are in the following link. 
# Because I did the analysis city by city, so there are a lot objects in each RData. 
# I think the object "indicators.post" should include the information you want. Tell me if you want something else.
# 
# https://www.dropbox.com/s/bpa0kyie7eomsyj/aberdeen_analysis.RData?dl=0
# https://www.dropbox.com/s/s9s5j3zrevmp3qu/edinburgh_analysis.RData?dl=0
# https://www.dropbox.com/s/h769s8mslaqu5r3/glasgow_analysis.RData?dl=0
# https://www.dropbox.com/s/3y1nitve9vb0v8k/london_analysis.RData?dl=0
# https://www.dropbox.com/s/02no8ms3twkoqwl/manchester_analysis.RData?dl=0


# Have put these into subdirs in rdata/ folder


# Aberdeen ----------------------------------------------------------------

load("rdata/aberdeen/aberdeen_analysis.RData")

dta <- data.frame(city = "Aberdeen", indicators.post)  %>% tbl_df

dta <- dta %>% 
  gather(key = "x", value ="value", -city) %>% 
  mutate(
    year = str_replace(x, pattern = "\\D{1,}", replacement = ""),
    type = str_replace(x, pattern = "\\d{1,}", replacement = "")     
         ) %>% 
  select(city, year, type, value)


write_csv(dta, path = "tidy_data/aberdeen.csv")


rm(list = ls())


# Glasgow ----------------------------------------------------------------

load("rdata/glasgow/glasgow_analysis.RData")

dta <- data.frame(city = "Glasgow", indicators.post)  %>% tbl_df

dta <- dta %>% 
  gather(key = "x", value ="value", -city) %>% 
  mutate(
    year = str_replace(x, pattern = "\\D{1,}", replacement = ""),
    type = str_replace(x, pattern = "\\d{1,}", replacement = "")     
  ) %>% 
  select(city, year, type, value)


write_csv(dta, path = "tidy_data/glasgow.csv")

rm(list = ls())

# Edinburgh ----------------------------------------------------------------

load("rdata/edinburgh/edinburgh_analysis.RData")

dta <- data.frame(city = "Edinburgh", indicators.post)  %>% tbl_df

dta <- dta %>% 
  gather(key = "x", value ="value", -city) %>% 
  mutate(
    year = str_replace(x, pattern = "\\D{1,}", replacement = ""),
    type = str_replace(x, pattern = "\\d{1,}", replacement = "")     
  ) %>% 
  select(city, year, type, value)


write_csv(dta, path = "tidy_data/edinburgh.csv")

rm(list = ls())

# London ----------------------------------------------------------------

load("rdata/london/london_analysis.RData")

dta <- data.frame(city = "London", indicators.post)  %>% tbl_df

dta <- dta %>% 
  gather(key = "x", value ="value", -city) %>% 
  mutate(
    year = str_replace(x, pattern = "\\D{1,}", replacement = ""),
    type = str_replace(x, pattern = "\\d{1,}", replacement = "")     
  ) %>% 
  select(city, year, type, value)


write_csv(dta, path = "tidy_data/london.csv")

rm(list = ls())


# Manchester ----------------------------------------------------------------

load("rdata/manchester/manchester_analysis.RData")

dta <- data.frame(city = "Manchester", indicators.post)  %>% tbl_df

dta <- dta %>% 
  gather(key = "x", value ="value", -city) %>% 
  mutate(
    year = str_replace(x, pattern = "\\D{1,}", replacement = ""),
    type = str_replace(x, pattern = "\\d{1,}", replacement = "")     
  ) %>% 
  select(city, year, type, value)


write_csv(dta, path = "tidy_data/manchester.csv")

rm(list = ls())



# Combine -----------------------------------------------------------------


dta_1 <- read_csv("tidy_data/aberdeen.csv")
dta_2 <- read_csv("tidy_data/glasgow.csv")
dta_3 <- read_csv("tidy_data/edinburgh.csv")
dta_4 <- read_csv("tidy_data/london.csv")
dta_5 <- read_csv("tidy_data/manchester.csv")


dta <- dta_1 %>% 
  bind_rows(dta_2) %>% 
  bind_rows(dta_3) %>% 
  bind_rows(dta_4) %>% 
  bind_rows(dta_5)

write_csv(dta, "tidy_data/all_combined.csv")

rm(list = ls())

# Data vis ----------------------------------------------------------------

dta <- read_csv("tidy_data/all_combined.csv")


dta %>% 
  filter(type == "D") %>% 
  mutate(Year = factor(year, levels = c("2001", "2011"))) %>%  
  ggplot(.) +
  geom_density(
    mapping = aes(x = value, group = Year, fill = Year),
    colour = NA,
    alpha = 0.9
               ) +
  facet_wrap(facets = ~ city, scales = "free_y", nrow = 1) +
  labs(x = "Dissimilarity Values", y= "") +
  theme_bw() +
  scale_fill_manual(values = c("grey", "black"))

ggsave(filename = "figures/D.png", dpi = 300, width = 30, height = 6, units = "cm")


dta %>% 
  filter(type == "RCI") %>% 
  mutate(Year = factor(year, levels = c("2001", "2011"))) %>%  
  ggplot(.) +
  geom_density(
    mapping = aes(x = value, group = Year, fill = Year),
    colour = NA,
    alpha = 0.9
  ) +
  facet_wrap(facets = ~ city, scales = "free_y", nrow = 1) +
  labs(x = "Relative Centralisation Index", y= "") +
  theme_bw() +
  scale_fill_manual(values = c("grey", "black"))

ggsave(filename = "figures/RCI.png", dpi = 300, width = 30, height = 6, units = "cm")
