library(tidyverse)
library(janitor)
library(readxl)

candy_2015 <- read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx")
candy_2016 <- read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx")
candy_2017 <- read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx")

candy_2015 <- clean_names(candy_2015)
candy_2016 <- clean_names(candy_2016)
candy_2017 <- clean_names(candy_2017)

candy_2015 <- candy_2015 %>% 
  select(how_old_are_you:york_peppermint_patties,
         sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year,
         necco_wafers)

candy_2015 <- candy_2015 %>% 
  rename(
    age = how_old_are_you,
    going = are_you_going_actually_going_trick_or_treating_yourself
  )

candy_2016 <- candy_2016 %>% 
  select(are_you_going_actually_going_trick_or_treating_yourself:york_peppermint_patties)

candy_2016 <- candy_2016 %>% 
  rename(
    age = how_old_are_you,
    going = are_you_going_actually_going_trick_or_treating_yourself,
    country = which_country_do_you_live_in,
    area = which_state_province_county_do_you_live_in,
    gender = your_gender
  )

names(candy_2017) <- str_remove(
  names(candy_2017),
  "q[0-9]+_"
)

candy_2017 <- candy_2017 %>% 
  rename(going = going_out,
         area = state_province_county_etc)

candy_2017 <- candy_2017 %>% 
  select(going:york_peppermint_patties)

candy_2015 <- candy_2015 %>% 
  mutate(id = seq.int(nrow(candy_2015)),
         year = 2015,
         area = NA,
         country = NA,
         gender = NA) %>% 
  mutate(id = str_c("2015-", as.character(id)))

candy_2015 <- candy_2015 %>% 
  pivot_longer(
    cols = -c(age, going, country, area, gender, id, year),
    names_to = "question",
    values_to = "response",
    values_drop_na = TRUE
  )

candy_2016 <- candy_2016 %>% 
  mutate(id = seq.int(nrow(candy_2016)),
         year = 2016) %>% 
  mutate(id = str_c("2016-", as.character(id)))

candy_2016 <- candy_2016 %>% 
  pivot_longer(
    cols = -c(age, going, country, area, gender, id, year),
    names_to = "question",
    values_to = "response",
    values_drop_na = TRUE
  )

candy_2017 <- candy_2017 %>% 
  mutate(id = seq.int(nrow(candy_2017)),
         year = 2017) %>% 
  mutate(id = str_c("2017-", as.character(id)))

candy_2017 <- candy_2017 %>% 
  pivot_longer(
    cols = -c(age, going, country, area, gender, id, year),
    names_to = "question",
    values_to = "response",
    values_drop_na = TRUE
  )

candy <- bind_rows(candy_2015, candy_2016, candy_2017)

candy <- candy %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(
    age = case_when(
    age < 2 ~ NA,
    age > 125 ~ NA,
    .default = age
  ),
  going = case_match(going,
    "Yes" ~ TRUE,
    "No" ~ FALSE,
    .default = NA
  )
  )

candy <- candy %>% 
  mutate(country = str_to_lower(country)) %>% 
  mutate(country = str_remove_all(country, "[.!'?`]")) %>% 
  mutate(country = str_replace_all(country, "  ", " ")) %>% 
  mutate(country = case_when(
    str_detect(country, "^usa ") ~ "usa",
    str_detect(country, "united s") ~ "usa",
    str_detect(country, "united k") ~ "uk",
    str_detect(country, "^[a-z]$") ~ NA,
    !is.na(as.numeric(country)) ~ NA,
    .default = country
  )) %>% 
  mutate(country = case_match(
    country,
    "us" ~ "usa",
    "usausausa" ~ "usa",
    "u s a" ~ "usa",
    "u s" ~ "usa",
    "ussa" ~ "usa",
    "us of a" ~ "usa",
    "unites states" ~ "usa",
    "units states" ~ "usa",
    "america" ~ "usa",
    "murica" ~ "usa",
    "merica" ~ "usa",
    "murrika" ~ "usa",
    "españa" ~ "spain",
    "endland" ~ "uk",
    "england" ~ "uk",
    "scotland" ~ "uk",
    .default = country
  ))

write_csv(candy, file = "clean_data/candy.csv")