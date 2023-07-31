library(tidyverse)
library(janitor)

decathlon <- read_rds("raw_data/decathlon.rds")

decathlon <- rownames_to_column(decathlon, var = "name")

decathlon <- clean_names(decathlon)

decathlon <- pivot_longer(
  decathlon, 
  cols = x100m:x1500m,
  names_to = "sport",
  values_to = "sport_score"
  )

decathlon <- mutate(
  decathlon,
  sport = str_remove(sport, "x")
)

write_csv(decathlon, file = "clean_data/decathlon.csv")
