library(tidyverse)

gnh <- tibble(
  town = c("New Haven", "Hamden", "East Haven", "West Haven", "Bethany", "Woodbridge", "Milford", "Orange", "Madison", "Guilford", "Branford", "North Branford", "North Haven")
) %>%
  mutate(region = as.factor(town) %>% forcats::fct_collapse(
    "Inner Ring" = c("Hamden", "East Haven", "West Haven"),
    "Outer Ring" = c("Bethany", "Woodbridge", "Milford", "Orange", "Madison", "Guilford", "Branford", "North Branford", "North Haven")
  ) %>% forcats::fct_relevel("New Haven", "Inner Ring", "Outer Ring"))

usethis::use_data(gnh)
