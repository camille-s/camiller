edu <- c(age25plus = "B15003_001", bachelors = "B15003_022", masters = "B15003_023") %>%
  tidycensus::get_acs(geography = "county subdivision", year = 2016, variables = ., state = "09", county = "09") %>%
  dplyr::mutate(NAME = stringr::str_remove(NAME, " town,.+")) %>%
  dplyr::select(name = NAME, variable:moe) %>%
  dplyr::filter(name %in% c("New Haven", "Hamden", "West Haven", "East Haven", "Bethany"))

use_data(edu, overwrite = T)
