edu <- c(age25plus = "B15003_001", bachelors = "B15003_022", masters = "B15003_023") %>%
  tidycensus::get_acs(geography = "county subdivision", year = 2016, variables = ., state = "09", county = "09") %>%
  dplyr::mutate(NAME = stringr::str_remove(NAME, " town,.+")) %>%
  dplyr::select(name = NAME, variable:moe) %>%
  dplyr::filter(name %in% c("New Haven", "Hamden", "West Haven", "East Haven", "Bethany"))

use_data(edu, overwrite = T)


edu_detail <- tidycensus::get_acs(geography = "county subdivision", table = "B15003", state = "09", county = "09", year = 2016) %>%
  dplyr::mutate(name = stringr::str_remove(NAME, " town,.+")) %>%
  dplyr::filter(name %in% c("New Haven", "Hamden", "West Haven", "East Haven", "Bethany")) %>%
  dplyr::inner_join(acs_vars, by = c("variable" = "name")) %>%
  dplyr::select(name, label, estimate, moe) %>%
  tidyr::separate(label, into = c("total", "variable"), sep = "!!") %>%
  tidyr::replace_na(list(variable = "ages25plus")) %>%
  dplyr::select(-total)

usethis::use_data(edu_detail, overwrite = T)
