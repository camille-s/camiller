acs_vars <- tidycensus::load_variables(year = 2016, "acs5") %>%
  dplyr::mutate(label = stringr::str_remove(label, "Estimate!!")) %>%
  dplyr::mutate(name = stringr::str_remove(name, "E$"))

usethis::use_data(acs_vars, overwrite = TRUE, internal = TRUE)
