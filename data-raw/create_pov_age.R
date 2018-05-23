pov_age_10_16 <- dplyr::bind_rows(
  tidycensus::get_acs(geography = "county subdivision", table = "B17024", year = 2010, state = "09", county = "09") %>%
    dplyr::mutate(year = 2010),
  tidycensus::get_acs(geography = "county subdivision", table = "B17024", year = 2016, state = "09", county = "09") %>%
    dplyr::mutate(year = 2016)
) %>%
  dplyr::mutate(NAME = stringr::str_remove(NAME, " town,.+")) %>%
  dplyr::rename(name = NAME) %>%
  dplyr::filter(name %in% gnh$town) %>%
  dplyr::left_join(acs_vars %>% dplyr::select(-concept), by = c("variable" = "name")) %>%
  tidyr::separate(label, into = c("total", "age", "ratio"), sep = "!!") %>%
  dplyr::filter(!is.na(age)) %>%
  tidyr::replace_na(list(ratio = "Poverty status determined")) %>%
  dplyr::select(name, year, age, ratio, estimate, moe)

pov_age <- pov_age_10_16 %>%
  dplyr::filter(year == 2016) %>%
  dplyr::select(-year)

use_data(pov_age_10_16, overwrite = T)
use_data(pov_age, overwrite = T)
