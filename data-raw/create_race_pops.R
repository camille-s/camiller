race_pops <- c(total = 1, white = 3, black = 4, latino = 12, asian = 6) %>%
  purrr::map_chr(~stringr::str_pad(., width = 3, pad = "0", side = "left") %>%
            sprintf("B03002_%s", .)) %>%
  tidycensus::get_acs(geography = "county subdivision", variables = ., year = 2016, state = "09", county = "09") %>%
  dplyr::mutate(NAME = stringr::str_remove(NAME, " town,.+")) %>%
  dplyr::select(name = NAME, variable:moe) %>%
  dplyr::inner_join(gnh, by = c("name" = "town"))

usethis::use_data(race_pops, overwrite = T)
