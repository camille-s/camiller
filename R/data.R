#' 2016 ACS variables
#'
#' Variable number-to-label crosswalk for the 2016 ACS from `tidycensus`
#'
#' @format A tibble with 22815 rows and 3 columns:
#' \describe{
#'   \item{name}{Variable code}
#'   \item{label}{Variable label}
#'   \item{concept}{Name of table that contains variable}
#' }
#' @source US Census Bureau, American Community Survey 2016 5-year estimates
"acs_vars"

#' Education levels
#'
#' Educational attainment data for 5 New Haven-area towns, 2016
#'
#' @format A tibble with 15 rows and 4 columns
#' \describe{
#'   \item{name}{Town name}
#'   \item{variable}{Education level}
#'   \item{estimate}{Estimated count}
#'   \item{moe}{Margin of error of estimates}
#' }
#' @source US Census Bureau, American Community Survey 2016 5-year estimates
"edu"

#' Greater New Haven towns
#'
#' Names of towns in Greater New Haven, with their region names, as defined by DataHaven
#'
#' @format A tibble with 13 rows and 2 columns
#' \describe{
#'   \item{town}{Town name}
#'   \item{region}{Region name}
#' }
"gnh"

#' Ratios to poverty by age
#'
#' Population by age group by ratio of household income to the federal poverty level for Greater New Haven towns, 2016
#'
#' @format A tibble with 1690 rows and 5 columns
#' \describe{
#'   \item{name}{Town name}
#'   \item{age}{Age group}
#'   \item{ratio}{Ratio bracket}
#'   \item{estimate}{Estimated count}
#'   \item{moe}{Margin of error of estimates}
#' }
#' @source US Census Bureau, American Community Survey 2016 5-year estimates
"pov_age"

#' Ratios to poverty by age for 2 years
#'
#' Population by age group by ratio of household income to the federal poverty level for Greater New Haven towns, 2010 and 2016
#'
#' @format A tibble with 3380 rows and 6 columns
#' \describe{
#'   \item{name}{Town name}
#'   \item{year}{Endyear of estimates}
#'   \item{age}{Age group}
#'   \item{ratio}{Ratio bracket}
#'   \item{estimate}{Estimated count}
#'   \item{moe}{Margin of error of estimates}
#' }
#' @source US Census Bureau, American Community Survey 2010 and 2016 5-year estimates
"pov_age_10_16"

#' Populations by race
#'
#' Population by race for Greater New Haven towns, 2016
#'
#' @format A tibble with 65 rows and 5 columns
#' \describe{
#'   \item{name}{Town name}
#'   \item{variable}{Racial group}
#'   \item{estimate}{Estimated count}
#'   \item{moe}{Margin of error of estimates}
#'   \item{region}{Region of town}
#' }
#' @source US Census Bureau, American Community Survey 2016 5-year estimates
"race_pops"
