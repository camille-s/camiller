#' Several options for formatting text into neater titles
#'
#' This function combines several ways titles and names may need to be formatted. It's meant to be simple, yet flexible.
#'
#' Examples of possible common operations include:
#'
#' \itemize{
#'   \item "TownName" --> "Town Name"
#'   \item "town_name" --> "Town Name"
#'   \item "town_name" --> "Town name"
#'   \item "RegionABC" --> "Region ABC"
#'   \item "TOWN_NAME" --> "Town Name"
#' }
#'
#' @param x A character vector
#' @param cap_all Logical: if `TRUE`, first letter of each word after splitting will be capitalized. If `FALSE`, only the first character of the string will be capitalized. Note that in order to balance this with respecting consecutive capital letters, such as from acronyms,
#' @param split_case Logical: if `TRUE`, consecutive lowercase-uppercase pairs will be treated as two words to be separated.
#' @param keep_running_caps Logical: if `TRUE`, consecutive uppercase letters will be kept uppercase.
#' @param space Character vector of characters and/or regex patterns that should be replaced with a space to separate words.
#' @param remove Character vector of characters and/or regex patterns that will be removed before any other operations; if `NULL`, nothing is removed.
#' @return A character vector with each item newly formatted
#' @examples
#' t1 <- c("GreaterNewHaven", "greater_new_haven", "GREATER_NEW_HAVEN")
#' clean_titles(t1, cap_all = TRUE, keep_running_caps = FALSE)
#'
#' t2 <- c("Male!CollegeGraduates", "Male CollegeGraduates")
#' clean_titles(t2, space = c("_", "!"))
#'
#' t3 <- c("Greater BPT Men", "Greater BPT Men HBP", "GreaterBPT_men", "greaterBPT")
#' clean_titles(t3, cap_all = FALSE)
#'
#' t4 <- c("New Haven town, New Haven County, Connecticut", "Newtown town, Fairfield County, Connecticut")
#' clean_titles(t4, cap_all = TRUE, remove = " town,.+")
#' @export
# clean up titles/names in strings with several options of capitalization, abbreviations, etc
clean_titles <- function(x, cap_all = FALSE, split_case = TRUE, keep_running_caps = TRUE, space = "_", remove = NULL) {
  # if cap_all, all words are capitalized; else only first character
  # if split_case, add space between [A-Z] and [a-z]
  # if keep_running_caps, consecutive [A-Z] will stay capitalized
  # space will be replaced with actual space
  if (length(remove)) {
    removed <- stringr::str_remove_all(x, .collapse_regex(remove))
  } else {
    removed <- x
  }
  spaced <- stringr::str_replace_all(removed, .collapse_regex(space), " ")
  if (split_case) {
    spaced <- stringr::str_replace_all(spaced, "(?<=[a-z])\\B(?=[A-Z])", " ")
  }
  # everybody gets a capital first letter
  capped <- sub("^([a-z])", "\\U\\1", spaced, perl = TRUE)
  if (cap_all) {
    if (!keep_running_caps) {
      capped <- gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2", capped, perl = TRUE)
    } else {
      capped <- gsub("\\b([a-z])", "\\U\\1", capped, perl = TRUE)
    }
  } else {
    # capped <- gsub("^\\b([a-z])", "\\U\\1", capped, perl = TRUE)
    if (!keep_running_caps) {
      capped <- gsub("^(\\w)(.+)$", "\\U\\1\\L\\2", capped, perl = TRUE)
    } else {
      capped <- gsub("(?<=.)([A-Z]{1})(?=[a-z]+)", "\\L\\1\\L\\2", capped, perl = TRUE)
    }
  }

  trimmed <- trimws(capped)
  trimmed <- stringr::str_replace_all(trimmed, "\\s+", " ")
  trimmed
}

.collapse_regex <- function(r) {
  r1 <- paste0(r, collapse = "|")
  r1 <- paste0("(", r1, ")")
  return(r1)
}
