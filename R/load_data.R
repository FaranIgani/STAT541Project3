#' Load Plastic Waste Data
#'
#' Loads the TidyTuesday plastic waste dataset from GitHub.
#'
#' @return A tibble containing plastic waste data.
#' @export
#' @importFrom readr read_csv
load_data <- function() {

  plastics <- readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-01-26/plastics.csv"
  )

  return(plastics)
}
