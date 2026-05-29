#' Create a table of plastic type proportions by year
#'
#' Calculates the proportion of each plastic type out of the total recorded
#' plastic waste for each year and returns a formatted gt table.
#'
#' @param data A data frame containing plastic waste data.
#'
#' @return A gt table showing plastic type proportions by year.
#' @export
#' @importFrom magrittr %>%
calc_plastic_type_props <- function(data) {

  plastic_vars <- c("empty", "hdpe", "ldpe", "o", "pet", "pp", "ps", "pvc")

  calc_prop <- function(var, data) {
    data %>%
      dplyr::filter(
        .data$grand_total > 0,
        .data$parent_company != "Grand Total"
      ) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(
        prop = sum(.data[[var]], na.rm = TRUE) /
          sum(grand_total, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(plastic_type = var)
  }

  purrr::map_dfr(plastic_vars, calc_prop, data = data) %>%
    dplyr::mutate(
      plastic_type = dplyr::recode(
        plastic_type,
        empty = "Empty",
        hdpe = "HDPE",
        ldpe = "LDPE",
        o = "Other",
        pet = "PET",
        pp = "PP",
        ps = "PS",
        pvc = "PVC"
      )
    ) %>%
    tidyr::pivot_wider(
      names_from = plastic_type,
      values_from = prop
    ) %>%
    dplyr::rename(Year = year)
}

table_plastic_type_by_year <- function(data) {

  table3_year <- calc_plastic_type_props(data)

  table3_year %>%
    gt::gt()
}
