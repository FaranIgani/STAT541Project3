#' Summarize plastic waste by company
#'
#' Calculates the total recorded plastic waste for each parent company
#' and returns the top companies by total plastic waste.
#'
#' @param data A data frame containing plastic waste data.
#' @param top_n Number of companies to return.
#'
#' @return A tibble containing parent companies and total plastic waste.
#' @export
plastic_by_company <- function(data, top_n = 10) {

  data |>
    dplyr::filter(
      !is.na(parent_company),
      parent_company != "Grand Total",
      parent_company != "NULL",
      parent_company != "null",
      parent_company != "Unbranded",
      parent_company != "Assorted",
    ) |>
    dplyr::group_by(parent_company) |>
    dplyr::summarise(
      total_plastic = sum(grand_total, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(total_plastic)) |>
    dplyr::slice_head(n = top_n)
}
