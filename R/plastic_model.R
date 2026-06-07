#' Fit a mixed effects model for plastic waste
#'
#' Summarizes data to the country-year level and fits a linear mixed effects
#' model predicting log total plastic waste, with a random intercept for country.
#'
#' @param data A data frame containing joined plastic waste, GDP, population,
#' and region data.
#'
#' @return A fitted lmer model object.
#' @export
fit_plastic_lmer <- function(data) {

  country_year <- data |>
    dplyr::group_by(.data$country, .data$year, .data$region) |>
    dplyr::summarise(
      total_plastic = sum(.data$grand_total, na.rm = TRUE),
      gdp = mean(.data$gdp, na.rm = TRUE),
      population = mean(.data$population, na.rm = TRUE),
      Temperature = mean(.data$Temperature, na.rm = TRUE),
      pop_density = mean(.data$pop_density, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(
      .data$total_plastic > 0,
      !is.na(.data$gdp),
      !is.na(.data$Temperature),
      !is.na(.data$pop_density),
      !is.na(.data$region),
      !is.na(.data$country)
    ) |>
    dplyr::mutate(
      log_total_plastic = log(.data$total_plastic),
      log_gdp = log(.data$gdp),
    )

  lme4::lmer(
    log_total_plastic ~ log_gdp * region +
      (1 | country),
    data = country_year
  )
}

#' Create a gt table for the plastic model
#'
#' @param model A fitted lmer model.
#'
#' @return A gt table.
#' @export
plastic_model_table <- function(model) {

  broom.mixed::tidy(
    model,
    effects = "fixed"
  ) |>
    gt::gt() |>
    gt::fmt_number(
      columns = c(estimate, std.error, statistic),
      decimals = 3
    ) |>
    gt::cols_label(
      term = "Predictor",
      estimate = "Estimate",
      std.error = "SE",
      statistic = "t value"
    ) |>
    gt::tab_header(
      title = "Mixed Effects Model Results"
    )
}
