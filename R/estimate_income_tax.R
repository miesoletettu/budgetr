#' Estimate municipality income taxes
#'
#' @description Estimate municipality income taxes
#' @param pop_data A tibble. Population like functions get_population_data and forecast-population return
#' @param tax_rate A numeric vector. Annual tax rate vector starting from 2021. The last value of vector will be repated.
#' @param inflation A numeric vector. Annual inflation vector starting from 2019 -> 2020. The last value of vector will be repated.
#' @return Atabble. The municipality income taxes by municipality and year.
#' @examples
#' income_tax <- estimate_income_tax()
#' @export

estimate_income_tax <- function(pop_data, tax_rate = 0.1975, earnings_index = indicies$earnings){

  years <- 2018:max(pop_data$year, 2020)
  if (is.na(tax_rate)) {
    tax_rate <- 0.1975
  }
  tax_rates <- c(0.1975, 0.1975, 0.1975)
  for (i in 4:length(years)) {
    tax_rates[i] = tax_rate[min(i - 3, length(tax_rate))]
  }

  tax_rate <- tibble::tibble(year = years, tax_rate = tax_rates)
  employee_rate <- tibble(year = 2018:2030,
                          employee_rate = c(1, 1.001713, 0.9656951, 0.98, 0.995, 1, 1, 1, 1, 1, 1, 1, 1))

  income_tax <- pop_data %>%
    dplyr::inner_join(earnings, by = "age", suffix = c("", "_earnings")) %>%
    dplyr::inner_join(earnings_index, by = "year", suffix = c("", "_earn_ind")) %>%
    dplyr::inner_join(earnings_index, by = c("year_earnings" = "year"), suffix = c("", "_base")) %>%
    dplyr::inner_join(tax_rate, by = "year") %>%
    dplyr::inner_join(employee_rate, by = "year") %>%
    dplyr::group_by(muni, year) %>%
    dplyr::summarise(income_tax = sum(population * 1.129 * earnings * index / index_base * (tax_rate - 0.0117))  - 10^6,
                     income_tax_2 = sum(population * 1.12924 * earnings * index / index_base * employee_rate * tax_rate * 0.934185456),
                     income_tax_covid = sum(population * 1.129 * earnings * index / index_base * employee_rate * (tax_rate - 0.0117))  - 10^6,
                     earnings = sum(population * 1.12924 * earnings * index / index_base)) %>%
    dplyr::ungroup()
}

