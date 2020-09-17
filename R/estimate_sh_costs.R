#' Estimate standardized social and health service ongoing costs per person
#'
#' @description Estimate standardized social and health service ongoing costs per person by municipality and year
#' @param pop_data A tibble. Population like functions get_population_data and forecast-population return
#' @param inflation A numeric vector. Annual inflation vector starting from 2019 -> 2020. The last value of vector will be repated.
#' @param needfactor A list. Internal data structure. See \code{\link{needfactor}}.
#' @return The mortality rates by municipality, year, age and sex.
#' @examples
#' fert_data <- get_mortality_data()        # return mortality rates by municipalities, year, age and sex
#' fert_data <- get_mortality_data("KU186") # return mortality rates by year, age and sex in Jarvenpaa
#'
#' @export

estimate_sh_costs <- function(pop_data, mspi = indicies$mspi, needfactor = needfactor, standardized_sh_cost = standardized_sh_cost, adj = indicies$adj){

  year <- 2018:max(pop_data$year)
  mspi_base <- mspi %>% filter(year == 2018) %>% select(index) %>% pull()
  popul <- pop_data %>% group_by(muni, year) %>% summarise(population = sum(population)) %>% ungroup()

  sh_cost <- pop_data %>%
    group_population_data() %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("age"), names_to = "age_group", values_to = "weight") %>%
    dplyr::inner_join(needfactor$coefficients, by = "age_group") %>%
    dplyr::group_by(muni, year) %>%
    dplyr::summarise(costs = needfactor$coefficients[1,2] + sum(weight * value)) %>%
    dplyr::inner_join(needfactor$residuals, by = "muni") %>%
    dplyr::mutate(costs = costs + residual) %>%
    dplyr::left_join(mspi, by = "year") %>%
    dplyr::mutate(sh_factor = costs * index / mspi_base) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(muni, year, sh_factor) %>%
    dplyr::rename(sh_factor = value) %>%
    dplyr::left_join(popul, by = c("muni", "year")) %>%
    dplyr::transmute(muni, year, sh_factor, standardized_costs = sh_factor * population * standardized_sh_cost)

}

