#' Population by age groups for social and heath service need factor calculation
#'
#' @param pop_data A tibble, population by municipality, year, age and sex (like get_population_data retunrs).
#' @examples
#' grouped_popul_data <- group_population_data(popul_data)
group_population_data <- function(pop_data) {
  group_pop_data <- pop_data %>%
    group_by(muni, year) %>%
    summarise(age1 = sum(population * (age <= 1)) / sum(population),
              age2 = sum(population * (age >= 2 & age <= 6)) / sum(population),
              age3 = sum(population * (age >= 7 & age <= 17)) / sum(population),
              age4 = sum(population * (age >= 18 & age <= 25)) / sum(population),
              age5 = sum(population * (age >= 26 & age <= 39)) / sum(population),
              age6 = sum(population * (age >= 40 & age <= 54)) / sum(population),
              age7 = sum(population * (age >= 55 & age <= 64)) / sum(population),
              age8 = sum(population * (age >= 65 & age <= 84)) / sum(population),
              age9 = sum(population * (age >= 85 & age <= 89)) / sum(population),
              age10 = sum(population * (age >= 90)) / sum(population)) %>%
    ungroup()
}
