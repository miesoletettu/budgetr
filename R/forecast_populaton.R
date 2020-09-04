#' Forecast population
#'
#' @param muni_code A character vector
#' @param netchangerate A numeric vector. An absolute net change rate in population by years. If a forecasting period is longer than netchangerate, the last value of vector will be repeted.
#' @param netchangecount A numeric vector. An absolute net change in population by years. If a forecasting period is longer than netchange, the last value of vector will be repeted. If both netchangerate and netchangecount is dilivered, netchangecount is used.
#' @param year A numeric value. A base year. Forecasting starts at the of this year.
#' @return A list on two tibble. The First is the end of the year population and the second is the average of the start and the end of year by municipality, year, age and sex.
#' @examples
#' popul_data <- get_mortality_data(muni_code = "KU186",  netchangerate = c(0.015, 0.015, 0.02, 0.02, 0.015), year = 2019)) # return populations by year, age and sex in Jarvenpaa

forecast_population <- function(muni_code = "KU186", netchangerate = 0.015, netchangecount = NA, year = 2019, adj = 1.061497) {

  fert_data <- get_fertility_data(muni_code)
  mort_data <- get_mortality_data(muni_code)
  mig_data <- get_migration_data(muni_code, year)
  pop_data <- get_population_data(muni_code, year)$end

  population_start <- pop_data
  for (counter in 1:11) {
    # väestön nettomuutos
    if (is.na(netchangecount)) {
      netchange <- netchangerate[pmin(counter, length(netchangerate))] * sum(population_start$population)
    } else {
      netchange <- netchangecount[pmin(counter, length(netchangecount))]
    }

    # syntyneet, eli uusi -1-ikäisten korortti
    births <- population_start %>% dplyr::filter(sex == 2 & age >=14 & age <= 50) %>%
      dplyr::left_join(fert_data, by = c("muni", "sex", "age")) %>%
      dplyr::summarise(births = round(sum(population * fertility * adj))) %>%
      dplyr::transmute(age = -1, `1` = round(0.5 * births), `2` = births - `1`) %>%
      tidyr::pivot_longer(-age, names_to = "sex", values_to = "population") %>%
      dplyr::mutate(muni = as.numeric(stringr::str_sub(muni_code, 3, 5)),
                    year = !!year + counter,
                    sex = as.numeric(sex))

    # tyhdistetään vuoden alun populaatioon syntyneet ja generoidaan kuolleet ja poismuuttaneet
    population_start <- population_start %>%
      dplyr::mutate(year = year + 1) %>%
      dplyr::union(births) %>%
      dplyr::left_join(mort_data, by = c("muni", "year", "sex", "age")) %>%
      dplyr::mutate(deaths = -population * mortality) %>%
      dplyr::mutate(age5 = pmin(floor(age / 5) * 5, 75)) %>%
      dplyr::left_join(mig_data %>% dplyr::select(muni, age5, sex, population5 = population, outmigrationpropensity, inmigrationintensity), by = c("muni", "age5", "sex")) %>%
      dplyr::mutate(outmigration = dplyr::if_else(is.na(outmigrationpropensity * population), 0, -outmigrationpropensity * population),
                    population5 = dplyr::if_else(is.na(population5), 0, population5),
                    outmigrationpropensity = dplyr::if_else(is.na(outmigrationpropensity), 0, outmigrationpropensity),
                    inmigrationintensity = dplyr::if_else(is.na(inmigrationintensity), 0, inmigrationintensity))

    # lisätään sisäänmuutto nettomuutoksesta
    inmigration_total <- netchange - sum(births$population) - sum(population_start$deaths) - sum(population_start$outmigration)
    population_start <- population_start %>%
      dplyr::mutate(inmigration = dplyr::if_else(age == -1 | age >= 80, 0, inmigrationintensity * inmigration_total / 5),
                    age = age + 1,
                    population = round(population + deaths + outmigration + inmigration)) %>%
      dplyr::filter(age<=100) %>%
      dplyr::select(muni, year, age, sex, population) %>%
      dplyr::arrange(year, age, sex)
    pop_data <- pop_data %>%
      dplyr::union(population_start)
  }

  pop_data_avg <- pop_data %>%
    dplyr::filter(year < !!year + 11) %>%
    dplyr::mutate(year = year + 1) %>%
    dplyr::union(pop_data %>% dplyr::filter(year > !!year)) %>%
    dplyr::group_by(muni, year, sex, age) %>%
    dplyr::summarise(population = mean(population)) %>%
    dplyr::ungroup()
  return(list(end = pop_data, avg = pop_data_avg))
}
