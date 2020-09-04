#' Get population data from  the the Statistic Finnish
#'
#' @param muni_code A character vector
#' @param year A character vector. A year or years.
#' @return A list on three tibble. The First is the start of the year population, the second is the start of the year population and the third is the average of the start and the end of year by municipality, year, age and sex.
#' @examples
#' popul_data <- get_population_data()                             # return population in 2019 by municipalities, age and sex
#' popul_data <- get_mortality_data("KU186", year = c(2018, 2019)) # return populations by year, age and sex in Jarvenpaa

get_population_data <- function(muni_code = "*", year = 2019) {
  pop_query_list <-
    list("Alue"= muni_code,
         "Ikä" = "*",
         "Sukupuoli" =  "*",
         "Vuosi" = as.character(c(year, year - 1)),
         "Tiedot" =  "*")

  # sex: 1 = mies, 2 = nainen
  # age5: ikäryhmän alaraja

  pop_data <-
    pxweb::pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/vrm/vaerak/statfin_vaerak_pxt_11re.px",
                     query = pop_query_list) %>%
    as.data.frame(column.name.type = "text", variable.value.type = "code") %>%
    tibble::as_tibble() %>%
    dplyr::filter(stringr::str_sub(Area, 1 , 2) == "KU", Age != "SSS" & Sex != "SSS") %>%
    dplyr::transmute(muni = as.numeric(stringr::str_sub(Area, 3 , 5)),
                     year = as.numeric(as.character(Year)),
                     sex = as.numeric(Sex),
                     age = as.numeric(stringr::str_sub(Age, 1, 3)),
                     population = `Population 31 Dec`)

  pop_data_end <- pop_data %>%
    dplyr::filter(year %in% !!year)

  pop_data_start <- pop_data %>%
    dplyr::filter(year %in% (!!year - 1)) %>%
    dplyr::mutate(year = year + 1)

  pop_data_avg <- pop_data_start %>%
    dplyr::left_join(pop_data_end, by = c("muni", "year", "sex", "age"), suffix = c("_start", "_end")) %>%
    dplyr::transmute(muni, year, sex, age, population = (population_start + population_end) / 2)

  pop_data = list(start = pop_data_start, end = pop_data_end, avg = pop_data_avg)
}
