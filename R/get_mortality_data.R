#' Get mortality data from  the the Statistic Finnish population forecast
#'
#' @param muni_code A character vector
#' @return The mortality rates by municipality, year, age and sex.
#' @examples
#' fert_data <- get_mortality_data()        # return mortality rates by municipalities, year, age and sex
#' fert_data <- get_mortality_data("KU186") # return mortality rates by year, age and sex in Jarvenpaa
get_mortality_data <- function(muni_code = "*") {
  mort_query_list <-
    list("Kunta" = muni_code,
         "Vuosi" = "*",
         "Sukupuoli" =  "*",
         "Ikä" =  "*",
         "Tiedot" =  "*")

  # sex: 1 = mies, 2 = nainen
  # age: ikä vuoden alussa, -1 = vuoden aikana syntyneneet

  mort_data <-
    pxweb::pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/vrm/vaenn/statfin_vaenn_pxt_129a.px",
                     query = mort_query_list) %>%
    as.data.frame(column.name.type = "text", variable.value.type = "code") %>%
    tibble::as_tibble() %>%
    dplyr::transmute(muni = as.numeric(stringr::str_sub(Municipality, 3, 5)),
                     year = as.numeric(as.character(Year)),
                     age = dplyr::if_else(Age == "alle0", -1, as.numeric(Age), -2),
                     sex = as.numeric(Sex),
                     mortality = `Mortality coefficient, per mille (projection 2019)`/1000)

  # lisätään 100-vuotiaille kuolevuus 1
  mort_data <- mort_data %>%
    dplyr::union(mort_data %>% dplyr::filter(age == 0) %>% dplyr::mutate(age = 100, mortality = 1)) %>%
    dplyr::arrange(year, sex, age)
}
