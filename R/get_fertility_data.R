#' Get fertility data from  the the Statistic Finnish population forecast
#'
#' @param muni_code A character vector
#' @return The fertility rates by municipality and age.
#' @examples
#' fert_data <- get_fertility_data()        # return fertility rates by municipalities and mother ages
#' fert_data <- get_fertility_data("KU186") # return fertility rates by mother ages in Jarvenpaa
get_fertility_data <-function(muni_code = "*") {
  fert_query_list <-
    list("Kunta" = muni_code,
         "Vuosi" = "*",
         "Ikä" = "*",
         "Tiedot" = "*")

  # age: ikä vuoden alussa
  fert_data <-
    pxweb::pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/vrm/vaenn/statfin_vaenn_pxt_128z.px",
                     query = fert_query_list) %>%
    as.data.frame(column.name.type = "text", variable.value.type = "code")  %>%
    tibble::as_tibble() %>%
    dplyr::transmute(muni = as.numeric(stringr::str_sub(Municipality, 3, 5)),
                     year = as.numeric(Year),
                     age = as.numeric(Age),
                     sex = 2,
                     fertility = `Fertility coefficient, per mille (projection 2019)`/1000)
}
