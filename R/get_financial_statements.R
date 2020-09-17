#' Nettokäyttökustannukset toiminnoittain
#'
#' @description Nettokäyttökustannukset toiminnoittain
#' @param year numero tai vektori. Käyvät arvot ovat 2015 - 2018
#' @return A tibble
#' @examples
#' cost_data <- get_cost_data() # Nettokäyttökustannukset
#'
#' @export

get_financial_statements <- function(municipality = "*", year = "*", lazy_load = TRUE) {

  if (municipality != "*"){
    municipality <- convert_municipality(municipality, to = "Vero")
  }
  if (year == "*"){year <- as.character(2015:2019)}
  year <- as.character(intersect(sort(unique(as.numeric(year))), 2015:2019))

  financial_statements_url <- tibble::tibble(year = as.character(2015:2019),
                                             url = c("http://pxnet2.stat.fi/PXWeb/api/v1/en/Kuntien_talous_ja_toiminta/Kunnat/1._Ulkoiset_tilinpaatoslaskelmat/011_kta_11_2015.px",
                                                     "http://pxnet2.stat.fi/PXWeb/api/v1/en/Kuntien_talous_ja_toiminta/Kunnat/1._Ulkoiset_tilinpaatoslaskelmat/010_kta_11_2016.px",
                                                     "http://pxnet2.stat.fi/PXWeb/api/v1/en/Kuntien_talous_ja_toiminta/Kunnat/1._Ulkoiset_tilinpaatoslaskelmat/009_kta_11_2017.px",
                                                     "http://pxnet2.stat.fi/PXWeb/api/v1/en/Kuntien_talous_ja_toiminta/Kunnat/1._Ulkoiset_tilinpaatoslaskelmat/008_kta_11_2018.px",
                                                     "http://pxnet2.stat.fi/PXWeb/api/v1/en/Kuntien_talous_ja_toiminta/Kunnat/1._Ulkoiset_tilinpaatoslaskelmat/007_kta_11_2019.px"))

  urls <- financial_statements_url %>%
    filter(year %in% !!year) %>%
    select(url) %>%
    pull()

  if (lazy_load) {
    municipality <- convert_municipality(municipality, to = "Vero")
    financial_statements <- financial_statements_data %>%
      dplyr::filter(municipality %in% !!municipality & year %in% !!year)
  } else {
    financial_statements <- purrr::map2_dfr(year, urls, ~ get_one_year_financial_statements(municipality, .x, .y))
  }

}

get_one_year_financial_statements <- function(municipality, year, url){
  financial_statements <- pxweb::pxweb_get(url = url,
                                           query = list(Alue = municipality,
                                                        Tilinpäätöserä = c('*'),
                                                        Vuosi = year)) %>%
    {cbind(as.data.frame(., column.name.type = "code", variable.value.type = "code"),as.data.frame(., column.name.type = "text", variable.value.type = "text"))} %>%
    dplyr::select(municipality = 1, year = 3, item = 6, value = 4) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(value = value * 1000)
}
