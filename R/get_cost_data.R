#' Nettokäyttökustannukset toiminnoittain
#'
#' @description Nettokäyttökustannukset toiminnoittain
#' @param year numero tai vektori. Käyvät arvot ovat 2015 - 2018
#' @return A tibble
#' @examples
#' cost_data <- get_cost_data() # Nettokäyttökustannukset
#'
#' @export

get_cost_data <- function(year = "*", lazy = TRUE) {

  if (year == "*"){year <- 2015:2019}
  year <- as.character(intersect(sort(unique(as.numeric(year))), 2015:2019))

  func_url <- tibble::tibble(year = 2015:2019,
                             url = c("http://pxnet2.stat.fi/PXWeb/api/v1/fi/Kuntien_talous_ja_toiminta/Kunnat/4._Kayttotalous_tehtavittain/011_kta_14_2015.px",
                                     "http://pxnet2.stat.fi/PXWeb/api/v1/fi/Kuntien_talous_ja_toiminta/Kunnat/4._Kayttotalous_tehtavittain/010_kta_14_2016.px",
                                     "http://pxnet2.stat.fi/PXWeb/api/v1/fi/Kuntien_talous_ja_toiminta/Kunnat/4._Kayttotalous_tehtavittain/009_kta_14_2017.px",
                                     "http://pxnet2.stat.fi/PXWeb/api/v1/fi/Kuntien_talous_ja_toiminta/Kunnat/4._Kayttotalous_tehtavittain/008_kta_14_2018.px",
                                     "http://pxnet2.stat.fi/PXWeb/api/v1/fi/Kuntien_talous_ja_toiminta/Kunnat/4._Kayttotalous_tehtavittain/007_kta_14_2019.px"))
  urls <- func_url %>%
    filter(year %in% !!year) %>%
    select(url) %>%
    pull()

  if (lazy) {
    cost_data <- costs
  } else {
    cost_data <- purrr::map2_dfr(year, urls, get_one_year_cost_data)
  }

}

get_one_year_cost_data <- function(year, url){
  cost <- pxweb::pxweb_get(url = url,
                           query = list("Alue"=c("*"),
                                        "Kulu-/tuottolaji"=c("toimintakulut_yht", "poistot_arvonalentumiset", "vyorytyskulut",
                                                             "toimintatuotot_yht", "valmistus_omaan_kayttoon", "valmistevarastojen_muutos", "vyorytystuotot"),
                                        "Tehtävä"=c("*"),
                                        "Vuosi"= as.character(year))) %>%
    {cbind(as.data.frame(., column.name.type = "code", variable.value.type = "code"),as.data.frame(., column.name.type = "code", variable.value.type = "text"))} %>%
    dplyr::select(1, 2, 8, 4, 5) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(amount = dplyr::if_else(`Kulu-/tuottolaji` %in% c("toimintatuotot_yht", "valmistus_omaan_kayttoon", "valmistevarastojen_muutos", "vyorytystuotot"),
                                          -1 * .[[5]], .[[5]], 0)) %>%
    dplyr::transmute(muni = dplyr::if_else(Alue == "911", 541, as.numeric(Alue)), type = `Kulu-/tuottolaji`, func = `Tehtävä`, year = Vuosi, amount) %>%
    dplyr::group_by(muni, func) %>%
    dplyr::summarise(amount = sum(amount) * 1000) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(year = !!year) %>%
    dplyr::select(muni, year, func, amount)
}
