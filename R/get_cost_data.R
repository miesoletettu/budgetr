#' Get net ongoing costs
#'
#' @description Get net ongoing costs by municipality and function
#' @param year A numeric value
#' @return A tibble
#' @examples
#' cost_data <- get_cost_data() # return ongoing costs in 2018 by municipality and function
get_cost_data <- function(year = 2018) {
  func_url <- if (year == 2019) {
      "http://pxnet2.stat.fi/PXWeb/api/v1/fi/Kuntien_talous_ja_toiminta/Kunnat/4._Kayttotalous_tehtavittain/007_kta_14_2019.px"
    } else if (year == 2018) {
      "http://pxnet2.stat.fi/PXWeb/api/v1/fi/Kuntien_talous_ja_toiminta/Kunnat/4._Kayttotalous_tehtavittain/008_kta_14_2018.px"
    } else if (year == 2017) {
      "http://pxnet2.stat.fi/PXWeb/api/v1/fi/Kuntien_talous_ja_toiminta/Kunnat/4._Kayttotalous_tehtavittain/009_kta_14_2017.px"
    } else if (year == 2016) {
      "http://pxnet2.stat.fi/PXWeb/api/v1/fi/Kuntien_talous_ja_toiminta/Kunnat/4._Kayttotalous_tehtavittain/010_kta_14_2016.px"
    } else if (year == 2015) {
      "http://pxnet2.stat.fi/PXWeb/api/v1/fi/Kuntien_talous_ja_toiminta/Kunnat/4._Kayttotalous_tehtavittain/011_kta_14_2015.px"
    }

  cost <- pxweb::pxweb_get(url = func_url,
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
