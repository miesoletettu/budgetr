#' Estimate central government transfers to municipality
#'
#' @description Estimate central government transfers to municipality
#' @param pop_data A tibble
#' @param gov_tranfers A tibble transfer per person
#' @return A tabble. central government transfers to municipality by year
#' @examples
#' goverment_trans <- estimate_goverment_transer()
#' @export

estimate_goverment_trasfer <- function(pop_data, gov_tranfers = indicies$gov_trans){

  gov_trans_last <- gov_tranfers %>% filter(is.na(gov_trans) == FALSE) %>% select(gov_trans) %>% pull() %>% .[length(.)]

  goverment_trasfer <- pop_data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(population = sum(population)) %>%
    dplyr::mutate(year = year + 2) %>%
    dplyr::left_join(gov_tranfers, by = "year", suffix = c("", "_ind")) %>%
    dplyr::transmute(year, gov_tranfers = dplyr::if_else(is.na(population), population_ind, population) * dplyr::if_else(is.na(gov_trans), gov_trans_last, gov_trans))
}
