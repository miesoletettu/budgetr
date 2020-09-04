#' Estimate corporate taxes
#'
#' @description Estimate municipality corporate taxes
#' @param corporate_tax_2019 A numeric value. The corporate tax at year 2019.
#' @param corporate_tax_index A numeric vector. A vector from 2018 to 2030.
#' @return Atabble. The municipality corporete taxes by municipality and year.
#' @examples
#' corporate_tax <- estimate_corporate_tax(corp_tax_2019 = 4677000)

estimate_corporate_tax <- function(corporate_tax_2019 = 4677000, corporate_tax_index = indicies$corp_tax){

  index_2019 <- corporate_tax_index %>% filter(year == 2019) %>% select(index) %>% pull()

  corporate_tax <- corporate_tax_index %>%
    transmute(year = year,
              corporate_tax = index / index_2019 * corporate_tax_2019)
}
