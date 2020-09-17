#' Estimate corporate taxes
#'
#' @description Estimate municipality corporate taxes
#' @param propertry_tax_2019 A numeric value. The corporate tax at year 2019.
#' @param property_tax_index A numeric vector. A vector from 2018 to 2030.
#' @return A tabble. The municipality corporete taxes by municipality and year.
#' @examples
#' property_tax <- estimate_property_tax(corp_tax_2019 = 4677000)
#' @export

estimate_property_tax <- function(property_tax_2019 = 14596000, property_tax_index = indicies$prop_tax){

  index_2019 <- property_tax_index %>% filter(year == 2019) %>% select(index) %>% pull()

  property_tax <- property_tax_index %>%
    transmute(year = year,
              property_tax = index / index_2019 * property_tax_2019)
}
