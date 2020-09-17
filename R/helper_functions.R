#' Helper functions
#'
#' @export

format_num <- function(x, digits = 2){
  formatC(x, format = "f", digits = digits, big.mark   = " ", decimal.mark = ",")
}

#'
#'@export
convert_municipality <- function(municipality, to = "muni_code"){
  regions %>%
    dplyr::filter(muni %in% municipality | muni_code %in% municipality | muni_name_fi %in% municipality |  muni_name_sv %in% municipality | "*" %in% municipality) %>%
    dplyr::select(to) %>%
    dplyr::pull()
}
