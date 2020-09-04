#' Helper functions

format_num <- function(x, digits = 2){
  formatC(x, format = "f", digits = digits, big.mark   = " ", decimal.mark = ",")
}
