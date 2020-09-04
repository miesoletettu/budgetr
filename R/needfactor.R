#' The need factors ja model of social and health function by minicipality
#'
#' A dataset contains the need factors of social and health services by municipality for 2017 and 2018.
#'
#' @format A list:
#' \describe{
#'   \item{factors}{A tibble:}
#'   \itemize{
#'     \item{muni}{numeric code of municipality of Finland}
#'     \item{needfactor_2017}{the need factors of social and health services for 2017}
#'     \item{needfactor_2018}{the need factors of social and health services for 2018}
#'   }
#'   \item{factors_source}{A tibble: source of data}
#'   \item{coefficients}{A tibble: coefficients of fitted model}
#'   \item{residuals}{A tbble: The parts of the need factors that model cannot explain.}
#'   \itemize{
#'     \item{muni}{numeric code of municipality of Finland}
#'     \item{residual}{the parts of the need factors that model cannot explain}
#'   }
#' }
#' @source \url{https://thl.fi/fi/web/sote-uudistus/talous-ja-politiikka/kustannukset-ja-vaikuttavuus/rahoitus/tarvevakioidut-menot}
"needfactor"
