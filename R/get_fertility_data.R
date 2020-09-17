#' Hedelmällisyystietojen haku Tilastokeskuksen StatFin-tietokannasta
#'
#' Funktio hakee Tilastokeskuksen StatFin-tietokannasta Väestöennuste 2019: Hedelmällisyyskertoimet äidin iän mukaan kunnittain, 2019-2040.
#' Hedelmällisyysluvut ovat koko ennustejaksolle samat.
#'
#' Tilastokeskuksen StatFin-tietokannan tiedot on julkaistu lisenssillä CC BY 4.0 (Katso: \url{https://www.stat.fi/org/lainsaadanto/copyright.html})
#'
#' Tietojen haku tapahtuu \code{pxweb} R-paketilla \url{http://github.com/ropengov/pxweb}
#'
#' @param municipality numero, merkkijono tai vektori. esim. \code{186}, \code{"186"}, \code{"KU186"}, \code{"Järvenpää"}, \code{c("KU186", "KU020")} tai \code{"*"} (kaikki)
#' @return lista tibblejä. 1.
#' @return lista:
#'   \itemize{
#'     \item \code{data} tibble: hedelmällisyyskertoimet kunnittain naisen iän mukaan
#'     \item \code{ref} merkkijono: Viitetieto tietolähteeseen
#'  }
#'  tibbleissä:
#'
#'  \code{sex}: 1 = mies, 2 = nainen, 0 = sukupuolet yhteensä
#'
#'  \code{age}: age: ikä täysinä vuosina ( 14 - 50)
#'
#' @examples
#' fert_data <- get_fertility_data()        # hedelmällisyyskertoimet iän mukaan kunnittain
#' fert_data <- get_fertility_data("KU186") # Järvenpään hedelmällisyyskertoimet
#'
#' @export

get_fertility_data <-function(municipality = "*") {

  # Viitetieto
  ref <- paste0("Lähde: Tilastokeskus, Väestöennuste 2019: Hedelmällisyyskertoimet äidin iän mukaan kunnittain, 2019-2040. Viitattu ", Sys.Date())

  muni_code <- convert_municipality(municipality, to = "muni_code")

  fert_query_list <-
    list("Kunta" = muni_code,
         "Vuosi" = "*",
         "Ikä" = "*",
         "Tiedot" = "*")

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

  fertility <- list(data = fert_data, ref = ref)
}
