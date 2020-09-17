#' Kuolevuustietojen haku Tilastokeskuksen StatFin-tietokannasta
#'
#' Funktio hakee Tilastokeskuksen StatFin-tietokannasta Väestöennuste 2019: Kuolleisuuskertoimet iän ja sukupuolen mukaan kunnittain, 2019-2040.
#' Kuolleisuuskertoimet muuttuvat vuosittain. Tilastokeskuksen ennusteessa on kuolevuudet 99 vuoden ikään saakka. Tämä funktio palautta viimeisenä ikäryhmänä 100-vuotiaat, joille kuolevuus on 1.
#'
#' Tilastokeskuksen StatFin-tietokannan tiedot on julkaistu lisenssillä CC BY 4.0 (Katso: \url{https://www.stat.fi/org/lainsaadanto/copyright.html})
#'
#' Tietojen haku tapahtuu \code{pxweb} R-paketilla \url{http://github.com/ropengov/pxweb}
#'
#' @param municipality numero, merkkijono tai vektori. esim. \code{186}, \code{"186"}, \code{"KU186"}, \code{"Järvenpää"}, \code{c("KU186", "KU020")} tai \code{"*"} (kaikki)
#' @return lista tibblejä. 1.
#' @return lista:
#'   \itemize{
#'     \item \code{data} tibble: kuolleisuuskertoimet kunnittain vuoden, iän ja sukupuolen mukaan
#'     \itemize{
#'       \item \code{muni}: kunnan numeerinen koodi
#'       \item \code{year}: vuosi
#'       \item \code{age}: age: ikä täysinä vuosina
#'       \item \code{sex}: 1 = mies, 2 = nainen, 0 = sukupuolet yhteensä
#'       \item \code{mortality}: kuolevuus
#'     }
#'     \item \code{ref} merkkijono: Viitetieto tietolähteeseen
#'  }
#'
#' @examples
#' mort_data <- get_mortality_data()        # Kaikkien kuntien kuolevuusennuste vuosille 2019 - 2040 iän ja sukuolen mukaan
#' mort_data <- get_mortality_data("KU186") # Järvenpään kuolevuusennuste vuosille 2019 - 2040 iän ja sukuolen mukaan
#'
#' @export

get_mortality_data <- function(municipality = "*") {

  # Viitetieto
  ref <- paste0("Lähde: Tilastokeskus, Väestöennuste 2019: Kuolleisuuskertoimet iän ja sukupuolen mukaan kunnittain, 2019-2040. Viitattu ", Sys.Date())

  muni_code <- convert_municipality(municipality, to = "muni_code")

  mort_query_list <-
    list("Kunta" = muni_code,
         "Vuosi" = "*",
         "Sukupuoli" =  "*",
         "Ikä" =  "*",
         "Tiedot" =  "*")

  # sex: 1 = mies, 2 = nainen
  # age: ikä vuoden alussa, -1 = vuoden aikana syntyneneet

  mort_data <-
    pxweb::pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/vrm/vaenn/statfin_vaenn_pxt_129a.px",
                     query = mort_query_list) %>%
    as.data.frame(column.name.type = "text", variable.value.type = "code") %>%
    tibble::as_tibble() %>%
    dplyr::transmute(muni = as.numeric(stringr::str_sub(Municipality, 3, 5)),
                     year = as.numeric(as.character(Year)),
                     age = dplyr::if_else(Age == "alle0", -1, as.numeric(Age), -2),
                     sex = as.numeric(Sex),
                     mortality = `Mortality coefficient, per mille (projection 2019)`/1000)

  # lisätään 100-vuotiaille kuolevuus 1
  mort_data <- mort_data %>%
    dplyr::union(mort_data %>% dplyr::filter(age == 0) %>% dplyr::mutate(age = 100, mortality = 1)) %>%
    dplyr::arrange(year, sex, age)

  mortality <- list(data = mort_data, ref = ref)
}
