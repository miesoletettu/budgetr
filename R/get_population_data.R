#' Väestötietojen haku Tilastokeskuksen StatFin-tietokannasta
#'
#' Funktio hakee Tilastokeskuksen StatFin-tietokannasta joko Väestö iän (1-v.) ja sukupuolen mukaan alueittain tai
#' väestö iän (1-v.) ja sukupuolen mukaan alueittain kunkin tilastovuoden aluejaolla.
#' Väestötiedot ovat tietokannassa vuodesta 2003 lähtien. Kunkin vuoden väestötieto on hetkeltä year-12-31.
#'
#' Tilastokeskuksen StatFin-tietokannan tiedot on julkaistu lisenssillä CC BY 4.0 (Katso: \url{https://www.stat.fi/org/lainsaadanto/copyright.html})
#'
#' Tietojen haku tapahtuu \code{pxweb} R-paketilla \url{http://github.com/ropengov/pxweb}
#'
#' @param municipality numero, merkkijono tai vektori. esim. \code{186}, \code{"186"}, \code{"KU186"}, \code{"Järvenpää"}, \code{c("KU186", "KU020")} tai \code{"*"} (kaikki)
#' @param year numero, merkkijono tai numeerinen tai merkkijono vektori. Esim. \code{2019}, \code{2017:2019}, \code{c("2010", "2019")} tai \code{"*"} (kaikki))
#' @param group_sex looginen muuttuja \code{default = FALSE}. Ilmoittaa summataanko molemmat sukupuolet yhteen.
#' @param group_age looginen muuttuja tai numeerinen vektori  \code{default = FALSE} = ei ryhmittelyä,  \code{TRUE} = kaikki yhteensä, numeerinen vektori ryhmien alarajoista.
#' @param current looginen muuttuja \code{default = TRUE} = käytetään nykyistä kuntajakoa, \code{FALSE} = käytetään ko. vuoden aluejakoa.
#' @return lista:
#'   \itemize{
#'     \item \code{data} lista:
#'       \itemize{
#'         \item \code{start} tibble: Vuoden alun väestö
#'           \itemize{
#'             \item \code{muni}: kunnan numeerinen koodi
#'             \item \code{year}: vuosi
#'             \item \code{age}: age: ikä täysinä vuosina
#'             \item \code{sex}: 1 = mies, 2 = nainen, 0 = sukupuolet yhteensä
#'             \item \code{population}: väestö vuoden alussa
#'           }
#'         \item \code{end} tibble: Vuoden lopun väestö
#'           \itemize{
#'             \item \code{muni}: kunnan numeerinen koodi
#'             \item \code{year}: vuosi
#'             \item \code{age}: age: ikä täysinä vuosina
#'             \item \code{sex}: 1 = mies, 2 = nainen, 0 = sukupuolet yhteensä
#'             \item \code{population}: väestö vuoden lopussa
#'           }
#'         \item \code{avg} tibble: Vuoden keskimääräinen väestö
#'           \itemize{
#'             \item \code{muni}: kunnan numeerinen koodi
#'             \item \code{year}: vuosi
#'             \item \code{age}: age: ikä täysinä vuosina
#'             \item \code{sex}: 1 = mies, 2 = nainen, 0 = sukupuolet yhteensä
#'             \item \code{population}: väestön määrä keskimäärin vuoden aikana
#'           }
#'      }
#'    \item \code{ref} merkkijono: Viitetieto tietolähteeseen
#'  }
#'
#' @examples
#' pop_data <- get_population_data()                              # kaikkien kuntien tiedot keskiasukasmäärät kaikilta vuosilta
#' pop_data <- get_population_data("KU186", year = c(2018, 2019)) # Järvenpään väestö vuosilta 2018 ja 2019
#'
#' @export

get_population_data <- function(municipality = "*", year = "*", group_sex = FALSE, group_age = FALSE, current = TRUE, lazy = TRUE) {

  # Viitetieto
  ref <- paste0("Lähde: Tilastokeskus, Väestö 31.12. muuttujina Alue, Ikä, Sukupuoli, Vuosi ja Tiedot. Viitattu ", Sys.Date())

  if (municipality == "*") {
    muni_code <-  "*"
  } else {
    muni_code <- convert_municipality(municipality, to = "muni_code")
  }

  if (year == "*"){
    year <- 2004:2019
  } else {
    year <- as.numeric(year)
  }

  years <- as.character(intersect(sort(unique(c(as.numeric(year), as.numeric(year) -1))), 2003:2019))

  if (current) {
    pop_query_list <-
      list("Alue"= muni_code,
           "Ikä" = "*",
           "Sukupuoli" =  "*",
           "Vuosi" = years,
           "Tiedot" =  "*")

    if (lazy) {
      pop_data <- populations$current$data$end %>%
        filter(muni %in% convert_municipality(muni_code, to = "muni") & year %in% !!year)
    } else {
      pop_data <-
        pxweb::pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/vrm/vaerak/statfin_vaerak_pxt_11re.px",
                         query = pop_query_list) %>%
        as.data.frame(column.name.type = "text", variable.value.type = "code") %>%
        tibble::as_tibble() %>%
        dplyr::filter(stringr::str_sub(Area, 1 , 2) == "KU", Age != "SSS" & Sex != "SSS") %>%
        dplyr::transmute(muni = as.numeric(stringr::str_sub(Area, 3 , 5)),
                         year = as.numeric(as.character(Year)),
                         sex = as.numeric(Sex),
                         age = as.numeric(stringr::str_sub(Age, 1, 3)),
                         population = `Population 31 Dec`)
    }
  } else {
    if (lazy) {
      pop_data <- populations$historic$data$end %>%
        filter(muni %in% convert_municipality(muni_code, to = "muni") & year %in% !!year)
    } else {
      pop_query_list <-
      list("Kunta"= muni_code,
           "Ikä" = "*",
           "Sukupuoli" =  "*",
           "Vuosi" = years,
           "Tiedot" =  "*")

    pop_data <-
      pxweb::pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/vrm/vaerak/statfin_vaerak_pxt_11rf.px",
                       query = pop_query_list) %>%
      as.data.frame(column.name.type = "text", variable.value.type = "code") %>%
      tibble::as_tibble() %>%
      dplyr::filter(stringr::str_sub(Municipality, 1 , 2) == "KU", Age != "SSS" & Sex != "SSS") %>%
      dplyr::transmute(muni = as.numeric(stringr::str_sub(Municipality, 3 , 5)),
                       year = as.numeric(as.character(Year)),
                       sex = as.numeric(Sex),
                       age = as.numeric(stringr::str_sub(Age, 1, 3)),
                       population = `Population 31 Dec`)
    }
  }


  if (group_sex) {
    pop_data <- pop_data %>%
      dplyr::group_by(muni, year, age) %>%
      dplyr::summarise(sex = 0, population = sum(population)) %>%
      dplyr::ungroup()
  }

  if (is.numeric(group_age)) {
    ages <- sort(unique(c(group_age, 0, 101)))
    age <- 0:100
    age_group <- 0
    for (i in 1:101) {age_group[i] = sum(i - 1 >= ages)}

    pop_data <- pop_data %>%
      dplyr::left_join(tibble::tibble(age, age_group), by = "age") %>%
      dplyr::group_by(muni, year, age_group, sex) %>%
      dplyr::summarise(age = min(age), population = sum(population)) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(muni, year, age, sex, population)

  } else if (group_age) {
    pop_data <- pop_data %>%
      dplyr::group_by(muni, year, sex) %>%
      dplyr::summarise(age = min(age), population = sum(population)) %>%
      dplyr::ungroup()
    }


  pop_data_end <- pop_data %>%
    dplyr::filter(year %in% !!year)

  pop_data_start <- pop_data %>%
    dplyr::filter(year %in% (!!year - 1)) %>%
    dplyr::mutate(year = year + 1)

  pop_data_avg <- pop_data_start %>%
    dplyr::left_join(pop_data_end, by = c("muni", "year", "sex", "age"), suffix = c("_start", "_end")) %>%
    dplyr::transmute(muni, year, sex, age, population = (population_start + population_end) / 2)

  population <- list(data = list(start = pop_data_start, end = pop_data_end, avg = pop_data_avg), ref = ref)
}
