#' Muuttoliiketietojen haku Tilastokeskuksen StatFin-tietokannasta
#'
#' Funktio hakee Tilastokeskuksen StatFin-tietokannasta kuntien välinen muutto iän (5-v.), sukupuolen ja muuton suunnan mukaan alueittain, 1990-2019 ja
#' muokkaa tästä asukkaan iästä ja sukupuolesta riippuvan poismuuttoalttiuden, sekä sisäänmuuttajalle iän ja sukupuolen todennäköisyysjakauman.
#' Vuotta 2004 vanhempia tietoja ei haeta.
#' Poismuuttoalttiuden laskennassa käytetään kunnan väestöjakaumaan (katso: \code{\link{get_population_data}})
#' Funktio laskee anttejun vuosien keskiarvon.
#'
#' Tilastokeskuksen StatFin-tietokannan tiedot on julkaistu lisenssillä CC BY 4.0 (Katso: \url{https://www.stat.fi/org/lainsaadanto/copyright.html})
#'
#' Tietojen haku tapahtuu \code{pxweb} R-paketilla \url{http://github.com/ropengov/pxweb}
#'
#' @param municipality numero, merkkijono tai vektori. esim. \code{186}, \code{"186"}, \code{"KU186"}, \code{"Järvenpää"}, \code{c("KU186", "KU020")} tai \code{"*"} (kaikki)
#' @param year numero, merkkijono tai numeerinen tai merkkijono vektori. Esim. \code{2019}, \code{2017:2019}, \code{c("2010", "2019")} tai \code{"*"} (kaikki))
#' @return lista:
#'   \itemize{
#'     \item \code{data} tibble:
#'       \itemize{
#'         \item \code{muni}: kunnan numeerinen koodi
#'         \item \code{year}: vuosi
#'         \item \code{age}: age: ikä täysinä vuosina
#'         \item \code{sex}: 1 = mies, 2 = nainen, 0 = sukupuolet yhteensä
#'         \item \code{outmigrationpropensity}
#'         \item \code{inmigrationpropensity}
#'         \item \code{inmigrationintensity}
#'         \item \code{netmigrationpropensity}
#'      }
#'    \item \code{ref} merkkijono: Viitetieto tietolähteeseen
#'  }
#'

#' @examples
#' migr_data <- get_migration_data()        # muuttointensiteetit kaikille kunnille iän ja sukupuolen mukaan
#' migr_data <- get_migration_data("KU186") #
#'
#' @export

get_migration_data <- function(municipality = "*", year = "*") {

  # Viitetieto
  ref <- paste0("Lähde: Tilastokeskus, Kuntien välinen muutto iän (5-v.), sukupuolen ja muuton suunnan mukaan alueittain, 1990-2019, Väestö iän (1-v.) ja sukupuolen mukaan alueittain, 1972-2019, ", Sys.Date())

  muni_code <- convert_municipality(municipality, to = "muni_code")
  if (year == "*"){year <- 2004:2019}
  year <- intersect(sort(unique(c(as.numeric(year), as.numeric(year) -1))), 2003:2019)

  mig_query_list <-
    list("Alue"= muni_code,
         "Vuosi"= as.character(year),
         "Sukupuoli" =  "*",
         "Ikä" = "*",
         "Tiedot" =  "*")

  mig_data <-
    pxweb::pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/vrm/muutl/statfin_muutl_pxt_11a2.px",
                     query = mig_query_list) %>%
    as.data.frame(column.name.type = "text", variable.value.type = "code") %>%
    tibble::as_tibble() %>%
    dplyr::filter(Age != "SSS" & Sex != "SSS") %>%
    dplyr::transmute(muni = as.numeric(stringr::str_sub(Area, 3, 5)),
                     year = as.numeric(Year),
                     age = as.numeric(stringr::str_sub(Age, 1, stringr::str_locate(Age, "-")[,1] - 1)),
                     sex = as.numeric(Sex),
                     inmigration = `Intermunicipal in-migration`,
                     outmigration = `Intermunicipal out-migration`,
                     netmigration = `Intermunicipal net migration`) %>%
    dplyr::group_by(muni, sex, age) %>%
    dplyr::summarise(inmigration = mean(inmigration), outmigration = mean(outmigration), netmigration = mean(netmigration)) %>%
    dplyr::ungroup()

  # Väestö muuttoliiketilaston mukaisissa ikäryhmissä
  pop_data <- get_population_data(muni_code, year, group_age = seq(5, 75, by = 5))$data$start %>%
    dplyr::group_by(muni, age, sex) %>%
    dplyr::summarise(year = max(year), population = sum(population)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(muni, year, age, sex, population)

  # Muuttoalttius
  inmigration_total <- sum(mig_data$inmigration)
  mig_data <- mig_data %>%
    dplyr::left_join(pop_data, by = c("muni", "sex", "age")) %>%
    dplyr::mutate(outmigrationpropensity = dplyr::if_else(population == 0, 0, outmigration / population),
                  inmigrationpropensity = dplyr::if_else(population == 0, 0, inmigration / population),
                  inmigrationintensity = inmigration / inmigration_total,
                  netmigrationpropensity = dplyr::if_else(population == 0, 0, netmigration / population))

  migration <- list(data = mig_data, ref = ref)
}
