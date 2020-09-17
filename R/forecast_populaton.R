#' Väestökehityksne ennustaminen
#'
#' Ennustetaan kunnan väestön ja väestörakenteen kehitystä. Hedelmällisyys ja kuolevuus perustuvat Tilastokeskuksen väestöennusteeseen.
#' Poismuutto perustuu Tilastokeskuksen muuttoliiketilastoon. Väestömäärän kokonaismuutos (netto) annetaan parametrina ja kunnan sisäänmuutto kalibroidaan siten, että ennuste vastaa annettia nettoväestömuutosta.
#' Sisäänmuuttajien ikä- ja sukupuolijakauma perustuu Tilastokeskuksen muuttoliiketilastoon.
#'
#' @param municipality numero, merkkijono tai vektori. esim. \code{186}, \code{"186"}, \code{"KU186"}, \code{"Järvenpää"}, \code{c("KU186", "KU020")} tai \code{"*"} (kaikki)
#' @param netchangerate numeerinen vektori. prosenttuaalinen vuotuinen nettomuutos. Jos vektori on lyhyempi kuin ennustejakso, niin vektorin viimeistä arvoa toistetaan.
#' @param netchangecount numeerinen vektori. absoluuttinen vuotuinen nettomuutos. Jos vektori on lyhyempi kuin ennustejakso, niin vektorin viimeistä arvoa toistetaan. Jos tämä on annettu, niin silloin \code{netchangerate} ohitetaan.
#' @param year numero tai merkkijono. Perusvuosi, jonka lopun väestön kehitystä ennustetaan eteenpäin.
#' @param fert_adj  numeerinen vektori. hedelmällisyyden mukautuskerroin ennustevuosille. Jos vektori on lyhyempi kuin ennustejakso, niin vektorin viimeistä arvoa toistetaan.
#' @param mort_adj  numeerinen vektori. kuolevuuden mukautuskerroin ennustevuosille. Jos vektori on lyhyempi kuin ennustejakso, niin vektorin viimeistä arvoa toistetaan.
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
#' @examples
#' popul_data <- forecast_population(muni_code = "KU186",  netchangerate = c(0.015, 0.015, 0.02, 0.02, 0.015), year = 2019)) #
#'
#' @export

forecast_population <- function(municipality = "KU186", netchangerate = 0.015, netchangecount = NA, year = 2019, adj = indicies$adj) {

  # Viitetieto
  ref <- paste0("Lähde: Tilastokeskus, ", Sys.Date(), ", muokattu")

  muni_code <- convert_municipality(municipality, to = "muni_code")
  year <- as.numeric(year)

  fert_data <- get_fertility_data(muni_code)$data
  mort_data <- get_mortality_data(muni_code)$data
  mig_data <- get_migration_data(muni_code, (year-3):year)$data
  pop_data <- get_population_data(muni_code, year)$data$end

  population_start <- pop_data
  for (counter in 1:11) {
    # väestön nettomuutos
    if (is.na(netchangecount)) {
      netchange <- netchangerate[pmin(counter, length(netchangerate))] * sum(population_start$population)
    } else {
      netchange <- netchangecount[pmin(counter, length(netchangecount))]
    }

    # syntyneet, eli uusi -1-ikäisten korortti
    births <- population_start %>% dplyr::filter(sex == 2 & age >=14 & age <= 50) %>%
      dplyr::left_join(fert_data, by = c("muni", "sex", "age"), suffix = c("", "_fert")) %>%
      dplyr::left_join(adj, by = "year") %>%
      dplyr::summarise(births = round(sum(population * fertility * fert))) %>%
      dplyr::transmute(age = -1, `1` = round(0.5 * births), `2` = births - `1`) %>%
      tidyr::pivot_longer(-age, names_to = "sex", values_to = "population") %>%
      dplyr::transmute(muni = as.numeric(stringr::str_sub(muni_code, 3, 5)),
                    year = !!year + counter,
                    age,
                    sex = as.numeric(sex),
                    population)

    # yhdistetään vuoden alun populaatioon syntyneet ja generoidaan kuolleet ja poismuuttaneet
    population_start <- population_start %>%
      dplyr::mutate(year = year + 1) %>%
      dplyr::union(births) %>%
      dplyr::left_join(mort_data, by = c("muni", "year", "sex", "age")) %>%
      dplyr::left_join(adj, by = "year") %>%
      dplyr::mutate(deaths = -population * mortality * mort) %>%
      dplyr::mutate(age5 = pmin(floor(age / 5) * 5, 75)) %>%
      dplyr::left_join(mig_data %>% dplyr::transmute(muni, age5 = age, sex, population5 = population, outmigrationpropensity, inmigrationintensity), by = c("muni", "age5", "sex")) %>%
      dplyr::mutate(outmigration = dplyr::if_else(is.na(outmigrationpropensity * population), 0, -outmigrationpropensity * population),
                    population5 = dplyr::if_else(is.na(population5), 0, population5),
                    outmigrationpropensity = dplyr::if_else(is.na(outmigrationpropensity), 0, outmigrationpropensity),
                    inmigrationintensity = dplyr::if_else(is.na(inmigrationintensity), 0, inmigrationintensity))

    # lisätään sisäänmuutto nettomuutoksesta
    inmigration_total <- netchange - sum(births$population) - sum(population_start$deaths) - sum(population_start$outmigration)
    population_start <- population_start %>%
      dplyr::mutate(inmigration = dplyr::if_else(age == -1 | age >= 80, 0, inmigrationintensity * inmigration_total / 5),
                    age = age + 1,
                    population = round(population + deaths + outmigration + inmigration)) %>%
      dplyr::filter(age<=100) %>%
      dplyr::select(muni, year, age, sex, population) %>%
      dplyr::arrange(year, age, sex)
    pop_data <- pop_data %>%
      dplyr::union(population_start)
  }
  pop_data_start <- pop_data %>%
    dplyr::filter(year < 2030) %>%
    dplyr::mutate(year = year + 1)

  pop_data_end <- pop_data %>%
    dplyr::filter(year > !!year)

  pop_data_avg <- pop_data_start %>%
    dplyr::union(pop_data_end) %>%
    dplyr::group_by(muni, year, sex, age) %>%
    dplyr::summarise(population = mean(population)) %>%
    dplyr::ungroup()

  pupulation_forecasted <- list(data = list(start = pop_data_start, end = pop_data_end, avg = pop_data_avg), ref = ref)
}
