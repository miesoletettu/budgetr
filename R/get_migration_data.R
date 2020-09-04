#' Get migration data from  the the Statistic Finnish
#'
#' @param muni_code A character vector like "KU186".
#' @param year A character vector. Rates have calculates as average between year - 3 and year.
#' @return In and out migration rates by municipality, age and sex.
#' @examples
#' migr_data <- get_migration_data()        # return in and out migrations rates by municipalities, age, sex
#' migr_data <- get_migration_data("KU186") # return in and out migrations rates by age and sex in Jarvenpaa


get_migration_data <- function(muni_code, year) {
  require(magrittr)
  mig_query_list <-
    list("Alue"= muni_code,
         "Vuosi"= as.character(seq(year - 3, year)),
         "Sukupuoli" =  "*",
         "Ikä" = "*",
         "Tiedot" =  "*")

  # sex: 1 = mies, 2 = nainen
  # age5: ikäryhmän alaraja
  mig_data <-
    pxweb::pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/vrm/muutl/statfin_muutl_pxt_11a2.px",
                     query = mig_query_list) %>%
    as.data.frame(column.name.type = "text", variable.value.type = "code") %>%
    tibble::as_tibble() %>%
    dplyr::filter(Age != "SSS" & Sex != "SSS") %>%
    dplyr::transmute(muni = as.numeric(stringr::str_sub(Area, 3, 5)),
                     year = as.numeric(Year),
                     age5 = as.numeric(stringr::str_sub(Age, 1, stringr::str_locate(Age, "-")[,1] - 1)),
                     sex = as.numeric(Sex),
                     inmigration = `Intermunicipal in-migration`,
                     outmigration = `Intermunicipal out-migration`,
                     netmigration = `Intermunicipal net migration`) %>%
    dplyr::group_by(muni, sex, age5) %>%
    dplyr::summarise(inmigration = mean(inmigration), outmigration = mean(outmigration), netmigration = mean(netmigration)) %>%
    dplyr::ungroup()

  # Väestö muuttoliiketilaston mukaisissa ikäryhmissä
  pop_query_list <-
    list("Alue"= muni_code,
         "Ikä" = "*",
         "Sukupuoli" =  "*",
         "Vuosi" = as.character(seq(year, year - 4)),
         "Tiedot" =  "*")

  # sex: 1 = mies, 2 = nainen
  # age5: ikäryhmän alaraja
  pop_data <-
    pxweb::pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/vrm/vaerak/statfin_vaerak_pxt_11re.px",
                     query = pop_query_list) %>%
    as.data.frame(column.name.type = "text", variable.value.type = "code") %>%
    tibble::as_tibble() %>%
    dplyr::filter(Age != "SSS" & Sex != "SSS") %>%
    dplyr::transmute(muni = as.numeric(stringr::str_sub(Area, 3, 5)),
                     year = as.numeric(Year),
                     age5 = pmin(floor(as.numeric(stringr::str_sub(Age, 1, 3)) / 5) * 5, 75),
                     sex = as.numeric(Sex),
                     population = `Population 31 Dec`) %>%
    dplyr::group_by(muni, year, age5, sex) %>%
    dplyr::summarise(population = sum(population)) %>%
    dplyr::group_by(muni, age5, sex) %>%
    dplyr::summarise(population = mean(population)) %>%
    dplyr::ungroup()

  # Muuttoalttius
  inmigration_total <- sum(mig_data$inmigration)
  mig_data <- mig_data %>%
    dplyr::left_join(pop_data, by = c("muni", "sex", "age5")) %>%
    dplyr::mutate(outmigrationpropensity = dplyr::if_else(population == 0, 0, outmigration / population),
                  inmigrationpropensity = dplyr::if_else(population == 0, 0, inmigration / population),
                  inmigrationintensity = inmigration / inmigration_total,
                  netmigrationpropensity = dplyr::if_else(population == 0, 0, netmigration / population))
}
