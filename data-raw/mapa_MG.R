## code to prepare `mapa_MG` dataset goes here

mapa_MG = sf::st_read("inst/extdata/geojs-31-mun.json", quiet = TRUE)

usethis::use_data(mapa_MG, overwrite = TRUE)
