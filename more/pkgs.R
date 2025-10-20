install.packages("pak")

pkgs = c(
  "readxl",
  "mice",
  "ggplot2",
  "tidyr",
  "dplyr",
  "writexl",
  "mice",
  "psych",
  "stringr",
  "readr",
  "readODS",
  "ggridges",
  "sf",
  "stringr",
  "geobr",
  "tmap"
)

pak::pak( setdiff(pkgs, rownames(installed.packages())) )

for (pkg in pkgs) {
  if (pkg %in% rownames(installed.packages())) {
    library(pkg, character.only = TRUE)
  } else {
    warning(cat(pkg, "não está instalado!"))
  }
}
