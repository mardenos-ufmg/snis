pkgs = c(
  "readxl",
  "mice",
  "tidyr",
  "dplyr",
  "writexl",
  "mice",
  "readr"
)

pak::pak( setdiff(pkgs, rownames(installed.packages())) )

for (pkg in pkgs) {
  library(pkg, character.only = TRUE)
}
