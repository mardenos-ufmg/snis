.onAttach <- function(libname, pkgname) {
  tmap::tmap_mode("view")
  packageStartupMessage("Modo interativo do tmap ativado: tmap_mode('view')")
}

