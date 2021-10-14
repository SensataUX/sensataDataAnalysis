.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This is version ", packageVersion(pkgname),
                        " of ", pkgname, ".
                        Remember to install Open Sans and Montserrat fonts on your machine for the theme to work perfectly.
                        Sensata UX (c)")
}
