.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This is version ", packageVersion(pkgname),
                        " of ", pkgname, ".
------------------------------------------------
IMPORTANT: For now this package only works when
connected to the internet to download fonts.
Sensata UX (c)
------------------------------------------------")
}

.onLoad <- function(libname, pkgname) {
  sysfonts::font_add_google("Open Sans", "open-sans")
  sysfonts::font_add_google("Montserrat", "Montserrat")
  showtext::showtext_auto()
}
