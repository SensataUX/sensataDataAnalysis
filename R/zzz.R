.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This is version ", packageVersion(pkgname),
                        " of ", pkgname, ".
------------------------------------------------
IMPORTANT: For now this package only works when
connected to the internet to download fonts.
Sensata UX (c)
------------------------------------------------")
}

.onAttach <- function(libname, pkgname) {
  # Load fonts
  sysfonts::font_add_google(name = "Open Sans", family = "Open Sans")
  sysfonts::font_add_google(name = "News Cycle", family = "News Cycle")
  sysfonts::font_add_google(name = "Montserrat", family = "montserrat")
  showtext::showtext_auto()

}
