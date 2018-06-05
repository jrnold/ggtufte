# adapted from hrbrthemes zzz.R
.onAttach <- function(libname, pkgname) {
  # nocov start
  if (.Platform$OS.type == "windows")  { # nocov start
    if (interactive()) {
      packageStartupMessage("Registering Windows fonts with R")
    }
    extrafont::loadfonts("win", quiet = TRUE)
  }

  if (getOption("ggtufte.loadfonts", default = FALSE)) {
    if (interactive()) {
      packageStartupMessage("Registering PDF & PostScript fonts with R")
    }
    extrafont::loadfonts("pdf", quiet = TRUE)
    extrafont::loadfonts("postscript", quiet = TRUE)
  }

  fnt <- extrafont::fonttable()
  if (!any(grepl("ETBembo", fnt$FamilyName))) {
    packageStartupMessage("NOTE: The ET Book fonts are required to use this theme.")
    packageStartupMessage("      Please use ggtufte::import_et_book() to install the ET Book fonts.")
  } # nocov end

}