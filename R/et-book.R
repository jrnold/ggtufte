#' Import ET Book font for use in plots
#'
#' There is an option `ggtufte.loadfonts` which -- if set to `TRUE` -- will
#' call `extrafont::loadfonts()` to register non-core fonts with R PDF & PostScript
#' devices. If you are running under Windows, the package calls the same function
#' to register non-core fonts with the Windows graphics device.
#'
#'

#' @md
#' @note This will take care of ensuring PDF/PostScript usage. The location of the
#'   font directory is displayed after the base import is complete. It is highly
#'   recommended that you install them on your system the same way you would any
#'   other font you wish to use in other programs.
#' @references <https://edwardtufte.github.io/et-book/>
#' @source <https://github.com/edwardtufte/et-book>
#' @export
import_et_book <- function() {
  # function adapted from hbrthemes::import_roboto_condensed
  font_dir <- system.file("fonts", package = "ggtufte")

  suppressWarnings(suppressMessages(extrafont::font_import(font_dir,
                                                           prompt = FALSE)))
  message(
    sprintf(
      paste0("You will likely need to install these fonts on your ",
             "system as well.",
             "\n\nYou can find them in [%s]"),
      font_dir)
  )

}

#' ET Book fonts
#'
#' ET Book (ETBembo font-family) comprises five fonts:
#'
#' - `"ETBembo RomanLF"`: Roman (lining figures)
#' - `"ETBembo RomanOSF"`: Roman (oldstyle figures)
#' - `"ETBembo SemiBoldOSF"`: Semi-bold (oldstyle figures)
#' - `"ETBembo BoldLF"`: Bold (lining figures)
#' - `"ETBembo DisplayItalic"`: Display Italic (oldstyle figures)
#'
#' @md
#' @title ET Book fonts R variable aliases
#' @format length 5 character vector. Values are the
#'   font names, and names are the description of the font.
#' @rdname ETBembo
#' @export
#' @examples
#' font_et
font_et <- c("Roman (line figures)" = "ETBembo RomanLF",
             "Roman (old-style figures)" = "ETBembo RomanOSF",
             "Bold (line figures)" = "ETBembo BoldLF",
             "Semi-bold (old-style figures)" = "ETBembo SemiBoldOSF",
             "Display italic" = "ETBembo DisplayItalic")