.onAttach <- function(libname, pkgname) {
  version <- utils::packageDescription(pkgname, fields = "Version")

  msg <- paste0(
    "
* ", pkgname, " version ", version, " - Copyright (C) 2021 David Barnett
* Website: https://david-barnett.github.io/microViz/
* Useful? Consider citing: https://zenodo.org/record/4644057
* Silence: suppressPackageStartupMessages(library(microViz))
")

  packageStartupMessage(msg)
}
