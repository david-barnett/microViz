.onAttach <- function(libname, pkgname) {
  version <- utils::packageDescription(pkgname, fields = "Version")

  msg <- paste0(
    "
", pkgname, " version ", version, " - Copyright (C) 2022 David Barnett
* Website: https://david-barnett.github.io/microViz/
* Useful? For citation info, run: citation('microViz')
* Silence: suppressPackageStartupMessages(library(microViz))
"
  )

  packageStartupMessage(msg)
}
