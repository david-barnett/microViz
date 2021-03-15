.onAttach = function(libname, pkgname) {
  version = packageDescription(pkgname, fields = "Version")

  msg = paste0("========================================
", pkgname, " version ", version, "
--- Copyright (C) 2021 David Barnett ---

GitHub: https://github.com/david-barnett/microViz
Help: https://david-barnett.github.io/microViz/

This message can be suppressed by:
  suppressPackageStartupMessages(library(microViz))
========================================
")

  packageStartupMessage(msg)
}
