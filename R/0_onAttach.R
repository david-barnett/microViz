
.onLoad <- function(libname, pkgname) {
  rlang::run_on_load()
}

rlang::on_load(rlang::local_use_cli(inline = TRUE))


.onAttach <- function(libname, pkgname) {
  version <- utils::packageDescription(pkgname, fields = "Version")

  rlang::inform(
    class = "packageStartupMessage",
    message = c(
      paste("{.strong", pkgname, "version", version, "- Copyright (C) 2022 David Barnett}"),
      "!" = "Website: \t{.url https://david-barnett.github.io/microViz}",
      "v" = 'Useful? \tFor citation details, run: {.code citation("microViz")}',
      "x" = "Silence? {.code suppressPackageStartupMessages(library(microViz))}"
    )
  )
}
