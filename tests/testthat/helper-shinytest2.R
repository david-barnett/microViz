# scrub environment-dependent values from shinytest2 expect_values() json

# html dependency versions (change with e.g. gdtools/crosstalk updates)
scrub_dep_versions <- function(lines) {
  lines <- sub('("version": )"[^"]*"', '\\1"<version>"', lines)
  sub('("href": "[A-Za-z.-]+)-[0-9][0-9.]*"', '\\1-<version>"', lines)
}

# DT Scroller page length (depends on browser row height rendering)
scrub_dt_scroller_length <- function(lines) {
  sub('("length": )[0-9]+', '\\1"<n>"', lines)
}

scrub_tax_fix_interactive_json <- function(lines) {
  scrub_dt_scroller_length(scrub_dep_versions(lines))
}
