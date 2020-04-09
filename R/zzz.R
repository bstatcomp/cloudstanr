.onAttach <- function(...) {
  ver <- utils::packageVersion("cloudstanr")
  packageStartupMessage("This is cloudstanr version ", ver)
}

.onLoad <- function(...) {
  cloudstanr_initialize()
}
