#' @import data.table ggplot2
#' @importFrom magrittr %>%
.onAttach <- function(libname, pkgname) {
  version <- tryCatch(
    utils::packageDescription("nowcast", fields = "Version"),
    warning = function(w){
      1
    }
  )

  packageStartupMessage(paste0(
    "nowcast ",
    version,
    "\n",
    "https://docs.sykdomspulsen.no/nowcast"
  ))
}
