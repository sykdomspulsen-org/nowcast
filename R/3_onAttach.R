#' @import data.table ggplot2
#' @importFrom magrittr %>%
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0(
    "nowcast ",
    # utils::packageDescription("nowcast")$Version,
    "\n",
    "https://docs.sykdomspulsen.no/nowcast"
  ))
}
