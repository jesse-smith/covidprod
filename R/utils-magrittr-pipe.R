#' @importFrom magrittr %>%
#'
#' @export
magrittr::`%>%`

# Suppress "no visible binding for global variable" when using `.`
if (getRversion() >= "2.15.1") utils::globalVariables(".")
