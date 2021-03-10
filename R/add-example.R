add2 <- function(x, y = NULL) {
  if (is.null(y)) return(x + x)
  s <- x + (y * 2)
  z <- 100
  return(x + y)
}
