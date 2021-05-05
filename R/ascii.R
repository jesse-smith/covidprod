#' Convert a `character` Vector to ASCII Encoding
#'
#' @param string A `character` vector
#'
#' @param trans Should non-ASCII characters be transliterated?
#'
#' @return The input, converted to ASCII representation
#'
#' @keywords internal
chr_to_ascii <- function(chr, trans = TRUE) {
  if (trans) {
    chr %>%
      stringi::stri_trans_general("Any-Latin;Latin-ASCII") %>%
      stringi::stri_enc_toascii()
  } else {
    stringi::stri_enc_toascii(chr)
  }
}
