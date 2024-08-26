#' Check if Coefficients of a Qubit State Object are Complex Numbers
#'
#' This function returns the class of the coefficients of a given qubit state object 
#'
#' @param qstate_obj A qubit state object created using the qsimulatR package.
#'
#' @return Coefficients class of given qubit state object
#'
#' @export
#'
#' @examples
#' library(qsimulatR)
#' ket1 <- qstate(nbits = 1, coefs = c(0, 1))
#' complex_check(ket1)
complex_check <- function(qstate_obj) {
  return(qstate_obj@coefs |>
           class()
  )
}