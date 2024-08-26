#' Calculate the Conjugate Transpose of a Quantum State
#'
#' This function calculates the conjugate transpose of a given quantum state represented by a qstate object.
#'
#' @param state A qstate object for which the conjugate transpose is to be calculated.
#'
#' @return The conjugate transpose of the input quantum state.
#'
#' @export
#'
#' @examples
#' library(qsimulatR)
#' library(qvirus)
#' # Calculate the conjugate transpose of ket0
#' state <- six_state(1)[[1]]
#' conjugate_transpose(state)
conjugate_transpose <- function(state) {
  if (!inherits(state, "qstate")) {
    stop("Input must be a qstate object.")
  }
  return(Conj(t(state@coefs)))
}