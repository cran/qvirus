#' Create Six Important States on the Bloch Sphere
#'
#' This function creates and returns six important states on the Bloch Sphere based on the specified vector numbers.
#'
#' @param vec_num A numeric vector specifying the indices of the states to include. Valid indices are 1 to 6.
#'
#' @return A list containing the selected quantum states based on the input vector vec_num.
#'
#' @export
#'
#' @examples
#' library(qsimulatR)
#' # Select and return states 1, 3, and 5
#' six_state(c(1, 3, 5))
six_state <- function(vec_num = c(1, 2, 3, 4, 5, 6)) {
  states <- list(
    qsimulatR::qstate(nbits = 1),
    qsimulatR::qstate(nbits = 1, coefs = c(0, 1)),
    qsimulatR::qstate(nbits = 1, coefs = c(1/sqrt(2), 1/sqrt(2))),
    qsimulatR::qstate(nbits = 1, coefs = c(1/sqrt(2), -1/sqrt(2))),
    qsimulatR::qstate(nbits = 1, coefs = c(1/sqrt(2), (1i)/sqrt(2))),
    qsimulatR::qstate(nbits = 1, coefs = c(1/sqrt(2), -(1i)/sqrt(2)))
  )
  
  selected_states <- states[vec_num]
  return(selected_states)
}