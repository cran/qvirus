#' Normalize Check Function for qstate Class Object
#' 
#' Check the normalization of a qstate object created by qsimulatR package.
#'
#' @param qstate_obj A quantum state object.
#' @param probs Are probabilities required as output?
#'
#' @return Either the sum of the squared magnitudes of the coefficients of the qstate object or its probabilities.
#' @export
#'
#' @examples 
#' library(qsimulatR)
#' ket0 <- qstate(nbits = 1) 
#' normalize_check(ket0)
normalize_check <- function(qstate_obj, probs = FALSE) { 
  if(probs == FALSE){
    return(sum(Mod(qstate_obj@coefs)^2))
  }
  else if(probs == TRUE){
    probs <- Mod(qstate_obj@coefs)^2
    names(probs) <- qstate_obj@basis
    return(probs)
  }
}