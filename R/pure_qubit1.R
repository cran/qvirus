#' Create a normalized pure quantum state for a 1-qubit system.
#'
#' @param theta The parameter theta in radians.
#' @param phi The parameter phi in radians.
#' @param spherical Whether to return coordinates in spherical form (default is FALSE).
#
#' @return A qstate object representing the normalized pure quantum state for a 1-qubit system.
#' @export
#'
#' @examples
#' # Quantum simulator
#' library(qsimulatR)
#' # Define the parameters
#' theta <- pi/4
#' phi <- pi/6
#' # Create the quantum state
#' psi_qubit1 <- pure_qubit1(theta, phi)
#' psi_qubit1
pure_qubit1 <- function(theta, phi, spherical = FALSE) {
  if(spherical == FALSE){
    # Calculate the coefficients
    alpha_0 <- cos(theta/2)
    alpha_1 <- exp(1i*phi) * sin(theta/2)
    
    # Create the quantum state
    psi_qubit1 <- qsimulatR::qstate(nbits = 1, coefs = c(alpha_0, alpha_1), basis = c("|0>", "|1>"))
    
    return(psi_qubit1)
  }
  else if(spherical == TRUE) {
    
    # Create a list containing the Bloch sphere coordinates
    result <- c(x = sin(theta)*cos(phi), y = sin(theta)*sin(phi), z = cos(theta))
    
    return(result)
  }
}