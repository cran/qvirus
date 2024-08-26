#' Create a normalized pure quantum state for a 2-qubit system.
#'
#' @param theta1 The parameter theta1 in radians for the first qubit.
#' @param theta2 The parameter theta2 in radians for the second qubit.
#' @param phi1 The phase parameter phi1 in radians for the first qubit.
#' @param phi2 The phase parameter phi2 in radians for the second qubit.
#'
#' @return A qstate object representing the normalized pure quantum state for a 2-qubit system.
#' @export
#'
#' @examples#' 
#' # Quantum simulator
#' library(qsimulatR)
#' # Define the parameters
#' theta1 <- pi/3
#' theta2 <- pi/4
#' phi1 <- pi/6
#' phi2 <- pi/5
#'
#' # Create the quantum state
#' psi_qubit2 <- pure_qubit2(theta1, theta2, phi1, phi2)
#' psi_qubit2
pure_qubit2 <- function(theta1, theta2, phi1, phi2) {
  # Calculate the coefficients
  alpha_00 <- cos(theta1/2) * cos(theta2/2)
  alpha_01 <- cos(theta1/2) * sin(theta2/2) * exp(1i * phi2)
  alpha_10 <- sin(theta1/2) * cos(theta2/2) * exp(1i * phi1)
  alpha_11 <- sin(theta1/2) * sin(theta2/2) * exp(1i * (phi1 + phi2))
  
  # Create the quantum state
  psi_qubit2 <- qsimulatR::qstate(nbits = 2, coefs = c(alpha_00, alpha_01, alpha_10, alpha_11), basis = c("|00>", "|01>", "|10>", "|11>"))
  
  return(psi_qubit2)
}