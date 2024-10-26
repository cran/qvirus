#' Generate Payoff List Based on Quantum Gates and Parameters
#'
#' This function generates a list of payoffs for different combinations of quantum gate matrices.
#' The payoffs are computed for two sets of parameters, where each set defines different values 
#' for the phenotype payoffs (v and V) in the quantum game model. The names of the payoffs are 
#' dynamically generated based on the provided gate names.
#'
#' @param gates A named list of gate matrices. Each element of the list is a quantum gate matrix 
#'   (e.g., T, X, Id, H, Z, S, Y). The names of the list elements are used to create payoff names dynamically.
#' @param alpha Numeric value for the first parameter set, defining payoff for v×v.
#' @param beta Numeric value for the first parameter set, defining payoff for v×V.
#' @param gamma Numeric value for the first parameter set, defining payoff for V×v.
#' @param theta Numeric value for the first parameter set, defining payoff for V×V.
#' @param alpha2 Numeric value for the second parameter set, defining payoff for v×v.
#' @param beta2 Numeric value for the second parameter set, defining payoff for v×V.
#' @param gamma2 Numeric value for the second parameter set, defining payoff for V×v.
#' @param theta2 Numeric value for the second parameter set, defining payoff for V×V.
#'
#' @return A list of payoffs where the list names correspond to the gate combinations, 
#'   and the values represent the computed payoffs based on the input parameters and gate matrices.
#' @export
#'
#' @examples
#' library(qsimulatR)
#' gates <- list(
#'   T = Tgate(2),
#'   X = X(2),
#'   Id = Id(2),
#'   H = H(2),
#'   Z = Z(2),
#'   S = S(2),
#'   Y = Y(2)
#' )
#' alpha <- 0.5; beta <- 0.2; gamma <- 0.3; theta <- 0.1
#' alpha2 <- 0.6; beta2 <- 0.25; gamma2 <- 0.35; theta2 <- 0.15
#' payoffs_list <- payoffs_list(gates, alpha, beta, gamma, theta, alpha2, beta2, gamma2, theta2)
#' print(payoffs_list)
payoffs_list <- function(gates, alpha, beta, gamma, theta, alpha2, beta2, gamma2, theta2) {
  
  # Ensure the gates input is a named list for easy access
  if (!is.list(gates) || is.null(names(gates))) {
    stop("Gates must be a named list of matrices.")
  }
  
  # Initialize an empty list to store the payoffs
  pays_list <- list()
  
  # Iterate over the gates and generate names dynamically
  for (gate1_name in names(gates)) {
    for (gate2_name in names(gates)) {
      
      # Create dynamic names for phenotypes v and V for each gate combination
      v_name <- paste("v", gate1_name, gate2_name, sep = "_")
      V_name <- paste("V", gate1_name, gate2_name, sep = "_")
      
      # Compute the payoffs for the given gate matrices
      pays_list[[v_name]] <- phen_hiv(gates[[gate1_name]]@M, gates[[gate2_name]]@M, alpha, beta, gamma, theta)[3]
      pays_list[[V_name]] <- phen_hiv(gates[[gate1_name]]@M, gates[[gate2_name]]@M, alpha, beta, gamma, theta)[4]
      
      # Create dynamic names for the second set of parameters (alpha2, beta2, gamma2, theta2)
      v_name_b <- paste("v", gate1_name, gate2_name, "b", sep = "_")
      V_name_b <- paste("V", gate1_name, gate2_name, "b", sep = "_")
      
      # Compute the payoffs for the second set of parameters
      pays_list[[v_name_b]] <- phen_hiv(gates[[gate1_name]]@M, gates[[gate2_name]]@M, alpha2, beta2, gamma2, theta2)[3]
      pays_list[[V_name_b]] <- phen_hiv(gates[[gate1_name]]@M, gates[[gate2_name]]@M, alpha2, beta2, gamma2, theta2)[4]
    }
  }
  
  return(pays_list)
}
