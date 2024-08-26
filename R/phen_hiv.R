#' Calculate Final State and Payoffs in Quantum Game
#'
#' This function calculates the final quantum state and expected payoffs for two 
#' players in a quantum game based on their strategies. The function uses quantum 
#' gates and unitary transformations to simulate the game dynamics.
#' 
#' @param strategy1 A 2x2 matrix representing the strategy of player 1.
#' @param strategy2 A 2x2 matrix representing the strategy of player 2.
#' @param alpha A numeric value representing the payoff for outcome |00>.
#' @param beta A numeric value representing the payoff for outcome |01>.
#' @param gamma A numeric value representing the payoff for outcome |10>.
#' @param theta A numeric value representing the payoff for outcome |11>. 
#'
#' @return A list containing the final quantum state (`final_state`), the payoffs
#' for each basis state (`payoffs`), and the expected payoffs for player 1 (`pi_v`)
#' and player 2 (`pi_V`).
#' @export
#'
#' @examples
#' library(qsimulatR)
#' strategy1 <- diag(2) # Identity matrix for strategy 1
#' strategy2 <- diag(2) # Identity matrix for strategy 2
#' alpha <- 1
#' beta <- 0.5
#' gamma <- 2
#' theta <- 0.1
#' result <- phen_hiv(strategy1, strategy2, alpha, beta, gamma, theta)
#' print(result)
#' 
#' @references
#' Özlüer Başer, B. (2022). "Analyzing the competition of HIV-1 phenotypes with quantum game theory". 
#' Gazi University Journal of Science, 35(3), 1190--1198. \doi{10.35378/gujs.772616}
phen_hiv <- function(strategy1, strategy2, alpha, beta, gamma, theta) {
  # Define the quantum gates
  I <- diag(2)
  X <- matrix(c(0, 1, 1, 0), nrow=2)
  H <- 1/sqrt(2) * matrix(c(1, 1, 1, -1), nrow=2)
  
  # Define the initial state |00>
  initial_state <- qsimulatR::qstate(nbits = 2, coefs = c(1, 0, 0, 0), basis = c("|00>", "|01>", "|10>", "|11>"))@coefs
  
  # Define the unitary transformation for mutant H gate
  U <- 1/sqrt(2) * (kronecker(I, I) - 1i * kronecker(X, X))
  V <- 1/sqrt(2) * (kronecker(I, I) + 1i * kronecker(X, X))
  
  # Apply the sequence of operations to the initial state
  state_after_U <- U %*% initial_state
  state_after_strategy <- kronecker(strategy1, strategy2) %*% state_after_U
  state_after_V <- V %*% state_after_strategy
  final_state <- U %*% state_after_V
  
  # Apply the inverse unitary transformation
  U_inverse <- Conj(t(U))
  psi_f <- U_inverse %*% final_state
  
  # Calculate the probabilities for each basis state
  prob_00 <- Mod(psi_f[1])^2
  prob_01 <- Mod(psi_f[2])^2
  prob_10 <- Mod(psi_f[3])^2
  prob_11 <- Mod(psi_f[4])^2
  
  
  
  # Calculate the expected payoffs for players v and V
  pi_v <- alpha * prob_00 + beta * prob_01 + gamma * prob_10 + theta * prob_11
  pi_V <- alpha * prob_00 + gamma * prob_01 + beta * prob_10 + theta * prob_11
  
  # Return the final state and the payoffs
  list(
    final_state = psi_f,
    payoffs = c("|00> alpha" = prob_00, "|01> beta" = prob_01, "|10> gamma" = prob_10, "|11> theta" = prob_11),
    pi_v = pi_v,
    pi_V = pi_V
  )
}