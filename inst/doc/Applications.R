## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(qvirus)

## ----qgame--------------------------------------------------------------------
library(qsimulatR)
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

## ----qgame2-------------------------------------------------------------------
strategy1 <- Z(2)@M # Identity matrix for strategy 1
strategy2 <- Z(2)@M # Identity matrix for strategy 2
alpha <- 1
beta <- 0.5
gamma <- 2
theta <- 0.1
phen_hiv(strategy1, strategy2, alpha, beta, gamma, theta)

