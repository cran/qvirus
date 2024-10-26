test_that("`payoffs_list` works as expected", {
  
  # Define some example gate matrices
  library(qsimulatR)
  gates <- list(
    T = Tgate(2),
    X = X(2),
    Id = Id(2),
    H = H(2),
    Z = Z(2),
    S = S(2),
    Y = Y(2)
  )
  
  # Set example payoff parameters
  alpha <- 0.3; beta <- 0.1; gamma <- 0.5; theta <- 0.2
  alpha2 <- 0.35; beta2 <- 0.15; gamma2 <- 0.6; theta2 <- 0.25
  
  # Call the payoffs_list function
  payoffs <- payoffs_list(gates, alpha, beta, gamma, theta, alpha2, beta2, gamma2, theta2)
  
  # Perform snapshot testing
  expect_snapshot(payoffs)
})