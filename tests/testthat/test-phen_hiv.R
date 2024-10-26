test_that("phen_hiv() works as expected", {
  library(qsimulatR)
  strategy1 <- diag(2) # Identity matrix for strategy 1
  strategy2 <- diag(2) # Identity matrix for strategy 2
  alpha <- 1
  beta <- 0.5
  gamma <- 2
  theta <- 0.1 
  expect_snapshot(
    print(
      phen_hiv(strategy1, strategy2, alpha, beta, gamma, theta) 
    )
  )
})