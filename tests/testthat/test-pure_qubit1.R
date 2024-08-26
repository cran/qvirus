test_that("`pure_qubit1()` works", {
  library(qsimulatR)
  theta <- pi/4
  phi <- pi/6
  psi_qubit1 <- pure_qubit1(theta, phi)
  expect_snapshot(
    print(
      psi_qubit1 
    )
  )
})