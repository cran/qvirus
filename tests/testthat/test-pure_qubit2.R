test_that("`pure_qubit2()` works", {
  library(qsimulatR)
  theta1 <- pi/3
  theta2 <- pi/4
  phi1 <- pi/6
  phi2 <- pi/5
  psi_qubit2 <- pure_qubit2(theta1, theta2, phi1, phi2)
  expect_snapshot(
    print(
      psi_qubit2 
    )
  )
})