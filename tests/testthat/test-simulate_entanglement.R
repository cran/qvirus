test_that("`simulate_entanglement()` works as expected", {
  library(qsimulatR)
  library(qvirus)
  x1 <- qstate(1, coefs = as.complex(c(0.8, 0.6)))
  x2 <- qstate(1, coefs = as.complex(c(0.38, 0.92))) 
  expect_snapshot(
    print(
      simulate_entanglement(x1, x2, iterations = 3, angle= pi/4, verbose = FALSE) 
    )
  )
})