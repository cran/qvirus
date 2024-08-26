test_that("`complex_check()` works as expected", {
  library(qsimulatR)
  ket1 <- qstate(nbits = 1, coefs = c(0, 1))
  expect_snapshot(
    print(
      complex_check(ket1) 
    )
  )
})