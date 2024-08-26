test_that("`normalize_check()` works as expected", {
  library(qsimulatR)
  ket0 <- qstate(nbits = 1) 
  expect_snapshot(
    print(
      normalize_check(ket0) 
    )
  )
})