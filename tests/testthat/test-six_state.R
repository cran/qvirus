test_that("`six_state()` works", {
  library(qsimulatR)
  # Select and return states 1, 3, and 5
  expect_snapshot(
    print(
      six_state(c(1, 3, 5))
    )
  )
})