test_that("`conjugate_transpose()` works", {
  library(qsimulatR)
  library(qvirus)
  state <- six_state(1)[[1]]
  expect_snapshot(
    print(
      conjugate_transpose(state)
    )
  )
})