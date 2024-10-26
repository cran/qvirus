test_that("`create_interactions()` works as expected", {
  data(cd_3)
  data(vl_3)
  expect_snapshot(
    print(
      create_interactions(cd_3[, -1], vl_3[, -1]) 
    )
  )
})