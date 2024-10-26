test_that("`summary.interaction()` works as expected", {
  # Load test data
  data(cd_3)
  data(vl_3)
  
  # Create interactions object
  interaction_obj <- create_interactions(cd_3[,-1], vl_3[,-1])
  
  # Capture the output of the summary method
  expect_snapshot({
    summary(interaction_obj)
  })
})