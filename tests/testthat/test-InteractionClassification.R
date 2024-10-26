test_that("`InteractionClassification()` works as expected", {
  # Load test data
  data(cd_3)
  data(vl_3)
  
  # Create interactions object
  interaction_obj <- create_interactions(cd_3[,-1], vl_3[,-1])
  
  # Use expect_snapshot to check the output
  expect_snapshot(
    InteractionClassification(interaction_obj$vlogs_diff, interaction_obj$cds_diff)
  )
})