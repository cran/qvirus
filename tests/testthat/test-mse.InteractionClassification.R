test_that("`mse.InteractionClassification()` works as expected", {
  # Load test data
  data(cd_3)
  data(vl_3)
  
  # Create interactions object
  interaction_obj <- create_interactions(cd_3[,-1], vl_3[,-1])
  
  # Create an InteractionClassification object
  class_obj <- InteractionClassification(interaction_obj$vlogs_diff, interaction_obj$cds_diff)
  
  # Capture the output of the calculate_errors method
  expect_snapshot({
    mse(class_obj)
  })
})
