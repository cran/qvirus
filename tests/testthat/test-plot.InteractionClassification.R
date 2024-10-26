test_that("`plot.InteractionClassification()` works as expected", {
  # Load test data
  data("cd_3")
  data("vl_3")
  
  # Create interactions object
  interaction_obj <- create_interactions(cd_3[,-1], vl_3[,-1])
  
  # Create an InteractionClassification object
  class_obj <- InteractionClassification(interaction_obj$vlogs_diff, interaction_obj$cds_diff)
  
  # Use expect_snapshot to check the output of the plot method
  local_edition(3)
  vdiffr::expect_doppelganger("InteractionClassification plot", plot(class_obj))
})