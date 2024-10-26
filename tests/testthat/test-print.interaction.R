test_that("`summary.interaction()` works as expected", {
  data(cd_3)
  data(vl_3)
  
  # Create interaction object using the provided data
  interaction_obj <- create_interactions(cd_3, vl_3)
  
  # Test the summary of the interaction object
  expect_snapshot(
    print(
      summary(interaction_obj)
    )
  )
})
