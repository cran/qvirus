test_that("`summary.payoffs()` works as expected", {
  library(dplyr)
  library(earth)
  library(baguette)
  library(rules)
  library(kknn)
  library(viralmodels)
  # Load required data
  data(vl_3)
  data(cd_3)
  
  # Create interaction object and classification
  interaction_obj <- create_interactions(cd_3[,-1], vl_3[,-1])
  class_obj <- InteractionClassification(interaction_obj$vlogs_diff, interaction_obj$cds_diff)
  
  # Prepare predictions
  traindata <- interaction_obj[c(2, 5)] |> dplyr::as_tibble()
  target <- "vlogs_diff"
  predictions_df <- viralmodels::viralpreds(target, 2, 1, 2, 123, traindata)
  
  # Estimate payoffs
  payoffs_results <- estimate_payoffs(class_obj, predictions_df)
  
  # Use expect_snapshot() to store the summary result
  expect_snapshot({summary(payoffs_results)})
})