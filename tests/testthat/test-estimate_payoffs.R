test_that("estimate_payoffs returns correct output", {
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
  pliegues <- 2
  repeticiones <- 1
  rejilla <- 2
  semilla <- 123
  predictions_df <- viralmodels::viralpreds(target, pliegues, repeticiones, rejilla, semilla, traindata)
  
  result <- estimate_payoffs(class_obj, predictions_df)
  
  # Use snapshot testing
  expect_snapshot(result)
})
