test_that("`nearest_payoff()` works as expected", {
  library(dplyr)
  library(qsimulatR)
  data(vl_3)
  data(cd_3)
  
  # Create interaction object and classification
  interaction_obj <- create_interactions(cd_3[,-1], vl_3[,-1])
  class_obj <- InteractionClassification(interaction_obj$vlogs_diff, interaction_obj$cds_diff)
  
  # Define gates and payoff parameters
  gates <- list(
    T = Tgate(2),
    X = X(2),
    Id = Id(2),
    H = H(2),
    Z = Z(2),
    S = S(2),
    Y = Y(2)
  )
  alpha <- -0.4518303; beta <- -1.654192; gamma <- -0.2638399; theta <- -0.5619246
  alpha2 <- -0.04186888; beta2 <- -3.01931; gamma2 <- 0.3922753; theta2 <-1.055114
  
  # Generate the payoffs list
  payoffs_list <- payoffs_list(gates, alpha, beta, gamma, theta, alpha2, beta2, gamma2, theta2)
  
  # Filter data based on specific patient IDs
  filtered_data <- class_obj$data |> 
    bind_cols(id = cd_3$ID) |> 
    relocate(id) |> 
    filter(id %in% c(37, 102, 148, 174, 180, 205))
  
  # Apply the nearest_payoff function to each vl_diff
  filtered_data <- filtered_data |>
    rowwise() |>
    mutate(nearest = list(nearest_payoff(vl_diff, payoffs_list))) |>
    mutate(nearest_payoff = nearest$value,
           payoff_name = nearest$name) |>
    select(-nearest) # Remove intermediate column
  
  # Take a snapshot of the results
  expect_snapshot(filtered_data)
  
})
