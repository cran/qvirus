# `nearest_payoff()` works as expected

    Code
      filtered_data
    Output
      # A tibble: 6 x 6
      # Rowwise: 
           id vl_diff cd_diff classification nearest_payoff payoff_name
        <dbl>   <dbl>   <dbl>          <int>          <dbl> <chr>      
      1    37 -0.484   0.0142              2        -0.468  v_T_Id     
      2   102 -3.02    0.466               1        -3.02   V_X_Id_b   
      3   148 -0.493   0.0184              2        -0.507  V_T_H      
      4   174 -1.43   -0.295               4        -1.45   v_T_X      
      5   180 -0.910   0.295               4        -0.959  v_X_S      
      6   205 -0.0335 -0.0995              2        -0.0419 v_X_Y_b    

