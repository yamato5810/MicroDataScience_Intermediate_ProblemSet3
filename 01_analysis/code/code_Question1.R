set.seed(1)

Simulation_Data <- tibble::tibble(
  logX =  rnorm(n = 500, mean = 3, sd = sqrt(2))
) |> 
  dplyr::mutate(X = exp(logX)) |> 
  dplyr::mutate(Y = purrr::map_dbl(.x = (logX + rnorm(n = 500, mean = 0, sd = 1)), .f = \(x) ifelse(x >= 4, 1, 0))) |> 
  dplyr::mutate(speed    = X + rnorm(n = 500, mean = 0, sd = 1)) |> 
  dplyr::mutate(shoot    = X + rnorm(n = 500, mean = 0, sd = 1)) |> 
  dplyr::mutate(height   = X + rnorm(n = 500, mean = 0, sd = 1)) |> 
  dplyr::mutate(age      = X + rnorm(n = 500, mean = 0, sd = 1)) |> 
  dplyr::mutate(teamwork = X + rnorm(n = 500, mean = 0, sd = 1)) |> 
  dplyr::mutate(Z = teamwork + 0.5*shoot + 0.5*speed + rnorm(n = 500, mean = 0, sd = 1))
                
write.csv(Simulation_Data,
          file = here::here("01_analysis", "output", "Simulation_Data.csv"))        


