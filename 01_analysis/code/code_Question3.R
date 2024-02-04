# read data
Simulation_Data <- readr::read_csv(here::here("01_analysis", "output", "Simulation_Data.csv")) |>
  dplyr::select(-1)



# a. Probit
Probit_marginal <- mfx::probitmfx(formula = Y ~ logX, data = Simulation_Data)
OLS <- estimatr::lm_robust(Y ~ logX, data = Simulation_Data)

Probit <- glm(Y ~ logX, family = binomial(probit), data = Simulation_Data)
Probit_predicted <- ggeffects::ggpredict(Probit, terms = "logX[all]", nsim = 100)
ggplot2::ggplot(data = Simulation_Data)+
  ggplot2::geom_point(ggplot2::aes(x = logX, y = Y)) +
  ggplot2::geom_abline(intercept = OLS$coefficients[1], slope = OLS$coefficients[2], color = "red") +
  ggplot2::geom_line(ggplot2::aes(x = x, y = predicted), data = Probit_predicted, color = "blue") +
  ggplot2::labs(title = "3.a. Comparison between Probit and Linear Probability") +
  ggplot2::theme_bw()



# b. Lasso

make_bootstrap_sample <- function(seed){
  set.seed(seed)
  bootstrap_sample <- dplyr::sample_n(Simulation_Data, size = 10000, replace = TRUE) |>
    as.matrix()
}
bootstrap_sample1 <- make_bootstrap_sample(1)
bootstrap_sample2 <- make_bootstrap_sample(2)
bootstrap_sample3 <- make_bootstrap_sample(3)
bootstrap_sample4 <- make_bootstrap_sample(4)
bootstrap_sample5 <- make_bootstrap_sample(5)
bootstrap_sample_list <- list(bootstrap_sample1, bootstrap_sample2, bootstrap_sample3, bootstrap_sample4, bootstrap_sample5)

calculate_coefficients_Lasso <- function(sample, lambda){
  coef(glmnet::glmnet(x = sample[, c("logX", "speed", "shoot", "height", "age", "teamwork")],
                      y = sample[, "Y"],
                      alpha = 1,
                      lambda = lambda))
}
purrr::map(bootstrap_sample_list, \(x) calculate_coefficients_Lasso(sample = x, lambda = 0.1))
purrr::map(bootstrap_sample_list, \(x) calculate_coefficients_Lasso(sample = x, lambda = 0.01))
purrr::map(bootstrap_sample_list, \(x) calculate_coefficients_Lasso(sample = x, lambda = 0.001))


# cf. optimal lambda
find_optimal_lambda <- function(sample){
  optimal_lambda <- glmnet::cv.glmnet(x = sample[, c("logX", "speed", "shoot", "height", "age", "teamwork")],
                                      y = sample[, "Y"],
                                      alpha = 1)
  return(optimal_lambda$lambda.min)
}
optimal_lambda1 <- find_optimal_lambda(bootstrap_sample1)
optimal_lambda2 <- find_optimal_lambda(bootstrap_sample2)
optimal_lambda3 <- find_optimal_lambda(bootstrap_sample3)
optimal_lambda4 <- find_optimal_lambda(bootstrap_sample4)
optimal_lambda5 <- find_optimal_lambda(bootstrap_sample5)
optimal_lambda_list <- list(optimal_lambda1, optimal_lambda2, optimal_lambda3, optimal_lambda4, optimal_lambda5)

purrr::map2(bootstrap_sample_list, optimal_lambda_list, \(x, y) calculate_coefficients_Lasso(sample = x, lambda = y))