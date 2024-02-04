# read data
Simulation_Data <- readr::read_csv(here::here("01_analysis", "output", "Simulation_Data.csv")) |>
  dplyr::select(-1)



# a. Histogram
make_histogram <- function(bin){
  plot_data <- ggplot2::ggplot(data = Simulation_Data, ggplot2::aes(x = X)) +
               ggplot2::geom_histogram(binwidth = bin) +
               ggplot2::ggtitle(paste0("binwidth = ", bin)) +
               ggplot2::labs(x = "X",
                             y = "Frequency") +
               ggplot2::theme_bw()
  return(plot_data)
}
Histogram_10bin   <- make_histogram(bin = 10)
Histogram_10bin
Histogram_100bin  <- make_histogram(bin = 100)
Histogram_100bin
Histogram_1000bin <- make_histogram(bin = 1000)
Histogram_1000bin

save_histogram <- function(plot_data, bin){
  ggplot2::ggsave(plot = plot_data,
                  filename = paste0("Histogram_", bin, ".png"),
                  path = here::here("01_analysis", "output"))
}
save_histogram(Histogram_10bin, bin = "10bin")
save_histogram(Histogram_100bin, bin = "100bin")
save_histogram(Histogram_1000bin, bin = "1000bin")



# b. Kernel density
make_kernel_density_plot <- function(adjust_bandwidth){
  plot(density(Simulation_Data$X, adjust = adjust_bandwidth),
                    xlim = c(-100,700), ylim = c(0, 0.05),
                    main = paste0("bandwidth = ", adjust_bandwidth, "*default"))
}
make_kernel_density_plot(adjust_bandwidth =  1)
make_kernel_density_plot(adjust_bandwidth =  0.2)
make_kernel_density_plot(adjust_bandwidth =  5)



# c.Quantile regression
estimatr::lm_robust(Z ~ X, data = Simulation_Data) 

quantile_regression <- function(Tau){
  quantreg::rq(Z ~ X, tau = Tau, data = Simulation_Data)
}
quantile_regression(0.5)
quantile_regression(0.25)
quantile_regression(0.75)

