
# function parameterised by: region, # iter, chains

fit_gp_county <- function(county_ts, iter = 2000, chains = 1) {
  
  # Not conditioned on data -------------------------------------------------
  
  stan_data <- list(
    N = nrow(county_ts),
    x = county_ts$year,
    y = county_ts$mort_rate_normal
  )
  
  
  # Condition on data -------------------------------------------------------
  
  x_predict <- c(stan_data$x
                 , (tail(county_ts$year,1) + 1):(tail(county_ts$year,1) + 5))
  N_predict <- length(x_predict)
  N_obs <- length(county_ts$year)
  
  pred_data <- list(N1=nrow(county_ts)
                    , x1=county_ts$year
                    , y1=county_ts$mort_rate_normal
                    , N2=N_predict, x2=x_predict)
  
  pred_fit <- stan(file='stan/method1.stan', data = pred_data, iter = iter, chains = chains)
  
  pred_params <- extract(pred_fit)
  
  # remove the 100 subset
  f_res <- pred_params$f[,(N_obs+1):(N_obs+N_predict)]
  f_res %<>% as_tibble() %>% rownames_to_column(var = "it")
  names(f_res)[2:length(f_res)] <- as.character(x_predict) 
  
  f_res_trans <- f_res %>% pivot_longer(-it,names_to = "year", values_to = "mort_rate")
  f_res_trans %<>% mutate(year = as.numeric(year)
                          , mort_rate = (mort_rate * unique(county_ts$sd_mort) ) + 
                            unique(county_ts$mean_mort)
  )
  
  f_res_trans <- f_res_trans %>% group_by(year) %>% mutate(estimate = mean(mort_rate))
  
  # kernal estimates as list as attribute
  # kernel_est <- list()
  mean(params$rho)
  mean(params$alpha)
  mean(params$sigma)
  
  f_res_trans
  
}



