library(rstan)
library(tidyverse)
library(magrittr)
library(ggplot2)


f_res <- pred_params$f[,(N_obs+1):(N_obs+N_predict)]
f_res %<>% as_tibble() %>% rownames_to_column(var = "it")
names(f_res)[2:length(f_res)] <- as.character(x_predict) 

f_res_trans <- f_res %>% pivot_longer(-it,names_to = "year", values_to = "mort_rate")
f_res_trans %<>% mutate(year = as.numeric(year)
                        , mort_rate = (mort_rate * unique(stockholm_ts$sd_mort) ) + 
                          unique(stockholm_ts$mean_mort)
)

f_res_trans <- f_res_trans %>% group_by(year) %>% mutate(estimate = mean(mort_rate))


yconf <- sapply(1:N, function(x) quantile(params$f[,x], probs=c(0.003,0.05,0.32,0.68,0.95,0.997)))

# calc qunatiles over all iterations for ech time point

f_ci <- f_res_trans %>% group_by(year) %>% summarise(CI_68 = quantile(mort_rate, probs = 68/100)
                                                  , CI_32 = quantile(mort_rate, probs = 32/100)
                                                  , CI_95 = quantile(mort_rate, probs = 95/100)
                                                  , CI_05 = quantile(mort_rate, probs = 5/100)
                                                  , estimate = unique(estimate))
  
res_plots[["stockholm_ci"]] <- f_ci %>% ggplot(aes(x = year, y = estimate)) + geom_line() + 
  geom_ribbon(aes(ymin = CI_32, ymax = CI_68, fill = "32%-68%"), alpha = .25) + 
  geom_ribbon(aes(ymin = CI_05, ymax = CI_95, fill = "5%-95%"), alpha = .25) + 
  scale_fill_manual(name = "", values = c("32%-68%" = "red", "5%-95%" = "blue")) +
  geom_point(data = stockholm_ts
             , aes(x = year, y = mort_rate), colour = "orange", size = 2) +
  ggtitle("Stockholm CI (1000 it)") + theme(legend.position="bottom")

# Save plot ---------------------------------------------------------------
saveRDS(res_plots, "data/res_plots.rds")
