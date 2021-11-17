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

f_ci <- f_res_trans %>% group_by(year) %>% summarise(CI_84 = quantile(mort_rate, probs = 84/100)
                                                  , CI_16 = quantile(mort_rate, probs = 16/100)
                                                  , CI_97_5 = quantile(mort_rate, probs = 97.5/100)
                                                  , CI_02_5 = quantile(mort_rate, probs = 2.5/100)
                                                  , estimate = unique(estimate))
  
res_plots[["stockholm_ci"]] <- f_ci %>% ggplot(aes(x = year, y = estimate)) + geom_line() + 
  geom_ribbon(aes(ymin = CI_16, ymax = CI_84, fill = "16%-84%"), alpha = .25) + 
  geom_ribbon(aes(ymin = CI_02_5, ymax = CI_97_5, fill = "2.5%-97.5%"), alpha = .25) + 
  scale_fill_manual(name = "", values = c("16%-84%" = "red", "2.5%-97.5%" = "blue")) +
  geom_point(data = stockholm_ts
             , aes(x = year, y = mort_rate), colour = "orange", size = 2) +
  ggtitle("Stockholm CI (1000 it)") + theme(legend.position="bottom")

# Save plot ---------------------------------------------------------------
saveRDS(res_plots, "data/res_plots.rds")
