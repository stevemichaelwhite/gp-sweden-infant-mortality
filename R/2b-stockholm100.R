f_100 <- pred_params$f[1:100,(N_obs+1):(N_obs+N_predict)]
f_100 %<>% as_tibble() %>% rownames_to_column(var = "it")
names(f_100)[2:length(f_100)] <- as.character(x_predict) 

f_100_trans <- f_100 %>% pivot_longer(-it,names_to = "year", values_to = "mort_rate")
f_100_trans %<>% mutate(year = as.numeric(year)
                        , mort_rate = (mort_rate * unique(stockholm_ts$sd_mort) ) + 
                          unique(stockholm_ts$mean_mort)
)

# f_100_trans <- f_100_trans %>% group_by(year) %>% mutate(estimate = mean(mort_rate))
# everything up to year becomes a function parameterised by: region, # iter, chains
# remove the 100 subset
# repeat for each region

# Un-normalised plot ------------------------------------------------------
# show with original/untransformed data


estimate_100 <- f_100_trans %>% group_by(year) %>% summarise(estimate = mean(mort_rate))
gray_100 <- rep("gray", 100)

res_plots[["stockholm_100"]] <- ggplot(f_100_trans, aes(x = year, y = mort_rate, color = it)) + 
  geom_line(show.legend = FALSE) +
  geom_line(data = estimate_100
            , aes(x = year, y = estimate, color = NULL), show.legend = FALSE) +
  geom_point(data = stockholm_ts
             , aes(x = year, y = mort_rate, color = NULL), colour = "orange", size = 2, show.legend = FALSE) +
  scale_color_manual(values = gray_100) + ggtitle("Stockholm - Estimated mortality rate (100 iterations)")



# Save plot ---------------------------------------------------------------

saveRDS(res_plots, "data/res_plots.rds")