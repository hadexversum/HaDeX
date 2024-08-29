library(HRaDeX)
library(HaDeX)

data("alpha_dat")

kin_dat_for_agg <- create_uptake_dataset(dat = alpha_dat, states = unique(alpha_dat[["State"]])[3], 
                                 time_0 = 0, time_100 = 1440)

agg_dat <- create_aggregated_uptake_dataset(kin_dat_for_agg)


results <- data.frame(position = c(150, 150, 150, 150, 150), 
           frac_uc = c(30.612998, 43.086626, 50.570774, 64.780313, 93.926741),
           err_frac_uc = c(0.22950843, 0.57844855, 0.36680007, 0.82943585, 0.52671167),
           Exposure = c(0.167, 1, 5, 25, 150))


for(i in 1:nrow(results)){
  
  test_that(
    paste0("Aggregation for ", i, " in ", results[i, "Exposure"]),
    expect_equal(results[i, "frac_uc"],
                         agg_dat[agg_dat[["position"]] == results[i, "position"] & agg_dat[["Exposure"]] == results[i, "Exposure"], "frac_uc"])
  )
  
}
  

# pos = 150
# time = 1440
# filter(kin_dat, Exposure == time) %>%
#      filter(pos >= Start, pos <= End) %>%
#      mutate(weight_1 = 1/MaxUptake) %>%
#      mutate(weight = weight_1 / x_sum) %>%
#      mutate(com = weight*frac_deut_uptake) %>%
#      mutate(com_e = weight * err_frac_deut_uptake,
#             com_e_2 = com_e^2) %>%
#   summarize(uc = sum(com),
#             e = sqrt(sum(com_e_2)))
