context("quality_control in test")

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

ref_dat <- data.frame(time_100 = c(5, 25, 120, 1440),
                      avg_err_state_1 = c(1.0371101576,  0.7297903193,   0.7442617735,    0.6476667184),
                      sd_err_state_1 = c(0.7664453626,  0.5406361520,   0.4309586306,    0.3544872867),
                      avg_err_state_2 = c(1.3311503092,  1.1825446202,   1.0238714977,    0.8902793214),
                      sd_err_state_2 = c(1.5650941701,  1.2823833668,   0.7456831628,    0.3915988119),
                      avg_diff = c(1.7307477289,  1.4195066191,   1.2888539817,    1.1252678251),
                      sd_diff = c(1.6986261954,  1.3604048521,   0.8254634528,    0.4727564705))
                     
#############
## DATASET ##
#############

result <- create_quality_control_dataset(dat = dat,
                                         protein = "db_CD160", 
                                         state_1 = "CD160",
                                         state_2 = "CD160_HVEM", 
                                         time_t = 1, 
                                         time_0 = 0.001, 
                                         deut_part = 1)

test_that("class is right",
          expect_is(result,
                    "data.frame"))

test_that("size is right",
          expect_equal(length(colnames(result)),
                       7))

times <- ref_dat[["out_time"]]
values <- colnames(ref_dat)[-1]

lapply(times, function(time){
  
  lapply(values, function(value){
    
    test_name <- paste0("quality_control_dataset-", time, "min-", value)
    test_that(test_name,
              expect_equal(ref_dat[ref_dat[["time_100"]] == time, value],
                           result[result[["time_100"]] == time, value]
              )
    )
    
  })
  
})

##########
## DATA ##
##########

show_result <- show_quality_control_data(result)

test_that("show_result class is right",
          expect_is(show_result,
                    "data.frame"))

test_that("show_result size is right",
          expect_equal(length(colnames(show_result)),
                       4))

##########
## PLOT ##
##########

quality_control_plot <- plot_quality_control(result)

expect_doppelganger("Quality control Plot", quality_control_plot)


          