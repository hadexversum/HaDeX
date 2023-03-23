test_that("doesn't work",
          expect_error(read_hdx("protein")))

test_that("class is right",
          expect_is(t_dat, 
                    "hdx_data"))