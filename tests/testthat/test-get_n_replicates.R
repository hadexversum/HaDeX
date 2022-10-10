n_rep <- get_n_replicates(t_dat)

test_that("class is right",
          expect_is(n_rep, "numeric"))

test_that("right value", 
          expect_equal(n_rep, 4))
