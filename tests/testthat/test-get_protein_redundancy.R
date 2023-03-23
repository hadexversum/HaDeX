test_that("Redundancy in right format",
          expect_type(
            get_protein_redundancy(t_dat),
            "double"
          )
)

test_that("Redundancy for test file",
          expect_equal(
            get_protein_redundancy(t_dat, 132),
            4.56
          )
)

test_that("Redundancy protein legth is ok",
          expect_error(
            get_protein_redundancy(t_dat, 100)
          )
)