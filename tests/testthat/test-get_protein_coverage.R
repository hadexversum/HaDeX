test_that("Coverage in right format",
          expect_type(
            get_protein_coverage(t_dat),
            "double"
          )
)

test_that("Coverage for test file",
          expect_equal(
            get_protein_coverage(t_dat, protein_length  = 132),
            86.36
          )
)

test_that("Coverage protein legth is ok",
          expect_error(get_protein_coverage(t_dat, protein_length  = 100))
)