context("getters in test")

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

###################################
## get_protein_coverage ###########
###################################

test_that("Coverage in right format",
          expect_type(
            get_protein_coverage(dat),
            "double"
          )
)

test_that("Coverage for test file",
          expect_equal(
            get_protein_coverage(dat, 132),
            86.36
          )
)

test_that("Coverage protein legth is ok",
          expect_error(
            get_protein_coverage(dat, 100)
          )
)

###################################
## get_protein_redundancy #########
###################################

test_that("Redundancy in right format",
          expect_type(
            get_protein_redundancy(dat),
            "double"
          )
)

test_that("Redundancy for test file",
          expect_equal(
            get_protein_redundancy(dat, 132),
            4.56
          )
)

test_that("Redundancy protein legth is ok",
          expect_error(
            get_protein_redundancy(dat, 100)
          )
)
