sequence <- get_peptide_sequence(t_dat, start = t_peptide_start, end = t_peptide_end)

test_that("class is right",
          expect_is(sequence, "character"))

test_that("right value", 
          expect_equal(sequence, t_peptide))
