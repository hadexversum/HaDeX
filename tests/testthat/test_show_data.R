context("show data in test")

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

#################
#################
## DEUT_UPTAKE ##
#################
#################

uptake_dat <- create_uptake_dataset(dat)

######################
## SHOW_UPTAKE_DATA ##
######################

test_that("Theo Frac show_uptake_data colnames",
          expect_equal(colnames(show_uptake_data(uptake_dat,
                                                 theoretical = T,
                                                 fractional = T)),
                       c("Protein", "Sequence", "ID", "State", "Start", "End", "Exposure", "Theo Frac DU [%]", "U(Theo Frac DU) [%]")
          )
)

test_that("Frac show_uptake_data colnames",
          expect_equal(colnames(show_uptake_data(uptake_dat,
                                                 theoretical = F,
                                                 fractional = T)),
                       c("Protein", "Sequence", "ID", "State", "Start", "End", "Exposure", "Frac DU [%]", "U(Frac DU) [%]")
          )
)

test_that("Theo show_uptake_data colnames",
          expect_equal(colnames(show_uptake_data(uptake_dat,
                                                 theoretical = T,
                                                 fractional = F)),
                       c("Protein", "Sequence", "ID", "State", "Start", "End", "Exposure", "Theo DU [Da]", "U(Theo DU) [Da]")
          )
)

test_that("show_uptake_data colnames",
          expect_equal(colnames(show_uptake_data(uptake_dat,
                                                 theoretical = F,
                                                 fractional = F)),
                       c("Protein", "Sequence", "ID", "State", "Start", "End", "Exposure", "DU [Da]", "U(DU) [Da]")
          )
)

######################
######################
## DIFF_DEUT_UPTAKE ##
######################
######################

diff_uptake_dat <- create_diff_uptake_dataset(dat)

###########################
## SHOW_DIFF_UPTAKE_DATA ##
###########################

test_that("Theo Frac show_diff_uptake_data colnames",
          expect_equal(colnames(show_diff_uptake_data(diff_uptake_dat,
                                                      theoretical = T,
                                                      fractional = T)),
                       c("Protein", "ID", "Sequence", "Start", "End", "Exposure", "Theo Frac Diff DU [%]", "U(Theo Frac Diff DU) [%]")
          )
)

test_that("Frac show_diff_uptake_data colnames",
          expect_equal(colnames(show_diff_uptake_data(diff_uptake_dat,
                                                      theoretical = F,
                                                      fractional = T)),
                       c("Protein", "ID", "Sequence",  "Start", "End", "Exposure", "Frac Diff DU [%]", "U(Frac Diff DU) [%]")
          )
)

test_that("Theo show_diff_uptake_data colnames",
          expect_equal(colnames(show_diff_uptake_data(diff_uptake_dat,
                                                      theoretical = T,
                                                      fractional = F)),
                       c("Protein", "ID", "Sequence", "Start", "End", "Exposure", "Theo Diff DU [Da]", "U(Theo Diff DU) [Da]")
          )
)

test_that("show_diff_uptake_data colnames",
          expect_equal(colnames(show_diff_uptake_data(diff_uptake_dat,
                                                      theoretical = F,
                                                      fractional = F)),
                       c("Protein", "ID", "Sequence", "Start", "End", "Exposure", "Diff DU [Da]", "U(Diff DU) [Da]")
          )
)

######################################
## SHOW_DIFF_UPTAKE_DATA_CONFIDENCE ##
######################################

test_that("Theo Frac show_diff_uptake_data_confidence colnames",
          expect_equal(colnames(show_diff_uptake_data_confidence(diff_uptake_dat,
                                                                 theoretical = T,
                                                                 fractional = T)),
                       c("Protein", "Sequence", "ID", "Start", "End", "Exposure", "Theo Frac Diff DU [%]", "U(Theo Frac Diff DU) [%]", "Valid At 0.98", "Valid At 0.99")
          )
)

test_that("Theo show_diff_uptake_data_confidence colnames",
          expect_equal(colnames(show_diff_uptake_data_confidence(diff_uptake_dat,
                                                                 theoretical = T,
                                                                 fractional = F)),
                       c("Protein", "Sequence", "ID", "Start", "End", "Exposure", "Theo Diff DU [Da]", "U(Theo Diff DU) [Da]", "Valid At 0.98", "Valid At 0.99")
          )
)

test_that("Frac show_diff_uptake_data_confidence colnames",
          expect_equal(colnames(show_diff_uptake_data_confidence(diff_uptake_dat,
                                                                 theoretical = F,
                                                                 fractional = T)),
                       c("Protein", "Sequence", "ID", "Start", "End", "Exposure", "Frac Diff DU [%]", "U(Frac Diff DU) [%]", "Valid At 0.98", "Valid At 0.99")
          )
)

test_that("show_diff_uptake_data_confidence colnames",
          expect_equal(colnames(show_diff_uptake_data_confidence(diff_uptake_dat,
                                                                 theoretical = F,
                                                                 fractional = F)),
                       c("Protein", "Sequence", "ID", "Start", "End", "Exposure", "Diff DU [Da]", "U(Diff DU) [Da]" , "Valid At 0.98", "Valid At 0.99")
          )
)

#######################
## SHOW_VOLCANO_DATA ##
#######################

vol_dat <- create_volcano_dataset(dat)

test_that("create_volcano_dataset colnames",
          expect_equal(colnames(show_volcano_data(vol_dat)),
                       c( "Sequence", "Start", "End", "Exposure", "Diff DU [Da]", "U(Diff DU) [Da]", "-log(P value)", "P value", "Valid At 0.98")
          )
)

#######################
## SHOW_SUMMARY_DATA ##
#######################

result_tmp <- show_summary_data(dat, 
                                confidence_limit_1 = 0.98,
                                confidence_limit_2 = 0.99,
                                overlap_distribution_data = create_overlap_distribution_dataset(dat, 
                                                                                                protein_sequence = reconstruct_sequence(dat)))

test_that("show_summary_data columnames",
          expect_equal(colnames(result_tmp),
                       c("Name", "Value")
          )
)


test_that("show_summary_data dimentions",
          expect_equal(dim(result_tmp),
                       c(7, 2)
          )
)

