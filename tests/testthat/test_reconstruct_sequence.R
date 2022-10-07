dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
dat_sequence <- "INITSSASQEGTRLNLICTVWHKKEEAEGFVVFLCKDRSGDCSPETSLKQLRLKRDPGIDGVGEISSQLMFTISQVTPLHSGTYQCCARSQKSGIRLQGHFFSILFxxxxxxxxxxxxxxxxxxFSHNEGTL"

test_that("example sequence1 is right length",
          expect_equal(str_length(reconstruct_sequence(dat)), 
                       132))

test_that("class is right",
          expect_is(reconstruct_sequence(dat), 
                    "character"))

test_that("sequence1 is correct",
          expect_equal(reconstruct_sequence(dat),
                       dat_sequence))


dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_190304_Nucb2_EDTA_CaCl2_test02_clusterdata.csv"))
dat_sequence <- "xxxxxxxxxxxxxxxxVPIDIDKTKVKGEGHVEGEKIENPDTGLYYDEYLRQVIDVLETDKHFREKLQTADIEEIKSGKLSRELDLVSHHVRTRLDELKRQEVARLRMLIKAKMDSVQDTGIDHQALLKQFEHLNHQNPDTFEPKDLDMLIKAATSDLENYDKTRHEEFKKYEMxxxxxxxxxxxxLDEEKRQREESKFGEMxxxxxxxxxxxxxxxxxxxKEVWEEADGLDPNEFDPKTFFKLHDVNNDRFLDEQELEAxFTKELEKVYDPKNEEDDMVEMEEERLxxxxHVMNEVDINKDRLVTLEEFLRATEKKEFLEPDSWETLDQQQLFTEDELKEFESHISQQEDELRKKAEELQKQKEELQRQHDQLQAQEQELQQVVKQMEQKKLQQANPPAGPAGELK"
  
test_that("example sequence2 is right length",
          expect_equal(str_length(reconstruct_sequence(dat)), 
                      407))

test_that("sequence2 is correct",
          expect_equal(reconstruct_sequence(dat),
                       dat_sequence))
