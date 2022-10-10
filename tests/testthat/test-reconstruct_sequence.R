test_that("class is right",
          expect_is(reconstruct_sequence(t_dat), 
                    "character"))

dat_sequence <- "INITSSASQEGTRLNLICTVWHKKEEAEGFVVFLCKDRSGDCSPETSLKQLRLKRDPGIDGVGEISSQLMFTISQVTPLHSGTYQCCARSQKSGIRLQGHFFSILFxxxxxxxxxxxxxxxxxxFSHNEGTL"

test_that("example sequence1 is right length",
          expect_equal(str_length(reconstruct_sequence(t_dat)), 
                       132))

test_that("sequence1 is correct",
          expect_equal(reconstruct_sequence(t_dat),
                       dat_sequence))

dat_sequence_2 <- "xxxxxxxxxxxxxxxxVPIDIDKTKVKGEGHVEGEKIENPDTGLYYDEYLRQVIDVLETDKHFREKLQTADIEEIKSGKLSRELDLVSHHVRTRLDELKRQEVARLRMLIKAKMDSVQDTGIDHQALLKQFEHLNHQNPDTFEPKDLDMLIKAATSDLENYDKTRHEEFKKYEMxxxxxxxxxxxxLDEEKRQREESKFGEMxxxxxxxxxxxxxxxxxxxKEVWEEADGLDPNEFDPKTFFKLHDVNNDRFLDEQELEAxFTKELEKVYDPKNEEDDMVEMEEERLxxxxHVMNEVDINKDRLVTLEEFLRATEKKEFLEPDSWETLDQQQLFTEDELKEFESHISQQEDELRKKAEELQKQKEELQRQHDQLQAQEQELQQVVKQMEQKKLQQANPPAGPAGELK"

test_that("example sequence2 is right length",
          expect_equal(str_length(reconstruct_sequence(t_dat_2)), 
                       407))

test_that("sequence2 is correct",
          expect_equal(reconstruct_sequence(t_dat_2),
                       dat_sequence_2))
