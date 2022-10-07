t_peptide <- "INITSSASQEGTRLN"

ref_dat <- data.frame(Exposure = c(0, 0.001, 0.167, 1, 5, 25, 120, 1440),
                      avg_mass = c(1590.712565, 1590.751171, 1598.75472106, 1.599519093e+03, 1.599866606e+03, 1.600456288e+03, 1.600696624e+03, 1.601164847e+03),
                      err_avg_mass = c(NA, NA, 0.0282742947, 5.566576636e-02, 5.293101952e-02, 4.964456678e-02, 7.940909861e-02, 3.663865611e-02))

times <-  ref_dat[["Exposure"]]
values <- colnames(ref_dat)[-1]

result_tmp <- calculate_exp_masses(t_dat)

###########
## TESTS ##
###########

test_that("data.table-d calculate_exp_masses", 
          expect_s3_class(result_tmp, "data.table"))

test_that("correct values",{
         
  lapply(times, function(time){
    
    lapply(values, function(value){
      
                expect_equal(ref_dat[ref_dat[["Exposure"]] == time, value],
                             result_tmp[result_tmp[["Protein"]] == t_protein &
                                          result_tmp[["State"]] == t_state_1 &
                                          result_tmp[["Sequence"]] == t_peptide &
                                          result_tmp[["Exposure"]] == time, get(value)]
                )
    })
  })   
})

