context("calculations in test")

library(data.table)

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

chosen_protein <- "db_CD160"
chosen_state <- "CD160"
chosen_peptide <- "INITSSASQEGTRLN"

##########################
## CALCULATE_EXP_MASSES ##
##########################

ref_dat <- data.frame(Exposure = c(0, 0.001, 0.167, 1, 5, 25, 120, 1440),
                      avg_mass = c(1590.712565, 1590.751171, 1598.75472106, 1.599519093e+03, 1.599866606e+03, 1.600456288e+03, 1.600696624e+03, 1.601164847e+03),
                      err_avg_mass = c(NA, NA, 0.0282742947, 5.566576636e-02, 5.293101952e-02, 4.964456678e-02, 7.940909861e-02, 3.663865611e-02))


times <-  ref_dat[["Exposure"]]
values <- colnames(ref_dat)[-1]

result_tmp <- calculate_exp_masses(dat)
result_tmp <- data.table(result_tmp) ##!!
lapply(times, function(time){

  lapply(values, function(value){

    test_name <- paste0("calculate_exp_masses-", time, "min-", value)
    test_that(test_name,
              expect_equal(ref_dat[ref_dat[["Exposure"]] == time, value],
                           result_tmp[result_tmp[["Protein"]] == chosen_protein &
                                        result_tmp[["State"]] == chosen_state &
                                        result_tmp[["Sequence"]] == chosen_peptide &
                                        result_tmp[["Exposure"]] == time, get(value)]
              )
    )

  })

})

###################
## CALCULATE_MHP ##
###################

test_that(paste0("MHP for ", chosen_peptide),
          expect_equal(round(calculate_MHP(chosen_peptide, mono = F)),
                       round(dat[dat[["Sequence"]] == chosen_peptide, "MHP"][1])
          )
)

test_that(paste0("mono MHP for ", chosen_peptide),
          expect_equal(round(calculate_MHP(chosen_peptide, mono = T)),
                       round(dat[dat[["Sequence"]] == chosen_peptide, "MHP"][1] - 1 )[[1]]
          )
)

## TODO: CHECK
# ref_dat <- dat %>%
#   select(Sequence, MHP) %>%
#   unique(.) %>%
#   mutate(MHP = round(MHP))
# 
# lapply(ref_dat[["Sequence"]], function(sequence){
# 
#   test_that(paste0("MHP for ", sequence),
#             expect_equal(round(calculate_MHP(sequence, mono = F)),
#                          ref_dat[ref_dat[["Sequence"]] == sequence, "MHP"]
#             )
#   )
# 
# })

