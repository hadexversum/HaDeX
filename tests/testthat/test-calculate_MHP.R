t_peptide <- "INITSSASQEGTRLN"

test_that(paste0("MHP for ", t_peptide),
          expect_equal(round(calculate_MHP(t_peptide, mono = F)),
                       round(t_dat[t_dat[["Sequence"]] == t_peptide, "MHP"][[1]][1])
          )
)

test_that(paste0("mono MHP for ", t_peptide),
          expect_equal(round(calculate_MHP(t_peptide, mono = T)),
                       round(t_dat[t_dat[["Sequence"]] == t_peptide, "MHP"][1] - 1)[[1]]
          )
)