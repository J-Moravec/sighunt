context("Reading fasta")

correct_fasta_path = file.path("test_files", "correct.fasta")
wrong_fasta_path = file.path("test_files", "wrong.fasta")

test_that("Reading correct (but ugly) fasta.", {
    expect_equivalent(unlist(read_fasta(correct_fasta_path)),
                 c("ACTG?", "ACTG--", "", "ACTGN")
                 )
    })


test_that("Reading incorrect fasta (not just random file).", {
    expect_error(read_fasta(wrong_fasta_path),
                 regexp="contains one or more illegal characters")
    })
