context("Reading fasta")

correct_fasta_path = file.path("test_files", "correct.fasta")
wrong_fasta_path = file.path("test_files", "wrong.fasta")

test_that("reading correct (but ugly) fasta", {
    expect_equal(read_fasta(correct_fasta_path),
                 c("ACTG?", "ACTG--", "", "ACTGN")
                 )
    })


test_that("reading incorrect fasta (not just random file)", {
    expect_error(read_fasta(wrong_fasta_path), "contains illegal characters")
    })
