context("Testing oligonucleotide generation")

test_that("Generating oligonucleotide of length 1", {
    expect_equal(generate_oligo(1), c("A", "C", "G", "T"))
    })

test_that("Generating oligonucleotide of length 2", {
    expect_equal(generate_oligo(2), c("AA", "AC", "AG", "AT", "CA", "CC", "CG",
    "CT", "GA", "GC", "GG", "GT", "TA", "TC", "TG", "TT"))
    })
