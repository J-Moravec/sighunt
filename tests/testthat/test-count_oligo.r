context("Testing oligonucleotide count")

test_that("Searching for single oligonucleotide", {
    expect_equal(count_oligo(oligo="A", sequence="ACTGACTG"), 2)
    expect_equal(count_oligo(oligo="CT", sequence="ACTG"), 1)
    expect_equal(count_oligo(oligo="TG", sequence="ACTGACTG"), 2)
    expect_equal(count_oligo(oligo="AGCT", sequence="ACTGACTG"), 0)
    })


test_that("Searching for multiple oligonucleotides", {
    expect_equal(count_oligo(oligo=c("A", "C"), sequence="ACTG"), c(1, 1))
    expect_equal(count_oligo(oligo=c("ACTG", "CG"), sequence="ACTGACTG"),
                 c(2, 0)
                 )
    })
