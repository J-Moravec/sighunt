context("Testing credibility interval")

signature = read.table(file=file.path("test_files", "sample_signature.txt"),
                       sep="\t", header=TRUE)
signature = as.matrix(signature)
density = density(signature[,1])

# TODO: somehow test for correct output?
test_that("Incorrect input", {
    expect_error(
        get_credibility_interval("test"),
        regexp="ERROR: Density must be \"density\" class!"
        )
    expect_error(
        get_credibility_interval(density, alpha=c("one", "two", "three")),
        regexp="ERROR: Alpha must be numeric vector!"
        )
    })


test_that("Correct format of output", {
    expect_equal(
        length(get_credibility_interval(density, alpha=c(0.05, 0.025, 0.01))),
        3
        )
    expect_equal(
        length(get_credibility_interval(density, alpha=c(0.05, 0.01))),
        2
        )
    expect_equal(names(get_credibility_interval(density, alpha=c(0.05, 0.025, 0.01))),
        c("0.05", "0.025", "0.01")
        )
    })
