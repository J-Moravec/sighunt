context("Testing sliding density method")
# TODO Unfortunatelly currently there is no way how to test if window parameter
# TODO is correctly processed given current structure. This means that I should
# TODO put this piece of code to a new function and test it.

signature = read.table(file=file.path("test_files", "sample_signature.txt"),
                       sep="\t", header=TRUE)
signature = as.matrix(signature)
signature = filter_signature(signature)

test_that("dimension of output is correct", {
    expect_equal(length(sliding_density(signature)), nrow(signature))
    expect_equal(names(sliding_density(signature)), rownames(signature))
    })
