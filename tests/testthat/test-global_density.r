context("Testing global density method")
# This is small since global_density is small wrapper around basic inner
# functions and those are already covered by its own tests.

signature = read.table(file=file.path("test_files", "sample_signature.txt"),
                       sep="\t", header=TRUE)
signature = as.matrix(signature)

test_that("dimension and names of output is correct", {
    expect_equal(length(global_density(signature)), nrow(signature))
    expect_equal(names(global_density(signature)), rownames(signature))
    })
