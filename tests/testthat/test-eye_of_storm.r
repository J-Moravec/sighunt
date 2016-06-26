context("Testing eye of storm method")

signature = read.table(file=file.path("test_files", "sample_signature.txt"),
                       sep="\t", header=TRUE)
signature = as.matrix(signature)
signature = filter_signature(signature)

test_that("dimension of output is correct", {
    expect_equal(length(eye_of_storm(signature, eye=1, bar=FALSE)), nrow(signature))
    expect_equal(names(eye_of_storm(signature, eye=1, bar=FALSE)), rownames(signature))
    })
