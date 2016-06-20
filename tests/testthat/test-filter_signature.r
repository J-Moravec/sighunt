context("Test of signature filtering")

# read sample signature
signature = read.table(file=file.path("test_files", "sample_signature.txt"),
                       sep="\t", header=TRUE)
signature = as.matrix(signature)
# remove last column
signature_no_quality = signature[, -ncol(signature)]
dimension = dim(signature)
dimension_no_quality = dim(signature_no_quality)
AAAA = signature[, which(colnames(signature) == "AAAA"), drop=FALSE]


test_that("Testing test set-up", {
    expect_equal(colnames(signature)[ncol(signature)], "quality")
    expect_equal(dim(signature), dimension)
    expect_equal(dim(signature_no_quality), dimension_no_quality)
    expect_equal(dimension, (dimension_no_quality + c(0,1)))
    })


test_that("Testing signature input and output", {
    expect_equal(filter_signature(signature_no_quality), signature_no_quality)
    expect_equal(filter_signature(signature), signature_no_quality)
    })


test_that("Testing filtering according to oligonucleotides", {
    expect_equal(filter_signature(signature, oligos=c(1,2)), signature[,1:2])
    expect_equal(filter_signature(signature, oligos="AAAA"), AAAA)
    })


test_that("Testing if colnames and rownames are pertained", {
    expect_equal(colnames(filter_signature(signature)),
                 colnames(signature_no_quality)
                 )
    expect_equal(rownames(filter_signature(signature)),
                 rownames(signature_no_quality)
                 )
    })
