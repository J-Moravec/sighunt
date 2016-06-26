context("Testing DIAS score")

signature = read.table(file=file.path("test_files", "sample_signature.txt"),
                       sep="\t", header=TRUE)
signature = as.matrix(signature)
signature = filter_signature(signature)

test_that("dimension of output is correct without optional parameters", {
    expect_equal(length(DIAS_score(signature[,1])), nrow(signature))
    expect_false(any(DIAS_score(signature[,1], alpha=c(0.05, 0.025, 0.01)) > 3))
    expect_false(any(DIAS_score(signature[,1], alpha=c(0.05, 0.025, 0.01)) < 0))
    #expect_equal(length(DIAS_subset_score(signature[,1], 1)), 1)
    })


test_that("dimension of output is correct with position parameter", {
    expect_equal(length(DIAS_score(signature[,1], 1)), 1)
    expect_false(DIAS_score(signature[,1], 1, alpha=c(0.05, 0.025, 0.01)) > 3)
    expect_false(DIAS_score(signature[,1], 1, alpha=c(0.05, 0.025, 0.01)) < 0)
    })


test_that("dimension of output is correct with position and eye parameter", {
    expect_equal(length(DIAS_score(signature[,1], 1, c(1, 5))), 1)
    expect_false(
        DIAS_score(signature[,1], 1, 1:5, alpha=c(0.05, 0.025, 0.01)) > 3
        )
    expect_false(
        DIAS_score(signature[,1], 1, 1:5, alpha=c(0.05, 0.025, 0.01)) > 3
        )
    })



test_that("summarization is correct without optional parameters", {
    expect_equal(length(DIAS_scores_sum(signature)), nrow(signature))
    expect_equal(names(DIAS_scores_sum(signature)), rownames(signature))
    })

test_that("summarization is correct with position parameter", {
    expect_equal(length(DIAS_scores_sum(signature, 1)), 1)
    expect_equal(
        names(DIAS_scores_sum(signature, 1)), rownames(signature)[1]
        )
    })

test_that("summarization is correct with position and eye parameter", {
    expect_equal(length(DIAS_scores_sum(signature, 1, c(1:5))), 1)
    expect_equal(
        names(DIAS_scores_sum(signature, 1, 1:5)),
        rownames(signature)[1]
        )
    })


test_that("error is reported when eye is provided but position is missing", {
    expect_error(
        DIAS_score(signature[,1], eye=1:5),
        regexp="ERROR: If parameter eye is supplied"
        )
    expect_error(
        DIAS_scores_sum(signature, eye=1:5),
        regexp="ERROR: If parameter eye is supplied"
        )
    })
