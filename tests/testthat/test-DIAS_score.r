context("Testing DIAS score")

signature = read.table(file=file.path("test_files", "sample_signature.txt"),
                       sep="\t", header=TRUE)
signature = as.matrix(signature)
signature = filter_signature(signature)

test_that("Correct dimension of output", {
    expect_equal(length(DIAS_score(signature[,1])), nrow(signature))
    expect_false(any(DIAS_score(signature[,1], alpha=c(0.05, 0.025, 0.01)) > 3))
    expect_false(any(DIAS_score(signature[,1], alpha=c(0.05, 0.025, 0.01)) < 0))
    })


test_that("Correct application and summarization", {
    expect_true(is.matrix(DIAS_scores(signature)))
    expect_equal(dim(DIAS_scores(signature)), dim(signature))

    expect_false(is.list(DIAS_scores_sum(signature)))
    expect_equal(length(DIAS_scores_sum(signature)), nrow(signature))
    expect_equal(names(DIAS_scores_sum(signature)), rownames(signature))
    })
