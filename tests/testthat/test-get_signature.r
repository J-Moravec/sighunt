context("Testing signature-generating function.")

test_that("Temporary text", {
    expect_equivalent(
        get_signature("AAAA", "A", window=2, step=1),
        matrix(c(1,1,1,0,0,0), nrow=3, ncol=2)
        )
    expect_equivalent(
        get_signature("AAAA", "AA", window=2, step=1),
        matrix(c(1,1,1,0,0,0), nrow=3, ncol=2)
        )
    })
