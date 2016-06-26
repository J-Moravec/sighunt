context("Testing signature-generating function.")

test_that("generated signature is correct", {
    expect_equivalent(
        get_signature("AAAA", oligos="A", window=2, step=1),
        matrix(c(1,1,1,0,0,0), nrow=3, ncol=2)
        )
    expect_equivalent(
        get_signature("AAAA", oligos="AA", window=2, step=1),
        matrix(c(1,1,1,0,0,0), nrow=3, ncol=2)
        )
    })
