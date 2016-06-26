context("Testing window-generation code")

test_that("window is correctly generated", {
    expect_equal(
        get_interval(position=1, window=5, max_length=10),
        c("from"=1, "to"=6)
        )

    expect_equivalent(get_interval(8, 5, 10), c("from"=3, "to"=10))
    })
