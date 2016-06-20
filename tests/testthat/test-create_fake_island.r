context("Testing fake islands generation")


test_that("Test bad input: sequences are not a character type.", {
    expect_error(
        crate_fake_island(123, "AAA", islands=list(c(1,1,1)),
        regexp="ERROR: Type of target must be character")
        )
    expect_error(
        create_fake_island(NA, "AAA", islands=list(c(1,1,1)),
        regexp="ERROR: Type of target must be character")
        )
    expect_error(
        create_fake_island(NaN, "AAA", islands=list(c(1,1,1)),
        regexp="ERROR: Type of target must be character")
        )
    expect_error(
        create_fake_island("AAA", 123, islands=list(c(1,1,1)),
        regexp="ERROR: Type of target must be character")
        )
    expect_error(
        create_fake_island("AAA", NA, islands=list(c(1,1,1)),
        regexp="ERROR: Type of target must be character")
        )
    expect_error(
        create_fake_island("AAA", NaN, islands=list(c(1,1,1)),
        regexp="ERROR: Type of target must be character")
        )
    })


test_that("Test bad input: malformed islands", {
    expect_error(
        create_fake_island("A", "C", islands=1),
        regexp="ERROR: islands must be a list!"
        )
    expect_error(
        create_fake_island("A", "C", islands=list(1)),
        regexp="ERROR: must provide valid number of parameters!"
        )
    expect_error(
        create_fake_island("A", "C", islands=list(c(1,1,5))),
        regexp="sequence ends before island stop position"
        )
    expect_error(
        create_fake_island("A", "C", islands=list(c(1,1,1), c(1,1))),
        regexp="ERROR: must provide valid number of parameters!"
        )
    })


test_that("Test good input: single island", {
    expect_equal(
        create_fake_island("A", "C", islands=list(c(1,1,1))),
        "C"
        )
    expect_equal(
        create_fake_island("AAA", "CCC", island=list(c(1,1,1))),
        "CAA"
        )
    expect_equal(
        create_fake_island("AAA", "CCC", island=list(c(1,1,2))),
        "CCA"
        )
    expect_equal(
        create_fake_island("AAA", "CCC", island=list(c(1,1,3))),
        "CCC"
        )
    expect_equal(
        create_fake_island("AAA", "CCC", island=list(c(1,2,1))),
        "ACA"
        )
    })
