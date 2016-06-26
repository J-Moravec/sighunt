context("Trying to get islands from score")

test_that("splitting name is correct", {
    expect_equal(split_name("one-two", 1), "one")
    expect_equal(split_name("one-two", 2), "two")
    })


test_that("border is correctly extracted", {
    name_vector = c("one-two", "two-three", "three-four")
    pos_1_len_1 = c("one-two", "one", "two")
    pos_1_len_2 = c("one-three", "one", "three")
    pos_2_len_2 = c("two-four", "two", "four")
    expect_equal(get_border(1, 1, name_vector), pos_1_len_1)
    expect_equal(get_border(1, 2, name_vector), pos_1_len_2)
    expect_equal(get_border(2, 2, name_vector), pos_2_len_2)
    })


test_that("getting multiple borders at once works", {
    name_vector = c("one-two", "two-three", "three-four", "four-five")
    segments = list(
        "positions" = c(1, 1, 2),
        "lengths" = c(1, 2, 2)
        )
    name_list = list(
        c("one-two", "one", "two"),
        c("one-three", "one", "three"),
        c("two-four", "two", "four")
        )
    expect_equal(get_borders(name_vector, segments), name_list)
    })


test_that("vector of \"present\" coded as ones is correctly recoded", {
    vector_1 = c(0,0,0,1)
    segments_1 = list("positions" = 4, "lengths" = 1)
    vector_2 = c(1,1,0,1)
    segments_2 = list("positions" = c(1, 4), "lengths" = c(2, 1))
    vector_3 = c(1,1,1,1)
    segments_3 = list("positions" = 1, "lengths" = 4)
    vector_4 = c(1,0,0,1,1,0,0)
    segments_4 = list("positions" = c(1, 4), "lengths" = c(1, 2))
    vector_5 = c(0,1,1,0,0,1,1)
    segments_5 = list("positions" = c(2, 6), "lengths" = c(2, 2))
    vector_6 = c(0,0,0,1,1,1)
    segments_6 = list("positions" = 4, "lengths" = 3)
    expect_equal(get_segments(vector_1), segments_1)
    expect_equal(get_segments(vector_2), segments_2)
    expect_equal(get_segments(vector_3), segments_3)
    expect_equal(get_segments(vector_4), segments_4)
    expect_equal(get_segments(vector_5), segments_5)
    expect_equal(get_segments(vector_6), segments_6)
    })


test_that("island is correctly exported", {
    sequence = "onetwothree"
    island_1 = c("one-two", "1", "6")
    island_1_out = list("name"="one-two", "sequence"="onetwo")
    island_2 = c("two-three", "4", "11")
    island_2_out = list("name"="two-three", "sequence"="twothree")
    expect_equal(get_island(island_1, sequence), island_1_out)
    expect_equal(get_island(island_2, sequence), island_2_out)
    })


test_that("main function works correctly", {
    score = c(0, 0, 3, 3, 2, 2, 4, 4, 5, 0, 6)
    names(score) = c(
        "1-2", "2-3", "3-4", "4-5", "5-6", "6-7",
        "7-8", "8-9", "9-10", "10-11", "11-12"
        )
    sequence="ABCDEFGHIJKL"
    tr_3_out = cbind(
        "name" = c("3-5", "7-10", "11-12"),
        "sequence" = c("CDE", "GHIJ", "KL")
        )
    expect_equal(get_islands(score, sequence, 3), tr_3_out)
    })
