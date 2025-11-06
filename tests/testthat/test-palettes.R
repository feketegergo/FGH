test_that("c10_colors exists and has 10 entries", {
    expect_true(exists("c10_colors"))
    expect_type(c10_colors, "character")
    expect_length(c10_colors, 10)
})
