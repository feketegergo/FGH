test_that("sd_if_enough works with NA handling and min_n", {
    x <- c(1, 2, NA, 3, 4)

    expect_equal(
        sd_if_enough(x, min_n = 4, na.rm = TRUE),
        stats::sd(c(1, 2, 3, 4))
    )

    expect_true(is.na(sd_if_enough(x, min_n = 10, na.rm = TRUE)))
    expect_true(is.na(sd_if_enough(c(NA, NA), min_n = 1, na.rm = TRUE)))
})

test_that("mean_if_enough works with NA handling and min_n", {
    x <- c(1, 2, NA, 3)
    expect_equal(
        mean_if_enough(x, min_n = 3, na.rm = TRUE),
        base::mean(c(1, 2, 3))
    )
    expect_true(is.na(mean_if_enough(x, min_n = 5, na.rm = TRUE)))
})
