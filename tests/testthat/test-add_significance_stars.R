test_that("add_significance_stars adds correct star labels", {
    df <- data.frame(p = c(0.2, 0.04, 0.009, 0.0009))
    out <- add_significance_stars(df, p, new_col = "stars")

    expect_true("stars" %in% names(out))
    expect_equal(
        out$stars,
        c(" ", "*", "**", "***")
    )
})

test_that("add_significance_stars works with different column names", {
    df <- data.frame(p_value = c(0.2, 0.04))
    out <- add_significance_stars(df, p_value, new_col = "sig")
    expect_equal(out$sig, c(" ", "*"))
})
