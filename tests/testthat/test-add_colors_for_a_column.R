test_that("add_colors_for_a_column joins two color columns by category", {
    nodes <- data.frame(grp = rep(LETTERS[1:3], each = 2), val = 1:6)
    out <- add_colors_for_a_column(nodes, grp)

    expect_true(all(c("color_of_grp", "complementer_color_of_grp") %in% names(out)))
    expect_equal(nrow(out), nrow(nodes))

    # Azonos kategóriához azonos szín jusson
    m <- unique(out[, c("grp", "color_of_grp")])
    expect_equal(nrow(m), 3)  # 3 egyedi kategória

    # Színek formája hex (hsv -> hex)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", out$color_of_grp)))
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", out$complementer_color_of_grp)))
})

test_that("add_colors_for_a_column errors if column missing", {
    nodes <- data.frame(a = 1:3)
    expect_error(add_colors_for_a_column(nodes, grp))
})
