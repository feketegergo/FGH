test_that("create_color_palette_for_N returns two hex color columns", {
    pal <- create_color_palette_for_N(7)
    expect_s3_class(pal, "tbl_df") # tibble
    expect_equal(names(pal), c("color", "color2"))
    expect_equal(nrow(pal), 7)

    # Alap ellenőrzés: hex vagy nevezett színek — hsv hexet ad
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal$color)))
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", pal$color2)))

    # Komplementer hue-eltolásból adódóan ne legyen minden pár azonos
    expect_false(any(pal$color == pal$color2))
})

test_that("create_color_palette_for_N validates n", {
    expect_error(create_color_palette_for_N(0))
    expect_error(create_color_palette_for_N(-3))
    expect_error(create_color_palette_for_N(c(1, 2)))
})
