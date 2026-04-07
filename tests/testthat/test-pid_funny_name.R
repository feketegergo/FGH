test_that("pid_funny_name returns a single character value", {
    x <- pid_funny_name(1234)

    expect_type(x, "character")
    expect_length(x, 1)
    expect_false(is.na(x))
})

test_that("pid_funny_name is deterministic for the same pid and language", {
    x1 <- pid_funny_name(1234, "hu")
    x2 <- pid_funny_name(1234, "hu")
    x3 <- pid_funny_name(1234, "en")
    x4 <- pid_funny_name(1234, "en")

    expect_identical(x1, x2)
    expect_identical(x3, x4)
})

test_that("pid_funny_name returns different values for different languages", {
    hu_name <- pid_funny_name(1234, "hu")
    en_name <- pid_funny_name(1234, "en")

    expect_type(hu_name, "character")
    expect_type(en_name, "character")
    expect_false(identical(hu_name, en_name))
})

test_that("pid_funny_name uses current pid by default", {
    expect_identical(
        pid_funny_name(),
        pid_funny_name(Sys.getpid(), "hu")
    )
})

test_that("pid_funny_name accepts integer-like inputs", {
    expect_identical(
        pid_funny_name(1234, "hu"),
        pid_funny_name("1234", "hu")
    )

    expect_identical(
        pid_funny_name(1234, "en"),
        pid_funny_name(1234L, "en")
    )
})

test_that("pid_funny_name handles negative pid values via abs()", {
    expect_identical(
        pid_funny_name(-1234, "hu"),
        pid_funny_name(1234, "hu")
    )
})

test_that("pid_funny_name rejects invalid pid values", {
    expect_error(
        pid_funny_name(NA),
        "`pid` must be a single non-missing value."
    )

    expect_error(
        pid_funny_name(c(1, 2)),
        "`pid` must be a single non-missing value."
    )

    expect_error(
        pid_funny_name("abc"),
        "`pid` must be coercible to integer."
    )
})

test_that("pid_funny_name rejects invalid lang values", {
    expect_error(
        pid_funny_name(1234, "de")
    )
})
