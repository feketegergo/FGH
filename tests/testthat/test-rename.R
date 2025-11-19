test_that("rename_columns átnevezi a megadott oszlopokat és megtartja a többieket", {
    df <- data.frame(
        old1 = 1:3,
        old2 = 4:6,
        keep = letters[1:3],
        stringsAsFactors = FALSE
    )

    #library(tidyverse)
    rename_tbl<-tibble::tibble(original_name=c("old1", "old2"),
                       new_name=c("new1", "new2"))


    out <- rename_columns(df, rename_tbl)

    # az oszlopnevek pontosan azok, amiket várunk
    expect_equal(
        names(out),
        c("new1", "new2", "keep")
    )

    # az értékek változatlanul átkerültek
    expect_equal(out$new1, df$old1)
    expect_equal(out$new2, df$old2)
    expect_equal(out$keep, df$keep)
})

# ------------------------------------------------------------------------------
# rename_strings tesztek
# ------------------------------------------------------------------------------

test_that("rename_strings lecseréli a megadott stringeket de mast nem", {
    x <- c("alma", "old1","körte", "alma", "old1", "old2", "szilva")
    rename_tbl<-tibble::tibble(
        original_name=c("old1", "old2"),
        new_name=c("new1", "new2"))


    out <- rename_strings(x, rename_tbl )

    expect_equal(
        out,
        c( "alma",   "new1" , "körte" ,  "alma" ,  "new1",   "new2", "szilva" )
    )

    expect_equal(typeof(out),typeof(x))
})


test_that("rename_strings üres vektorra is működik", {
    x <- character(0)

    rename_tbl<-tibble::tibble(original_name=c("old1", "old2"),
                       new_name=c("new1", "new2"))
    out <- rename_strings(x,rename_tbl)

    expect_equal(length(out), 0L)
    expect_type(out, "character")
})
