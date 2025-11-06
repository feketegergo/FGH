# R/colors-utils.R

#' N jól elkülönülő alapszín és hozzájuk tartozó komplementer színpár generálása
#'
#' HSL kör mentén generál \code{n} darab színt (hex formátumban), valamint
#' mindegyikhez egy kb. komplementer színt, így a kettő együtt jól olvasható
#' felirat/előtétszín–háttér kombinációkat adhat. A visszatérő tibből
#' \code{scale_*_identity()} skálákkal közvetlenül lehet dolgozni.
#'
#' @param n Egész szám, a kért színpárok száma.
#'
#' @return Tibble két karakteroszloppal: \code{color}, \code{color2}.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   pal <- create_color_palette_for_N(8)
#'   df <- data.frame(x = factor(seq_len(nrow(pal))), y = 1)
#'   ggplot2::ggplot(
#'     dplyr::bind_cols(df, pal),
#'     ggplot2::aes(x, y, fill = color)
#'   ) +
#'     ggplot2::geom_col() +
#'     ggplot2::scale_fill_identity() +
#'     ggplot2::theme_minimal()
#' }
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select
#' @importFrom grDevices hsv
#' @export
create_color_palette_for_N <- function(n) {
    stopifnot(length(n) == 1L, is.numeric(n), n >= 1)

    # fényesség/érték és telítettség variációk
    va <- c(0.33, 0.66, 1.00)
    vs <- c(0.5, 1, 1, 1, 1, 1)

    tibble::tibble(x = 0:(n - 1)) |>
        dplyr::mutate(
            color  = grDevices::hsv(h = x / (n + 1), s = vs[(x %% 6) + 1], v = va[(x %% 3) + 1]),
            color2 = grDevices::hsv(h = (0.5 + x / (n + 1)) %% 1, s = 1, v = 1)
        ) |>
        dplyr::select(-x)
}


#' Színek hozzárendelése egy tábla egy választott oszlopának kategóriáihoz
#'
#' A \code{nodes_tbl} megadott \code{colname} oszlopának egyedi értékeihez
#' készít színpárokat a \code{\link{create_color_palette_for_N}} segítségével,
#' majd a táblához bal oldali illesztéssel hozzáad két új oszlopot:
#' \itemize{
#' \item \code{color_of_<colname>}
#' \item \code{complementer_color_of_<colname>}
#' }
#' Ezek \code{scale_fill_identity()} és/vagy \code{scale_color_identity()}
#' skálákkal közvetlenül használhatók ggplotban.
#'
#' @param nodes_tbl data.frame / tibble.
#' @param colname Oszlopnév (tidy-eval: névként vagy stringként is megadható).
#'
#' @return Az eredeti tábla két új karakteroszloppal.
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   nodes <- data.frame(grp = rep(LETTERS[1:5], each = 2), val = 1:10)
#'   with_cols <- add_colors_for_a_column(nodes, grp)
#'   ggplot2::ggplot(
#'     with_cols,
#'     ggplot2::aes(grp, val, fill = .data[[paste0('color_of_', 'grp')]])
#'   ) +
#'     ggplot2::geom_col() +
#'     ggplot2::scale_fill_identity() +
#'     ggplot2::theme_minimal()
#' }
#'
#' @importFrom rlang ensym as_name
#' @importFrom dplyr distinct rename left_join join_by
#' @export
add_colors_for_a_column <- function(nodes_tbl, colname) {
    col_sym <- rlang::ensym(colname)
    col_chr <- rlang::as_name(col_sym)

    stopifnot(col_chr %in% names(nodes_tbl))

    # Egyedi kategóriák
    tmp <- dplyr::distinct(nodes_tbl, !!col_sym)

    # Paletta hozzárendelése
    tmp <- dplyr::left_join(
        tmp,
        create_color_palette_for_N(n = nrow(tmp)),
        by = character()
    )

    # Programozott átnevezés
    tmp <- dplyr::rename(
        tmp,
        !!paste0("color_of_", col_chr) := .data$color,
        !!paste0("complementer_color_of_", col_chr) := .data$color2
    )

    # Visszacsatolás az eredeti táblához
    dplyr::left_join(
        nodes_tbl,
        tmp,
        by = dplyr::join_by(!!col_sym)
    )
}
