

#' Szórás, ha elég megfigyelés áll rendelkezésre
#'
#' A vektor szórását adja vissza, ha a nem hiányzó elemek száma
#' legalább \code{min_n}, különben \code{NA_real_}.
#'
#' @param x numeric vektor.
#' @param min_n integer, a minimálisan elvárt elemszám (alapértelmezés: 5).
#' @param na.rm logical, a hiányzó értékek eltávolítása számítás előtt (TRUE).
#' @return Szám (\code{numeric(1)}), vagy \code{NA_real_}, ha kevés az adat.
#' @examples
#' sd_if_enough(c(1, 2, NA, 3), min_n = 3)
#' @export
sd_if_enough <- function(x, min_n = 5, na.rm = TRUE) {
    if (na.rm) x <- x[!is.na(x)]
    if (length(x) < min_n) NA_real_ else stats::sd(x)
}

#' Átlag, ha elég megfigyelés áll rendelkezésre
#'
#' A vektor átlagát adja vissza, ha a nem hiányzó elemek száma
#' legalább \code{min_n}, különben \code{NA_real_}.
#'
#' @param x numeric vektor.
#' @param min_n integer, a minimálisan elvárt elemszám (alapértelmezés: 5).
#' @param na.rm logical, a hiányzó értékek eltávolítása számítás előtt (TRUE).
#' @return Szám (\code{numeric(1)}), vagy \code{NA_real_}, ha kevés az adat.
#' @examples
#' mean_if_enough(c(1, 2, NA, 3), min_n = 3)
#' @export
mean_if_enough <- function(x, min_n = 5, na.rm = TRUE) {
    if (na.rm) x <- x[!is.na(x)]
    if (length(x) < min_n) NA_real_ else base::mean(x)
}

#' Szignifikanciaszint-jelölések (csillagok) hozzáadása egy adathoz
#'
#' A \code{data} táblában a \code{p_col} (tidy-eval) oszlopot értelmezi
#' p-értékként, és létrehoz egy új, \code{new_col} nevű karakteroszlopot
#' a szokásos „csillagos” jelöléssel.
#'
#' @param data data.frame vagy tibble.
#' @param p_col Oszlopnév, tidy-eval stílusban megadható (pl. \code{p_value} idézőjelek nélkül).
#' @param new_col Karakter, az új oszlop neve (alapértelmezés: "sig_label").
#' @return A bemeneti \code{data} kiegészítve a \code{new_col} oszloppal.
#' @examples
#' # library(dplyr)
#' # tibble::tibble(p = c(0.2, 0.04, 0.009)) |>
#' #   add_significance_stars(p, new_col = "stars")
#' @importFrom rlang enquo sym
#' @importFrom dplyr mutate case_when
#' @export
add_significance_stars <- function(data, p_col, new_col = "sig_label") {
    p_col <- rlang::enquo(p_col)

    dplyr::mutate(
        .data = data,
        # Név szerinti oszlop-hozzárendelés tidy-eval-lel:
        !!rlang::sym(new_col) := dplyr::case_when(
            !!p_col < 0.001 ~ "***",
            !!p_col < 0.01  ~ "**",
            !!p_col < 0.05  ~ "*",
            !!p_col < 0.10  ~ ".",
            TRUE            ~ " "
        )
    )
}

#' Karaktervektor idézőjelezése és vesszővel elválasztott formázása
#'
#' A bemeneti karaktervektor elemeit idézőjelek közé teszi és
#' vesszővel elválasztva kiírja a konzolra (copy–paste barát).
#'
#' @param x karaktervektor.
#' @return Látható kimenet a konzolra (\code{cat}), visszatérési értéke \code{NULL}.
#' @examples
#' # print_as_string_list(c("alma", "korte", "szilva"))
#' @export
print_as_string_list <- function(x) {
    str <- sprintf("\"%s\"", x) |>
        base::paste(collapse = ", ")
    base::cat(str)
}





