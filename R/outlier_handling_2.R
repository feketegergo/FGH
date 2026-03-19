#' Outlier kezelés robusztus z-score alapján
#'
#' A függvény a kijelölt oszlopokban robusztus z-score-t számol
#' medián és MAD (median absolute deviation) segítségével, majd
#' a megadott \code{mode} szerint kezeli az extrém outliereket.
#'
#' A robusztus z-score képlete:
#' \deqn{z = \frac{x - median(x)}{1.4826 * MAD(x)}}
#'
#' Egy érték outliernek számít, ha \code{abs(z) > z_threshold}.
#'
#' @param .data Egy data.frame vagy tibble.
#' @param ... Tidy-selecttel megadott oszlopok. Ha nincs megadva,
#'   akkor minden numerikus oszlopra lefut.
#' @param z_threshold Numerikus küszöb a robusztus z-score abszolút értékére.
#'   Alapértelmezés: \code{5.5}.
#' @param mode Az outlierek kezelésének módja.
#'   Lehetséges értékek:
#'   \itemize{
#'     \item \code{"remove_rows"}: azok a sorok törlődnek, ahol bármely kijelölt
#'       oszlopban outlier van
#'     \item \code{"replace_na"}: az outlier értékek \code{NA}-ra cserélődnek
#'     \item \code{"winsorize"}: az outlier értékek a küszöbértékre vágódnak
#'   }
#'
#' @return
#' A \code{mode} beállítástól függően:
#' \itemize{
#'   \item \code{"remove_rows"} esetén egy szűrt tibble
#'   \item \code{"replace_na"} vagy \code{"winsorize"} esetén az eredetivel azonos
#'     sor- és oszlopszámú tibble
#' }
#'
#' @details
#' A függvény oszloponként külön számolja a mediánt és a MAD-et.
#'
#' Ha egy oszlopban a MAD nulla vagy hiányzó, akkor azon az oszlopon nem jelöl
#' outliert.
#'
#' A \code{"winsorize"} mód a robusztus z-score küszöbhöz tartozó alsó és felső
#' határra vágja le az értékeket:
#'
#' \deqn{lower = median - z\_threshold * 1.4826 * MAD}
#' \deqn{upper = median + z\_threshold * 1.4826 * MAD}
#'
#' @examples
#' library(dplyr)
#'
#' df <- tibble(
#'   x = c(1:10, 100),
#'   y = c(rnorm(10), 20),
#'   g = letters[1:11]
#' )
#'
#' # Minden numerikus oszlopra
#' outlier_handle(df)
#'
#' # Csak kijelölt oszlopokra
#' outlier_handle(df, x, y, mode = "replace_na")
#'
#' # Minden numerikus oszlop winsorizálása
#' outlier_handle(df, where(is.numeric), mode = "winsorize", z_threshold = 5)
#'
#' @importFrom dplyr select where as_tibble mutate across all_of
#' @importFrom rlang enquos
#' @export
outlier_handle <- function(.data, ..., z_threshold = 5.5,
                           mode = c("remove_rows", "replace_na", "winsorize")) {
    mode <- match.arg(mode)

    if (!inherits(.data, "data.frame")) {
        stop("`.data` must be a data.frame or tibble.", call. = FALSE)
    }

    quos <- rlang::enquos(...)

    if (length(quos) == 0) {
        col_names <- names(dplyr::select(.data, where(is.numeric)))
    } else {
        col_names <- names(dplyr::select(.data, !!!quos))
    }

    if (length(col_names) == 0) {
        return(dplyr::as_tibble(.data))
    }

    is_extreme_outlier <- function(x, z_threshold) {
        med <- stats::median(x, na.rm = TRUE)
        mad0 <- stats::mad(x, constant = 1, na.rm = TRUE)

        if (is.na(mad0) || mad0 == 0) {
            return(rep(FALSE, length(x)))
        }

        z_rob <- (x - med) / (1.4826 * mad0)
        abs(z_rob) > z_threshold
    }

    winsorize_extreme <- function(x, z_threshold) {
        med <- stats::median(x, na.rm = TRUE)
        mad0 <- stats::mad(x, constant = 1, na.rm = TRUE)

        if (is.na(mad0) || mad0 == 0) {
            return(x)
        }

        scale_rob <- 1.4826 * mad0
        lower <- med - z_threshold * scale_rob
        upper <- med + z_threshold * scale_rob

        x <- ifelse(!is.na(x) & x < lower, lower, x)
        x <- ifelse(!is.na(x) & x > upper, upper, x)
        x
    }

    out <- dplyr::as_tibble(.data)

    if (mode == "remove_rows") {
        outlier_mat <- lapply(out[col_names], is_extreme_outlier, z_threshold = z_threshold)
        outlier_mat <- as.data.frame(outlier_mat, check.names = FALSE)

        keep <- !apply(outlier_mat, 1, any)
        out <- out[keep, , drop = FALSE]
    } else if (mode == "replace_na") {
        out <- dplyr::mutate(
            out,
            dplyr::across(
                dplyr::all_of(col_names),
                ~ {
                    flag <- is_extreme_outlier(.x, z_threshold = z_threshold)
                    .x[flag] <- NA
                    .x
                }
            )
        )
    } else if (mode == "winsorize") {
        out <- dplyr::mutate(
            out,
            dplyr::across(
                dplyr::all_of(col_names),
                ~ winsorize_extreme(.x, z_threshold = z_threshold)
            )
        )
    }

    out
}
