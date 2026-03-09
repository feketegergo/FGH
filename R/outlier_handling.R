
#' @param x numeric vektor.
#' @param min_n integer, a minimálisan elvárt elemszám (alapértelmezés: 5).
#' @param na.rm logical, a hiányzó értékek eltávolítása számítás előtt (TRUE).
#' @return Szám (\code{numeric(1)}), vagy \code{NA_real_}, ha kevés az adat.
#' @examples
#' sd_if_enough(c(1, 2, NA, 3), min_n = 3)
#' @export


#'
#' Abból kiindulva, hogy normális eloszású a minta, kiszámolja hogy ennyi
#' mintaelemszám esetén hol lenne aa  olyan kétoldali határ, amelyen kívül egy
#' N-elemű normális mintában csak ritkán várnánk értéket.
#'
#' Az átlag és a szórás becslésére robosztus becslést használ, hogy az outlierek ne
#' zavarjanak be
#'
#'  @param x numeric vektor.
#'  @param conf numeric vektor.
#'  @return thr 2 elemű vektorban egy intervallummal. Az első érték kell a kisebb
#'
#' @examples
#' dat |>  mutate(burden=winsorize(burden, outlier_threshold_at_normal_sample(burden,0.99)))
#' @export
outlier_threshold_at_normal_sample <- function(x, conf = 0.95) {
    N <- sum(!is.na(x))
    if (N == 0) return(c(lower = NA_real_, upper = NA_real_))

    med <- median(x, na.rm = TRUE)
    sigma_rob <- 1.4826 * mad(x, constant = 1, na.rm = TRUE)

    p <- ((1 + conf) / 2)^(1 / N)
    cN <- qnorm(p)

    c(lower = med - cN * sigma_rob,
      upper = med + cN * sigma_rob)
}


#'
#' @param x numeric vektor., ezt winzorizáljuk
#' @param thr 2 elemű vektor, egy intervallummal. Az első érték kell a kisebb legyen
#'
#' @return x-hez hasonló numerikus vektor, csak a szélsőséges értékek le vannak cserélva
#'
#' @examples
#' dat |>  mutate(burden=winsorize(burden, outlier_threshold_at_normal_sample(burden,0.99)))
#' @export
winsorize<-function(x, thr)
{
    stopifnot(is.numeric(thr))
    stopifnot(length(thr)==2)
    stopifnot(thr[[1]]<=thr[[2]])

    pmax(pmin(x, thr[2]), thr[1])
}
