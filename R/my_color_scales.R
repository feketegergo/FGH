#' 25 vizuálisan jól elkülöníthető szín kódja
#'
#' Ez a vektor 25 színt tartalmaz, amelyek jól megkülönböztethetők
#' grafikonokon, kategóriák színezéséhez stb.
#'
#' A színek részben R beépített névvel (`"dodgerblue2"`, `"gold1"`, stb.),
#' részben hex kóddal vannak megadva.
#'
#' @format Karaktervektor, 25 elem.
#' @examples
#' # Példa: oszlopdiagram színezése
#' barplot(1:25, col = c25_colors)
#'
#' @export
c25_colors <- c(
    "dodgerblue2",
    "#E31A1C",   # red
    "green4",
    "#6A3D9A",   # purple
    "#FF7F00",   # orange
    "black",
    "gold1",
    "skyblue2",
    "#FB9A99",   # light pink
    "palegreen2",
    "#CAB2D6",   # light purple
    "#FDBF6F",   # light orange
    "gray70",
    "khaki2",
    "maroon",
    "orchid1",
    "deeppink1",
    "blue1",
    "steelblue4",
    "darkturquoise",
    "green1",
    "yellow4",
    "yellow3",
    "darkorange4",
    "brown"
)






#' 10 vizuálisan jól elkülöníthető szín kódja
#'
#' Ez a paletta 10 jól megkülönböztethető színt ad, kategóriákhoz,
#' csoportokhoz. A vektor R beépített név- és hex-kódokat vegyesen tartalmaz.
#'
#' @format Karaktervektor, 10 elem.
#'
#' @examples
#' # ggplot példa: kategóriák színezése fix palettával
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   df <- data.frame(g = factor(1:10), y = 1:10)
#'   ggplot2::ggplot(df, ggplot2::aes(g, y, fill = g)) +
#'     ggplot2::geom_col() +
#'     ggplot2::scale_fill_manual(values = c10_colors) +
#'     ggplot2::theme_minimal()
#' }
#'
#' @export
c10_colors <- c(
    "dodgerblue2",
    "#E31A1C",   # red
    "green4",
    "#6A3D9A",   # purple
    "#FF7F00",   # orange
    "gold1",
    "skyblue2",
    "#FB9A99",   # light pink
    "#CAB2D6",   # light purple
    "#FDBF6F"    # light orange
)


