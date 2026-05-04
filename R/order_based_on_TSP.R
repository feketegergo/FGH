#' Sorrendezés TSP alapján
#'
#' Távolságmátrix alapján keres egy kis költségű Hamilton-utat a
#' travelling salesman problem (TSP) átalakításával. Hasznos például
#' korrelációs mátrixok heatmap ábrázolásánál a sorok és oszlopok
#' sorrendjének meghatározásához.
#'
#' A függvény egy dummy csomópontot ad a távolságmátrixhoz, TSP körutat
#' számol, majd a dummy csomópont eltávolításával Hamilton-utat nyer vissza.
#'
#' @param distance_mx Szimmetruikus négyzetes numerikus mátrix. A kisebb értékek közeli
#'   elemeket jelentenek. Ha vannak sornevek, a függvény ezek sorrendjét adja
#'   vissza; ha nincsenek, automatikus neveket használ.
#'
#' @return Karaktervektor: az elemek TSP-alapú sorrendje.
#'
#' @examples
#' set.seed(1)
#' x <- matrix(stats::rnorm(25), nrow = 5)
#' cor_mx <- stats::cor(x)
#' dist_mx <- as.matrix(stats::dist(cor_mx))
#' rownames(dist_mx) <- paste0("v", seq_len(nrow(dist_mx)))
#' colnames(dist_mx) <- rownames(dist_mx)
#'
#' order_based_on_TSP(dist_mx)
#'
#' @export
order_based_on_TSP <- function(distance_mx) {
    stopifnot(is.matrix(distance_mx))
    stopifnot(is.numeric(distance_mx))
    stopifnot(nrow(distance_mx) == ncol(distance_mx))

    n <- nrow(distance_mx)

    if (n == 0L) {
        return(character(0))
    }

    if (is.null(rownames(distance_mx))) {
        rownames(distance_mx) <- as.character(seq_len(n))
    }

    if (is.null(colnames(distance_mx))) {
        colnames(distance_mx) <- rownames(distance_mx)
    }

    if (n == 1L) {
        return(rownames(distance_mx))
    }


    # Dummy csomópont hozzáadása
    # Így az új mátrix mérete: (n+1)x(n+1)
    distance_mx_dummy <- matrix(
        0,
        nrow = n + 1L,
        ncol = n + 1L
    )

    distance_mx_dummy[seq_len(n), seq_len(n)] <- distance_mx

    rownames(distance_mx_dummy) <- c(rownames(distance_mx), "dummy")
    colnames(distance_mx_dummy) <- c(colnames(distance_mx), "dummy")

    tsp_instance <- TSP::TSP(distance_mx_dummy)
    # Választhatsz egzakt vagy heurisztikus megoldást, itt például az egzakt:
    tour_with_dummy <- TSP::solve_TSP(tsp_instance) #, method = "arbitrary insertion")

    order_with_dummy <- as.integer(tour_with_dummy)

    # Hamiltoni út visszanyerése a dummy csomópont kivágásával

    # Megkeressük, hol szerepel a dummy csomópont az eredményben
    dummy_index <- which(order_with_dummy == n + 1L)

    if (dummy_index < length(order_with_dummy)) {
        path <- c(
            order_with_dummy[(dummy_index + 1L):length(order_with_dummy)],
            order_with_dummy[seq_len(dummy_index - 1L)]
        )
    } else {
        path <- order_with_dummy[seq_len(dummy_index - 1L)]
    }

    rownames(distance_mx_dummy)[path]
}
