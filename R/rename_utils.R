#' Oszlopnevek átnevezése táblázat alapján
#'
#' A \code{tbl1} data.frame/tibble oszlopneveit nevezi át a
#' \code{rename_tbl} táblázat alapján. A \code{rename_tbl} két kötelező
#' oszlopa az \code{original_name} és a \code{new_name}. Ha vannak
#' további oszlopok, azokat a függvény figyelmen kívül hagyja.
#'
#' Csak azokat az oszlopokat nevezi át, amelyek:
#' \itemize{
#'   \item szerepelnek az \code{original_name} oszlopban, \emph{és}
#'   \item ténylegesen léteznek a \code{tbl1} oszlopai között.
#' }
#'
#' Ha a \code{original_name} és a \code{new_name} megegyezik, az adott
#' sor figyelmen kívül lesz hagyva (nincs értelme átnevezni ugyanarra).
#'
#' @param tbl1 data.frame vagy tibble, amelynek az oszlopneveit át szeretnéd nevezni.
#' @param rename_tbl data.frame vagy tibble legalább két oszloppal:
#'   \code{original_name} és \code{new_name}. Ezek karaktervektorok.
#'
#' @return A bemeneti \code{tbl1}-hez hasonló típusú objektum, az
#'   átnevezett oszlopokkal.
#'
#' @examples
#' df <- data.frame(
#'   "My Column" = 1:3,
#'   "Other.Col" = 4:6,
#'   check.names = FALSE
#' )
#'
#' rename_tbl <- data.frame(
#'   original_name = c("My Column", "Other.Col"),
#'   new_name      = c("my_column", "other_col"),
#'   stringsAsFactors = FALSE
#' )
#'
#' rename_columns(df, rename_tbl)
#'
#' @importFrom dplyr rename filter
#' @importFrom stats setNames
#' @export
rename_columns <- function(tbl1, rename_tbl) {
    stopifnot(is.data.frame(tbl1))
    stopifnot(is.data.frame(rename_tbl))
    stopifnot(all(c("original_name", "new_name") %in% names(rename_tbl)))

    # Csak azokat a sorokat tartjuk meg, ahol tényleg változik a név
    rename_tbl <- rename_tbl |>
        dplyr::filter(.data$original_name != .data$new_name)

    # Named vektor: names = új nevek, values = régi nevek
    # dplyr::rename(!!!c(new = "old")) formában működik.
    rename_vec <- stats::setNames(rename_tbl$original_name,
                                  rename_tbl$new_name)

    # Csak létező oszlopokra
    rename_vec <- rename_vec[rename_vec %in% colnames(tbl1)]

    if (length(rename_vec) == 0) {
        # nincs mit átnevezni
        res <- tbl1
    } else {
        res <- dplyr::rename(tbl1, !!!rename_vec)
    }

    res
}

#' Karaktervektor elemeinek átnevezése táblázat alapján
#'
#' Egy karaktervektor elemeit cseréli le a \code{rename_tbl} táblázat
#' alapján. A \code{rename_tbl} két kötelező oszlopa az
#' \code{original_name} és a \code{new_name}. Ha a \code{strings}
#' vektor egy eleme megegyezik valamelyik \code{original_name}
#' értékkel, akkor a megfelelő \code{new_name}-re lesz lecserélve.
#' Ha nincs rá szabály, az elem változatlan marad.
#'
#' A függvény:
#' \itemize{
#'   \item megköveteli, hogy \code{strings} karaktervektor legyen,
#'   \item megköveteli, hogy az \code{original_name} oszlopban ne legyen duplikáció,
#'   \item megőrzi a \code{names(strings)} neveket (ha léteznek),
#'   \item üres bemenet esetén is karaktervektort ad vissza.
#' }
#'
#' @param strings Karaktervektor, amelynek elemeit át akarod nevezni.
#' @param rename_tbl data.frame vagy tibble legalább két oszloppal:
#'   \code{original_name} és \code{new_name}.
#'
#' @return Karaktervektor ugyanannyi elemmel, mint \code{strings},
#'   a megfelelő helyeken lecserélt értékekkel.
#'
#' @examples
#' strings <- c(a = "alma", b = "korte", c = "szilva")
#' rename_tbl <- data.frame(
#'   original_name = c("alma", "korte"),
#'   new_name      = c("ALMA", "KÖRTE"),
#'   stringsAsFactors = FALSE
#' )
#'
#' rename_strings(strings, rename_tbl)
#'
#' @export
rename_strings <- function(strings, rename_tbl) {
    stopifnot(typeof(strings) == "character")
    stopifnot(is.data.frame(rename_tbl))
    stopifnot(all(c("original_name", "new_name") %in% names(rename_tbl)))
    stopifnot(!any(duplicated(rename_tbl$original_name)))

    # Üres vektor esetén csak térjünk vissza vele; típus és names megmarad
    if (length(strings) == 0L) {
        return(strings)
    }

    # Egyszerű, karakter alapú átnevezés
    for (i in seq_along(strings)) {
        idx <- rename_tbl$original_name == strings[[i]]
        if (any(idx)) {
            strings[[i]] <- rename_tbl$new_name[idx][[1L]]
        }
    }

    strings
}
