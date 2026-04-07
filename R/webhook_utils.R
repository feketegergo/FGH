#' Discord webhook üzenet küldése
#'
#' Üzenetet küld egy Discord csatornára webhook URL-en keresztül.
#' Hasznos hosszú számítások/HPC jobok végén értesítéshez.
#'
#' A webhook URL-t a \code{DISCORD_WEBHOOK_URL} környezeti változóban várja
#' (jellemzően \code{~/.Renviron}-ben beállítva).
#'
#' @param msg Karakter (hossz 1). Az elküldendő üzenet. Nem lehet üres vagy NA.
#' @param env_var Karakter. A webhook URL-t tartalmazó környezeti változó neve.
#' @param quiet Logikai. Ha TRUE, ne írjon üzenetet hiányzó env var vagy hiba esetén.
#' @param timeout Másodpercben. HTTP kérés timeout.
#'
#' @return Logikai: TRUE ha sikeres volt a küldés, különben FALSE.
#'
#' @examples
#'   send_webhook_message("HPC job finished.")
#' @export
send_webhook_message <- function(msg = "automatic message",
                                 env_var = "DISCORD_WEBHOOK_URL",
                                 quiet = FALSE,
                                 timeout = 10) {
    stopifnot(is.character(env_var), length(env_var) == 1L, nzchar(env_var))
    stopifnot(is.numeric(timeout), length(timeout) == 1L, timeout > 0)

    stopifnot(is.character(msg), length(msg) == 1L, !is.na(msg))

    if (!nzchar(msg)) {
        if (!quiet) warning("msg is empty; nothing sent.", call. = FALSE)
        return(FALSE)
    }

    # Discord content limit (általában 2000 karakter)
    if (nchar(msg, type = "chars", allowNA = FALSE) > 2000) {
        msg <- substr(msg, 1, 2000)
        if (!quiet) warning("msg is too long; it was trimmed to 2000 characters.", call. = FALSE)
    }

    webhook <- Sys.getenv(env_var, unset = "")
    if (!nzchar(webhook)) {
        if (!quiet) message(env_var, " environment variable is missing; message not sent.")
        return(FALSE)
    }

    ok <- tryCatch({
        req <- httr2::request(webhook) |>
            httr2::req_method("POST") |>
            httr2::req_timeout(timeout) |>
            httr2::req_body_json(list(content = msg), auto_unbox = TRUE)

        resp <- httr2::req_perform(req)

        # Biztos siker-ellenőrzés
        if (httr2::resp_is_error(resp)) {
            if (!quiet) warning("Discord webhook returned HTTP ", httr2::resp_status(resp), ".", call. = FALSE)
            FALSE
        } else {
            TRUE
        }
    }, error = function(e) {
        if (!quiet) warning("Failed to send Discord webhook message: ", conditionMessage(e), call. = FALSE)
        FALSE
    })

    ok
}


#' Generate a funny memorable name from a process ID
#'
#' Deterministically generates a three-part funny name from a process ID
#' (`pid`), in the form of:
#' property + color + animal.
#'
#' If `pid` is not provided, the function uses the current R process ID
#' returned by [Sys.getpid()].
#'
#' Supported languages:
#' \itemize{
#'   \item `"hu"`: Hungarian
#'   \item `"en"`: English
#' }
#'
#' The same `pid` and `lang` combination always produces the same result.
#'
#' @param pid Integer-like process ID. If omitted, the current process ID is
#'   used via [Sys.getpid()].
#' @param lang Output language. Either `"hu"` or `"en"`. Default is `"hu"`.
#'
#' @return A character scalar containing a three-word memorable name.
#'
#' @examples
#' pid_funny_name()
#' pid_funny_name(1234)
#' pid_funny_name(1234, "en")
#'
#' @export
pid_funny_name <- function(pid = Sys.getpid(), lang = "hu") {
    lang <- match.arg(lang, c("hu", "en"))

    if (length(pid) != 1 || is.na(pid)) {
        stop("`pid` must be a single non-missing value.", call. = FALSE)
    }

    pid_num <- suppressWarnings(as.integer(pid))
    if (is.na(pid_num)) {
        stop("`pid` must be coercible to integer.", call. = FALSE)
    }

    pid_num <- abs(pid_num)

    words <- list(
        hu = list(
            trait = c(
                "kicsi", "nagy", "hosszú", "fürge", "lomha", "bátor",
                "vidám", "morgós", "csíkos", "bolyhos", "kövér", "csendes",
                "zajos", "ravasz", "álmos", "ugráló"
            ),
            color = c(
                "sárga", "piros", "kék", "zöld", "lila", "narancs",
                "fekete", "fehér", "szürke", "barna", "arany", "ezüst"
            ),
            animal = c(
                "fóka", "kutya", "macska", "rozmár", "pingvin", "teve",
                "vidra", "hörcsög", "panda", "zsiráf", "mosómedve", "bagoly",
                "lajhár", "vakond", "pelikán", "borz"
            )
        ),
        en = list(
            trait = c(
                "tiny", "big", "long", "swift", "sleepy", "brave",
                "cheerful", "grumpy", "striped", "fluffy", "chubby", "silent",
                "noisy", "sly", "dreamy", "bouncy"
            ),
            color = c(
                "yellow", "red", "blue", "green", "purple", "orange",
                "black", "white", "gray", "brown", "golden", "silver"
            ),
            animal = c(
                "seal", "dog", "cat", "walrus", "penguin", "camel",
                "otter", "hamster", "panda", "giraffe", "raccoon", "owl",
                "sloth", "mole", "pelican", "badger"
            )
        )
    )

    w <- words[[lang]]

    i_trait  <- (pid_num %% length(w$trait)) + 1L
    i_color  <- ((pid_num * 7L + 3L) %% length(w$color)) + 1L
    i_animal <- ((pid_num * 11L + 5L) %% length(w$animal)) + 1L

    paste(w$trait[i_trait], w$color[i_color], w$animal[i_animal])
}
