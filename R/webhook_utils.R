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
