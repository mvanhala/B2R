
#' Authorize Backblaze B2
#'
#' Log in to B2 API
#'
#' See \url{https://www.backblaze.com/b2/docs/b2_authorize_account.html}
#'
#' @param account_id B2 account ID
#' @param application_key B2 application key for the account
#' @return Invisibly, a list with the values from the JSON response
#' @export
b2_authorize_account <- function(account_id = Sys.getenv("B2_ACCOUNT_ID"),
                                 application_key = Sys.getenv("B2_APPLICATION_KEY")) {
  auth_string <- paste(
    "Basic",
    base64enc::base64encode(charToRaw(paste0(account_id, ":", application_key)))
  )

  auth_resp <- httr::GET(
    "https://api.backblazeb2.com/b2api/v1/b2_authorize_account",
    httr::add_headers(Authorization = auth_string)
  )

  if (httr::status_code(auth_resp) != 200L) {
    stop("\n  Authorization failed \n  ", httr::content(auth_resp)$message)
  }

  auth_info <- httr::content(auth_resp)

  Sys.setenv("B2_AUTHORIZATION_TOKEN" = auth_info$authorizationToken,
             "B2_API_URL" = auth_info$apiUrl,
             "B2_DOWNLOAD_URL" = auth_info$downloadUrl)

  invisible(auth_info)
}



