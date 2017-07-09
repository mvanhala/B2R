

#' List B2 buckets
#'
#' List buckets associated with an account
#'
#' See \url{https://www.backblaze.com/b2/docs/b2_list_buckets.html}
#'
#' @param account_id B2 account ID
#' @return \code{b2_list_buckets} returns a list from the JSON reponse.
#'
#' \code{b2_list_buckets_df} returns a tibble with information about the buckets
#' @export
b2_list_buckets <- function(account_id = Sys.getenv("B2_ACCOUNT_ID")) {
  bucket_resp <- httr::POST(
    paste0(Sys.getenv("B2_API_URL"), "/b2api/v1/b2_list_buckets"),
    httr::add_headers(Authorization = Sys.getenv("B2_AUTHORIZATION_TOKEN")),
    body = list(accountId = account_id),
    encode = "json"
  )

  if (httr::status_code(bucket_resp) != 200L) {
    stop("\n  Bucket listing failed \n  ", httr::content(bucket_resp)$message)
  }

  httr::content(bucket_resp)
}

#' @importFrom purrr simplify_all transpose
#' @importFrom dplyr as_tibble
#' @export
#' @rdname b2_list_buckets
b2_list_buckets_df <- function(account_id = Sys.getenv("B2_ACCOUNT_ID")) {
  bucket_list <- b2_list_buckets(account_id)
  as_tibble(simplify_all(transpose(bucket_list[["buckets"]])))
}

