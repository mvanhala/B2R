
#' List file names
#'
#' List of files in a B2 bucket, starting at a given file name
#'
#' See \url{https://www.backblaze.com/b2/docs/b2_list_file_names.html}
#'
#' Note that you provide the id of the bucket to this function, not the name
#' of the bucket. The id of the bucket is obtained from a call to
#' \code{\link{b2_list_buckets}}.
#'
#' To get the a list of files by bucket name, call \code{\link{b2_files_by_bucket_name}}
#'
#' \subsection{Optional parameters}{
#' Optional arguments to the API call can be passed as named arguments
#'
#' \code{startFileName} - The first file name to return
#'
#' \code{maxFileCount} - Maximum number of files to return (defaults to 100, maximum 10000)
#'
#' \code{prefix} - Return files starting with prefix
#'
#' \code{delimiter} - Used to limit return to top folder or single subfolder
#' }
#'
#' @param bucket_id Id of the bucket to get list of files. Note that this is not the name
#' of the bucket.
#' @param ... Optional parameters (see details)
#' @return \code{b2_list_file_names} returns a list of the JSON response.
#'
#' \code{b2_list_file_names_df} returns a tibble of the file information.
#' @export
b2_list_file_names <- function(bucket_id, ...) {
  file_resp <- httr::POST(
    paste0(Sys.getenv("B2_API_URL"), "/b2api/v1/b2_list_file_names"),
    httr::add_headers(Authorization = Sys.getenv("B2_AUTHORIZATION_TOKEN")),
    body = list(bucketId = bucket_id, ...),
    encode = "json"
  )

  if (httr::status_code(file_resp) != 200L) {
    stop("\n  File list failed \n  ", httr::content(file_resp)$message)
  }

  httr::content(file_resp)
}


#' @importFrom purrr simplify_all transpose
#' @importFrom dplyr as_tibble
#' @export
#' @rdname b2_list_file_names
b2_list_file_names_df <- function(bucket_id, ...) {
  file_list <- b2_list_file_names(bucket_id, ...)
  as_tibble(simplify_all(transpose(file_list[["files"]])))
}

#' List file names
#'
#' List of files in a B2 bucket, starting at a given file name
#'
#' See \url{https://www.backblaze.com/b2/docs/b2_list_file_names.html}
#'
#' In this function, you provide the name of the bucket, as opposed to
#' \code{\link{b2_list_file_names}}, where you provide the id of the bucket.
#'
#' \subsection{Optional parameters}{
#' Optional arguments to the API call can be passed as named arguments
#'
#' \code{startFileName} - The first file name to return
#'
#' \code{maxFileCount} - Maximum number of files to return (defaults to 100, maximum 10000)
#'
#' \code{prefix} - Return files starting with prefix
#'
#' \code{delimiter} - Used to limit return to top folder or single subfolder
#' }
#'
#' @param bucket_name Name of the bucket to get list of files
#' @param account_id B2 account ID
#' @param ... Optional parameters (see details)
#' @return \code{b2_files_by_bucket_name} returns a list of the JSON response.
#'
#' \code{b2_files_by_bucket_name_df} returns a tibble of the file information.
#' @importFrom rlang .data
#' @export
b2_files_by_bucket_name <- function(bucket_name, account_id = Sys.getenv("B2_ACCOUNT_ID"), ...) {
  buckets <- b2_list_buckets_df(account_id)
  specific_bucket <- dplyr::filter(buckets, .data$bucketName == bucket_name)
  if (nrow(specific_bucket) == 0) stop("Bucket does not exist in account")
  bucket_id <- specific_bucket[["bucketId"]]
  b2_list_file_names(bucket_id, ...)
}


#' @importFrom purrr simplify_all transpose
#' @importFrom dplyr as_tibble
#' @export
#' @rdname b2_files_by_bucket_name
b2_files_by_bucket_name_df <- function(bucket_name, account_id = Sys.getenv("B2_ACCOUNT_ID"), ...) {
  file_list <- b2_files_by_bucket_name(bucket_name, account_id, ...)
  as_tibble(simplify_all(transpose(file_list[["files"]])))
}



