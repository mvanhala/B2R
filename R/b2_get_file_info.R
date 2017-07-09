
#' Get file info
#'
#' Get information about a file stored in B2
#'
#' See \url{https://www.backblaze.com/b2/docs/b2_get_file_info.html}
#'
#' Note that the file id, not the file name, is required. To get file
#' information using the bucket name and file name, use
#' \code{\link{b2_get_file_info_by_name}}.
#'
#' @param file_id The file id
#' @return A list of the JSON response
#' @export
b2_get_file_info <- function(file_id) {
  file_resp <- httr::POST(
    paste0(Sys.getenv("B2_API_URL"), "/b2api/v1/b2_get_file_info"),
    httr::add_headers(Authorization = Sys.getenv("B2_AUTHORIZATION_TOKEN")),
    body = list(fileId = file_id),
    encode = "json"
  )

  if (httr::status_code(file_resp) != 200L) {
    stop("\n  File info failed \n  ", httr::content(file_resp)$message)
  }

  httr::content(file_resp)
}

#' @importFrom rlang .data
get_file_id <- function(bucket_name, file_name) {
  bucket_files <- b2_files_by_bucket_name_df(bucket_name)
  specific_file <- dplyr::filter(bucket_files, .data$fileName == file_name)
  if (nrow(specific_file) == 0) stop("File does not exist in bucket")
  specific_file[["fileId"]]
}

#' Get file info
#'
#' Get information about a file stored in B2
#'
#' To get file information using the file id, use \code{\link{b2_get_file_info}}.
#'
#' @param bucket_name The name of the bucket containing the file
#' @param file_name The name of the file
#' @return A list of the JSON response
#' @export
b2_get_file_info_by_name <- function(bucket_name, file_name) {
  file_id <- get_file_id(bucket_name, file_name)
  b2_get_file_info(file_id)
}




