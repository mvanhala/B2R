

#' Delete file version
#'
#' Delete latest version of a file stored in B2
#'
#' See \url{https://www.backblaze.com/b2/docs/b2_delete_file_version.html}
#'
#' Note that the file id is required in addition to the file name. To
#' delete a file using the bucket name and file name, use
#' \code{\link{b2_delete_file_by_name}}.
#'
#' @param file_name The file name
#' @param file_id The file id
#' @return A list of the JSON response
#' @export
b2_delete_file_version <- function(file_name, file_id) {
  file_resp <- httr::POST(
    paste0(Sys.getenv("B2_API_URL"), "/b2api/v1/b2_delete_file_version"),
    httr::add_headers(Authorization = Sys.getenv("B2_AUTHORIZATION_TOKEN")),
    body = list(fileName = file_name, fileId = file_id),
    encode = "json"
  )

  if (httr::status_code(file_resp) != 200L) {
    stop("\n  File info failed \n  ", httr::content(file_resp)$message)
  }

  httr::content(file_resp)
}


#' Delete file version
#'
#' Delete latest version of a file stored in B2 by name
#'
#' See \url{https://www.backblaze.com/b2/docs/b2_delete_file_version.html}
#'
#' To delete a file using the file name and file id, use
#' \code{\link{b2_delete_file_by_name}}.
#'
#' @param bucket_name The name of the bucket containing the file
#' @param file_name The file name
#' @return A list of the JSON response
#' @export
b2_delete_file_by_name <- function(bucket_name, file_name) {
  file_id <- get_file_id(bucket_name, file_name)
  b2_delete_file_version(file_name, file_id)
}

