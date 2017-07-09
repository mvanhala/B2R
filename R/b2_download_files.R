
#' Download a file
#'
#' Download a file in B2 by the file id
#'
#' See \url{https://www.backblaze.com/b2/docs/b2_download_file_by_id.html}
#'
#' The file id can be obtained from a call to \code{\link{b2_list_file_names}}.
#'
#' To download a file using the bucket name and file use, call
#' \code{\link{b2_download_file_by_name}}
#'
#' \subsection{Optional parameters}{
#' An optional arguments to the API call can be passed as named arguments
#'
#' \code{Range} - A byte-range request, as described in
#' \url{https://www.backblaze.com/b2/docs/b2_download_file_by_id.html}
#'
#' }
#'
#' @param file_id The file id
#' @param output_file (Optional) Path of the file on disk to which to save
#' the downloaded content
#' @param ... Optional parameters (see details)
#' @return The response from \code{httr::GET}
#' @export
b2_download_file_by_id <- function(file_id, output_file, ...) {
  output <- httr::write_memory()
  if (!missing(output_file)) {
    output <- httr::write_disk(output_file)
  }

  file_resp <- httr::GET(
    paste0(Sys.getenv("B2_DOWNLOAD_URL"), "/b2api/v1/b2_download_file_by_id"),
    httr::add_headers(Authorization = Sys.getenv("B2_AUTHORIZATION_TOKEN"), ...),
    output,
    query = list(fileId = file_id)
  )

  if (httr::status_code(file_resp) != 200L) {
    if (!((httr::status_code(file_resp) == 206L) && ("Range" %in% names(rlang::dots_list(...))))) {
      stop("\n  Download failed \n  ", httr::content(file_resp)$message)
    }
  }

  file_resp
}

#' Download a file
#'
#' Download a file in B2 by the file name
#'
#' See \url{https://www.backblaze.com/b2/docs/b2_download_file_by_name.html}
#'
#' To download a file using the file id, call
#' \code{\link{b2_download_file_by_id}}
#'
#' \subsection{Optional parameters}{
#' An optional arguments to the API call can be passed as named arguments
#'
#' \code{Range} - A byte-range request, as described in
#' \url{https://www.backblaze.com/b2/docs/b2_download_file_by_id.html}
#'
#' }
#'
#' @param bucket_name The name of the bucket containing the file
#' @param file_name The name of the file in the bucket
#' @param output_file (Optional) Path of the file on disk to which to save
#' the downloaded content
#' @param ... Optional parameters (see details)
#' @return The response from \code{httr::GET}
#' @export
b2_download_file_by_name <- function(bucket_name, file_name, output_file, ...) {
  output <- httr::write_memory()
  if (!missing(output_file)) {
    output <- httr::write_disk(output_file)
  }

  file_resp <- httr::GET(
    paste0(Sys.getenv("B2_DOWNLOAD_URL"), "/file", "/", bucket_name, "/", file_name),
    httr::add_headers(Authorization = Sys.getenv("B2_AUTHORIZATION_TOKEN"), ...),
    output
  )

  if (httr::status_code(file_resp) != 200L) {
    if (!((httr::status_code(file_resp) == 206L) && ("Range" %in% names(rlang::dots_list(...))))) {
      stop("\n  Download failed \n  ", httr::content(file_resp)$message)
    }
  }

  file_resp
}
