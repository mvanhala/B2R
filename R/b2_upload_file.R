

#' Get upload URL
#'
#' Get a URL for uploading a file to B2
#'
#' See \url{https://www.backblaze.com/b2/docs/b2_get_upload_url.html}
#'
#' @param bucket_name The name of the bucket containing the file
#' @param file_name The name of the file
#' @return A list of the JSON response
#' @export
b2_get_upload_url <- function(bucket_id) {
  file_resp <- httr::POST(
    paste0(Sys.getenv("B2_API_URL"), "/b2api/v1/b2_get_upload_url"),
    httr::add_headers(Authorization = Sys.getenv("B2_AUTHORIZATION_TOKEN")),
    body = list(bucketId = bucket_id),
    encode = "json"
  )

  if (httr::status_code(file_resp) != 200L) {
    stop("\n  Get upload URL failed \n  ", httr::content(file_resp)$message)
  }

  httr::content(file_resp)
}

#' Upload a file
#'
#' Upload a file to B2
#'
#' See \url{https://www.backblaze.com/b2/docs/b2_upload_file.html}
#'
#' This requires getting an upload URL and token by calling
#' \code{\link{b2_get_upload_url}} first
#'
#' To upload a file directly, use \code{\link{b2_upload_file_direct}}
#'
#' @param data Either a raw vector of the data or the path to the file
#' @param upload_url The upload_url
#' @param upload_token The upload token
#' @param upload_filename Name of the file to save in B2
#' @param mime_type MIME type
#' @return A list of the JSON response
#' @export
b2_upload_file <- function(data, upload_url, upload_token, upload_filename,
                           mime_type = mime::guess_type(upload_filename)) {
  if (!is.raw(data) && !file.exists(data)) stop("File does not exist")
  if (is.raw(data)) {
    len <- length(data)
    hash <- as.character(openssl::sha1(data))
  } else  {
    len <- file.info(data)[["size"]]
    con <- file(data, open = "rb")
    hash <- as.character(openssl::sha1(con))
    close(con)
    data <- httr::upload_file(data)
  }

  upload_resp <- httr::POST(
    upload_url,
    httr::add_headers(
      Authorization = upload_token,
      `X-Bz-File-Name` = utils::URLencode(upload_filename),
      `Content-Type` = mime_type,
      `Content-Length` = len,
      `X-Bz-Content-Sha1` = hash
    ),
    body = data
  )

  if (httr::status_code(upload_resp) != 200L) {
    stop("\n  Upload failed \n  ", httr::content(upload_resp)$message)
  }

  httr::content(upload_resp)
}

#' Upload a file
#'
#' Upload a file to B2
#'
#' This performs both the step of getting an upload URL (\code{\link{b2_get_upload_url}})
#' and uploading the file (\code{\link{b2_upload_file}})
#'
#' @param data Either a raw vector of the data or the path to the file
#' @param bucket_id The id of the bucket to which to upload the file
#' @param upload_filename Name of the file to save in B2
#' @param mime_type MIME type
#' @return A list of the JSON response
#' @export
b2_upload_file_direct <- function(data, bucket_id, upload_filename,
                                  mime_type = mime::guess_type(upload_filename)) {
  upload_info <- b2_get_upload_url(bucket_id)

  if (!is.raw(data) && !file.exists(data)) stop("File does not exist")
  if (is.raw(data)) {
    len <- length(data)
    hash <- as.character(openssl::sha1(data))
  } else {
    len <- file.info(data)[["size"]]
    con <- file(data, open = "rb")
    hash <- as.character(openssl::sha1(con))
    close(con)
    data <- httr::upload_file(data)
  }

  upload_resp <- httr::POST(
    upload_info[["uploadUrl"]],
    httr::add_headers(
      Authorization = upload_info[["authorizationToken"]],
      `X-Bz-File-Name` = utils::URLencode(upload_filename),
      `Content-Type` = mime_type,
      `Content-Length` = len,
      `X-Bz-Content-Sha1` = hash
    ),
    body = data
  )

  if (httr::status_code(upload_resp) != 200L) {
    stop("\n  Upload failed \n  ", httr::content(upload_resp)$message)
  }

  httr::content(upload_resp)
}

