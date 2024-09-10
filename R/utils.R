#' @importFrom vctrs vec_slice
#' @export
vctrs::vec_slice

#' @importFrom vctrs `%0%`
#' @export
vctrs::`%0%`

#' @importFrom rlang `%||%`
#' @export
rlang::`%||%`

#' @importFrom rlang `%@%`
#' @export
rlang::`%@%`

#' @importFrom rlang `%@%<-`
#' @export
rlang::`%@%<-`

#' Title
#'
#' MODIFY A FUNCTION SO THAT IT REEXECUTES IF AN ERROR IS THROWN THAT CONTAINS
#' "TIMEOUT WAS REACHED" OR "COULD NOT RESOLVE HOST". IF AN OTHER ERROR IS
#' THROWN RETURN THAT ERROR OR STOP (depending on .return_error)
#'
#' @param .function ...
#' @param .return_error ...
#'
#' @return A copy of .function
#'
#' @examples NULL
with_retries <- function(.function, .return_error=FALSE){

  checkmate::assert_function(.function, null.ok=FALSE)
  checkmate::assert_logical(
    .return_error, len=1, null.ok=FALSE, any.missing=FALSE
  )

  .function_mod <- function(...){
    .result <- NULL; while(is.null(.result)){
      tryCatch({
        .result <- .function(...)
        if(is.null(.result)){stop("function returned NULL")}
      }, error=function(.error){
        .error_is_connection <- stringr::str_detect(
          .error$message, "(Timeout was reached)|(Could not resolve host)"
        )
        .error_is_forbidden <- stringr::str_detect(
          .error$message, "HTTP 403 Forbidden"
        )
        if(.error_is_connection){
          warning(
            format(lubridate::now()), ": connection failure - retrying in 30s",
            call.=FALSE
          )
          Sys.sleep(30)
        }else if(.error_is_forbidden){
          warning(
            format(lubridate::now()), ": changing user agent - retrying in 30s",
            call.=FALSE
          )
          options("edgr.user_agent"=random_user_agent())
          Sys.sleep(30)
        }else if(.return_error){
          warning(.error$message)
          .result <<- simpleError(.error$message)
        }else{
          stop(.error)
        }
      })
    }
    return(.result)
  }

  return(.function_mod)

}

#' Title
#'
#' @param .str ...
#'
#' @return A character vector
#'
#' @examples NULL
str_unescape_html <- function(.str){

  checkmate::assert_character(.str)

  .str |> purrr::map_chr(function(..str){
      xml2::xml_text(
        xml2::read_html(
          x=charToRaw(stringi::stri_c("<str>", ..str, "</str>")),
          options=c("RECOVER", "NOERROR", "NOBLANKS", "HUGE")
        )
      )
  })

}


#' Title
#'
#' @param .doc_data ...
#'
#' @return data_dir
#' @export
#'
#' @examples
#' if(FALSE){
#'   data_dir <- get_data_dir(doc_data)
#' }
get_data_dir <- function(.doc_data){

  checkmate::assert_tibble(.doc_data)

  .data_dir <- .doc_data %@% "data_dir"

  if(is.null(.data_dir)){stop(
    ".doc_data's 'data_dir' attribute is not specified. Use set_data_dir ",
    "to specify the directory that is used by edgr to store data."
  )}
  if(!fs::dir_exists(.data_dir)){stop(
    ".doc_data's 'data_dir' points to path that does not exist. ",
    "Use set_data_dir to specify the directory that is used by edgr to ",
    "store data"
  )}

  return(.data_dir)

}

#' Title ...
#'
#' @param .file_set character ....
#' @param .com_cik character ...
#' @param .doc_an character ...
#'
#' @return ...
get_storage_rel_path <- function(.file_set, .com_cik, .doc_an){

  .file_ext <- dplyr::case_when(
    .file_set %in% c("doc_clean_body_string") ~ "txt",
    .file_set %in% c(
      "doc_raw_string", "doc_clean_head_data", "doc_clean_head_data",
      "doc_parse_head_data", "doc_parse_body_string"
    ) ~ "qs",
    TRUE ~ NA_character_
  )

  stopifnot(!is.na(.file_ext))

  .storage_rel_path <- fs::path(
    stringi::stri_c(.file_set, "_files"), .com_cik, .doc_an,
    ext=.file_ext
  )

  return(.storage_rel_path)

}

#' Title
#'
#' @param .doc_data ...
#' @param .data_dir ...
#'
#'
#' @return ...
#'
#' @section Notes:
#' LALLALALAL
#'
#' @export
#'
#'
#'
#' @examples NULL
set_data_dir <- function(.doc_data, .data_dir){

  checkmate::assert_tibble(.doc_data)
  checkmate::assert_character(.data_dir, len=1, any.missing=FALSE, null.ok=TRUE)

  .data_dir_abs <- fs::path_abs(.data_dir)

  if(!fs::dir_exists(.data_dir_abs)){stop(
    "'", .data_dir, "' (.data_dir) is not an existing directory."
  )}

  .doc_data %@% "data_dir" <- .data_dir

  return(.doc_data)

}

#' Title
#'
#' @return ...
#' @export
#'
#' @examples NULL
random_user_agent <- function(){base::sample(user_agent_list, size=1)}
