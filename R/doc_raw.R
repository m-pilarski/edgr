prep_doc_raw_string <- function(.doc_raw_url){

  checkmate::assert_character(
    x=.doc_raw_url, len=1, null.ok=TRUE, any.missing=FALSE
  )

  tryCatch({

    .doc_raw_response <-
      httr2::request(.doc_raw_url) |>
      httr2::req_timeout(60) |>
      (with_retries(\(...req){
        ...req |>
          httr2::req_headers(`user-agent`=getOption("edgr.user_agent")) |>
          httr2::req_perform()
      }))()

    if(httr2::resp_status(.doc_raw_response) != 200){
      stop(httr2::resp_status_desc(.doc_raw_response), call.=FALSE)
    }

    .doc_raw_string <- httr2::resp_body_string(
      .doc_raw_response, encoding="UTF-8"
    )

  }, error=function(..error){

    message("Error: ", ..error$message)
    .doc_raw_string <<- simpleError(..error$message)

  }, interrupt=function(...){

    stop("Interrupted by the user", call.=FALSE)

  })

  return(.doc_raw_string)

}

#' Title
#'
#' @param .doc_data ...
#'
#' @return ...
#' @export
#'
#' @examples NULL
gather_doc_raw_data <- function(.doc_data){

  # .doc_data <- set_data_dir(doc_raw_data, "examples/data/")

  checkmate::assert_tibble(.doc_data, min.rows=1)

  .data_dir <- get_data_dir(.doc_data)

  .doc_data <-
    .doc_data |>
    dplyr::rowwise() |>
    dplyr::group_split() |>
    burrr::best_map(\(..row_data){

      ..row_data <<- ..row_data

      ..doc_raw_string_rel_path <- get_storage_rel_path(
        .file_set="doc_raw_string", .com_cik=..row_data[["com_cik"]],
        .doc_an=..row_data[["doc_an"]]
      )

      ..doc_raw_string_abs_path <- fs::path_abs(
        fs::path(.data_dir, ..doc_raw_string_rel_path)
      )

      fs::dir_create(fs::path_dir(..doc_raw_string_abs_path))

      if(!fs::file_exists(..doc_raw_string_abs_path)){

        ..doc_raw_string <- prep_doc_raw_string(
          .doc_raw_url=..row_data[["doc_raw_url"]]
        )
        qs::qsave(..doc_raw_string, ..doc_raw_string_abs_path, preset="archive")

      }

      ..row_data <- tibble::add_column(
        ..row_data, doc_raw_string_rel_path = list(..doc_raw_string_rel_path)
      )

      return(..row_data)

    }, .workers=getOption("edgr.n_workers")) |>
    dplyr::bind_rows() |>
    set_data_dir(.data_dir=.data_dir)

  return(.doc_data)

}



