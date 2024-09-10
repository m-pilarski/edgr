# load("R/sysdata.rda")
#
# options(edgr.n_workers=4)
# options(edgr.user_agent=sample(user_agent_list)[[1]])

#' Title
#'
#' @param .years ...
#' @param .quarters ...
#' @param .form_types ...
#' @param .data_dir ...
#' #'
#' @return A tibble ...
#' @export
#'
#' @examples
#' if(FALSE){
#'   doc_index <- get_doc_index(.years=2021:2022)
#' }
gather_doc_index_data <- function(
  .years, .quarters=1:4, .form_types="10-K", .data_dir=NULL
){

  checkmate::assert_numeric(x=.years, any.missing=FALSE, null.ok=FALSE)

  checkmate::assert_numeric(
    x=.quarters, lower=1, upper=4, any.missing=FALSE, null.ok=FALSE
  )

  checkmate::assert_character(.data_dir, len=1, null.ok=TRUE)

  stopifnot(all(c(.years, .quarters) %% 1 == 0))

  checkmate::assert_character(x=.form_types, null.ok=TRUE, any.missing=FALSE)

  if(any(.form_types != "10-K") | is.null(.form_types)){
    warning(
      "Everything in this package was only testet for the .form_type ",
      "\"10-K\". Proceed with caution!"
    )
  }

  .doc_index <-
    tidyr::expand_grid(year=.years, quarter=.quarters) |>
    purrr::transpose() |>
    burrr::best_map(function(..pars){

      # ..pars <<- ..pars

      ..doc_index_dir <- fs::dir_create(fs::file_temp())
      ..doc_index_zip_file <- fs::path(..doc_index_dir, "master.zip")
      ..doc_index_idx_file <- fs::path(..doc_index_dir, "master.idx")

      ..doc_index_resp <-
        httr2::request("https://www.sec.gov/Archives/edgar/full-index/") |>
        httr2::req_url_path_append(..pars[["year"]]) |>
        httr2::req_url_path_append(
          stringr::str_c("QTR", ..pars[["quarter"]])
        ) |>
        httr2::req_url_path_append("master.zip") |>
        httr2::req_timeout(60) |>
        httr2::req_headers(`user-agent`=getOption("edgr.user_agent")) |>
        (with_retries(httr2::req_perform))(path=..doc_index_zip_file)

      if(httr2::resp_status(..doc_index_resp) != 200){
        stop(httr2::resp_status_desc(..doc_index_chunk_dl), call.=FALSE)
      }

      utils::unzip(zipfile=..doc_index_zip_file, exdir=..doc_index_dir)

      ..doc_index_data <-
        readr::read_lines(..doc_index_idx_file, progress=FALSE) |>
        purrr::keep(function(...line){
          stringr::str_count(...line, "\\|") == 4
        }) |>
        stringr::str_c(collapse="\n") |>
        # stringi::stri_enc_toutf8() |>
        base::charToRaw() |>
        readr::read_delim(delim="|", progress=FALSE, show_col_types=FALSE) |>
        dplyr::mutate(
          com_cik = as.character(CIK),
          com_name = as.character(`Company Name`),
          doc_an = as.character(
            stringr::str_extract(Filename, "\\d{10}-\\d{2}-\\d{6}(?=\\.txt$)")
          ),
          doc_form_type = as.character(`Form Type`),
          doc_date_filed = lubridate::ymd(`Date Filed`),
          doc_raw_url = stringi::stri_c(
            "https://www.sec.gov/Archives/edgar/data/", com_cik, "/", doc_an,
            ".txt"
          ),
          .keep="none"
        )

      fs::dir_delete(..doc_index_dir)

      if(!is.null(.form_types)){
        ..doc_index_data <- dplyr::filter(
          ..doc_index_data, doc_form_type %in% .form_types
        )
      }

      return(..doc_index_data)

    }, .workers=getOption("edgr.n_workers"), .scheduling=Inf) |>
    dplyr::bind_rows()

  if(!is.null(.data_dir)){
    .doc_index <- set_data_dir(.doc_index, .data_dir)
  }

  gc()

  return(.doc_index)

}
