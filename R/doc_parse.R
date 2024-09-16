#' Title
#'
#' @param .doc_raw_string ...
#'
#' @return a `list` ...
prep_doc_parse_list <- function(.doc_raw_string){

  checkmate::assert_character(.doc_raw_string, len=1)

  # remove document nodes of certain types or with pdf ()
  .doc_document_type_drop_regex <- stringi::stri_c(
    "^(?i)(ZIP|EXCEL|XML|GRAPHIC|COVER|XBRL|RENDERED XBRL|JSON|EX\\-|TABLE)"
  )
  .doc_filter_document_idx_mat <-
    .doc_raw_string |>
    stringi::stri_locate_all_regex(
      "(?i)<DOCUMENT>(?s:.)*?(?!<DOCUMENT>)</DOCUMENT>"
    ) |>
    purrr::chuck(1) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      doc_document_raw =
        .doc_raw_string |>
        stringi::stri_sub(start, end, use_matrix=FALSE),
      doc_document_type_is_drop =
        doc_document_raw |>
        stringi::stri_extract_first_regex("(?i)(?<=\\<TYPE\\>).+?(?=\n)") |>
        stringi::stri_trim_both() |>
        stringi::stri_detect_regex(.doc_document_type_drop_regex),
      doc_document_has_pdf =
        doc_document_raw |>
        stringi::stri_detect_regex("(?i)\\<PDF\\>")
    ) |>
    dplyr::filter(doc_document_type_is_drop | doc_document_has_pdf) |>
    dplyr::select(from=start, to=end) |>
    as.matrix()

  if(nrow(.doc_filter_document_idx_mat) > 0){
    .doc_filter_document_string <- stringi::stri_sub_replace_all(
      .doc_raw_string, from=.doc_filter_document_idx_mat, use_matrix=TRUE,
      replacement=" "
    )
  }else{
    .doc_filter_document_string <- .doc_raw_string
  }

  .doc_xml <- xml2::read_html(
    stringi::stri_c("<EDGR:DOC>", .doc_filter_document_string, "</EDGR:DOC>"),
    options=c("RECOVER", "NOERROR", "NOBLANKS", "HUGE")
  )

  # parse the document header to a table
  .doc_parse_head_data <-
    .doc_xml |>
    xml2::xml_find_all("//sec-header") |>
    xml2::xml_text() |>
    stringi::stri_split_fixed("\n") |>
    purrr::flatten_chr() |>
    tibble::as_tibble_col("key_value_raw") |>
    dplyr::mutate(
      level = nchar(
        tidyr::replace_na(
          stringi::stri_extract_first_regex(key_value_raw, "^\t+"), ""
        )
      ),
      key = stringi::stri_trim_both(
        stringi::stri_extract_first_regex(key_value_raw, ".+(?=:)")
      ),
      value = stringi::stri_trim_both(
        stringi::stri_extract_first_regex(key_value_raw, "(?<=:).+")
      )
    )

  # remove the header
  .doc_xml |>
    xml2::xml_find_all("//sec-header") |>
    xml2::xml_remove()

  # # remove document nodes with certain types
  # .node_type_remove <- c(
  #   "ZIP", "EXCEL", "XML", "GRAPHIC", "COVER", "XBRL", "RENDERED XBRL", "JSON",
  #   "EX-"
  # )
  # purrr::walk(.node_type_remove, \(.node_type){
  #   .doc_xml |>
  #     xml2::xml_find_all(
  #       stringi::stri_c(
  #         "//type[starts-with(text(), '", .node_type, "')]",
  #         "/ancestor::document[position() = 1]"
  #       )
  #     ) |>
  #     xml2::xml_remove()
  # })

  # remove document nodes containing pdf
  .doc_xml |>
    xml2::xml_find_all("//pdf/ancestor::document[position() = 1]") |>
    xml2::xml_remove()

  # remove nodes from the ix namespace
  .doc_xml |>
    xml2::xml_find_all("//*[starts-with(name(), 'ix:')]") |>
    xml2::xml_remove()

  # convert html tables to json lists and enclose in <EDGR:TABLE_JSON> tags
  .doc_xml |>
    xml2::xml_find_all("//*/ancestor::table[last()]") |>
    xml2::xml_remove() # JUST REMOVE FOR NOW
    # purrr::walk(\(.node){
    #   .table_json <- jsonlite::toJSON(xml2::as_list(.node))
    #   xml2::xml_remove(xml2::xml_children(.node))
    #   xml2::xml_name(.node) <- "div"
    #   xml2::xml_text(.node) <- stringi::stri_c(
    #     "<EDGR:TABLE_JSON>", .table_json, "</EDGR:TABLE_JSON>"
    #   )
    # })

  # add two newlines to the end of all <div> and <p>
  .doc_xml |>
    xml2::xml_find_all("//*[name()='div' or name()='p']") |>
    purrr::walk(\(.x){
      xml2::xml_text(.x) <- stringi::stri_c(xml2::xml_text(.x), "\n\n")
    })

  # extract text and perform basic standardization of whitespaces
  .doc_parse_body_string <-
    .doc_xml |>
    xml2::xml_find_all("//*[normalize-space(text()) != '']") |>
    xml2::xml_text() |>
    stringi::stri_c(collapse=" ") |>
    stringi::stri_replace_all_regex("\\v", "\n") |>
    stringi::stri_replace_all_regex("\\h", " ")

  .doc_parse_list <- list(
    doc_parse_head_data = .doc_parse_head_data,
    doc_parse_body_string = .doc_parse_body_string
  )

  return(.doc_parse_list)

}

#' Title
#'
#' @param .doc_data ...
#'
#' @return ...
#' @export
#'
#' @examples NULL
gather_doc_parse_data <- function(.doc_data){

  checkmate::assert_tibble(.doc_data)

  .data_dir <<- get_data_dir(.doc_data)

  .doc_data <-
    .doc_data |>
    dplyr::rowwise() |>
    dplyr::group_split() |>
    burrr::best_map(\(..row_data){

      ..row_data <<- ..row_data#; stop()

      ..doc_raw_string_rel_path <- get_storage_rel_path(
        .file_set="doc_raw_string", .com_cik=..row_data[["com_cik"]],
        .doc_an=..row_data[["doc_an"]]
      )

      ..doc_raw_string_abs_path <- fs::path_abs(
        fs::path(.data_dir, ..doc_raw_string_rel_path)
      )

      ..doc_parse_body_string_rel_path <- get_storage_rel_path(
        .file_set="doc_parse_body_string", .com_cik=..row_data[["com_cik"]],
        .doc_an=..row_data[["doc_an"]]
      )

      ..doc_parse_body_string_abs_path <- fs::path_abs(
        fs::path(.data_dir, ..doc_parse_body_string_rel_path)
      )

      ..doc_parse_head_data_rel_path <- get_storage_rel_path(
        .file_set="doc_parse_head_data", .com_cik=..row_data[["com_cik"]],
        .doc_an=..row_data[["doc_an"]]
      )

      ..doc_parse_head_data_abs_path <- fs::path_abs(
        fs::path(.data_dir, ..doc_parse_head_data_rel_path)
      )

      fs::dir_create(fs::path_dir(..doc_parse_body_string_abs_path))
      fs::dir_create(fs::path_dir(..doc_parse_head_data_abs_path))


      ..doc_parse_is_stored <- all(
        fs::file_exists(..doc_parse_head_data_abs_path),
        fs::file_exists(..doc_parse_body_string_abs_path)
      )

      ..doc_raw_is_stored <- all(
        fs::file_exists(..doc_raw_string_abs_path)
      )

      if(!..doc_parse_is_stored){

        if(!..doc_raw_is_stored){

          ..doc_raw_string <- prep_doc_raw_string(
            .doc_raw_url=..row_data[["doc_raw_url"]]
          )

        }else{

          ..doc_raw_string <- qs::qread(..doc_raw_string_abs_path)

        }

        if(!is(..doc_raw_string, "error")){
          ..doc_parse_list <- prep_doc_parse_list(
            .doc_raw_string=..doc_raw_string
          )
        }else{
          ..doc_parse_list <- list(
            doc_parse_head_data = rlang::error_cnd(
              message="Error in previous step"
            ),
            doc_parse_body_string = rlang::error_cnd(
              message="Error in previous step"
            )
          )
        }

        qs::qsave(
          ..doc_parse_list[["doc_parse_head_data"]],
          ..doc_parse_head_data_abs_path, preset="archive"
        )
        qs::qsave(
          ..doc_parse_list[["doc_parse_body_string"]],
          ..doc_parse_body_string_abs_path, preset="archive"
        )

      }

      ..row_data <- ..row_data |> tibble::add_column(
        doc_parse_head_data_rel_path = list(..doc_parse_head_data_rel_path),
        doc_parse_body_string_rel_path = list(..doc_parse_body_string_rel_path)
      )

      return(..row_data)

    }, .workers=getOption("edgr.n_workers")) |>
    dplyr::bind_rows() |>
    set_data_dir(.data_dir=.data_dir)

  return(.doc_data)

}
