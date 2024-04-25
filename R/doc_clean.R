#' Title
#'
#' @param .doc_raw_string ...
#'
#' @return ...
#' @export
#'
#' @examples NULL
prep_doc_clean_list <- function(.doc_raw_string){

  node_not_text_regex <- stringi::stri_c(
    stringi::stri_c(
      "(?:",
      c(
        "(?i)<SEC-DOCUMENT>.*\\n",
        "(?i)<SEC-HEADER>(?s:.)*?</SEC-HEADER>",
        stringi::stri_c(
          "(?i)<DOCUMENT>\r?\n<TYPE>(EX|XML|ZIP|GRAPHIC|COVER|RENDERED XBRL|",
          "JSON)(?s:.)*?</DOCUMENT>"
        ),
        "(?i)<IX:HEADER>(?s:.)*?</IX:HEADER>",
        stringi::stri_c(
          "(?i)<TABLE( +[a-z0-9]+ *= *((\"[^\">]+?\")|(\'[^\'>]+?\')|",
          "[a-z0-9\\\\.]+))* *>(?s:.)*?</TABLE>"
        ),
        "(?i)<DOCUMENT>\r?\n(.*\r?\n){0,10}<PDF>\r?\nbegin(?s:.)*?</DOCUMENT>"
      ),
      ")"
    ),
    collapse="|"
  )

  abbrevs_regex_vec <-
    abbrevs_adjusted |>
    dplyr::select(name=pattern, value=replacement) |>
    tibble::deframe()

  dash_chars_regex <-
    "(\\u2013|\\u2014|\\u2212|\\u002D)"
  # u0092 is not a real apostrophe-character
  apost_chars_regex <-
    "(\\u0027|\\u0060|\\u00B4|\\u2018|\\u2019|\\u0092)"
  quote_chars_regex <-
    "(\\u0022|\\u0027|\\u0060|\\u00B4|\\u2018|\\u2019|\\u201C|\\u201D)"

  # all ascii and currency symbols
  allowed_chars_regex <- "[[\\x00-\\x7F][\\p{Sc}]]"

  checkmark_regex <- stringi::stri_c(
    "(?i)((\\bindicate\\s+by\\s+check\\s+mark\\b)|",
    "(\\byes[[^a-z]xo]+no[[^a-z]xo]+))"
  )

  url_regex <- stringi::stri_c(
    "\\b((http|ftp)s?://|mailto:|www\\.)[^\\s/$.?#][[[:alnum:]]-._~:/?#",
    "\\[\\]@!$&'()*+%,;=]+[^\\.]\\b"
  )

  checkmate::assert_character(
    x=.doc_raw_string, len=1, null.ok=FALSE, any.missing=FALSE
  )

  .doc_clean_head_data <-
    .doc_raw_string |>
    stringi::stri_extract_first_regex(
      "(?i)(?<=<SEC-HEADER>)(?s:.)*?(?=</SEC-HEADER>)"
    ) |>
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

  .doc_clean_body_string <-
    .doc_raw_string |>
    stringi::stri_replace_all_fixed("\u2026", "...") |>
    stringi::stri_replace_all_regex(node_not_text_regex, "\u2026\n") |>
    # REPLACE </?DIV> WITH NEWLINE
    stringi::stri_replace_all_regex(
      "(?i)&lt;/?DIV.*?&gt;|</?DIV.*?>", ".\n"
    ) |>
    # IF MISSING, ADD A SENTENCE END MARKER AT END OF PARAGRAPHS
    stringi::stri_replace_all_regex(
      "(?i)(?<!\\.)\\s*((?:</P>)|(?:&lt;/P&gt;))", "\u2026 $1"
    ) |>
    str_unescape_html() |>
    # REPLACE ALL VERTICAL WHITSPACES WITH LINEFEEDS
    stringi::stri_replace_all_regex("\\v+", "\n") |>
    # REPLACE ALL HORIZONTAL WHITSPACES WITH SPACES
    stringi::stri_replace_all_regex("\\h+", " ") |>
    # REMOVES ALL SPACES PRECEEDED OR FOLLOWED BY LINEFEEDS
    stringi::stri_replace_all_regex("(?<=\n) +| +(?=\n)", "") |>
    # REMOVE ALL THE PAGE NUMBERS FOLLOWED BY "TABLE OF CONTENTS"
    stringi::stri_replace_all_regex("\n\\d+\nTable of Contents", "\n") |>
    # remove unwanted sentence-end-markers
    klartext::str_blur_url(.replacement_url="<URL>") |>
    # stringi::stri_replace_all_regex(abbrevs_regex_vec) |>
    stringi::stri_replace_all_regex("(?<=\\b[[:alpha:]])\\.", "") |>
    stringi::stri_replace_all_regex(
      "(?<=[[:digit:]])[.](?=[[:digit:]])", ","
    ) |>
    stringi::stri_replace_all_regex(
      "(?<=[[:alnum:]])[.](?=[[:alnum:]])", " "
    ) |>
    # insert . as sentence-end-marker before 2 or more linefeeds
    stringi::stri_replace_all_regex("(?<![.!?])(?=(\\s*?\n){2,}+)", ".") |>
    # split sentence after each [.!?]
    stringi::stri_split_regex("(?<=[.!?\u2026])") |>
    unlist() |>
    stringi::stri_subset_regex("[^\\s]\\s*[.!?]") |>
    tibble::as_tibble_col("sen_text") |>
    dplyr::mutate(
      sen_id = dplyr::row_number(),
      sen_digit_char_share = tidyr::replace_na(
        stringi::stri_count_regex(sen_text, "[0-9]") / nchar(sen_text), 0
      ),
      sen_digit_chunk_count = stringi::stri_count_regex(sen_text, "[0-9]+"),
      sen_digit_chunk_seq_len_max =
        sen_text |>
        stringi::stri_extract_all_regex(
          "(?<=(^| ))[:digit:]+( [:digit:]+)*(?=( |$))"
        ) |>
        purrr::flatten_chr() |>
        purrr::discard(is.na) %0% "" |>
        stringi::stri_count_regex("[:digit:]+") |>
        max(),
      sen_alpha_char_share = tidyr::replace_na(
        stringi::stri_count_regex(sen_text, "[a-zA-Z]") / nchar(sen_text), 0
      ),
      sen_alpha_chunk_count = stringi::stri_count_regex(sen_text, "[a-zA-Z]+"),
      sen_line_nchar =
        sen_text |>
        stringi::stri_split_regex("\\h*\n\\h*") |>
        purrr::map(nchar),
      sen_has_checkmark = vctrs::`vec_slice<-`(
        x=stringi::stri_detect_regex(sen_text, checkmark_regex),
        i=sen_id > 200,
        value=FALSE
      ),
      sen_or_follow_has_checkmark =
        sen_id < max(c(sen_id[sen_has_checkmark], 0))
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      sen_is_valid = !any(
        all(
          `>`(
            purrr::map_dbl(sen_line_nchar, max),
            purrr::map_dbl(sen_line_nchar, min) * 2
          ),
          `<`(
            purrr::map_dbl(sen_line_nchar, median),
            purrr::map_dbl(sen_line_nchar, mean)
          ),
          `>=`(
            purrr::map_dbl(sen_line_nchar, length), 10
          )
        ),
        sen_digit_char_share > 0.25,
        sen_alpha_char_share < 0.50,
        sen_alpha_chunk_count <= 4,
        sen_digit_chunk_count >= 8,
        sen_digit_chunk_seq_len_max >= 4,
        length(sen_line_nchar) >= 20,
        sen_or_follow_has_checkmark
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(sen_is_valid) |>
    # mutate(sen = str_c(sen_id, " +-+ ", sen)) |>
    dplyr::pull(sen_text) |>
    # multiplication sign
    stringi::stri_replace_all_regex("(\\u00D7)", "*") |>
    # division sign
    stringi::stri_replace_all_regex("(\\u00F7|\u2215)", "/") |>
    # dash/minus sign
    stringi::stri_replace_all_regex(dash_chars_regex, "-") |>
    stringi::stri_replace_all_regex(
      stringi::stri_c("(", apost_chars_regex, "|", quote_chars_regex, ")"), "\'"
    ) |>
    # stringi::stri_replace_all_regex(bracket_chars$o, "(") |>
    # stringi::stri_replace_all_regex(bracket_chars$c, ")") |>
    # remove hyphens
    stringi::stri_replace_all_regex(
      stringi::stri_c("(?<=[[:lower:]])- *\n+ *(?=[[:lower:]])"), ""
    ) |>
    # replace all controll characters with spaces
    stringi::stri_replace_all_regex("[[:cntrl:]]+", " ") |>
    # replace all latin characters with extensions (apostrophe etc.) with
    # their basic form
    klartext::str_convert_nonascii(.repl_no_trans=NULL) |>
    # replace all unwanted characters with spaces
    stringi::stri_replace_all_regex(
      stringi::stri_c(" *[^{", allowed_chars_regex, "}]+ *"), " "
    ) |>
    # remove the start of each sentence up to the first alphanumeric character
    stringi::stri_replace_all_regex("^[^[:alnum:] ]+", "") |>
    # ...
    klartext::str_unify_spacing()

  .result <- list(
    doc_clean_head_data = .doc_clean_head_data,
    doc_clean_body_string = .doc_clean_body_string
  )

  return(.result)

}


#' Title
#'
#' @param .doc_data ...
#'
#' @return ...
#' @export
#'
#' @examples NULL
gather_doc_clean_data <- function(.doc_data){

  checkmate::assert_tibble(.doc_data)

  .data_dir <- get_data_dir(.doc_data)

  .doc_data <-
    .doc_data |>
    dplyr::rowwise() |>
    dplyr::group_split() |>
    burrr::best_map(\(..row_data){

      # ..row_data <<- ..row_data

      ..doc_raw_string_rel_path <- get_storage_rel_path(
        .file_set="doc_raw_string", .com_cik=..row_data[["com_cik"]],
        .doc_an=..row_data[["doc_an"]]
      )

      ..doc_raw_string_abs_path <- fs::path_abs(
        fs::path(.data_dir, ..doc_raw_string_rel_path)
      )

      ..doc_clean_body_string_rel_path <- get_storage_rel_path(
        .file_set="doc_clean_body_string", .com_cik=..row_data[["com_cik"]],
        .doc_an=..row_data[["doc_an"]]
      )

      ..doc_clean_body_string_abs_path <- fs::path_abs(
        fs::path(.data_dir, ..doc_clean_body_string_rel_path)
      )

      ..doc_clean_head_data_rel_path <- get_storage_rel_path(
        .file_set="doc_clean_head_data", .com_cik=..row_data[["com_cik"]],
        .doc_an=..row_data[["doc_an"]]
      )

      ..doc_clean_head_data_abs_path <- fs::path_abs(
        fs::path(.data_dir, ..doc_clean_head_data_rel_path)
      )

      fs::dir_create(fs::path_dir(..doc_clean_body_string_abs_path))
      fs::dir_create(fs::path_dir(..doc_clean_head_data_abs_path))

      ..doc_raw_is_stored <- all(
        fs::file_exists(..doc_raw_string_abs_path)
      )

      if(!..doc_raw_is_stored){

        ..doc_raw_string <- prep_doc_raw_string(
          .doc_raw_url=..row_data[["doc_raw_url"]]
        )

      }else{

        ..doc_raw_string <- qs::qread(..doc_raw_string_abs_path)

      }

      ..doc_clean_is_stored <- all(
        fs::file_exists(..doc_clean_head_data_abs_path),
        fs::file_exists(..doc_clean_body_string_abs_path)
      )

      if(!..doc_clean_is_stored){

        ..doc_clean_list <- prep_doc_clean_list(
          .doc_raw_string=..doc_raw_string
        )
        qs::qsave(
          ..doc_clean_list[["doc_clean_head_data"]],
          ..doc_clean_head_data_abs_path, preset="archive"
        )
        readr::write_lines(
          ..doc_clean_list[["doc_clean_body_string"]],
          ..doc_clean_body_string_abs_path,
        )

      }

      ..row_data <- ..row_data |> tibble::add_column(
        doc_clean_head_data_rel_path = list(..doc_clean_head_data_rel_path),
        doc_clean_body_string_rel_path = list(..doc_clean_body_string_rel_path)
      )

      return(..row_data)

    }, .workers=getOption("edgr.n_workers")) |>
    dplyr::bind_rows() |>
    set_data_dir(.data_dir=.data_dir)

  return(.doc_data)

}


#' Title
#'
#' @param .doc_data ...
#' @param .doc_aggregate_fn ...
#'
#' @return ...
#' @export
#'
#' @examples
#' if(FALSE){
#'   NULL
#' }
aggregate_doc_clean_data <- function(.doc_data, .doc_aggregate_fn){

  .data_dir <- get_data_dir(.doc_data)

  # .doc_aggregate_fn <<- .doc_aggregate_fn

  .doc_aggregate_fn <- rlang::as_function(.doc_aggregate_fn)
  .doc_aggregate_fn_form <- formals(.doc_aggregate_fn)
  .doc_aggregate_fn_nargs_fail <- `&&`(
    length(.doc_aggregate_fn_form) < 2,
    !"..." %in% names(.doc_aggregate_fn_form)
  )
  if(.doc_aggregate_fn_nargs_fail){
    rlang::abort(
      message=c(
        "`.doc_aggregate_fn` must accept at least two arguments.",
        i = "You can use `...` to absorb unused components."
      ),
      call=NULL
    )
  }

  .doc_data <-
    .doc_data |>
    burrr::chunk_df(.n_rows=1) |>
    burrr::best_map(function(.row_data){

      .doc_clean_head_data_abs_path <- fs::path(
        .data_dir, .row_data$doc_clean_head_data_rel_path
      )

      .doc_clean_body_string_abs_path <- fs::path(
        .data_dir, .row_data$doc_clean_body_string_rel_path
      )

      .doc_clean_head_data <- qs::qread(.doc_clean_head_data_abs_path)

      .doc_clean_body_string <- readr::read_file(
        .doc_clean_body_string_abs_path
      )

      .doc_clean_aggregate_data <- rlang::exec(
        .doc_aggregate_fn, .doc_clean_body_string, .doc_clean_head_data
      )

      stopifnot(inherits(.doc_clean_aggregate_data, "data.frame"))
      stopifnot(nrow(.doc_clean_aggregate_data) == 1)

      .row_data <- bind_cols(.row_data, .doc_clean_aggregate_data)

      stopifnot(nrow(.row_data) == 1)

      return(.row_data)

    }, .workers=getOption("edgr.n_workers")) |>
    dplyr::bind_rows() |>
    set_data_dir(.data_dir=.data_dir)

  gc()
  return(.doc_data)

}

# .agg_head_date_report <-
#   .head_data |>
#   filter(str_detect(key, "CONFORMED PERIOD OF REPORT")) |>
#   pluck("value", 1, .default=NA_character_) |>
#   strptime("%Y%m%d") |>
#   as.POSIXct() |>
#   as_tibble_col("doc_date_report")
