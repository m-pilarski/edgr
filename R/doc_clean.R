#' Title
#'
#' @param .doc_parse_body_string ...
#'
#' @return ...
#' @export
#'
#' @examples NULL
prep_doc_clean_body_string <- function(.doc_parse_body_string){

  abbrevs_regex_vec <-
    abbrevs_adjusted |>
    dplyr::mutate(
      name=pattern,
      # value=replacement
      value=stringi::stri_replace_all_regex(pattern, "[.!?]", ""),
      .keep="none"
    ) |>
    tibble::deframe()

  dash_chars_regex <- "(\\u2013|\\u2014|\\u2212|\\u002D)"
  # u0092 is not a real apostrophe-character
  apost_chars_regex <- "(\\u0027|\\u0060|\\u00B4|\\u2018|\\u2019|\\u0092)"
  quote_chars_regex <- (
    "(\\u0022|\\u0027|\\u0060|\\u00B4|\\u2018|\\u2019|\\u201C|\\u201D)"
  )

  # all ascii and currency symbols
  allowed_chars_regex <- "[[\\x00-\\x7F][\\p{Sc}]]"

  checkmark_regex <- stringi::stri_c(
    "(?i)((\\bindicate\\s+by\\s+check\\s+mark\\b)|",
    "(\\byes[[^a-z]xo]+no[[^a-z]xo]+))"
  )

  checkmate::assert_character(
    x=.doc_parse_body_string, len=1, null.ok=FALSE, any.missing=FALSE
  )

  .doc_clean_body_string <-
    .doc_parse_body_string |>
    # REMOVE "EDGR NODES"
    stringi::stri_replace_all_regex("<(EDGR:.+?)>(?s:.)*?</\\1>", " ") |>
    # CONVERT HTML CHARACTER REFERENCES TO UNICODE CHARACTERS
    klartext::str_convert_html() |>
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
    stringi::stri_replace_all_regex(
      pattern=names(abbrevs_regex_vec), replacement=unname(abbrevs_regex_vec),
      vectorize_all=FALSE
    ) |>
    stringi::stri_replace_all_regex("(?<=\\b[[:alpha:]])\\.", "") |>
    stringi::stri_replace_all_regex(
      "(?<=[[:digit:]])[.](?=[[:digit:]])", ","
    ) |>
    stringi::stri_replace_all_regex(
      "(?i)((^|\n) *(?:ITEM|SECTION) *[[:alnum:]]+).", "$1"
    ) |>
    stringi::stri_replace_all_regex(
      "(?<=[[:alnum:]])[.](?=[[:alnum:]])", " "
    ) |>
    # insert . as sentence-end-marker before 2 or more linefeeds
    stringi::stri_replace_all_regex("(?<![.!?])(?=(\\s*?\n){2,}+)", ".") |>
    # split sentence after each [.!?]
    stringi::stri_split_regex("(?<=[.!?])") |>
    unlist() |>
    stringi::stri_trim_both() |>
    stringi::stri_subset_regex("[^\\s]\\s*[.!?]") |>
    tibble::as_tibble_col("sen_text") |>
    dplyr::mutate(
      sen_id = dplyr::row_number(),
      sen_has_item_section_start = stringi::stri_detect_regex(
        sen_text, "^(?i)[[:space:]]*(ITEM|SECTION)[[:space:]]*[[:digit:]]"
      ),
      sen_digit_char_share = tidyr::replace_na(
        stringi::stri_count_regex(sen_text, "[0-9]") / nchar(sen_text), 0
      ),
      sen_digit_chunk_count = stringi::stri_count_regex(sen_text, "[0-9]+"),
      sen_digit_chunk_seq_len_max =
        sen_text |>
        stringi::stri_extract_all_regex(
          "(?<=(^| ))[:digit:]+( [:digit:]+)*(?=( |$))"
        ) |>
        purrr::map_int(\(..x){
          ..x %0% "" |> stringi::stri_count_regex("[:digit:]+") |> max()
        }),
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
    dplyr::filter(sen_is_valid | sen_has_item_section_start) |>
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

  return(.doc_clean_body_string)

}

#' Title
#'
#' @param .doc_data ...
#' @param .doc_stat_body_fn ...
#' @param .n_workers ...
#'
#' @return ...
#' @export
#'
#' @examples NULL
gather_doc_stat_body_data <- function(
  .doc_data, .doc_stat_body_fn, .n_workers=NULL
){

  checkmate::assert_tibble(.doc_data)
  if(!"doc_parse_body_string_rel_path" %in% colnames(.doc_data)){
    rlang::abort(
      message=c(
        "`.doc_data` has no `doc_parse_body_string_rel_path` column.",
        i = stringi::stri_c(
          "Run `gather_doc_parse_data(.doc_data)` before ",
          "`gather_doc_body_stat_data(.doc_data)`."#
        )
      ),
      call=NULL
    )
  }

  .data_dir <- get_data_dir(.doc_data)

  .doc_data_colnames <- colnames(.doc_data)

  .doc_stat_body_fn <- rlang::as_function(.doc_stat_body_fn)

  if(length(formals(.doc_stat_body_fn)) > 1){
    rlang::inform(
      message=c(
        "`.doc_stat_body_fn` takes more than one argument.",
        i = "Only the first argument is used."
      )
    )
  }

  .doc_data <-
    .doc_data |>
    burrr::chunk_df(.n_rows=1) |>
    burrr::best_map(function(..row_data){

      ..doc_parse_body_string_abs_path <- fs::path(
        .data_dir, ..row_data$doc_parse_body_string_rel_path
      )

      ..doc_parse_body_string <- qs::qread(
        ..doc_parse_body_string_abs_path
      )

      if(is(..doc_parse_body_string, "error")){
        return(NULL)
      }

      ..doc_stat_body_data <- rlang::exec(
        .doc_stat_body_fn, ..doc_parse_body_string
      )

      ..doc_stat_body_data_invalid <- purrr::reduce(
        .x=c(
          inherits(..doc_stat_body_data, "data.frame"),
          nrow(..doc_stat_body_data) == 1,
          any(colnames(..doc_stat_body_data) %in% .doc_data_colnames)
        ),
        .f=`|`
      )

      if(..doc_stat_body_data_invalid){
        rlang::inform(
          message=c(
            "The result of `.doc_stat_body_fn` is not valid.",
            i = stringi::stri_c(
              "It has to be a data frame with one row and can not contain ",
              "column names that are already present in `.doc_data`"
            )
          )
        )
      }
      ..row_data <- bind_cols(..row_data, ..doc_stat_body_data)

      return(..row_data)

    }, .workers=.n_workers %||% getOption("edgr.n_workers")) |>
    dplyr::bind_rows() |>
    set_data_dir(.data_dir=.data_dir)

  gc()
  return(.doc_data)

}

