abbrevs_adjusted <-
  readxl::read_excel("./data-raw/files/abbrevs_adjusted.xlsx") |>
  dplyr::filter(!stringi::stri_detect_regex(abbrev, "^([[:alpha:]]\\.)+$")) |>
  dplyr::arrange(-stringi::stri_count_regex(abbrev, "\\.")) |>
  dplyr::mutate(
    pattern =
      abbrev |>
      stringi::stri_trim_both() |>
      stringi::stri_replace_all_regex(
        "([.|()\\^{}+$*?\\\\]|\\[|\\])", "\\\\$1"
      ) |>
      (function(.str){stringi::stri_c("(?i)\\b", .str, "(?=( |\\Z))")})(),
    replacement = stringi::stri_replace_all_regex(abbrev, "[.!?]", "")

  )

usethis::use_data(abbrevs_adjusted, overwrite=TRUE)
