library(RSelenium)

# when you're having trouble setting up the rsDriver, try deleting
# ~/.local/share/binman_seleniumserver

gather_com_indu_data <- function(.rs_driver){

  rlang::check_installed(c("rvest", "RSelenium"))

  stopifnot(is(.rs_driver, "rsClientServer"))

  .rs_driver$client$navigate(
    stringi::stri_c(
      "https://www.sec.gov/corpfin/division-of-corporation-finance-standard-",
      "industrial-classification-sic-code-list"
    )
  )

  .com_indu_data <-
    .rs_driver$client$getPageSource() |>
    purrr::pluck(1) |>
    xml2::read_html() |>
    xml2::xml_find_all(xpath="//table") |>
    rvest::html_table(convert=FALSE) |>
    dplyr::bind_rows() |>
    dplyr::select(
      indu_sic = `SIC Code`, indu_office = Office, indu_title = `Industry Title`
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      com_indu_data = burrr::best_map(indu_sic, \(..indu_sic){

        ..request_url <- stringi::stri_c(
          "https://www.sec.gov/cgi-bin/browse-edgar?company=+&",
          "match=starts-with&filenum=&State=&Country=&SIC=", ..indu_sic, "&",
          "myowner=exclude&action=getcompany"
        )

        ..page_loaded <- FALSE
        while(!..page_loaded){
          try({
            .rs_driver$client$navigate(..request_url)
            ..page_loaded <- TRUE
          })
          if(!..page_loaded){
            Sys.sleep(10)
          }else{
            Sys.sleep(runif(min=0.5, max=2, n=1))
          }
        }
        .rs_driver$client$navigate(..request_url)

        ..response_data <- dplyr::tibble(
          com_cik = character(),
          com_name = character(),
          com_state_country = character()
        )

        repeat{

          ..response_source <- .rs_driver$client$getPageSource()[[1]]

          ..response_html <- xml2::read_html(..response_source)

          ..response_table_list <-
            ..response_html |>
            xml2::xml_find_all(xpath="//table[@summary='Results']") |>
            rvest::html_table(convert=FALSE) |>
            purrr::keep(\(..tbl){
              all(c("CIK", "Company", "State/Country") %in% colnames(..tbl))
            }) |>
            dplyr::bind_rows()

          if(nrow(..response_table_list) > 0){

            ..response_data <- ..response_data |> dplyr::bind_rows(
              ..response_table_list |> dplyr::select(
                com_cik = CIK,
                com_name = Company,
                com_state_country = `State/Country`
              )
            )

          }else{

            return(..response_data)

          }

          ..next_button_list <- .rs_driver$client$findElements(
            value="//input[@type='button' and @value='Next 40']"
          )

          if(length(..next_button_list) > 0){

            ..page_loaded <- FALSE
            while(!..page_loaded){
              try({
                ..next_button_list[[1]]$clickElement()
                ..page_loaded <- TRUE
              })
              if(!..page_loaded){
                Sys.sleep(10)
              }else{
                Sys.sleep(runif(min=0.5, max=2, n=1))
              }
            }

          }else{

            return(..response_data)

          }

        }

      })

    ) |>
    # dplyr::mutate(
    #   dplyr::across(c(indu_sic, indu_office, indu_title), function(.vec){
    #     forcats::as_factor(stringr::str_to_title(.vec))
    #   })
    # ) |>
    dplyr::mutate(
      dplyr::across(c(indu_sic, indu_office, indu_title), forcats::as_factor)
    ) |>
    tidyr::hoist(com_indu_data, "com_cik") |>
    dplyr::select(-com_indu_data) |>
    tidyr::unnest_longer(
      com_cik, transform=\(..x){stringr::str_remove(..x, "^0+")}
    ) |>
    dplyr::distinct() |>
    dplyr::relocate(com_cik, .before=1)


  return(.com_indu_data)

}

rs_driver <- RSelenium::rsDriver(
  port=4449L,
  browser="firefox",
  chromever=NULL
)

com_indu_data <- gather_com_indu_data(.rs_driver=rs_driver)

rs_driver$client$close()
rs_driver$server$stop()

usethis::use_data(com_indu_data, overwrite=TRUE)
