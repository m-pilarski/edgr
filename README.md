
<!-- README.md is generated from README.Rmd. Please edit that file -->

# edgr

<!-- badges: start -->
<!-- badges: end -->

The goal of {edgr} is to help you analyze 10-K filings from the SEC’s
EDGAR database.

## Installation

You can install the development version of {edgr} from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("m-pilarski/edgr")
```

## Example

``` r
library(tidyverse)
library(edgr)

# Set the number of workers for parallel processing
options("edgr.n_workers"=1)
# Set the number of workers for parallel processing
options("edgr.user_agent"=paste0(
  "Mozilla/5.0 (iPhone; CPU iPhone OS 17_6_1 like Mac OS X) ",
  "AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.6 Mobile/15E148 ",
  "Safari/604.1"
))
```

Get the document index files for the years 2020 to 2023

``` r
doc_index_data <- gather_doc_index_data(
  .years=2010:2015, .data_dir=fs::dir_create(here::here("README_files/data/"))
)

doc_index_data
#> # A tibble: 50,572 × 6
#>    com_cik com_name              doc_an doc_form_type doc_date_filed doc_raw_url
#>    <chr>   <chr>                 <chr>  <chr>         <date>         <chr>      
#>  1 1000180 SANDISK CORP          00010… 10-K          2010-02-25     https://ww…
#>  2 1000209 MEDALLION FINANCIAL … 00011… 10-K          2010-03-12     https://ww…
#>  3 1000228 HENRY SCHEIN INC      00010… 10-K          2010-02-23     https://ww…
#>  4 1000229 CORE LABORATORIES N V 00010… 10-K          2010-02-19     https://ww…
#>  5 1000230 OPTICAL CABLE CORP    00011… 10-K          2010-01-29     https://ww…
#>  6 1000232 KENTUCKY BANCSHARES … 00011… 10-K          2010-03-31     https://ww…
#>  7 1000278 PACIFICHEALTH LABORA… 00011… 10-K          2010-03-30     https://ww…
#>  8 1000298 IMPAC MORTGAGE HOLDI… 00010… 10-K          2010-03-16     https://ww…
#>  9 1000623 SCHWEITZER MAUDUIT I… 00011… 10-K          2010-03-08     https://ww…
#> 10 1000683 BLONDER TONGUE LABOR… 00013… 10-K          2010-03-25     https://ww…
#> # ℹ 50,562 more rows
```

{edgr} includes a scraped dataset `com_indu_data` that includes
information on companies’ sectors.

``` r
com_indu_data
#> # A tibble: 64,812 × 4
#>    com_cik indu_sic indu_office                       indu_title  
#>    <chr>   <fct>    <fct>                             <fct>       
#>  1 825171  1000     Office of Energy & Transportation METAL MINING
#>  2 1011903 1000     Office of Energy & Transportation METAL MINING
#>  3 1071832 1000     Office of Energy & Transportation METAL MINING
#>  4 1194506 1000     Office of Energy & Transportation METAL MINING
#>  5 1171008 1000     Office of Energy & Transportation METAL MINING
#>  6 1050602 1000     Office of Energy & Transportation METAL MINING
#>  7 830821  1000     Office of Energy & Transportation METAL MINING
#>  8 1318196 1000     Office of Energy & Transportation METAL MINING
#>  9 1360903 1000     Office of Energy & Transportation METAL MINING
#> 10 1142462 1000     Office of Energy & Transportation METAL MINING
#> # ℹ 64,802 more rows

com_indu_data |> distinct(indu_sic, indu_title)
#> # A tibble: 437 × 2
#>    indu_sic indu_title                                           
#>    <fct>    <fct>                                                
#>  1 1000     METAL MINING                                         
#>  2 1040     GOLD AND SILVER ORES                                 
#>  3 1090     MISCELLANEOUS METAL ORES                             
#>  4 1220     BITUMINOUS COAL & LIGNITE MINING                     
#>  5 1221     BITUMINOUS COAL & LIGNITE SURFACE MINING             
#>  6 1311     CRUDE PETROLEUM & NATURAL GAS                        
#>  7 1381     DRILLING OIL & GAS WELLS                             
#>  8 1382     OIL & GAS FIELD EXPLORATION SERVICES                 
#>  9 1389     OIL & GAS FIELD SERVICES, NEC                        
#> 10 1400     MINING & QUARRYING OF NONMETALLIC MINERALS (NO FUELS)
#> # ℹ 427 more rows
```

You can use it to get the CIKs for companies from sectors that are
relevant for your analysis.

``` r
com_indu_health_data <- com_indu_data |> filter(
  str_detect(indu_sic, "^80\\d\\d$")
)

com_indu_health_data
#> # A tibble: 1,186 × 4
#>    com_cik indu_sic indu_office                          indu_title             
#>    <chr>   <fct>    <fct>                                <fct>                  
#>  1 1950429 8000     Industrial Applications and Services SERVICES-HEALTH SERVIC…
#>  2 1444144 8000     Industrial Applications and Services SERVICES-HEALTH SERVIC…
#>  3 1844507 8000     Industrial Applications and Services SERVICES-HEALTH SERVIC…
#>  4 22872   8000     Industrial Applications and Services SERVICES-HEALTH SERVIC…
#>  5 1713210 8000     Industrial Applications and Services SERVICES-HEALTH SERVIC…
#>  6 908516  8000     Industrial Applications and Services SERVICES-HEALTH SERVIC…
#>  7 1836967 8000     Industrial Applications and Services SERVICES-HEALTH SERVIC…
#>  8 1048494 8000     Industrial Applications and Services SERVICES-HEALTH SERVIC…
#>  9 1839998 8000     Industrial Applications and Services SERVICES-HEALTH SERVIC…
#> 10 1257499 8000     Industrial Applications and Services SERVICES-HEALTH SERVIC…
#> # ℹ 1,176 more rows

com_indu_health_data |> distinct(indu_sic, indu_title)
#> # A tibble: 10 × 2
#>    indu_sic indu_title                                        
#>    <fct>    <fct>                                             
#>  1 8000     SERVICES-HEALTH SERVICES                          
#>  2 8011     SERVICES-OFFICES & CLINICS OF DOCTORS OF MEDICINE 
#>  3 8050     SERVICES-NURSING & PERSONAL CARE FACILITIES       
#>  4 8051     SERVICES-SKILLED NURSING CARE FACILITIES          
#>  5 8060     SERVICES-HOSPITALS                                
#>  6 8062     SERVICES-GENERAL MEDICAL & SURGICAL HOSPITALS, NEC
#>  7 8071     SERVICES-MEDICAL LABORATORIES                     
#>  8 8082     SERVICES-HOME HEALTH CARE SERVICES                
#>  9 8090     SERVICES-MISC HEALTH & ALLIED SERVICES, NEC       
#> 10 8093     SERVICES-SPECIALTY OUTPATIENT FACILITIES, NEC
```

Collect and clean the 10-Ks for the selected companies and years.

``` r
doc_parse_data <- doc_index_data |> 
  # keep only companies from the relevant sectors
  semi_join(com_indu_health_data, by=join_by(com_cik)) |>
  # download raw filings from the SEC
  gather_doc_raw_data() |> 
  # parse the raw filings and store body and head into separate files
  gather_doc_parse_data()
```

The processed data is now stored in the directory that you set above.

``` r
fs::dir_info(get_data_dir(doc_parse_data), recurse=FALSE)
#> # A tibble: 3 × 18
#>   path         type   size permissions modification_time   user  group device_id
#>   <fs::path>   <fct> <fs:> <fs::perms> <dttm>              <chr> <chr>     <dbl>
#> 1 …tring_files dire…    4K rwxr-xr-x   2024-09-09 16:50:53 mori… mori…     65024
#> 2 …_data_files dire…    4K rwxr-xr-x   2024-09-09 16:50:53 mori… mori…     65024
#> 3 …tring_files dire…    4K rwxr-xr-x   2024-09-09 16:35:48 mori… mori…     65024
#> # ℹ 10 more variables: hard_links <dbl>, special_device_id <dbl>, inode <dbl>,
#> #   block_size <dbl>, blocks <dbl>, flags <int>, generation <dbl>,
#> #   access_time <dttm>, change_time <dttm>, birth_time <dttm>
```

``` r
doc_parse_data |> 
  slice_sample(n=10) |> 
  gather_doc_stat_body_data(function(.doc_parse){
    # .doc_parse <<- .doc_parse; stop()
    .doc_parse |> 
      prep_doc_clean_body_string() |> 
      sentimentr::get_sentences() |> 
      sentimentr::sentiment() |> 
      summarize(
        sentence_count = n(), 
        word_count = sum(word_count),
        sentiment_mean = mean(sentiment),
        sentiment_sd = sd(sentiment)
      ) |> 
      as_tibble()
  }, .n_workers=1) |> 
  glimpse()
#> The result of `.doc_stat_body_fn` is not valid.
#> ℹ It has to be a data frame with one row and can not contain column names that are already present in `.doc_data`
#> The result of `.doc_stat_body_fn` is not valid.
#> ℹ It has to be a data frame with one row and can not contain column names that are already present in `.doc_data`
#> The result of `.doc_stat_body_fn` is not valid.
#> ℹ It has to be a data frame with one row and can not contain column names that are already present in `.doc_data`
#> The result of `.doc_stat_body_fn` is not valid.
#> ℹ It has to be a data frame with one row and can not contain column names that are already present in `.doc_data`
#> The result of `.doc_stat_body_fn` is not valid.
#> ℹ It has to be a data frame with one row and can not contain column names that are already present in `.doc_data`
#> The result of `.doc_stat_body_fn` is not valid.
#> ℹ It has to be a data frame with one row and can not contain column names that are already present in `.doc_data`
#> The result of `.doc_stat_body_fn` is not valid.
#> ℹ It has to be a data frame with one row and can not contain column names that are already present in `.doc_data`
#> The result of `.doc_stat_body_fn` is not valid.
#> ℹ It has to be a data frame with one row and can not contain column names that are already present in `.doc_data`
#> The result of `.doc_stat_body_fn` is not valid.
#> ℹ It has to be a data frame with one row and can not contain column names that are already present in `.doc_data`
#> The result of `.doc_stat_body_fn` is not valid.
#> ℹ It has to be a data frame with one row and can not contain column names that are already present in `.doc_data`
#> Rows: 10
#> Columns: 13
#> $ com_cik                        <chr> "885074", "941020", "1136174", "1301611…
#> $ com_name                       <chr> "AUTHENTIDATE HOLDING CORP", "GENELINK …
#> $ doc_an                         <chr> "0001193125-15-342743", "0001144204-12-…
#> $ doc_form_type                  <chr> "10-K", "10-K", "10-K", "10-K", "10-K",…
#> $ doc_date_filed                 <date> 2015-10-13, 2012-05-14, 2012-03-30, 20…
#> $ doc_raw_url                    <chr> "https://www.sec.gov/Archives/edgar/dat…
#> $ doc_raw_string_rel_path        <list> "doc_raw_string_files/885074/000119312…
#> $ doc_parse_head_data_rel_path   <list> "doc_parse_head_data_files/885074/0001…
#> $ doc_parse_body_string_rel_path <list> "doc_parse_body_string_files/885074/00…
#> $ sentence_count                 <int> 3533, 1392, 8015, 8485, 7123, 3642, 331…
#> $ word_count                     <int> 120059, 30802, 209337, 237350, 192472, …
#> $ sentiment_mean                 <dbl> 0.16784392, 0.15887404, 0.14250165, 0.1…
#> $ sentiment_sd                   <dbl> 0.2341361, 0.2435872, 0.2279123, 0.2387…
```

# License

{edgr} is distributed under the MIT License. See the
[LICENSE](./LICENSE) file for details.
