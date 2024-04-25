
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
options("edgr.n_workers"=3)
```

Get the document index files for the years 2020 to 2023

``` r
doc_index_data <- gather_doc_index_data(
  .years=2021:2023, .data_dir=fs::dir_create(here::here("README_files/data/"))
)

doc_index_data
#> # A tibble: 22,339 × 6
#>    com_cik com_name              doc_an doc_form_type doc_date_filed doc_raw_url
#>    <chr>   <chr>                 <chr>  <chr>         <date>         <chr>      
#>  1 1000209 MEDALLION FINANCIAL … 00015… 10-K          2021-03-16     https://ww…
#>  2 1000228 HENRY SCHEIN INC      00010… 10-K          2021-02-17     https://ww…
#>  3 1000229 CORE LABORATORIES N V 00015… 10-K          2021-02-08     https://ww…
#>  4 1000232 KENTUCKY BANCSHARES … 00015… 10-K          2021-03-03     https://ww…
#>  5 1000298 IMPAC MORTGAGE HOLDI… 00015… 10-K          2021-03-12     https://ww…
#>  6 1000623 SCHWEITZER MAUDUIT I… 00010… 10-K          2021-03-01     https://ww…
#>  7 1000683 BLONDER TONGUE LABOR… 00012… 10-K          2021-03-25     https://ww…
#>  8 1000694 NOVAVAX INC           00010… 10-K          2021-03-01     https://ww…
#>  9 1000697 WATERS CORP /DE/      00011… 10-K          2021-02-24     https://ww…
#> 10 1000753 INSPERITY, INC.       00010… 10-K          2021-02-12     https://ww…
#> # ℹ 22,329 more rows
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
doc_clean_data <- doc_index_data |> 
  # keep only companies from the relevant sectors
  semi_join(com_indu_health_data, by=join_by(com_cik)) |>
  # for this example: only use the first 10 of the matched companies
  filter(com_cik %in% unique(com_cik)[1:10]) |> 
  gather_doc_clean_data()
```

The processed data is now stored in the directory that you set above.

``` r
fs::dir_tree(doc_clean_data %@% "data_dir", recurse=TRUE)
#> /home/moritz/Documents/R-Packages/edgr/README_files/data
#> ├── doc_clean_body_string_files
#> │   ├── 1014739
#> │   │   ├── 0001014739-21-000011.txt
#> │   │   ├── 0001014739-22-000009.txt
#> │   │   └── 0001014739-23-000009.txt
#> │   ├── 1022079
#> │   │   ├── 0001022079-21-000029.txt
#> │   │   ├── 0001022079-22-000027.txt
#> │   │   └── 0001022079-23-000018.txt
#> │   ├── 1043000
#> │   │   ├── 0001564590-21-017126.txt
#> │   │   ├── 0001628280-22-009364.txt
#> │   │   └── 0001628280-23-009942.txt
#> │   ├── 1044378
#> │   │   ├── 0000950170-23-013109.txt
#> │   │   ├── 0001564590-21-017097.txt
#> │   │   └── 0001564590-22-013599.txt
#> │   ├── 1047335
#> │   │   ├── 0001437749-21-003415.txt
#> │   │   ├── 0001437749-22-003787.txt
#> │   │   └── 0001437749-23-003830.txt
#> │   ├── 1108109
#> │   │   ├── 0001564590-21-006686.txt
#> │   │   ├── 0001564590-22-005570.txt
#> │   │   └── 0001564590-23-002038.txt
#> │   ├── 1124140
#> │   │   ├── 0001124140-21-000029.txt
#> │   │   ├── 0001124140-22-000022.txt
#> │   │   └── 0001124140-23-000014.txt
#> │   ├── 1125376
#> │   │   ├── 0001125376-21-000020.txt
#> │   │   ├── 0001125376-22-000019.txt
#> │   │   └── 0001125376-23-000018.txt
#> │   ├── 1136174
#> │   │   ├── 0001628280-21-004260.txt
#> │   │   ├── 0001628280-22-009356.txt
#> │   │   └── 0001628280-23-011799.txt
#> │   └── 1138476
#> │       ├── 0001185185-21-000438.txt
#> │       ├── 0001185185-22-000436.txt
#> │       └── 0001185185-23-000301.txt
#> └── doc_clean_head_data_files
#>     ├── 1014739
#>     │   ├── 0001014739-21-000011.qs
#>     │   ├── 0001014739-22-000009.qs
#>     │   └── 0001014739-23-000009.qs
#>     ├── 1022079
#>     │   ├── 0001022079-21-000029.qs
#>     │   ├── 0001022079-22-000027.qs
#>     │   └── 0001022079-23-000018.qs
#>     ├── 1043000
#>     │   ├── 0001564590-21-017126.qs
#>     │   ├── 0001628280-22-009364.qs
#>     │   └── 0001628280-23-009942.qs
#>     ├── 1044378
#>     │   ├── 0000950170-23-013109.qs
#>     │   ├── 0001564590-21-017097.qs
#>     │   └── 0001564590-22-013599.qs
#>     ├── 1047335
#>     │   ├── 0001437749-21-003415.qs
#>     │   ├── 0001437749-22-003787.qs
#>     │   └── 0001437749-23-003830.qs
#>     ├── 1108109
#>     │   ├── 0001564590-21-006686.qs
#>     │   ├── 0001564590-22-005570.qs
#>     │   └── 0001564590-23-002038.qs
#>     ├── 1124140
#>     │   ├── 0001124140-21-000029.qs
#>     │   ├── 0001124140-22-000022.qs
#>     │   └── 0001124140-23-000014.qs
#>     ├── 1125376
#>     │   ├── 0001125376-21-000020.qs
#>     │   ├── 0001125376-22-000019.qs
#>     │   └── 0001125376-23-000018.qs
#>     ├── 1136174
#>     │   ├── 0001628280-21-004260.qs
#>     │   ├── 0001628280-22-009356.qs
#>     │   └── 0001628280-23-011799.qs
#>     └── 1138476
#>         ├── 0001185185-21-000438.qs
#>         ├── 0001185185-22-000436.qs
#>         └── 0001185185-23-000301.qs
```

# License

{edgr} is distributed under the MIT License. See the
[LICENSE](./LICENSE) file for details.
