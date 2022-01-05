
<!-- README.md is generated from README.Rmd. Please edit that file -->

# o2eie

<!-- badges: start -->
<!-- badges: end -->

The goal of o2eie is to provide an easy and consistent workflow for
processing O2 data generated from the O2-microcompensation labs from
EIE.

## Installation

You can install the o2eie package from GitHub by running:

``` r
remotes::install_github("gpatoine/o2eie")
```

## Example

This is a basic example which shows you how to use the package. The
function `o2_process_all` will generate a tibble (similar to a
data.frame) with all main measurements, including basal respiration,
microbial biomass, and microbial growth in different columns. The
function only requires a vector of weighing sheets.

``` r
library(o2eie)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: ggplot2

weights <- list.files(system.file("extdata", package = "o2eie"),
                      pattern = "^w_.*\\.xlsx",
                      full.names = T)

o2meas <- o2_process_all(files = weights)
#> Processing w_DIETER_6391-6420_2019-11-26_Project1.xlsx
#> Processing w_DIETER_6661-6690_2020-02-10_Project1.xlsx
```

### Reports

It is recommended to review the datapoints used for basal respiration
and cmic measurements for each sample. This can be done using PDF
reports.

``` r
o2meas %>% 
  bas_report("bas.pdf") %>% 
  cmic_report("cmic.pdf") %>% 
  mgrow_report("mgrowth.pdf")
#> # A tibble: 23 x 46
#>    idSequence channel device name1  name2 name3 name_c weight_file bas_file$...3
#>    <chr>      <chr>   <chr>  <chr>  <chr> <chr> <chr>  <chr>       <chr>        
#>  1 6391       1       DIETER 326_04 1     <NA>  326_0~ C:/Users/g~ bas_DIETER_6~
#>  2 6392       2       DIETER 326_04 2     <NA>  326_0~ C:/Users/g~ bas_DIETER_6~
#>  3 6393       3       DIETER 326_03 2     <NA>  326_0~ C:/Users/g~ bas_DIETER_6~
#>  4 6394       4       DIETER 272_05 1     <NA>  272_0~ C:/Users/g~ bas_DIETER_6~
#>  5 6395       5       DIETER 272_01 1     <NA>  272_0~ C:/Users/g~ bas_DIETER_6~
#>  6 6396       6       DIETER 326_02 2     <NA>  326_0~ C:/Users/g~ bas_DIETER_6~
#>  7 6397       7       DIETER 326_03 1     <NA>  326_0~ C:/Users/g~ bas_DIETER_6~
#>  8 6398       8       DIETER 326_01 1     <NA>  326_0~ C:/Users/g~ bas_DIETER_6~
#>  9 6399       9       DIETER 326_01 2     <NA>  326_0~ C:/Users/g~ bas_DIETER_6~
#> 10 6400       10      DIETER 272_05 2     <NA>  272_0~ C:/Users/g~ bas_DIETER_6~
#> # ... with 13 more rows, and 37 more variables: cmic_file <tibble[,1]>,
#> #   wei_cont_empty <dbl>, wei_sample_fresh <dbl>, wei_cont_plus_samp_dry <dbl>,
#> #   soil_type <chr>, glucose <dbl>, water_added <dbl>, date_sampling <chr>,
#> #   comment <chr>, bas_start <dbl>, bas_stop <dbl>,
#> #   wei_cont_plus_samp_fresh <dbl>, wei_samp_dry <dbl>, h2o_samp <dbl>,
#> #   h2o_perc <dbl>, bas_raw <named list>, cmic_raw <named list>,
#> #   date_bas_meas <date>, date_cmic_meas <date>, bas_corfct <dbl>, ...
```

![basal respiration report](fig/bas.png)

### Manually adjusting calculation periods

Based on the reports, we can adjust the period considered for the
calculation of basal respiration and microbial biomass. The easiest way
to do this is a tibble that contains the new values. Be sure to use the
columns “name_c” and “times”, as shown below.

``` r
adjust_bas <- tribble(
  ~name_c, ~times,
  "326_04_1", c(10:17, 19:20),
  "121_2", c(25:35),

)

o2meas <- o2meas %>% 
  set_bas_times(tib = adjust_bas) %>% 
  o2_bas(only_sets = TRUE)

o2meas %>% 
  filter(name_c %in% adjust_bas$name_c) %>% 
  select(name_c, bas_set)
#> # A tibble: 2 x 2
#>   name_c   bas_set   
#>   <chr>    <list>    
#> 1 326_04_1 <int [10]>
#> 2 121_2    <int [11]>

bas_report(o2meas, "bas2.pdf")
#> # A tibble: 23 x 47
#>    idSequence channel device name1  name2 name3 name_c weight_file bas_file$...3
#>    <chr>      <chr>   <chr>  <chr>  <chr> <chr> <chr>  <chr>       <chr>        
#>  1 6391       1       DIETER 326_04 1     <NA>  326_0~ C:/Users/g~ bas_DIETER_6~
#>  2 6392       2       DIETER 326_04 2     <NA>  326_0~ C:/Users/g~ bas_DIETER_6~
#>  3 6393       3       DIETER 326_03 2     <NA>  326_0~ C:/Users/g~ bas_DIETER_6~
#>  4 6394       4       DIETER 272_05 1     <NA>  272_0~ C:/Users/g~ bas_DIETER_6~
#>  5 6395       5       DIETER 272_01 1     <NA>  272_0~ C:/Users/g~ bas_DIETER_6~
#>  6 6396       6       DIETER 326_02 2     <NA>  326_0~ C:/Users/g~ bas_DIETER_6~
#>  7 6397       7       DIETER 326_03 1     <NA>  326_0~ C:/Users/g~ bas_DIETER_6~
#>  8 6398       8       DIETER 326_01 1     <NA>  326_0~ C:/Users/g~ bas_DIETER_6~
#>  9 6399       9       DIETER 326_01 2     <NA>  326_0~ C:/Users/g~ bas_DIETER_6~
#> 10 6400       10      DIETER 272_05 2     <NA>  272_0~ C:/Users/g~ bas_DIETER_6~
#> # ... with 13 more rows, and 38 more variables: cmic_file <tibble[,1]>,
#> #   wei_cont_empty <dbl>, wei_sample_fresh <dbl>, wei_cont_plus_samp_dry <dbl>,
#> #   soil_type <chr>, glucose <dbl>, water_added <dbl>, date_sampling <chr>,
#> #   comment <chr>, bas_start <dbl>, bas_stop <dbl>,
#> #   wei_cont_plus_samp_fresh <dbl>, wei_samp_dry <dbl>, h2o_samp <dbl>,
#> #   h2o_perc <dbl>, bas_raw <named list>, cmic_raw <named list>,
#> #   date_bas_meas <date>, date_cmic_meas <date>, bas_corfct <dbl>, ...
```

Contact: <guillaume.patoine@idiv.de>
