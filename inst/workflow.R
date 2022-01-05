
# initial setup

library(available)
available("o2eie")

# File => New Project => Package with git

library(devtools)
use_description()
use_namespace()
use_git()
use_package("dplyr")
use_pipe()
use_gpl3_license()


# functions scripts

use_r("reading_writing")
use_r("calculating")
use_r("plotting")
use_r("review")

# TODO check o2_prepare_files and o2_calculation for vignette ideas and samples scripts


# repeating workflow ------------------------------------------------------

document()
check()
spell_check()
build()
tidy_dir("/R")

use_version("patch")


# add readme
use_readme_rmd()
build_readme()

use_vignette("o2eie_vignette")



# data --------------------------------------------------------------------


# https://r-pkgs.org/data.html
# system.file("extdata", "mtcars.csv", package = "readr")



# optional ----------------------------------------------------------------

# source("https://install-github.me/MangoTheCat/goodpractice")
remotes::install_github("MangoTheCat/goodpractice", force = TRUE)
library(goodpractice)
gp()

