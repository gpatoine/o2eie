# is name available?
library(available)
available("o2eie")

# Create package project
# File => New Project => Package with git

library(devtools)

use_description()
use_namespace()
use_git() # only run once
use_pipe()
use_gpl3_license()


# functions scripts
use_r("reading_writing")
use_r("calculating")
use_r("plotting")
use_r("review")

# add readme + vignette
use_readme_rmd()
use_vignette("o2eie_vignette")


# repeating workflow ------------------------------------------------------

devtools::document()
devtools::check()

devtools::build_readme()
devtools::build_vignettes()

spell_check()
build()
# formatR::tidy_dir("/R")

use_version("patch")



# optional ----------------------------------------------------------------

# source("https://install-github.me/MangoTheCat/goodpractice")
remotes::install_github("MangoTheCat/goodpractice", force = TRUE)
library(goodpractice)
gp()

