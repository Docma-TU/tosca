try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))

##############
## Roxygen2 ##
##############
library(roxygen2)
roxygenize(package.dir = ".")

##############
## testthat ##
##############
library(testthat)
test_check("tosca", path = "./tests")

###################
## build install ##
###################
setwd("..")
system("R CMD build tosca --resave-data")

# verify current version
system("R CMD check tosca_0.3-3.tar.gz --as-cran")

###############################################
## Install locally to test for hidden errors ##
###############################################
# devtools::install_deps("tosca")
# devtools::check("tosca")
devtools::install("tosca")
devtools::test_coverage("tosca")


#########################
## Install from github ##
#########################
setwd("./tosca")
devtools::install_github("Docma-TU/tosca")
library(tosca)

