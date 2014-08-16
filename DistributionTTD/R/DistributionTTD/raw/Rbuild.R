parent.path <- "/home/triffe/git/DistributionTTD/DistributionTTD/R"     # for laptop
#parent.path <- ""
# Code to update documentation, quick builds for non-github sharing. Github package installation testing.
system(paste0("cd ", parent.path, " \n git checkout master \ncommit -a -m 'full commit'"))
# devtools::install_github("devtools")
library(devtools)
#install_github("TimUtils", subdir = "TimUtils", username = "timriffe")
library(TimUtils)
#install.packages("roxygen2")
#load_all(file.path(parent.path ,"DemogBerkeley"))
document(file.path(parent.path ,"DistributionTTD"),clean = TRUE)

IncrementVersion(file.path(parent.path ,"DistributionTTD"),"01","2014-07-29")


