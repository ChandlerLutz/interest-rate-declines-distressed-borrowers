## ./_RunAll_.R

if (!require(renv)) {
  install.packages("renv")
  library(renv)
}

renv::restore()

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("./"))

suppressPackageStartupMessages({library(CLmisc); })

R.files <- list.files("./analyze", recursive = TRUE, full.names = TRUE) %>%
  .[grepl(".R$", x = .)]


lapply(R.files, source, chdir = TRUE)
