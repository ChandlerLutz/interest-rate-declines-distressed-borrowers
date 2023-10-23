## analyze/170-diff_mod_takeup/150-corr_arm_margin_zip_characteristics.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/170-diff_mod_takeup/"))

suppressPackageStartupMessages({library(CLmisc); })

##For data a cleaning script for the origination data
source(here::here("core/f_clean_bbx_orig_data.R"))
##For the regression strings common throughout the project.
source(here::here("core/rhs_strings.R"))

DT <- read_fst(
  here::here("data/DT_for_2sls_libor_diff.fst"),
  as.data.table = TRUE
) %>%
  f_clean_bbx_orig_data(.)

DT[!is.na(MARGINCALC) & !is.na(hh.income.zip), cor(MARGINCALC, hh.income.zip)]

DT[!is.na(MARGINCALC) & !is.na(zip.hp.200708), cor(MARGINCALC, zip.hp.200708)]
