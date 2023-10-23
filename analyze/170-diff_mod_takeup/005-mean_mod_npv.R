## analyze/170-diff_mod_takeup/005-mean_mod_npv.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/170-diff_mod_takeup/"))

suppressPackageStartupMessages({library(CLmisc); })

DT.npv.at.first.adj <- read_fst(
  here::here("data/DT_npv_of_mod_at_first_adj.fst"),
  as.data.table = TRUE
)

## E.g., for intro numbers. 
DT.npv.at.first.adj[, mean(npv.at.first.adj)]

DT.npv.at.first.adj[, mean(npv.at.first.adj)] * 0.0557
