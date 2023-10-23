## analyze/170-diff_mod_takeup/045-npv_of_mods_from_first_adj_for_char_vars.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/170-diff_mod_takeup/"))

suppressPackageStartupMessages({
  library(CLmisc); library(readxl)
})

##For data a cleaning script for the origination data
source(here::here("core/f_clean_bbx_orig_data.R"))
##For the regression strings common throughout the project.
source(here::here("core/rhs_strings.R"))

DT.orig <- read_fst(
  here::here("data/DT_for_2sls_libor_diff.fst"),
  as.data.table = TRUE
) %>%
  f_clean_bbx_orig_data(.) %>%
  .[, MBADELINQUENCYSTATUS_PRE4_FIRST := MBADELINQUENCYSTATUS_PRE4_FIRSTPAYMTADJDTCALC]

DT.npv.at.first.adj <- read_fst(
  here::here("data/DT_npv_of_mod_at_first_adj.fst"),
  as.data.table = TRUE
)


f_get_mod_npv <- function(sep.var) {

  DT.lkp <- read_excel("./030-diff_mod_takeup_char_vars_lkp.xlsx",
                       sheet = sep.var) %>%
    setDT

  sep.var.values <- DT.lkp$value

  DT <- DT.orig[, .(LOANID, sep.var = get(sep.var), zip3)] %>%
    .[, sep.var.fact := factor(sep.var, levels = sep.var.values)] %>%
    .[!is.na(sep.var.fact)] %>%
    merge(DT.npv.at.first.adj, by = c("LOANID"))

  DT.est <- feols(npv.at.first.adj ~ 0 + sep.var.fact, data = DT, cluster = "zip3") %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, est.type := "main"]
  

  DT.est.rel.to.ntile1 <- feols(npv.at.first.adj ~ sep.var.fact, data = DT, cluster = "zip3") %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, est.type := "rel"]


  
  DT.est.all <- rbind(
    DT.est,
    DT.est.rel.to.ntile1
  ) %>%
    .[, sep.var := c(sep.var)] %>%
    .[, sep.var.value := gsub("^sep.var.fact", "", term)] %>%
    .[term == "(Intercept)", sep.var.value := sep.var.values[1]] %>%
    setcolorder(c("sep.var", "sep.var.value", "est.type"))

  return(DT.est.all)

}


sep.vars <- c("DOCTYPESUMMARY", "OCCTYPE", "MBADELINQUENCYSTATUS_PRE4_FIRST")
DT.npv.est.all <- lapply(sep.vars, f_get_mod_npv) %>%
  rbindlist

mkdir_p(here::here("work/diff_mod_takeup"))
write_fst(
  DT.npv.est.all,
  here::here("work/diff_mod_takeup/045-DT_npv_of_mods_char_vars.fst"),
  compress = 100
)

