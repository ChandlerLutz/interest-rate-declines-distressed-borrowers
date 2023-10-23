## analyze/170-diff_mod_takeup/040-npv_of_mods_from_first_adj_for_numeric_vars.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/170-diff_mod_takeup/"))


suppressPackageStartupMessages({library(CLmisc); library(readxl)})

##For data a cleaning script for the origination data
source(here::here("core/f_clean_bbx_orig_data.R"))
##For the regression strings common throughout the project.
source(here::here("core/rhs_strings.R"))

DT.lkp <- read_excel("./010-diff_mod_takeup_numeric_vars_lkp.xlsx") %>%
    setDT %>%
    .[for.appendix == 0]

DT.orig <- read_fst(
  here::here("data/DT_for_2sls_libor_diff.fst"),
  as.data.table = TRUE
) %>%
  f_clean_bbx_orig_data(.) %>%
  .[, hp.200708 := fifelse(!is.na(zip.hp.200708), zip.hp.200708, zip3.hp.200708)]


DT.npv.at.first.adj <- read_fst(
    here::here("data/DT_npv_of_mod_at_first_adj.fst"),
    as.data.table = TRUE
)


f_get_mod_npv <- function(sep.var) {

    ntiles <- 5

    sep.var.tmp <- sep.var

    DT <- DT.orig[, .(LOANID, sep.var = get(sep.var), zip3)] %>% 
        .[, sep.var.ntile := fntile(sep.var, ntiles)] %>%
        .[, sep.var.ntile.char := sprintf("%02.f", sep.var.ntile)] %>%
        .[, sep.var.ntile.fact := as.factor(sep.var.ntile.char)] %>%
        .[, sep.var.ntile.fact := relevel(sep.var.ntile.fact, 1)] %>% 
        .[!is.na(sep.var.ntile)] %>% 
        merge(DT.npv.at.first.adj, by = "LOANID") 

    DT.est <- feols(npv.at.first.adj ~ 0 + sep.var.ntile.fact,
                    data = DT, cluster = "zip3") %>%
        broom::tidy(.) %>%
        setDT(.) %>%
        .[, est.type := "main"]
        

    DT.est.rel.to.ntile1 <- feols(npv.at.first.adj ~ sep.var.ntile.fact, data = DT, cluster = "zip3") %>%
        broom::tidy(.) %>%
        setDT(.) %>%
        .[, est.type := "rel.to.ntile01"]


    
    DT.est.all <- rbind(
        DT.est,
        DT.est.rel.to.ntile1
    ) %>%
        .[, sep.var := c(sep.var.tmp)] %>%
        .[, ntile := gsub(".*([0-9]{1})$", "\\1", term)] %>%
        .[ntile == "(Intercept)", ntile := 1] %>%
        setcolorder(c("sep.var", "ntile", "est.type"))

    return(DT.est.all)

}

DT.npv.est.all <- lapply(DT.lkp$sep.var, f_get_mod_npv) %>%
    rbindlist

mkdir_p(here::here("work/diff_mod_takeup"))
write_fst(
    DT.npv.est.all,
    here::here(
              "work/diff_mod_takeup/040-DT_npv_of_mods_numeric_vars.fst"
          ),
    compress = 100
)

