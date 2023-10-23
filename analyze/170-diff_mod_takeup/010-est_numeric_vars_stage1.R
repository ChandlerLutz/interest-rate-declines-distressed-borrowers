## analyze/170-diff_mod_takeup/010-est_numeric_vars_stage1.R

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
  .[, controls.to.remove := as.list(controls.to.remove)] %>% 
  .[, controls.to.remove := ifelse(grepl(", ", controls.to.remove),
                                   strsplit(controls.to.remove, ", "),
                                   controls.to.remove)] %>%
  .[, id := 1:.N] %>%
  setcolorder("id")


DT <- read_fst(
  here::here("data/DT_for_2sls_libor_diff.fst"),
  as.data.table = TRUE
) %>%
  f_clean_bbx_orig_data(.) %>%
  .[, hp.200708 := fifelse(!is.na(zip.hp.200708), zip.hp.200708, zip3.hp.200708)]


f_ntile_est <- function(lkp.id) {

  DT.lkp.tmp <- DT.lkp[id == c(lkp.id)]

  sep.var <- DT.lkp.tmp$sep.var

  controls.to.remove <- DT.lkp.tmp$controls.to.remove %>% unlist

  ntiles <- 5

  base.xpd.fe <- c("sep.var.ntile.char[NOARMLOOKBKDAYS]", 
                   "sep.var.ntile.char^first.payment.adj.month.char")

  rhs.numeric.vars <- rhs.numeric.vars %>%
    .[!(. %chin% controls.to.remove)]

  rhs.varying.slopes <- paste0(rhs.numeric.vars, collapse = ", ") %>%
    sprintf("sep.var.ntile.char[%s]", .)

  rhs.fe.vars <- rhs.fe.vars %>%
    .[!(. %chin% controls.to.remove)]

  rhs.fe <- paste0("sep.var.ntile.char^", rhs.fe.vars) %>%
    paste0(collapse = " + ")

  full.xpd.fe <- c(rhs.varying.slopes, rhs.fe)

  DT <- DT %>%
    .[, sep.var.ntile := fntile(get(sep.var), ntiles)] %>%
    .[, sep.var.ntile.char := sprintf("%02.f", sep.var.ntile)] %>% 
    .[, sep.var.ntile.fact := as.factor(sep.var.ntile.char)] %>%
    .[, sep.var.ntile.fact := relevel(sep.var.ntile.fact, 1)] %>%
    .[!is.na(sep.var.ntile)]


  f.base <- xpd(
    rate.actual.predicted.mod.1.0 ~ 0 +
      LIBOR_FIRST_MEAS_PAY_ADJ_DIFF:sep.var.ntile.char |
      ..base.xpd.fe, 
    ..base.xpd.fe = base.xpd.fe)

  DT.est.base <- feols(f.base, DT, cluster = "zip3", lean = TRUE) %>%
    broom::tidy(.) %>%
    setDT(.) %>% 
    .[, est.type := "main"] %>%
    .[, controls := "base"]

  f.base.rel.to.ntile1 <- xpd(
    rate.actual.predicted.mod.1.0 ~ LIBOR_FIRST_MEAS_PAY_ADJ_DIFF * sep.var.ntile.fact |
      ..base.xpd.fe, 
    ..base.xpd.fe = base.xpd.fe
  )
  
  DT.est.base.rel.to.ntile1 <- feols(f.base.rel.to.ntile1, DT,
                                     cluster = "zip3", lean = TRUE) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, est.type := "rel.to.ntile01"] %>%
    .[, controls := "base"]  

  f.full <- xpd(
    rate.actual.predicted.mod.1.0 ~ 0 + LIBOR_FIRST_MEAS_PAY_ADJ_DIFF:sep.var.ntile.char |
      ..full.xpd.fe, 
      ..full.xpd.fe = full.xpd.fe
  )

  DT.est.full <- feols(f.full, DT, cluster = "zip3", lean = TRUE) %>%
    broom::tidy(.) %>%
    setDT(.) %>% 
    .[, est.type := "main"] %>%
    .[, controls := "full"]

  f.full.rel.to.ntile1 <- xpd(
    rate.actual.predicted.mod.1.0 ~ LIBOR_FIRST_MEAS_PAY_ADJ_DIFF * sep.var.ntile.fact |
      ..full.xpd.fe, 
    ..full.xpd.fe = full.xpd.fe
  )

  DT.est.full.rel.to.ntile1 <- feols(f.full.rel.to.ntile1, DT,
                                         cluster = "zip3", lean = TRUE) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, est.type := "rel.to.ntile01"] %>%
    .[, controls := "full"]


  DT.est.all <- rbind(DT.est.base,
                      DT.est.base.rel.to.ntile1,
                      DT.est.full,
                      DT.est.full.rel.to.ntile1) %>%
    .[, ntile := gsub(".*([0-9]{2})$", "\\1", x = term)] %>%
    .[ntile == "LIBOR_FIRST_MEAS_PAY_ADJ_DIFF", ntile := "01"] %>% 
    .[, sep.var := c(sep.var)] %>%
    setcolorder(c("sep.var", "controls", "est.type", "ntile"))
    

  ## Clean up
  DT <- DT %>%
    .[, c("sep.var.ntile", "sep.var.ntile.char", "sep.var.ntile.fact") := NULL]

  return(DT.est.all)
  
}

## DT.test <- f_ntile_est(1)

DT.out <- lapply(DT.lkp$id, f_ntile_est) %>%
  rbindlist

mkdir_p(here::here("work/diff_mod_takeup"))
write_fst(
  DT.out,
  here::here("work/diff_mod_takeup/010-DT_est_numeric_vars_stage1.fst"),
  compress = 100
)

##  DT <- DT %>%
##   .[, doc.type.fact := as.factor(DOCTYPESUMMARY)]
## feols(rate.actual.predicted.mod.1.0 ~ 0 + LIBOR_FIRST_MEAS_PAY_ADJ_DIFF * DOCTYPESUMMARY + NOARMLOOKBKDAYS |
##         first.payment.adj.month, DT, cluster = "zip3", lean = TRUE)
  
