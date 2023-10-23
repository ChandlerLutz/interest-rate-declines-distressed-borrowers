## analyze/170-diff_mod_takeup/030-est_char_vars.R

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

DT <- read_fst(
  here::here("data/DT_for_2sls_libor_diff.fst"),
  as.data.table = TRUE
) %>%
  f_clean_bbx_orig_data(.) %>%
  .[, MBADELINQUENCYSTATUS_PRE4_FIRST := MBADELINQUENCYSTATUS_PRE4_FIRSTPAYMTADJDTCALC]

f_est <- function(sep.var) {

  DT.lkp <- read_excel("./030-diff_mod_takeup_char_vars_lkp.xlsx",
                       sheet = sep.var) %>%
    setDT

  sep.var.values <- DT.lkp$value

  base.xpd.fe <- c("sep.var.fact[NOARMLOOKBKDAYS]", 
                   "sep.var.fact^first.payment.adj.month.char")

  rhs.varying.slopes <- paste0(rhs.numeric.vars, collapse = ", ") %>%
    sprintf("sep.var[%s]", .)

  rhs.fe.vars <- rhs.fe.vars %>%
    .[!(. %chin% sep.var)]

  rhs.fe <- paste0("sep.var.fact^", rhs.fe.vars) %>%
    paste0(collapse = " + ")

  full.xpd.fe <- c(rhs.varying.slopes, rhs.fe)

  DT <- DT %>%
    .[get(sep.var) %chin% sep.var.values] %>%
    .[, sep.var := get(sep.var)] %>%
    .[, sep.var.fact := factor(sep.var, levels = sep.var.values)]

  DT.sep.var.dummies <- DT %>%
    .[, .(LOANID, sep.var.fact)] %>%
    dummy_cols(select_columns = "sep.var.fact", remove_first_dummy = TRUE) %>%
    .[, sep.var.fact := NULL]

  DT.sep.var.dummies.mod.ind <- DT.sep.var.dummies %>%
    merge(DT[, .(LOANID, rate.actual.predicted.mod.1.0)], by = "LOANID") %>%
    .[, c(.(LOANID = LOANID), lapply(.SD, \(x) x * rate.actual.predicted.mod.1.0)),
      .SDcols = grep("sep.var.fact", names(.))]

  old.names <- names(DT.sep.var.dummies.mod.ind) %>%
    .[grepl("sep.var.fact", .)]
  sep.var.dummies.mod.ind <- gsub("sep.var.fact", "mod.ind", old.names)
  DT.sep.var.dummies.mod.ind <- DT.sep.var.dummies.mod.ind %>%
    setnames(old.names, sep.var.dummies.mod.ind)

  rm(old.names)

  DT.sep.var.dummies.libor.diff <- DT.sep.var.dummies %>% 
    merge(DT[, .(LOANID, LIBOR_FIRST_MEAS_PAY_ADJ_DIFF)], by = "LOANID") %>%
    .[, c(.(LOANID = LOANID), lapply(.SD, \(x) x * LIBOR_FIRST_MEAS_PAY_ADJ_DIFF)),
      .SDcols = grep("sep.var.fact", names(.))]

  old.names <- names(DT.sep.var.dummies.libor.diff) %>%
    .[grepl("sep.var.fact", .)]
  sep.var.dummies.libor.diff <- gsub("sep.var.fact", "libor.diff", old.names)

  DT.sep.var.dummies.libor.diff <- DT.sep.var.dummies.libor.diff %>%
    setnames(old.names, sep.var.dummies.libor.diff)

  rm(old.names)


  DT <- merge(DT, DT.sep.var.dummies.mod.ind, by = "LOANID") %>% 
    merge(DT.sep.var.dummies.libor.diff, by = "LOANID")
  
  DT.stage1.base <- xpd(
    rate.actual.predicted.mod.1.0 ~ 0 +
      LIBOR_FIRST_MEAS_PAY_ADJ_DIFF:sep.var.fact |
      ..base.xpd.fe, 
    ..base.xpd.fe = base.xpd.fe) %>%
    feols(DT, cluster = "zip3", lean = TRUE) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, stage := "stage1"] %>% 
    .[, est.type := "main"] %>%
    .[, controls := "base"]

  DT.stage1.base.rel <- xpd(
    rate.actual.predicted.mod.1.0 ~ 0 +
      LIBOR_FIRST_MEAS_PAY_ADJ_DIFF * sep.var.fact |
      ..base.xpd.fe, 
    ..base.xpd.fe = base.xpd.fe) %>%
    feols(DT, cluster = "zip3", lean = TRUE) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, stage := "stage1"] %>% 
    .[, est.type := "rel"] %>%
    .[, controls := "base"]

  DT.stage1.full <- xpd(
    rate.actual.predicted.mod.1.0 ~ 0 +
      LIBOR_FIRST_MEAS_PAY_ADJ_DIFF:sep.var.fact |
      ..full.xpd.fe, 
    ..full.xpd.fe = full.xpd.fe) %>%
    feols(DT, cluster = "zip3", lean = TRUE) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, stage := "stage1"] %>% 
    .[, est.type := "main"] %>%
    .[, controls := "full"]

  DT.stage1.full.rel <- xpd(
    rate.actual.predicted.mod.1.0 ~ 0 +
      LIBOR_FIRST_MEAS_PAY_ADJ_DIFF * sep.var.fact |
      ..full.xpd.fe, 
    ..full.xpd.fe = full.xpd.fe) %>%
    feols(DT, cluster = "zip3", lean = TRUE) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, stage := "stage1"] %>% 
    .[, est.type := "rel"] %>%
    .[, controls := "full"]
  
  DT.reduced.form.base <- xpd(
    discounted.cum.loss ~ 0 +
      LIBOR_FIRST_MEAS_PAY_ADJ_DIFF:sep.var.fact |
      ..base.xpd.fe, 
    ..base.xpd.fe = base.xpd.fe) %>%
    feols(DT, cluster = "zip3", lean = TRUE) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, stage := "reduced.form"] %>% 
    .[, est.type := "main"] %>%
    .[, controls := "base"]

  DT.reduced.form.base.rel <- xpd(
    discounted.cum.loss ~ 0 +
      LIBOR_FIRST_MEAS_PAY_ADJ_DIFF * sep.var.fact |
      ..base.xpd.fe, 
    ..base.xpd.fe = base.xpd.fe) %>%
    feols(DT, cluster = "zip3", lean = TRUE) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, stage := "reduced.form"] %>% 
    .[, est.type := "rel"] %>%
    .[, controls := "base"]

  DT.reduced.form.full <- xpd(
    discounted.cum.loss ~ 0 +
      LIBOR_FIRST_MEAS_PAY_ADJ_DIFF:sep.var.fact |
      ..full.xpd.fe, 
    ..full.xpd.fe = full.xpd.fe) %>%
    feols(DT, cluster = "zip3", lean = TRUE) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, stage := "reduced.form"] %>% 
    .[, est.type := "main"] %>%
    .[, controls := "full"]

  DT.reduced.form.full.rel <- xpd(
    discounted.cum.loss ~ 0 +
      LIBOR_FIRST_MEAS_PAY_ADJ_DIFF * sep.var.fact |
      ..full.xpd.fe, 
    ..full.xpd.fe = full.xpd.fe) %>%
    feols(DT, cluster = "zip3", lean = TRUE) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, stage := "reduced.form"] %>% 
    .[, est.type := "rel"] %>%
    .[, controls := "full"]

  DT.stage2.base <- xpd(
    discounted.cum.loss ~ 1 | ..base.xpd.fe | 
      rate.actual.predicted.mod.1.0:sep.var.fact ~
        LIBOR_FIRST_MEAS_PAY_ADJ_DIFF:sep.var.fact, 
    ..base.xpd.fe = base.xpd.fe) %>%
    feols(DT, cluster = "zip3", lean = TRUE) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, stage := "stage2"] %>% 
    .[, est.type := "main"] %>%
    .[, controls := "base"]

  DT.stage2.base.rel <- xpd(
    discounted.cum.loss ~ 1 | ..base.xpd.fe |
      rate.actual.predicted.mod.1.0 + ..sep.var.dummies.mod.ind ~
        LIBOR_FIRST_MEAS_PAY_ADJ_DIFF + ..sep.var.dummies.libor.diff,
    ..base.xpd.fe = base.xpd.fe,
    ..sep.var.dummies.mod.ind = sep.var.dummies.mod.ind,
    ..sep.var.dummies.libor.diff = sep.var.dummies.libor.diff) %>%
    feols(DT, cluster = "zip3", lean = TRUE) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, stage := "stage2"] %>% 
    .[, est.type := "rel"] %>%
    .[, controls := "base"]

  DT.stage2.full <- xpd(
    discounted.cum.loss ~ 1 | ..full.xpd.fe | 
      rate.actual.predicted.mod.1.0:sep.var.fact ~
        LIBOR_FIRST_MEAS_PAY_ADJ_DIFF:sep.var.fact, 
    ..full.xpd.fe = full.xpd.fe) %>%
    feols(DT, cluster = "zip3", lean = TRUE) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, stage := "stage2"] %>% 
    .[, est.type := "main"] %>%
    .[, controls := "full"]

  DT.stage2.full.rel <- xpd(
    discounted.cum.loss ~ 1 | ..full.xpd.fe |
      rate.actual.predicted.mod.1.0 + ..sep.var.dummies.mod.ind ~
        LIBOR_FIRST_MEAS_PAY_ADJ_DIFF + ..sep.var.dummies.libor.diff,
    ..full.xpd.fe = full.xpd.fe,
    ..sep.var.dummies.mod.ind = sep.var.dummies.mod.ind,
    ..sep.var.dummies.libor.diff = sep.var.dummies.libor.diff) %>%
    feols(DT, cluster = "zip3", lean = TRUE) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, stage := "stage2"] %>% 
    .[, est.type := "rel"] %>%
    .[, controls := "full"]

  DT.est.all <- rbind(
    DT.stage1.base,
    DT.stage1.base.rel,
    DT.stage1.full,
    DT.stage1.full.rel,
    DT.reduced.form.base,
    DT.reduced.form.base.rel,
    DT.reduced.form.full,
    DT.reduced.form.full.rel,
    DT.stage2.base,
    DT.stage2.base.rel,
    DT.stage2.full,
    DT.stage2.full.rel
  ) %>%
    .[, sep.var := c(sep.var)] %>% 
    setcolorder(
      c("sep.var", "stage", "est.type", "controls", "term", "estimate", "std.error")
    )
  
  return(DT.est.all)
  
}

char.vars <- c("DOCTYPESUMMARY", "OCCTYPE", "MBADELINQUENCYSTATUS_PRE4_FIRST")

## DT.test <- f_est("DOCTYPESUMMARY")

DT.out <- lapply(char.vars, f_est) %>%
  rbindlist

mkdir_p(here::here("work/diff_mod_takeup"))
write_fst(DT.out,
          here::here("work/diff_mod_takeup/030-DT_est_char_vars.fst"),
          compress = 100)


