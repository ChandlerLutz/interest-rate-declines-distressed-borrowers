## analyze/230-est_loan_perf_by_bartik_quintile.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

suppressPackageStartupMessages({library(CLmisc); library(stargazer); library(readxl)})

## For (never REO or loss or 90+ delin) or (never REO or loss or 180+ delin)
## after 48 months 

##For data a cleaning script for the origination data
source(here::here("core/f_clean_bbx_orig_data.R"))
##For the regression strings common throughout the project.
source(here::here("core/rhs_strings.R"))

DT.params <- read_excel("./230-loan_perf_by_bartik_param_lkp.xlsx") %>%
  setDT

data.file <- here::here("work/025-DT_all_for_table_regs_id_01.fst")

if (!file.exists(data.file)) {
  print("This will take a while -- grab a snickers")
  source(here::here("analyze/025-run_first_adj_perf_regs_for_tables.R"))
}

DT.all <- read_fst(data.file, as.data.table = TRUE)

DT.all <- DT.all %>% 
  .[, bartik.recession.zip.quintile := fntile(bartik.recession.zip, n = 5)] %>%
  .[, bartik.recession.zip.quintile := sprintf("%02.f",
                                               bartik.recession.zip.quintile)] %>%
  .[, bartik.recession.zip.decile := fntile(bartik.recession.zip, n = 10)] %>%
  .[, bartik.recession.zip.decile := sprintf("%02.f", bartik.recession.zip.decile)]

f_est <- function(id) {

  id.tmp <- id
  DT.params.tmp <- DT.params[id == id.tmp]

  DT <- DT.all %>%
    copy %>% 
    .[, bartik.ntile.var := get(DT.params.tmp$bartik.ntile.var)] %>% 
    .[bartik.ntile.var != "NA"]

  ##Bartik ntiles x mod.ind dummies
  
  DT.bartik.dummies.mod.ind <- DT %>%
    .[, .(LOANID, bartik.ntile.var, rate.actual.predicted.mod.1.0)] %>%
    dummy_cols(select_columns = "bartik.ntile.var") %>%
    ##remove the first dummy b/c these are relative to the first ntile
    .[, bartik.ntile.var_01 := NULL]

  bartik.mod.ind.old.names <- names(DT.bartik.dummies.mod.ind) %>%
    .[. %like% "^bartik.ntile.var_"] %>%
    sort
  bartik.ntile.mod.ind.vars <- bartik.mod.ind.old.names %>%
    paste0(., ".mod.ind")
  DT.bartik.dummies.mod.ind <- DT.bartik.dummies.mod.ind %>%
    setnames(bartik.mod.ind.old.names, bartik.ntile.mod.ind.vars)

  DT.bartik.dummies.mod.ind <- DT.bartik.dummies.mod.ind %>%
    .[, c(bartik.ntile.mod.ind.vars) :=
          lapply(.SD, \(x) x * rate.actual.predicted.mod.1.0),
      .SDcols = bartik.ntile.mod.ind.vars] %>%
    .[, c("bartik.ntile.var", "rate.actual.predicted.mod.1.0") := NULL]

  ##Bartik ntiles x libor.diff dummies 

  DT.bartik.dummies.libor.diff <- DT %>%
    .[, .(LOANID, bartik.ntile.var, LIBOR_FIRST_MEAS_PAY_ADJ_DIFF)] %>%
    dummy_cols(select_columns = "bartik.ntile.var") %>%
    ##remove the first dummy b/c these are relative to the first ntile
    .[, bartik.ntile.var_01 := NULL]

  bartik.libor.diff.old.names <- names(DT.bartik.dummies.libor.diff) %>%
    .[. %like% "^bartik.ntile.var_"]
  bartik.ntile.libor.diff.vars <- bartik.libor.diff.old.names %>%
    paste0(., ".libor.diff")
  DT.bartik.dummies.libor.diff <- DT.bartik.dummies.libor.diff %>%
    setnames(bartik.libor.diff.old.names, bartik.ntile.libor.diff.vars)

  DT.bartik.dummies.libor.diff <- DT.bartik.dummies.libor.diff %>% 
    .[, c(bartik.ntile.libor.diff.vars) :=
          lapply(.SD, \(x) x * LIBOR_FIRST_MEAS_PAY_ADJ_DIFF),
      .SDcols = bartik.ntile.libor.diff.vars] %>%
    .[, c("bartik.ntile.var", "LIBOR_FIRST_MEAS_PAY_ADJ_DIFF") := NULL]

  DT <- DT %>%
    merge(DT.bartik.dummies.mod.ind, by = "LOANID") %>% 
    merge(DT.bartik.dummies.libor.diff, by = "LOANID")

  rhs.fe <- rhs.fe.vars %>%
    .[!(. %chin% eval(parse(text = DT.params.tmp$controls.to.remove)))]

  rhs.varying.slopes <- paste0(rhs.numeric.vars, collapse = ", ") %>%
    sprintf("bartik.ntile.var[%s]", .)

  rhs.fe <- paste0("bartik.ntile.var^", rhs.fe) %>%
    paste0(collapse = " + ")

  full.xpd.fe <- c(rhs.varying.slopes, rhs.fe)
  
  ## Run the regressions
  
  DT.2sls.90.main <- xpd(
    ever.reo.or.is.delin90.plus ~ 1 |
      ..full.xpd.fe |
      rate.actual.predicted.mod.1.0:bartik.ntile.var ~
        LIBOR_FIRST_MEAS_PAY_ADJ_DIFF:bartik.ntile.var, 
    ..full.xpd.fe = full.xpd.fe
  ) %>%
    feols(., data = DT, cluster = "zip3") %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[term %like% "^fit_"] %>%
    .[, stage := "stage2"] %>% 
    .[, est.type := "main"] %>% 
    .[, controls := "full"] %>%
    .[, lhs.var := "ever.reo.or.is.delin90.plus"] %>% 
    .[, ntile := gsub(".*([0-9]{2})$", "\\1", x = term)] %>%
    .[order(ntile)]
  
  DT.2sls.90.rel <- xpd(
    ever.reo.or.is.delin90.plus ~ 1 |
      ..full.xpd.fe |
      rate.actual.predicted.mod.1.0 + ..bartik.ntile.mod.ind.vars ~
        LIBOR_FIRST_MEAS_PAY_ADJ_DIFF + ..bartik.ntile.libor.diff.vars, 
    ..full.xpd.fe = full.xpd.fe,
    ..bartik.ntile.mod.ind.vars = bartik.ntile.mod.ind.vars, 
    ..bartik.ntile.libor.diff.vars = bartik.ntile.libor.diff.vars
  ) %>%
    feols(., data = DT, cluster = "zip3") %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, stage := "stage2"] %>%
    .[, est.type := "rel"] %>%
    .[, controls := "full"] %>%
    .[, lhs.var := "ever.reo.or.is.delin90.plus"] %>% 
    .[, ntile := gsub("fit_bartik.ntile.var_([0-9]{2}).mod.ind", "\\1", x = term)] %>%
    .[ntile == "fit_rate.actual.predicted.mod.1.0", ntile := "01"] %>%
    .[order(ntile)]

  DT.2sls.180.main <- xpd(
    ever.reo.or.is.delin180.plus ~ 1 |
      ..full.xpd.fe |
      rate.actual.predicted.mod.1.0:bartik.ntile.var ~
        LIBOR_FIRST_MEAS_PAY_ADJ_DIFF:bartik.ntile.var, 
    ..full.xpd.fe = full.xpd.fe
  ) %>%
    feols(., data = DT, cluster = "zip3") %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[term %like% "^fit_"] %>%
    .[, stage := "stage2"] %>% 
    .[, est.type := "main"] %>% 
    .[, controls := "full"] %>%
    .[, lhs.var := "ever.reo.or.is.delin180.plus"] %>% 
    .[, ntile := gsub(".*([0-9]{2})$", "\\1", x = term)] %>%
    .[order(ntile)]
  
  DT.2sls.180.rel <- xpd(
    ever.reo.or.is.delin180.plus ~ 1 |
      ..full.xpd.fe |
      rate.actual.predicted.mod.1.0 + ..bartik.ntile.mod.ind.vars ~
        LIBOR_FIRST_MEAS_PAY_ADJ_DIFF + ..bartik.ntile.libor.diff.vars, 
    ..full.xpd.fe = full.xpd.fe,
    ..bartik.ntile.mod.ind.vars = bartik.ntile.mod.ind.vars, 
    ..bartik.ntile.libor.diff.vars = bartik.ntile.libor.diff.vars
  ) %>%
    feols(., data = DT, cluster = "zip3") %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, stage := "stage2"] %>%
    .[, est.type := "rel"] %>%
    .[, controls := "full"] %>%
    .[, lhs.var := "ever.reo.or.is.delin180.plus"] %>% 
    .[, ntile := gsub("fit_bartik.ntile.var_([0-9]{2}).mod.ind", "\\1", x = term)] %>%
    .[ntile == "fit_rate.actual.predicted.mod.1.0", ntile := "01"] %>%
    .[order(ntile)]


  DT.est <- rbind(DT.2sls.90.main, DT.2sls.90.rel, DT.2sls.180.main, DT.2sls.180.rel) %>%
    .[, id := c(id)] %>%
    setcolorder(c("id", "stage", "est.type", "controls", "lhs.var", "ntile"))
  
  return(DT.est)
  
}

## DT.tmp <- f_est(1)

DT.out <- lapply(DT.params$id, f_est) %>%
  rbindlist

mkdir_p(here::here("work/"))
write_fst(
  DT.out,
  here::here(
    "work/230-DT_est_reo_loss_delin_after_48m_by_bartik_ntile.fst"
  ),
  compress = 100
)
