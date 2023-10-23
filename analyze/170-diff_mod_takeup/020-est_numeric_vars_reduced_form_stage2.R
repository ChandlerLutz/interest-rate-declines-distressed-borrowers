## analyze/170-diff_mod_takeup/020-est_numeric_vars_reduced_form_stage2.R

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
  setcolorder("id") %>%
  ##only compute the reduced form and stage2 for non-appendix variables
  .[for.appendix == 0]

DT <- read_fst(
  here::here("data/DT_for_2sls_libor_diff.fst"),
  as.data.table = TRUE
) %>%
  f_clean_bbx_orig_data(.)


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

  DT <- copy(DT) %>% 
    .[, sep.var.ntile := fntile(get(sep.var), ntiles)] %>%
    .[, sep.var.ntile.char := sprintf("%02.f", sep.var.ntile)] %>% 
    .[, sep.var.ntile.fact := as.factor(sep.var.ntile.char)] %>%
    .[, sep.var.ntile.fact := relevel(sep.var.ntile.fact, 1)] %>%
    .[!is.na(sep.var.ntile)]

  DT.ntile.dummies <- DT %>%
    .[, .(LOANID, sep.var.ntile.char)] %>%
    dummy_cols(select_columns = "sep.var.ntile.char") %>%
    .[, sep.var.ntile.char := NULL] %>%
    .[, sep.var.ntile.char_01 := NULL]

  ## Libor diff x ntile dummies 

  DT.libor.diff.ntile.dummies <- DT.ntile.dummies %>%
    merge(DT[, .(LOANID, LIBOR_FIRST_MEAS_PAY_ADJ_DIFF)], by = "LOANID") %>% 
    .[, c(.(LOANID = LOANID), 
          lapply(.SD, \(x) x * LIBOR_FIRST_MEAS_PAY_ADJ_DIFF)),
      .SDcols = patterns("sep.var.ntile.char_")]

  old.names <- names(DT.libor.diff.ntile.dummies) %>%
    .[grepl("sep.var.ntile.char_", x = .)] %>%
    sort
  libor.diff.ntile.dummy.names <- paste0(old.names, ".libor.diff")

  DT.libor.diff.ntile.dummies <- DT.libor.diff.ntile.dummies %>%
    setnames(old.names, libor.diff.ntile.dummy.names) %>%
    setcolorder(c("LOANID", libor.diff.ntile.dummy.names))
  

  rm(old.names)

  ## Mod ind x ntile dummies 

  DT.mod.ind.ntile.dummies <- DT.ntile.dummies %>%
    merge(DT[, .(LOANID, rate.actual.predicted.mod.1.0)], by = "LOANID") %>% 
    .[, c(.(LOANID = LOANID), 
          lapply(.SD, \(x) x * rate.actual.predicted.mod.1.0)),
      .SDcols = patterns("sep.var.ntile.char_")]

  old.names <- names(DT.mod.ind.ntile.dummies) %>%
    .[grepl("sep.var.ntile.char_", x = .)] %>%
    sort
  mod.ind.ntile.dummy.names <- paste0(old.names, ".mod.ind")

  DT.mod.ind.ntile.dummies <- DT.mod.ind.ntile.dummies %>%
    setnames(old.names, mod.ind.ntile.dummy.names) %>%
    setcolorder(c("LOANID", mod.ind.ntile.dummy.names))

  rm(old.names)

  ## Merge 

  DT <- merge(DT, DT.libor.diff.ntile.dummies, by = "LOANID") %>%
    merge(DT.mod.ind.ntile.dummies, by = "LOANID")


  ## Estimation

  f.base.reduced.form <- xpd(
    discounted.cum.loss ~ 0 + LIBOR_FIRST_MEAS_PAY_ADJ_DIFF:sep.var.ntile.char |
      ..base.xpd.fe,
    ..base.xpd.fe = base.xpd.fe
  )

  DT.base.reduced.form <- feols(f.base.reduced.form, DT, cluster = "zip3", lean = TRUE) %>%
    broom::tidy(.) %>%
    setDT(.) %>% 
    .[, est.type := "main"] %>%
    .[, controls := "base"] %>%
    .[, stage := "reduced.form"]

  f.base.reduced.form.rel.to.ntile1 <- xpd(
    discounted.cum.loss ~ LIBOR_FIRST_MEAS_PAY_ADJ_DIFF *
      sep.var.ntile.fact | ..base.xpd.fe,
    ..base.xpd.fe = base.xpd.fe
    )

  DT.base.reduced.form.rel.to.ntile1 <- feols(
    f.base.reduced.form.rel.to.ntile1, DT,
    cluster = "zip3", lean = TRUE
  ) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, est.type := "rel.to.ntile01"] %>%
    .[, controls := "base"] %>%
    .[, stage := "reduced.form"]
  
  f.base.2sls <- xpd(
    discounted.cum.loss ~ 1 | ..base.xpd.fe | 
      rate.actual.predicted.mod.1.0:sep.var.ntile.char ~
        LIBOR_FIRST_MEAS_PAY_ADJ_DIFF:sep.var.ntile.char,
    ..base.xpd.fe = base.xpd.fe
  )
  
  DT.base.2sls <- feols(
    f.base.2sls, DT,
    cluster = "zip3", lean = TRUE
  ) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, est.type := "main"] %>%
    .[, controls := "base"] %>%
    .[, stage := "stage2"]


  f.base.2sls.rel.to.ntile1 <- xpd(
    discounted.cum.loss ~ 1 | ..base.xpd.fe |
      rate.actual.predicted.mod.1.0 + ..mod.ind.interactions ~
        LIBOR_FIRST_MEAS_PAY_ADJ_DIFF + ..libor.diff.interactions,
    ..base.xpd.fe = base.xpd.fe, 
    ..libor.diff.interactions = libor.diff.ntile.dummy.names,
    ..mod.ind.interactions = mod.ind.ntile.dummy.names)

  DT.base.2sls.rel.to.ntile1 <- feols(
    f.base.2sls.rel.to.ntile1, DT,
    cluster = "zip3", lean = TRUE
  ) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, est.type := "rel.to.ntile01"] %>%
    .[, controls := "base"] %>%
    .[, stage := "stage2"]


  f.full.reduced.form <- xpd(
    discounted.cum.loss ~ LIBOR_FIRST_MEAS_PAY_ADJ_DIFF:sep.var.ntile.fact |
      sep.var.ntile.char + ..rhs.varying.slopes + ..rhs.fe,
    ..rhs.varying.slopes = rhs.varying.slopes,
    ..rhs.fe = rhs.fe
  )

  DT.full.reduced.form <- feols(
    f.full.reduced.form, DT,
    cluster = "zip3", lean = TRUE
  ) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, est.type := "main"] %>%
    .[, controls := "full"] %>%
    .[, stage := "reduced.form"]
  
  f.full.reduced.form.rel.to.ntile1 <- xpd(
    discounted.cum.loss ~ LIBOR_FIRST_MEAS_PAY_ADJ_DIFF *
      sep.var.ntile.fact |
      sep.var.ntile.char + ..rhs.varying.slopes + ..rhs.fe,
    ..rhs.varying.slopes = rhs.varying.slopes,
    ..rhs.fe = rhs.fe)

  DT.full.reduced.form.rel.to.ntile1 <- feols(
    f.full.reduced.form.rel.to.ntile1, DT,
    cluster = "zip3", lean = TRUE
  ) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, est.type := "rel.to.ntile01"] %>%
    .[, controls := "full"] %>%
    .[, stage := "reduced.form"]

  f.full.2sls <- xpd(
    discounted.cum.loss ~ 1 | ..full.xpd.fe |
      rate.actual.predicted.mod.1.0:sep.var.ntile.char ~
        LIBOR_FIRST_MEAS_PAY_ADJ_DIFF:sep.var.ntile.char,
    ..full.xpd.fe = full.xpd.fe
  )

  DT.full.2sls <- feols(
    f.full.2sls, DT,
    cluster = "zip3", lean = TRUE
  ) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, est.type := "main"] %>%
    .[, controls := "full"] %>%
    .[, stage := "stage2"]

  f.full.2sls.rel.to.ntile1 <- xpd(
    discounted.cum.loss ~ 1 | ..full.xpd.fe |
      rate.actual.predicted.mod.1.0 + ..mod.ind.interactions ~
        LIBOR_FIRST_MEAS_PAY_ADJ_DIFF + ..libor.diff.interactions,
    ..full.xpd.fe = full.xpd.fe, 
    ..libor.diff.interactions = libor.diff.ntile.dummy.names,
    ..mod.ind.interactions = mod.ind.ntile.dummy.names)

  DT.full.2sls.rel.to.ntile1 <- feols(
    f.full.2sls.rel.to.ntile1, DT,
    cluster = "zip3", lean = TRUE
  ) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[, est.type := "rel.to.ntile01"] %>%
    .[, controls := "full"] %>%
    .[, stage := "stage2"]


  DT.est.all <- rbind(
    DT.base.reduced.form, 
    DT.base.reduced.form.rel.to.ntile1,
    DT.base.2sls,
    DT.base.2sls.rel.to.ntile1,
    DT.full.reduced.form, 
    DT.full.reduced.form.rel.to.ntile1,
    DT.full.2sls, 
    DT.full.2sls.rel.to.ntile1
  ) %>%
    .[, sep.var := c(sep.var)] %>%
    setcolorder(c("sep.var", "controls", "est.type", "stage"))

  return(DT.est.all)

}

## DT.test <- f_ntile_est(1)

DT.out <- lapply(DT.lkp$id, f_ntile_est) %>%
  rbindlist



DT.out <- DT.out %>% 
  .[, ntile := NA_character_] %>%
  .[, ntile := fcase(
    term == "fit_rate.actual.predicted.mod.1.0", "01",
    term == "LIBOR_FIRST_MEAS_PAY_ADJ_DIFF", "01",
    grepl("[0-5]{2}", x = term), gsub(".*([0-9]{2}).*", "\\1", term)    
  )] %>%
  .[, ntile := substr(ntile, 2, 2)] %>%
  setcolorder(c("sep.var", "controls", "est.type", "ntile"))

mkdir_p(here::here("work/diff_mod_takeup"))
write_fst(
  DT.out,
  here::here("work/diff_mod_takeup/020-DT_est_numeric_vars_reduced_form_stage2.fst"),
  compress = 100
)



## ggplot(DT.out[stage == "stage2" & est.type == "main" & controls == "full"], aes(x = ntile)) +
##   geom_errorbar(aes(ymin = estimate - 2 * std.error,
##                     ymax = estimate + 2 * std.error)) +
##   geom_point(aes(y = estimate), size = 3) + 
##   facet_wrap(facets = vars(sep.var))

