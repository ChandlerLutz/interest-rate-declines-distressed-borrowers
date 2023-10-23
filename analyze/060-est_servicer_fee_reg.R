## analyze/060-est_servicer_fee_reg.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({library(CLmisc); library(stargazer)})

##For data a cleaning script for the origination data
source(here::here("core/f_clean_bbx_orig_data.R"))
##For the regression strings common throughout the project.
source(here::here("core/rhs_strings.R"))

DT <- read_fst(
  here::here("data/DT_for_2sls_libor_diff.fst"),
  as.data.table = TRUE
) %>%
  f_clean_bbx_orig_data(.) %>%
  .[FICOSCOREORIGINATIONCALC <= 660]

DT.servicer.fees <- read_fst(
  here::here("data/discounted_cum_servicing_fee_post_first_adj.fst"),
  as.data.table = TRUE
) %>%
  setnames("DISC_SERVICER_FEE_DOLLARS_POST_FIRST_ADJ", "servicer.fees")

DT <- merge(DT, DT.servicer.fees, by = c("LOANID")) %>%
  .[, servicer.fees.over.3k := as.integer(servicer.fees > 3000)] %>%
  .[, servicer.fees.over.7.5k := as.integer(servicer.fees > 7500)]


print("Probability that servicer fees are over 3k")
DT[rate.actual.predicted.mod.1.0 == 0, mean(servicer.fees.over.3k)]

print("Probability that servicer fees are over 7.5k")
DT[rate.actual.predicted.mod.1.0 == 0, mean(servicer.fees.over.7.5k)]

## OLS

ols.3k <- xpd(
  servicer.fees.over.3k ~ rate.actual.predicted.mod.1.0 + ..rhs.numeric.vars |
    ..rhs.fe.vars | 0 | zip3, 
  ..rhs.numeric.vars = rhs.numeric.vars, 
  ..rhs.fe.vars = rhs.fe.vars
) %>%
  felm(., data = DT) %>%
  reduce_felm_object_size

ols.7.5k <- xpd(
  servicer.fees.over.7.5k ~ rate.actual.predicted.mod.1.0 + ..rhs.numeric.vars |
    ..rhs.fe.vars | 0 | zip3, 
  ..rhs.numeric.vars = rhs.numeric.vars, 
  ..rhs.fe.vars = rhs.fe.vars
) %>%
  felm(., data = DT) %>%
  reduce_felm_object_size

ols <- xpd(
  servicer.fees ~ rate.actual.predicted.mod.1.0 + ..rhs.numeric.vars |
    ..rhs.fe.vars | 0 | zip3, 
  ..rhs.numeric.vars = rhs.numeric.vars, 
  ..rhs.fe.vars = rhs.fe.vars
) %>%
  felm(., data = DT) %>%
  reduce_felm_object_size

stargazer(ols.3k, ols.7.5k, ols,
          type = "text", keep.stat = "n", keep = "rate.actual.predicted.mod.1.0")

mod.ols.list <- list(ols.3k, ols.7.5k, ols)
mkdir_p(here::here("work/"))
saveRDS(
  mod.ols.list,
  here::here("work/060-ols_servicer_fee_felm_regs.rds")
)


## Reduced Form

reduced.form.3k <- xpd(
  servicer.fees.over.3k ~ LIBOR_FIRST_MEAS_PAY_ADJ_DIFF + ..rhs.numeric.vars |
    ..rhs.fe.vars | 0 | zip3,
  ..rhs.numeric.vars = rhs.numeric.vars,
  ..rhs.fe.vars = rhs.fe.vars
) %>%
  felm(., data = DT) %>%
  reduce_felm_object_size

reduced.form.7.5k <- xpd(
  servicer.fees.over.7.5k ~ LIBOR_FIRST_MEAS_PAY_ADJ_DIFF + ..rhs.numeric.vars |
    ..rhs.fe.vars | 0 | zip3,
  ..rhs.numeric.vars = rhs.numeric.vars,
  ..rhs.fe.vars = rhs.fe.vars
) %>%
  felm(., data = DT) %>%
  reduce_felm_object_size


reduced.form <- xpd(
  servicer.fees ~ LIBOR_FIRST_MEAS_PAY_ADJ_DIFF + ..rhs.numeric.vars |
    ..rhs.fe.vars | 0 | zip3, 
  ..rhs.numeric.vars = rhs.numeric.vars, 
  ..rhs.fe.vars = rhs.fe.vars
) %>%
  felm(., data = DT) %>%
  reduce_felm_object_size

stargazer(reduced.form.3k, reduced.form.7.5k, reduced.form,
          type = "text", keep.stat = "n", keep = "LIBOR_FIRST_MEAS_PAY_ADJ_DIFF")

mod.reduced.form.list <- list(reduced.form.3k, reduced.form.7.5k, reduced.form)
saveRDS(
  mod.reduced.form.list,
  here::here("work/060-reduced_form_servicer_fee_felm_regs.rds")
)

## 2SLS

stage2.3k <- xpd(
  servicer.fees.over.3k ~ ..rhs.numeric.vars |
    ..rhs.fe.vars | (rate.actual.predicted.mod.1.0 ~ LIBOR_FIRST_MEAS_PAY_ADJ_DIFF) | zip3, 
  ..rhs.numeric.vars = rhs.numeric.vars, 
  ..rhs.fe.vars = rhs.fe.vars
) %>%
  felm(., data = DT) %>%
  reduce_felm_object_size

stage2.7.5k <- xpd(
  servicer.fees.over.7.5k ~ ..rhs.numeric.vars |
    ..rhs.fe.vars | (rate.actual.predicted.mod.1.0 ~ LIBOR_FIRST_MEAS_PAY_ADJ_DIFF) | zip3, 
  ..rhs.numeric.vars = rhs.numeric.vars, 
  ..rhs.fe.vars = rhs.fe.vars
) %>%
  felm(., data = DT) %>%
  reduce_felm_object_size

stage2 <- xpd(
  servicer.fees ~ ..rhs.numeric.vars |
    ..rhs.fe.vars | (rate.actual.predicted.mod.1.0 ~ LIBOR_FIRST_MEAS_PAY_ADJ_DIFF) | zip3, 
  ..rhs.numeric.vars = rhs.numeric.vars, 
  ..rhs.fe.vars = rhs.fe.vars
) %>%
  felm(., data = DT) %>%
  reduce_felm_object_size

stargazer(stage2.3k, stage2.7.5k, stage2,
          type = "text", keep.stat = "n", keep = "rate.actual.predicted.mod.1.0")

mod.stage2.list <- list(stage2.3k, stage2.7.5k, stage2)
saveRDS(
  mod.stage2.list,
  here::here("work/060-stage2_servicer_fee_felm_regs.rds")
)
