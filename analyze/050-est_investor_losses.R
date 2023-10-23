## analyze/050-est_investor_losses.R

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
  .[, cum.loss.over.5k := as.integer(discounted.cum.loss > 5000)] %>%
  .[, cum.loss.over.10k := as.integer(discounted.cum.loss > 10000)] %>%
  .[, cum.loss.over.25k := as.integer(discounted.cum.loss > 25000)] %>%
  .[, cum.loss.over.50k := as.integer(discounted.cum.loss > 50000)] %>%
  .[, cum.loss.over.100k := as.integer(discounted.cum.loss > 100000)] %>%
  .[, cum.loss.over.150k := as.integer(discounted.cum.loss > 150000)] %>%
  .[, cum.loss.over.200k := as.integer(discounted.cum.loss > 200000)] %>%
  .[, discounted.cum.loss.above.zero := fifelse(discounted.cum.loss > 0,
                                                discounted.cum.loss,
                                                NA_real_)] %>% 
  .[FICOSCOREORIGINATIONCALC <= 660]


## Summary stats
DT[rate.actual.predicted.mod.1.0 == 0, mean(discounted.cum.loss > 0, na.rm = TRUE)]
DT[rate.actual.predicted.mod.1.0 == 0, summary(discounted.cum.loss)]
DT[rate.actual.predicted.mod.1.0 == 0 & discounted.cum.loss > 0, summary(discounted.cum.loss)]


cum.loss.binary.vars <- names(DT) %>%
  .[grepl("^cum.loss.over", x = .)]

lhs.vars <- c("discounted.cum.loss", cum.loss.binary.vars,
              "discounted.cum.loss.above.zero")

f_est_ols <- function(lhs.var) {

  if (lhs.var == "discounted.cum.loss.above.zero") {
    DT <- DT[discounted.cum.loss > 0]
  }
  
  f.formula <- xpd(
    ..lhs.var ~ rate.actual.predicted.mod.1.0 + ..rhs.numeric.vars |
      ..rhs.fe.vars | 0 | zip3, 
    ..lhs.var = lhs.var, 
    ..rhs.numeric.vars = rhs.numeric.vars, 
    ..rhs.fe.vars = rhs.fe.vars
  )

  felm(f.formula, data = DT)
}

f_est_reduced_form <- function(lhs.var) {

  if (lhs.var == "discounted.cum.loss.above.zero") {
    DT <- DT[discounted.cum.loss > 0]
  }
  
  f.formula <- xpd(
    ..lhs.var ~ LIBOR_FIRST_MEAS_PAY_ADJ_DIFF + ..rhs.numeric.vars |
      ..rhs.fe.vars | 0 | zip3, 
    ..lhs.var = lhs.var, 
    ..rhs.numeric.vars = rhs.numeric.vars, 
    ..rhs.fe.vars = rhs.fe.vars
  )

  felm(f.formula, data = DT)
}

f_est_2sls <- function(lhs.var) {

  if (lhs.var == "discounted.cum.loss.above.zero") {
    DT <- DT[discounted.cum.loss > 0]
  }

  f.formula <- xpd(
    ..lhs.var ~ ..rhs.numeric.vars | ..rhs.fe.vars |
      (rate.actual.predicted.mod.1.0 ~ LIBOR_FIRST_MEAS_PAY_ADJ_DIFF) |
      zip3, 
    ..lhs.var = lhs.var, 
    ..rhs.numeric.vars = rhs.numeric.vars, 
    ..rhs.fe.vars = rhs.fe.vars
  )

  mod <- felm(f.formula, data = DT)
  
}

cum.loss.2sls <- lapply(lhs.vars, f_est_2sls) %>%
  lapply(reduce_felm_object_size)

mkdir_p(here::here("work"))
saveRDS(cum.loss.2sls,
        here::here("work/050-cum_loss_2sls.rds"))

cum.loss.reduced.form <- lapply(lhs.vars, f_est_reduced_form) %>%
  lapply(reduce_felm_object_size)

saveRDS(cum.loss.reduced.form,
        here::here("work/050-cum_loss_reduced_form.rds"))

cum.loss.ols <- lapply(lhs.vars, f_est_ols) %>%
  lapply(reduce_felm_object_size)

saveRDS(cum.loss.ols,
        here::here("work/050-cum_loss_ols.rds"))
