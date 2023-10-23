## analyze/125-est_binscatters.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({
  library(CLmisc); library(binsreg); library(readxl)
})

DT.lhs.lkp <- read_excel("./125-binscatter_lhs_vars_lkp.xlsx") %>%
  setDT

DT <- read_fst(
  here::here("data/DT_for_binscatters.fst"),
  as.data.table = TRUE
)

## ## Basic binsreg usage 
## bs.01 <- binsreg(y = rate.actual.predicted.mod.1.0,
##                  x = LIBOR_FIRST_MEAS_PAY_ADJ_DIFF,
##                  w = ~ first.payment.adj.month.char, 
##                  data = DT,
##                  randcut = 1 ## To use full data set in bin calculation 
##                  )

## bs.02 <- binsreg(y = rate.actual.predicted.mod.1.0,
##                  x = LIBOR_FIRST_MEAS_PAY_ADJ_DIFF,
##                  w = ~ first.payment.adj.month.char + fico.demeaned + cltv.demeaned + margin.demeaned, 
##                  data = DT,
##                  randcut = 1)


## http://adv-r.had.co.nz/Computing-on-the-language.html#substitute
f_bins_reg <- function(y, x, w) {
  binsreg.call <-
    substitute(
      binsreg(y = y, x = x, w = w,
              data = DT,
              randcut = 1)
    )
  binsreg.mod <- eval(binsreg.call)
  DT.binsreg <- binsreg.mod$data.plot[[1]]$data.dots %>%
    as.data.table
  return(DT.binsreg)
}

## ## Test f_bins_reg
## bs.reg.test <- f_bins_reg(y = rate.actual.predicted.mod.1.0,
##                           x = LIBOR_FIRST_MEAS_PAY_ADJ_DIFF,
##                           w = ~ first.payment.adj.month.char
##                           )


## See
## http://adv-r.had.co.nz/Computing-on-the-language.html#substitute
## https://rdatatable.gitlab.io/data.table/articles/datatable-programming.html#substitute-variables-and-character-values
f_bins_reg2 <- function(y, x, w) {

  stopifnot(is.character(y),
            is.character(x),
            is.character(w))
  
  binsreg.call <-
    substitute(
      binsreg(y = y, x = x, w = w,
              data = DT,
              randcut = 1 ## To use full data set in bin calculation 
              ), 
      env = list(y = as.name(y), x = as.name(x), w = as.formula(w))
    )
  binsreg.mod <- eval(binsreg.call)
  DT.binsreg <- binsreg.mod$data.plot[[1]]$data.dots %>%
    as.data.table
  return(DT.binsreg)
}

## ## Test f_bins_reg2
## bs.reg.test <- f_bins_reg2(y = "rate.actual.predicted.mod.1.0",
##                            x = "LIBOR_FIRST_MEAS_PAY_ADJ_DIFF",
##                            w = "~ first.payment.adj.month.char"
##                            )


f_worker <- function(lhs.var) {

  DT.bs.baseline <- f_bins_reg2(
    y = lhs.var,
    x = "LIBOR_FIRST_MEAS_PAY_ADJ_DIFF",
    w = "~ first.payment.adj.month.char"
  ) %>%
    .[, plot.label := "Baseline"]

  DT.bs.risk.cntrls <- f_bins_reg2(
    y = lhs.var,
    x = "LIBOR_FIRST_MEAS_PAY_ADJ_DIFF",
    w = "~ first.payment.adj.month.char + fico.demeaned + cltv.demeaned + margin.demeaned"
  ) %>%
    .[, plot.label := "Credit Risk-Adjusted"]
  

  DT.out <- data.table(lhs.var = lhs.var) %>% 
    .[, DT.bs.baseline := list(DT.bs.baseline)] %>%
    .[, DT.bs.risk.cntrls := list(DT.bs.risk.cntrls)]

  return(DT.out)
  
}

DT <- lapply(DT.lhs.lkp$lhs.var, f_worker) %>%
  rbindlist

mkdir_p(here::here("work/"))
saveRDS(DT, here::here("work/125-binscatter_estimates.rds"))


