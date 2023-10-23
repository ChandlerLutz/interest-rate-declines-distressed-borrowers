## analyze/210-reg_dynamic_perf.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({
  library(CLmisc); library(parallel);
})

options(lfe.threads = detectCores(logical = FALSE))

##For data a cleaning script for the origination data
source(here::here("core/f_clean_bbx_orig_data.R"))
##For the regression strings common throughout the project.
source(here::here("core/rhs_strings.R"))


DT <- read_fst(
  here::here("data/DT_for_2sls_libor_diff.fst"),
  as.data.table = TRUE
) %>%
  ##from core/f_clean_bbx_orig_data.R
  f_clean_bbx_orig_data

vars.to.demean <- c("NOARMLOOKBKDAYS", "FICOSCOREORIGINATIONCALC", "COMBINEDLIENLTVCALC", "BEGBALCALC_PRE4_FIRSTPAYMTADJDTCALC", "ORIGINTRTCALC")
vars.demeaned <- paste0(vars.to.demean, ".demeaned")
DT <- DT %>%
  .[, c(vars.demeaned) := lapply(.SD, function(x) x - mean(x, na.rm = TRUE)),
    .SDcols = vars.to.demean]
  

DT.perf.last <- read_fst( here::here("data/DT_post_adj_perf_last.fst"), as.data.table = TRUE) %>%
  setnames(names(.), paste0(names(.), ".last")) %>%
  ##set back the name for the loan id
  setnames("LOANID.last", "LOANID") %>%
  ##remove mismatches for liquidated with a loss that don't have a
  ##date associated with the liquidation
  .[!(EVER_LIQUIDATED_WITH_LOSS.last == 0 & MBADELINQUENCYSTATUS.last == "L")]

##Make sure that all of the loans that are in DT are in DT.perf.last
DT <- DT %>%
  .[LOANID %in% DT.perf.last[["LOANID"]]]

f_merge_orig_perf_data <- function(DT.orig, months.after.first.adj) {

  ##The pereformance data
  perf.file <- sprintf("data/post_adj_perf/DT_post_adj_perf_%s.fst", months.after.first.adj)
  DT.perf <- read_fst(here::here(perf.file), as.data.table = TRUE)
  perf.vars <- names(DT.perf) %>% .[!grepl("^LOANID$", x = .)]

  ##If missing, fill it in the with the last avaiable observations

  DT.all <- merge(DT.orig, DT.perf, by = "LOANID", all.x = TRUE) %>%
    merge(DT.perf.last, by = "LOANID", all.x = TRUE)

  for (var in perf.vars)
    DT.all <- DT.all[is.na(get(var)), c(var) := .SD, .SDcols = paste0(var, ".last")]

  ##delete the *.last variables
  .last.vars <- names(DT.all) %>% .[grepl(".last$", x = .)]
  DT.all <- DT.all[, c(.last.vars) := NULL]

  ##udpate the perf var names
  setnames(DT.all, perf.vars, paste0(perf.vars, ".perf"))

  DT.all <- DT.all %>%
    .[, is.active := as.integer(EVER_REO_FORC.perf + EVER_LIQUIDATED_WITH_LOSS.perf + EVER_PAID_OFF.perf < 1)] %>%
    .[, ever.reo.or.loss := as.integer(EVER_REO_FORC.perf + EVER_LIQUIDATED_WITH_LOSS.perf >= 1)] %>%
    .[, ever.paid.off := as.integer(ever.reo.or.loss == 0 & EVER_PAID_OFF.perf >= 1)] %>%
    ## See https://www.urban.org/urban-wire/understand-mortgage-default-rates-ask-these-three-questions
    .[, EVER_DEFAULT.perf := as.integer(EVER_DAYS_DELIN180.perf + ever.reo.or.loss >= 1)] %>%
    .[, is.current.or.ever.paid.off.perf := as.integer((MBADELINQUENCYSTATUS.perf == "C" | ever.paid.off >= 1) &
                                                         ever.reo.or.loss == 0)] %>%
    .[, is.in.forc := as.integer(MBADELINQUENCYSTATUS.perf == "F" & ever.reo.or.loss == 0)] %>%
    .[, is.delin.leq.30.or.ever.paid.off := as.integer((DAYS_DELIN.perf <= 30 | ever.paid.off >= 1) &
                                                         ever.reo.or.loss == 0)] %>%
    .[, is.delin60.plus := as.integer(DAYS_DELIN.perf >= 60 & ever.reo.or.loss == 0 & ever.paid.off == 0)] %>%
    .[, is.delin90.plus := as.integer(DAYS_DELIN.perf >= 90 & ever.reo.or.loss == 0 & ever.paid.off == 0)] %>%
    .[, is.delin180.plus := as.integer(DAYS_DELIN.perf >= 180 & ever.reo.or.loss == 0 & ever.paid.off == 0)] %>%
    .[, ever.reo.or.is.delin60.plus := as.integer((ever.reo.or.loss == 1 | is.delin60.plus == 1) & (ever.paid.off == 0))] %>%
    .[, ever.reo.or.is.delin90.plus := as.integer((ever.reo.or.loss == 1 | is.delin90.plus == 1) & (ever.paid.off == 0))] %>%
    .[, ever.reo.or.is.delin180.plus := as.integer((ever.reo.or.loss == 1 | is.delin180.plus == 1) & (ever.paid.off == 0))]

  return(DT.all)

}

f_reg_perf <- function(DT.temp, months.after.first.adj) {

  print(months.after.first.adj)
  
  DT.temp <- f_merge_orig_perf_data(
    DT.temp,
    months.after.first.adj = months.after.first.adj
  )

  ##make sure everything is binary
  binary.vars <- names(DT.temp) %>% .[grepl("EVER_|is\\.|ever\\.", x = .)]
  DT.temp <- DT.temp %>%
    .[, c(binary.vars) := lapply(.SD, function(x) as.integer(x >= 1)), .SDcols = binary.vars]

  f_reg <- function(lhs.var, filter.string = NULL) {

    DT.reg <- copy(DT.temp)

    if (!is.null(filter.string)) {
      DT.reg <- DT.reg[eval(parse(text = filter.string))]
    }

    DT.2sls.base <- sprintf(
      "%s ~ first.payment.adj.month.char + NOARMLOOKBKDAYS.demeaned + FICOSCOREORIGINATIONCALC.demeaned + COMBINEDLIENLTVCALC.demeaned + BEGBALCALC_PRE4_FIRSTPAYMTADJDTCALC.demeaned + ORIGINTRTCALC.demeaned | 0 | (rate.actual.predicted.mod.1.0 ~ LIBOR_FIRST_MEAS_PAY_ADJ_DIFF) | zip3",
      lhs.var
    ) %>%
      as.formula %>%
      felm(., data = DT.reg) %>%
      felm_broom_tidy %>%
      .[, lhs.var := c(lhs.var)] %>%
      .[, est.type := "2sls"] %>%
      .[, controls := "base"]
    
    DT.2sls.full <- sprintf(
      rhs.perf,
      ##The instruments  
      "(rate.actual.predicted.mod.1.0 ~ LIBOR_FIRST_MEAS_PAY_ADJ_DIFF)"
    ) %>%
    paste0(lhs.var, " ~ ", .) %>%
    as.formula %>%
    felm(., data = DT.reg) %>%
    felm_broom_tidy %>% 
    .[, lhs.var := c(lhs.var)] %>%
    .[, est.type := "2sls"] %>%
    .[, controls := "full"]

    DT.out <- rbind(DT.2sls.base,
                    DT.2sls.full
                    )

    if (is.null(filter.string)) {
      DT.out <- DT.out[, filter.string := NA]
    } else {
      DT.out <- DT.out[, filter.string := filter.string]
    }

    DT.out <- DT.out %>%
      .[, months.after.first.adj := months.after.first.adj]

    DT.out <- DT.out %>%
      setcolorder(c("months.after.first.adj", "lhs.var", "filter.string", "est.type", "controls"))

    if (is.null(filter.string)) {
      DT.out <- DT.out[, sample := "full"]
    } else {
      DT.out <- DT.out[, sample := "filtered"]
    }

    return(DT.out)
  }

  ##test
  ##temp <- f_reg(lhs.var = "ever.reo.or.loss")

  DT.reo.loss <- f_reg(
    lhs.var = "ever.reo.or.loss",
    filter.string = "FICOSCOREORIGINATIONCALC <= 660"
  )
  
  DT.ever.paid.off <- f_reg(
    lhs.var = "ever.paid.off",
    filter.string = "FICOSCOREORIGINATIONCALC <= 660"
  )
  
  DT.ever.reo.or.is.delin90.plus <- f_reg(
    lhs.var = "ever.reo.or.is.delin90.plus",
    filter.string = "FICOSCOREORIGINATIONCALC <= 660"
  )

  DT.ever.reo.or.is.delin180.plus <- f_reg(
    lhs.var = "ever.reo.or.is.delin180.plus",
    filter.string = "FICOSCOREORIGINATIONCALC <= 660"
    )

  
  ## -- Non-reo or liquidated with loss loans -- ##
  
  DT.is.delin.leq.30.or.ever.paid.off <- f_reg(
    lhs.var = "is.delin.leq.30.or.ever.paid.off",
    filter.string = "FICOSCOREORIGINATIONCALC <= 660 & ever.reo.or.loss == 0"
  )
  DT.is.delin90.plus <- f_reg(
    lhs.var = "is.delin90.plus",
    filter.string = "FICOSCOREORIGINATIONCALC <= 660 & ever.reo.or.loss == 0"
  )
  DT.is.delin180.plus <- f_reg(
    lhs.var = "is.delin180.plus",
    filter.string = "FICOSCOREORIGINATIONCALC <= 660 & ever.reo.or.loss == 0"
  )

  DT.all <- rbind(
    ##All Loans 
    DT.reo.loss,
    DT.ever.paid.off,
    DT.ever.reo.or.is.delin90.plus,
    DT.ever.reo.or.is.delin180.plus,
    ## -- Non-reo or liquidated with loss loans -- ##
    DT.is.delin.leq.30.or.ever.paid.off,
    DT.is.delin90.plus,
    DT.is.delin180.plus
    )

  ##Could filter for the intercept and the regression terms of interest just with
  ##DT.all %>% .[grepl("Intercept|fit|_DIFF",term)]
  return(DT.all)
}

f_worker <- function(months.after.first.adj) {
  f_reg_perf(DT, months.after.first.adj = months.after.first.adj)
}

## -- For 48 months after first adjustment, for testing -- ##
## DT.48 <- f_worker(48)
## DT.48 %>%
##   .[grepl("rate.actual.predicted.mod.1.0", term) & controls == "full"] %>%
##   .[, .(lhs.var, estimate, std.error)] %>%
##   print

## cl <- makeCluster(4)
## clusterEvalQ(cl, expr = {library(CLmisc);})
## clusterExport(cl, varlist = c("DT", "DT.perf.last", "f_merge_orig_perf_data", "f_reg_perf", "rhs.perf"))

## DT.all <- parLapplyLB(cl = cl, 1:48, f_worker) %>%
##   rbindlist

## stopCluster(cl)

## test <- f_worker(months.after.first.adj = 6)

DT.all <- lapply(1:48, f_worker) %>%
  rbindlist

##could filter with
##DT.all %>% .[grepl("Intercept|fit|_DIFF",term)]

mkdir_p("work")

write_fst(
  DT.all,
  here::here("work/210-DT_est_months_after_first_adj.fst"),
  compress = 100
)

