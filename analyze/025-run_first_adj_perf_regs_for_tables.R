## analyze/025-run_first_adj_perf_regs_for_tables.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({library(CLmisc); library(readxl); library(patchwork)})

##For data a cleaning script for the origination data
source(here::here("core/f_clean_bbx_orig_data.R"))
##For the regression strings common throughout the project.
source(here::here("core/rhs_strings.R"))

f_quarter_label <- function(x) paste0(year(x), "Q", quarter(x))

DT.params <- read_xlsx("./025-run_regs_for_tex_tables_lkp.xlsx",
                       sheet = "tables") %>%
  setDT

f_reg_all <- function(table.id) {

  print(table.id)

  DT.params.temp <- DT.params[id == c(table.id)]

  if (DT.params.temp$first.adj.fe.var != "first.payment.adj.month.char") {
    rhs.first.pay.adj <- sub("first.payment.adj.month.char",
                             DT.params.temp$first.adj.fe.var, 
                             x = rhs.first.pay.adj)
    rhs.perf <- sub("first.payment.adj.month.char",
                    DT.params.temp$first.adj.fe.var, 
                    x = rhs.perf)
  }
  

  DT <- read_fst(here::here(DT.params.temp$orig.file),
                 as.data.table = TRUE
                 ) %>%
    ##from core/f_clean_bbx_orig_data.R
    f_clean_bbx_orig_data


  if (!is.na(DT.params.temp$orig.data.filter)) {
    DT <- DT[eval(parse(text = DT.params.temp$orig.data.filter))]
  }
  
  DT.jackknife.iv <- read_fst(
    here::here(
      "data/DT_z_jackknife_iv_first_meas_adj_diff.fst"
    ),
    as.data.table = TRUE
  )

  DT <- merge(DT, DT.jackknife.iv, by = "LOANID")

  efx.edti.4m.pre.first.adj.sd <- DT %>%
    .[, sd(EFX_EDTI_4M_PRE_FIRSTPAYMTADJDTCALC, na.rm = TRUE)]

  DT <- DT %>%
    .[, EFX_EDTI_1M_post_4M_pre_subprime_edti_4m_pre := EFX_EDTI_1M_post_4M_pre / c(efx.edti.4m.pre.first.adj.sd)]

  ## -- The near term post 1st adj data -- ##
  post.6m.ever.vars <- c("EVER_PAID_OFF.6m.post.first.adj",
                         "EVER_REO_FORC.6m.post.first.adj",
                         "EVER_LIQUIDATED_WITH_LOSS.6m.post.first.adj")

  DT.6m.post.first.adj <- read_fst(
    here::here(DT.params.temp$perf.6m.post.first.adj),
    as.data.table = TRUE
  ) %>%
    .[, c("LOANID_OLDLOANID", "FIRSTPAYMTADJDTCALC") := NULL] %>%
    select_by_ref(c("LOANID", "SCHEDINTAMTCALC.6m.post.first.adj",
                    "CURRENTINTRTCALC.6m.post.first.adj",
                    post.6m.ever.vars)) %>%
    .[, ever.reo.or.loss.6m.post.first.adj := as.integer(
      EVER_REO_FORC.6m.post.first.adj + EVER_LIQUIDATED_WITH_LOSS.6m.post.first.adj >= 1
    )]

  for (var in post.6m.ever.vars) {
    DT.6m.post.first.adj[, c(var) := as.integer(get(var) >= 1)]
  }


  DT <- merge(DT, DT.6m.post.first.adj, by = "LOANID", all.x = TRUE) %>%
    .[, rate.payment.actual.6m.post.first.adj.actual := SCHEDINTAMTCALC.6m.post.first.adj - SCHEDINTAMTCALC_FIRST_PAY_ADJ] %>%
    .[, rate.6m.post_first.adj := CURRENTINTRTCALC.6m.post.first.adj - CURRENTINTRTCALC_FIRST_PAY_ADJ]


  ## -- The last data file -- ##
  DT.perf.last <- read_fst(
    here::here(DT.params.temp$perf.last.file),
    as.data.table = TRUE
  ) %>%
    setnames(names(.), paste0(names(.), ".last")) %>%
    ##set back the name for the loan id
    setnames("LOANID.last", "LOANID") %>%
    ##remove mismatches for liquidated with a loss that don't have a
    ##date associated with the liquidation
    .[!(EVER_LIQUIDATED_WITH_LOSS.last == 0 & MBADELINQUENCYSTATUS.last == "L")]

  ##Make sure that all of the loans that are in DT are in DT.perf.last
  DT <- DT %>%
    .[LOANID %in% DT.perf.last[["LOANID"]]]

  ## -- The performance data after 48 months -- ##
  perf.file <- paste0(
    DT.params.temp$perf.after.first.adj.date.dir,
    "DT_post_adj_perf_48.fst"
  )

  DT.perf <- read_fst(here::here(perf.file), as.data.table = TRUE)
  perf.vars <- names(DT.perf) %>% .[!grepl("^LOANID$", x = .)]

  DT.all <- merge(DT, DT.perf, by = "LOANID", all.x = TRUE) %>%
    merge(DT.perf.last, by = "LOANID", all.x = TRUE)

  for (var in perf.vars)
    DT.all <- DT.all[is.na(get(var)), c(var) := .SD, .SDcols = paste0(var, ".last")]

  ##delete the *.last variables
  .last.vars <- names(DT.all) %>% .[grepl(".last$", x = .)]
  DT.all <- DT.all[, c(.last.vars) := NULL]

  ##udpate the perf var names
  setnames(DT.all, perf.vars, paste0(perf.vars, ".perf"))

  ##Performance variables
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

  ## save the data file right before we run the regressions for use
  ## elsewhere as necessary
  print("Saving data used in the regressions")
  mkdir_p(here::here("work/"))
  save.data.location <- sprintf(
    "work/025-DT_all_for_table_regs_id_%s.fst",
    sprintf("%02.f", table.id)
  )
  write_fst(DT.all, here::here(save.data.location), compress = 100)

  ## -- The Regression formulas -- ##

  ##First pay adjustment date
  rhs.first.pay.adj.2sls <- sprintf(
    rhs.first.pay.adj,
    ##for the endogenous variables and instruments
    sprintf("(%s ~ %s)", DT.params.temp$endog.var, DT.params.temp$iv)
  )

  rhs.first.pay.adj.reduced.form <- sprintf(
    rhs.first.pay.adj,
    ##for NO endogenous variables and NO instruments
    "0"
  ) %>%
    paste0(DT.params.temp$iv, " + ", .)

  rhs.first.pay.adj.ols <- sprintf(
    rhs.first.pay.adj,
    ##for NO endogenous variables and NO instruments
    "0"
  ) %>%
    paste0(DT.params.temp$endog.var, " + ", .)

  ##performance
  rhs.perf.2sls <- sprintf(
    rhs.perf,
    ##for the endogenous variables and instruments
    sprintf("(%s ~ %s)", DT.params.temp$endog.var, DT.params.temp$iv)
  )

  rhs.perf.reduced.form <- sprintf(
    rhs.perf,
    ##for NO endogenous variables and NO instruments
    "0"
  ) %>%
    paste0(DT.params.temp$iv, " + ", .)

  rhs.perf.ols <- sprintf(
    rhs.perf,
    ##for NO endogenous variables and NO instruments
    "0"
  ) %>%
    paste0(DT.params.temp$endog.var, " + ", .)


  ## -- Regression helper functions -- ##

  mkdir_p(here::here("work"))

  f_reg_first_pay_adj_date <- function(lhs, rhs, mod.type) {
    reg.formula <- paste0(lhs, " ~ ", rhs) %>%
      as.formula
    mod <- eval(bquote(felm(.(reg.formula), data = DT.all))) %>%
      reduce_felm_object_size


    save.file.location <- sprintf(
      "work/025-felm_first_adj_id_%s_lhs_%s_%s.rds",
      sprintf("%02.f", DT.params.temp$id), lhs, mod.type
    )
    saveRDS(mod, here::here(save.file.location), compress = FALSE)

    return(invisible())
  }

  f_reg_first_pay_adj_date_2sls <- function(lhs) {
    f_reg_first_pay_adj_date(lhs = lhs, rhs = rhs.first.pay.adj.2sls,
                             mod.type = "2sls")
  }

  f_reg_first_pay_adj_date_reduced_form <- function(lhs) {
    f_reg_first_pay_adj_date(lhs = lhs,
                             rhs = rhs.first.pay.adj.reduced.form,
                             mod.type = "reduced_form"
                             )
  }

  f_reg_first_pay_adj_date_ols <- function(lhs) {
    f_reg_first_pay_adj_date(lhs = lhs,
                             rhs = rhs.first.pay.adj.ols,
                             mod.type = "ols"
                             )
  }

  f_reg_perf <- function(lhs, rhs, mod.type, filter.string = NULL) {
    reg.formula <- paste0(lhs, " ~ ", rhs) %>%
      as.formula
    if (!is.null(filter.string) && !is.na(filter.string)) {
      DT.temp <- DT.all[eval(parse(text = filter.string))]
    } else {
      DT.temp <- DT.all
    }

    mod <- eval(bquote(felm(.(reg.formula), data = DT.temp))) %>%
      reduce_felm_object_size

    ##Add the data filter string as an attribute to the model
    if (!is.null(filter.string) && !is.na(filter.string))
      mod$filter.string <- filter.string

    save.file.location <- sprintf(
      "work/025-felm_perf_id_%s_lhs_%s_%s.rds",
      sprintf("%02.f", DT.params.temp$id), lhs, mod.type
    )
    saveRDS(mod, here::here(save.file.location), compress = FALSE)

    return(invisible())

  }

  f_reg_perf_2sls <- function(lhs, data.filter.string = NULL) {
    f_reg_perf(lhs, rhs.perf.2sls, "2sls", data.filter.string)
  }

  f_reg_perf_reduced_form <- function(lhs, data.filter.string = NULL) {
    f_reg_perf(lhs, rhs.perf.reduced.form, "reduced_form", data.filter.string)
  }

  f_reg_perf_ols <- function(lhs, data.filter.string = NULL) {
    f_reg_perf(lhs, rhs.perf.ols, "ols", data.filter.string)
  }

  ##########################################
  ## -- First Pay Adj Date Regressions -- ##
  ##########################################

  first.adj.lhs.vars <- c(
    "ln.rate.payment.actual.predicted",
    "rate.payment.actual.predicted",
    "EFX_EDTI_1M_post_4M_pre_subprime_edti_4m_pre",
    "dlog.non.1st.mtg.pymt.due.post1.pre4", 
    "rate.6m.post_first.adj",
    "ever.reo.or.loss.6m.post.first.adj",
    "EVER_PAID_OFF.6m.post.first.adj"
  )

  ##test
  ## f_reg_first_pay_adj_date_2sls("ln.rate.payment.actual.predicted")
  ## f_reg_first_pay_adj_date_2sls("dlog.non.1st.mtg.pymt.due.post1.pre4")
  ## f_reg_first_pay_adj_date_reduced_form("dlog.non.1st.mtg.pymt.due.post1.pre4")
  ## f_reg_first_pay_adj_date_ols("dlog.non.1st.mtg.pymt.due.post1.pre4")
    


  lapply(first.adj.lhs.vars, f_reg_first_pay_adj_date_2sls)
  lapply(first.adj.lhs.vars, f_reg_first_pay_adj_date_reduced_form)
  lapply(first.adj.lhs.vars, f_reg_first_pay_adj_date_ols)

  ###################################
  ## -- Performance Regressions -- ##
  ###################################

  DT.perf.lhs.vars <- list(
    data.table("ever.reo.or.loss", NA_character_),
    data.table("ever.paid.off", NA_character_),
    data.table("ever.reo.or.is.delin90.plus", NA_character_),
    data.table("ever.reo.or.is.delin180.plus", NA_character_),
    data.table("is.delin.leq.30.or.ever.paid.off", "ever.reo.or.loss == 0"),
    data.table("is.delin90.plus", "ever.reo.or.loss == 0"),
    data.table("is.delin180.plus", "ever.reo.or.loss == 0"),
    data.table("cum.loss_beg.bal.pre4", "EVER_ANY_MOD_PRE4_FIRSTPAYMTADJDTCALC == 0")
  ) %>%
    rbindlist %>%
  setnames(names(.), c("lhs.var", "filter.string"))

  ##test
  ## f_reg_perf_2sls(DT.perf.lhs.vars$lhs.var[1],
  ##                 DT.perf.lhs.vars$filter.string[1])

  

  reg.perf.2sls <- Map(f_reg_perf_2sls,
                       DT.perf.lhs.vars$lhs.var,
                       DT.perf.lhs.vars$filter.string
                       )

  reg.perf.reduced.form <- Map(f_reg_perf_reduced_form,
                               DT.perf.lhs.vars$lhs.var,
                               DT.perf.lhs.vars$filter.string
                               )

  reg.perf.ols <- Map(f_reg_perf_ols,
                      DT.perf.lhs.vars$lhs.var,
                      DT.perf.lhs.vars$filter.string
                      )

  return(invisible())

}

##test
## f_reg_all(table.id = 1)
## f_reg_all(table.id = 9)
## f_reg_all(table.id = 10)


table.ids <- DT.params$id %>% unique
lapply(table.ids, f_reg_all)





       
