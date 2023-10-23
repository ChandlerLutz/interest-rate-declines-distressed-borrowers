## analyze/145-libor_mod_stage1_falsification.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({library(CLmisc); library(cowplot); })


##For data a cleaning script for the origination data
source(here::here("core/f_clean_bbx_orig_data.R"))

f_quarter_label <- function(x) paste0(year(x), "Q", quarter(x))


DT <- read_fst(
  here::here("data/DT_falsificaiton.fst"),
  as.data.table = TRUE
) %>%
  .[FICOSCOREORIGINATIONCALC <= 660] %>%
  .[, efx_heloc_avail_mortgage_size_pre12 := EFX_HELOC_DOLLAR_AVAIL_PRE12 / BEGBALCALC_PRE12_FIRSTPAYMTADJDTCALC] %>%
  .[, efx_credit_card_mortgage_size_pre12 := EFX_CREDIT_CARD_DOLLAR_AVAIL_PRE12 / BEGBALCALC_PRE12_FIRSTPAYMTADJDTCALC] %>%
  .[, efx_log_pim := log(EFX_PIM)] %>%
  .[!is.infinite(efx_heloc_avail_mortgage_size_pre12)] %>%
  .[!is.infinite(efx_credit_card_mortgage_size_pre12)] %>%
  .[!is.infinite(efx_log_pim)] 

DT <- DT %>%
  .[, ln.ORIGINALBALCALC := log(ORIGINALBALCALC)]


##Zip code data
DT.zip <- read_fst(
  here::here("data/DT_zip.fst"),
  as.data.table = TRUE
) %>%
  .[is.na(d.log.hp.pre.zip), d.log.hp.pre.zip := d.log.hp.pre.zip3] %>%
  .[, zip3 := NULL]

DT <- DT %>%
  merge(DT.zip, by.x = "PROPERTYZIPCDCALC", by.y = "zip", all.x = TRUE) %>%
  .[, purpose.type := fcase(PURPOSETYPE == "CSH", "CSH",
                            PURPOSETYPE == "PUR", "PUR",
                            PURPOSETYPE == "REF", "REF",
                            default = "Other")] %>%
  .[, prop.type := fifelse(PROPTYPE == "SF", "SF", "Other")] %>%
  .[, doc.type.summary := fifelse(DOCTYPESUMMARY == "FU", "full", "other")] %>%
  .[, loan.type.yes.only := gsub("U", "N", x = loan.type)] %>%
  .[, N.loan.type.yes.only := .N, by = loan.type.yes.only] %>%
  .[N.loan.type.yes.only < 500, loan.type.yes.only := "Other"] %>%
  .[, N.loan.type.yes.only := NULL] %>%
  .[, bartik.pre.hp.growth.zip := bartik.recession.zip * d.log.hp.pre.zip]

ntile.controls <- c(
  "hh.income.zip", "d.log.hp.pre.zip"
)

f_ntile <- function(x) {
  x <- CLmisc::fntile(x, n = 20) %>%
    as.character(.)
  ##set missing values to zero
  x[is.na(x)] <- "0"
  return(x)
}


DT <- DT[, paste0(ntile.controls, ".ntile") := lapply(.SD, f_ntile),
         .SDcols = ntile.controls]



DT <- DT %>%
  .[MBADELINQUENCYSTATUS_PRE12_FIRSTPAYMTADJDTCALC != "O"] %>%
  .[, is.current.or.delin30.12.mon.pre.adj := as.integer(MBADELINQUENCYSTATUS_PRE12_FIRSTPAYMTADJDTCALC %chin% c("C", "P", "3"))] %>%
  .[, is.current.12.mon.pre.adj := as.integer(MBADELINQUENCYSTATUS_PRE12_FIRSTPAYMTADJDTCALC %chin% c("C", "P"))] %>%
  .[, is.current.6.mon.pre.adj := as.integer(MBADELINQUENCYSTATUS_PRE6_FIRSTPAYMTADJDTCALC %chin% c("C", "P"))] %>%
  .[, is.current.4.mon.pre.adj := as.integer(MBADELINQUENCYSTATUS_PRE6_FIRSTPAYMTADJDTCALC %chin% c("C", "P"))]
  ## .[, day_delin60.or.more.12.mon.pre.adj := as.integer(DAYS_DELIN_12_MON_PRE_ADJ >= 60)] %>%
  ##.[, NOARMLOOKBKDAYS.2 := (NOARMLOOKBKDAYS) ^ 2] %>%
  ## .[, ORIGINTRTCALC.month := ORIGINTRTCALC - median(ORIGINTRTCALC), by = orig.month] %>%
  ## .[, mba.default.12.mon.pre.adj := as.integer(MBADELINQUENCYSTATUS_12_MON_PRE_ADJ %chin% c("6", "9", "B", "F", "R", "L"))]

f_reg_pay_adj_quarter <- function(temp.first.pay.adj.qrtr) {

  DT.temp <- DT[first.payment.adj.quarter.char == c(temp.first.pay.adj.qrtr) & FICOSCOREORIGINATIONCALC <= 660]

  ##helper function
  f_reg <- function(lhs.var) {
    reg.formula <- sprintf("%s ~ LIBOR_FIRST_MEAS_PAY_ADJ_DIFF + NOARMLOOKBKDAYS + EVER_ANY_MOD_PRE4_FIRSTPAYMTADJDTCALC + FICOSCOREORIGINATIONCALC + COMBINEDLIENLTVCALC + BEGBALCALC_PRE4_FIRSTPAYMTADJDTCALC + ORIGINTRTCALC | zip3 + hh.income.zip.ntile + d.log.hp.pre.zip.ntile + first.payment.adj.month.char + orig.quarter + OCCTYPE +  purpose.type + prop.type + doc.type.summary + loan.type.yes.only | 0 | zip3",
                           lhs.var) %>%
      as.formula

    DT.reg.output <- eval(bquote(felm(.(reg.formula), data = DT.temp, exactDOF = FALSE))) %>%
      felm_broom_tidy %>%
      .[grepl("LIBOR_FIRST_MEAS_PAY_ADJ_DIFF", term)] %>%
      .[, lhs.var := c(lhs.var)] %>%
      .[, first.payment.adj.quarter := as.Date(temp.first.pay.adj.qrtr)] %>%
      setcolorder(c("lhs.var", "first.payment.adj.quarter", "term"))

    return(DT.reg.output)

  }

  ##For reduced form regressions and first stage
  lhs.vars <- c(

    "is.current.12.mon.pre.adj",
    "EVER_ANY_MOD_PRE12_FIRSTPAYMTADJDTCALC",
    "EVER_DELIN60_PRE12_FIRSTPAYMTADJDTCALC",
    "EVER_DELIN90_PRE12_FIRSTPAYMTADJDTCALC",

    "EFX_FICO_PRE12",
    "EFX_EDTI_PRE12",
    "efx_heloc_avail_mortgage_size_pre12",
    "efx_credit_card_mortgage_size_pre12"

  )

  DT.temp <- DT.temp %>%
    .[, c(lhs.vars) := lapply(.SD, function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)),
      .SDcols = lhs.vars]

  DT.reg.first.pay.adj.quarter <- lapply(lhs.vars, f_reg) %>%
    rbindlist

  return(DT.reg.first.pay.adj.quarter)


}

DT.reg.output <- lapply(unique(DT$first.payment.adj.quarter.char),
                        f_reg_pay_adj_quarter) %>%
  rbindlist(use.names = TRUE) %>%
  setkey(lhs.var, first.payment.adj.quarter, term) %>%
  .[, year.quarter := paste0(year(first.payment.adj.quarter), "Q", quarter(first.payment.adj.quarter))]

##The falsification variables

DT.false.vars <- list(
  ## Mortgage vars
  c("is.current.12.mon.pre.adj", "Is Current or Paid Off"),
  c("EVER_ANY_MOD_PRE12_FIRSTPAYMTADJDTCALC", "Ever Modified"),
  c("EVER_DELIN60_PRE12_FIRSTPAYMTADJDTCALC", "Ever 60 Days Delin"),
  c("EVER_DELIN90_PRE12_FIRSTPAYMTADJDTCALC", "Ever 90 Days Delin"),
  ## Credit vars
  c("EFX_FICO_PRE12", "FICO Credit Score"),
  c("EFX_EDTI_PRE12", "Estimated DTI"),
  c("efx_heloc_avail_mortgage_size_pre12", "Available HELOC Funds / Mtg Balance"),
  c("efx_credit_card_mortgage_size_pre12", "Available Credit Card Funds / Mtg Balance")
)

DT.false.vars<- do.call("rbind", DT.false.vars) %>%
  as.data.frame(., stringsAsFactors = FALSE)
DT.false.vars[] <- lapply(DT.false.vars, function(x) unlist(x))
DT.false.vars<- setNames(DT.false.vars, c("lhs.var", "lhs.label"))
DT.false.vars  <- setDT(DT.false.vars)

DT.reg.output <- merge(DT.reg.output, DT.false.vars, by = "lhs.var", all.x = TRUE)


f_plot <- function(DT.temp) {
  p <- ggplot(DT.temp, aes(x = year.quarter)) +
    geom_hline(yintercept = 0, linetype = "dotted", size = 1.1) +
    geom_linerange(aes(ymin = estimate - 2.5 * std.error,
                       ymax = estimate + 2.5 * std.error, color = lhs.label),
                   size = 1.01,
                   position = position_dodge(width = 0.3)) +
    theme_cowplot_cl() +
    theme(
      legend.background = element_blank(),
      ## axis.title.x = element_blank(),
      axis.title.y = element_blank(),
    )

  return(p)
}


DT.reg.mtg.vars <- DT.reg.output[!grepl("^EFX|^efx", lhs.var)] %>%
  .[, lhs.label := factor(lhs.label,
                          levels = c("Ever Modified", "Ever 60 Days Delin",
                                     "Ever 90 Days Delin", "Is Current or Paid Off"))]

p.falsification.mtg.vars <- f_plot(DT.reg.mtg.vars) +
  labs(title = "Mortgage Perf Falsificaiton Tests \u2013 Standard Error Bands",
       subtitle = "Key RHS Var: LIBOR Change Between First Measurement & First Adj.\nEach LHS Var (Indicator) is Measured 1 Year Prior to First Adjustment Date") +
  guides(color = guide_legend(title = "LHS Variable:")) +
  theme(legend.position = c(0.5, 0.2))


DT.reg.credit.vars <- DT.reg.output[grepl("^EFX|^efx", lhs.var)] %>%
  .[, lhs.label := factor(lhs.label,
                          levels = c("Estimated DTI", "FICO Credit Score",
                                     "Available Credit Card Funds / Mtg Balance",
                                     "Available HELOC Funds / Mtg Balance"))]

p.falsification.credit.vars <- f_plot(DT.reg.credit.vars) +
  labs(title = "Credit Perf Falsificaiton Tests \u2013 Standard Error Bands",
       subtitle = "Key RHS Var: LIBOR Change Between First Measurement & First Adj.\nEach LHS Var (Standardized) is Measured 1 Year Prior to First Adjustment Date") +
  guides(color = guide_legend(title = "LHS Variable:")) +
  theme(legend.position = c(0.35, 0.2))





p.all <- list(
  p.falsification.mtg.vars = p.falsification.mtg.vars,
  p.falsification.credit.vars = p.falsification.credit.vars
)

mkdir_p("work")

saveRDS(p.all, here::here("work/145_p_stage1_mtg_credit_falsificaiton.rds"))

