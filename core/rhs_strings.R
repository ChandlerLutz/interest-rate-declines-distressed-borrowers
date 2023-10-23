## core/rhs_felm_strings.R

## RHS strings to be used used across the project

rhs.numeric.vars <- c(
  "NOARMLOOKBKDAYS", "FICOSCOREORIGINATIONCALC", "COMBINEDLIENLTVCALC",
  "MARGINCALC", "ORIGINTRTCALC", "BEGBALCALC_PRE4_FIRSTPAYMTADJDTCALC",
  "EVER_DELIN60_PRE4_FIRSTPAYMTADJDTCALC", "EVER_ANY_MOD_PRE4_FIRSTPAYMTADJDTCALC"
)

rhs.fe.vars <- c(
  "first.payment.adj.month.char",
  "zip3", "orig.quarter",  "OCCTYPE", "purpose.type", "prop.type", "doc.type.summary",
  "loan.type.yes.only", "MBADELINQUENCYSTATUS_PRE4_FIRSTPAYMTADJDTCALC",
  "ltv.pre4.ntile",
  "EFX_FICO_PRE_FIRSTPAYMTADJDTCALC.ntile", "EFX_EDTI_PRE_FIRSTPAYMTADJDTCALC.ntile",
  "ln.EFX_PIM_PRE_FIRSTPAYMTADJDTCALC.ntile",
  "efx_heloc_avail_mortgage_payment.ntile",
  "efx_credit_card_avail_mortgage_payment.ntile",
  "hh.income.zip.ntile", "d.log.hp.pre.zip.ntile", "bartik.recession.zip.ntile"
)

##The RHS for regressions evaluated at the first payment adjustment date.
##Note that the endogenous variable needs to be filled in by sprintf

rhs.first.pay.adj <- paste(paste0(rhs.numeric.vars, collapse = " + "),
                           paste0(rhs.fe.vars, collapse = " + "),
                           sep = " | ") %>%
  paste0(., " | %s | zip3")

rhs.perf <- rhs.first.pay.adj

rhs.perf.feols <- paste(paste0(rhs.numeric.vars, collapse = " + "),
                        paste0(rhs.fe.vars, collapse = " + "),
                        sep = " | ") %>%
  paste0(., " | %s ")


##The RHS for regressions that use performance data. Note that this also includes
##The bartik ntile `bartik.recession.zip.ntile` and the ntile of the bartik interacted with pre-treatment hp growth `bartik.pre.hp.growth.zip.ntile`
rhs.perf.bartik <- "NOARMLOOKBKDAYS + EVER_ANY_MOD_PRE4_FIRSTPAYMTADJDTCALC + FICOSCOREORIGINATIONCALC + COMBINEDLIENLTVCALC + MARGINCALC + BEGBALCALC_PRE4_FIRSTPAYMTADJDTCALC + ORIGINTRTCALC + EVER_DELIN60_PRE4_FIRSTPAYMTADJDTCALC | zip3 + hh.income.zip.ntile + d.log.hp.pre.zip.ntile + bartik.recession.zip.ntile + bartik.pre.hp.growth.zip.ntile + first.payment.adj.month.char + orig.quarter + OCCTYPE +  purpose.type + prop.type + doc.type.summary + loan.type.yes.only + MBADELINQUENCYSTATUS_PRE4_FIRSTPAYMTADJDTCALC + EFX_FICO_PRE_FIRSTPAYMTADJDTCALC.ntile + EFX_EDTI_PRE_FIRSTPAYMTADJDTCALC.ntile + ltv.pre4.ntile +  ln.EFX_PIM_PRE_FIRSTPAYMTADJDTCALC.ntile + efx_heloc_avail_mortgage_payment.ntile + efx_credit_card_avail_mortgage_payment.ntile | %s | zip3"

rhs.perf.feols.bartik <- "NOARMLOOKBKDAYS + EVER_ANY_MOD_PRE4_FIRSTPAYMTADJDTCALC + FICOSCOREORIGINATIONCALC + COMBINEDLIENLTVCALC + MARGINCALC + BEGBALCALC_PRE4_FIRSTPAYMTADJDTCALC + ORIGINTRTCALC + EVER_DELIN60_PRE4_FIRSTPAYMTADJDTCALC | zip3 + hh.income.zip.ntile + d.log.hp.pre.zip.ntile + bartik.recession.zip.ntile + bartik.pre.hp.growth.zip.ntile + first.payment.adj.month.char + orig.quarter + OCCTYPE +  purpose.type + prop.type + doc.type.summary + loan.type.yes.only + MBADELINQUENCYSTATUS_PRE4_FIRSTPAYMTADJDTCALC + EFX_FICO_PRE_FIRSTPAYMTADJDTCALC.ntile + EFX_EDTI_PRE_FIRSTPAYMTADJDTCALC.ntile + ltv.pre4.ntile +  ln.EFX_PIM_PRE_FIRSTPAYMTADJDTCALC.ntile + efx_heloc_avail_mortgage_payment.ntile + efx_credit_card_avail_mortgage_payment.ntile | %s "
