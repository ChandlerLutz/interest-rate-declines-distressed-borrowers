## core/f_clean_orig_data.R

f_clean_bbx_orig_data <- function(DT) {

  f_ntile <- function(x) {
    x <- CLmisc::fntile(x, n = 20) %>%
      as.character(.)
    ##set missing values to zero
    x[is.na(x)] <- "0"
    return(x)
  }


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
    "EFX_FICO_PRE_FIRSTPAYMTADJDTCALC", "ltv.pre4", "EFX_EDTI_PRE_FIRSTPAYMTADJDTCALC",
    "ln.EFX_PIM_PRE_FIRSTPAYMTADJDTCALC", "efx_heloc_avail_mortgage_payment",
    "efx_credit_card_avail_mortgage_payment", "bartik.recession.zip",
    "bartik.pre.hp.growth.zip",
    "hh.income.zip", "d.log.hp.pre.zip"
  )

  DT <- DT[, paste0(ntile.controls, ".ntile") := lapply(.SD, f_ntile),
           .SDcols = ntile.controls]
  
  ##Merge the efx EDTI data post first adj
  DT.efx.edti <- read_fst(
    here::here("data/efx_edti_1m_post_4m_pre.fst"),
    as.data.table = TRUE
  )

  DT <- merge(DT, DT.efx.edti, by = "LOANID_OLDLOANID", all.x = TRUE)

  return(DT)

}



