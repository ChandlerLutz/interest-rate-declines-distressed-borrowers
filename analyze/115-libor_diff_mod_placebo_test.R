## analyze/115-libor_diff_mod_placebo_test.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({library(CLmisc); })

DT.libor.diff <- read_fst(
  here::here(
    "data/DT_for_2sls_libor_diff.fst"
  ),
  columns = c("LOANID", "LIBOR_FIRST_MEAS_PAY_ADJ_DIFF"),
  as.data.table = TRUE
)

DT.orig <- read_fst(here::here("data/bbx_orig_data.fst"),
                    as.data.table = TRUE) %>%
  .[, zip3 := substr(PROPERTYZIPCDCALC, 1, 3)] %>%
  .[, FIRSTPAYMTADJDTCALC := as.Date(FIRSTPAYMTADJDTCALC)] %>%
  .[, first.pay.adj.month := floor_date(FIRSTPAYMTADJDTCALC, unit = "month")] %>%
  .[, first.pay.adj.month.char := as.character(first.pay.adj.month)] %>%
  .[, first.pay.adj.day := day(FIRSTPAYMTADJDTCALC)] %>% 
  .[, first.pay.adj.day.char := sprintf("%02.f", day(FIRSTPAYMTADJDTCALC))] %>%
  merge(DT.libor.diff, by = "LOANID", all.x = TRUE)

DT.perf <- read_fst(here::here("data/DT_bbx_perf_data.fst"),
                    as.data.table = TRUE) %>%
  .[CURRENTINTRTCALC > 0] %>% 
  setkey(LOANID, ACTIVITYDT) %>% 
  .[order(LOANID, ACTIVITYDT)] %>%
  merge(DT.orig[, .(LOANID, FIRSTPAYMTADJDTCALC)],
        by = "LOANID") %>%
  .[!is.na(FIRSTPAYMTADJDTCALC)] %>% 
  .[ACTIVITYDT <= (FIRSTPAYMTADJDTCALC %m-% months(4))] %>%
  .[, FIRSTPAYMTADJDTCALC := NULL] %>% 
  .[order(LOANID, ACTIVITYDT)] %>% 
  .[, d.int.rate := CURRENTINTRTCALC - shift(CURRENTINTRTCALC, 1), by = LOANID] %>%
  .[, int.rate.algo.mod := as.integer(d.int.rate <= (-1))]

DT.libor6m <- read_fst(
  here::here("data/DT_libor_ff6.fst"),
  as.data.table = TRUE
) %>%
  select_by_ref(c("index", "libor6m")) %>%
  .[!is.na(libor6m)] %>% 
  setkey(index)

DT <- merge(DT.perf, DT.orig)


f_get_libor_diff_mod_prob_est <- function(perf.date) {

  print(perf.date)

  stopifnot(is.Date(perf.date))

  DT.perf.tmp <- DT.perf[ACTIVITYDT == c(perf.date)]

  perf.date.plus.4 <- perf.date %m+% months(4)
  DT.orig.tmp <- DT.orig %>%
    .[FIRSTPAYMTADJDTCALC >= c(perf.date.plus.4)] %>% 
    .[LOANID %chin% c(unique(DT.perf.tmp[["LOANID"]]))]

  DT <- merge(DT.orig.tmp, DT.perf.tmp, by = "LOANID") %>%
    .[, first.pay.adj.date.perf := ACTIVITYDT + first.pay.adj.day - 1] %>%
    .[, first.meas.date.perf := first.pay.adj.date.perf - NOARMLOOKBKDAYS] %>%
    ##Get the closest libor value, looking forward
    setkey(first.meas.date.perf) %>%
    DT.libor6m[., roll = TRUE] %>%
    setnames("libor6m", "first.meas.date.perf.libor6m") %>%
    .[, index := NULL] %>%
    setkey(first.pay.adj.date.perf) %>%
    DT.libor6m[., roll = TRUE] %>%
    setnames("libor6m", "first.pay.adj.date.perf.libor6m") %>%
    .[, index := NULL] %>%
    .[, libor.diff := first.pay.adj.date.perf.libor6m - first.meas.date.perf.libor6m] %>%
    .[, mod.ind := fifelse(MODIFICATIONINDCALC %chin% c("P", "L"), 1, 0)]

  f_not_na <- function(DT, cols) {
    call <- sprintf("!is.na(%s)", cols) %>%
      paste0(., collapse = " & ")
    DT <- DT[eval(parse(text = call))]
  }
    
  

  print(nrow(DT))

  DT.est.int.rate.algo.falsification <- feols(
    int.rate.algo.mod ~ NOARMLOOKBKDAYS + FICOSCOREORIGINATIONCALC +
      ##ORIGLTVRATIOCALC +
      COMBINEDLIENLTVCALC + 
      MARGINCALC + ORIGINALBALCALC + INITIALINTRTCALC + LIBOR_FIRST_MEAS_PAY_ADJ_DIFF | first.pay.adj.month.char,
    DT, cluster = ~ zip3
  ) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[grepl("LIBOR_FIRST_MEAS_PAY_ADJ_DIFF", term)] %>%
    .[, mod.type := "int.rate.algo"] %>% 
    .[, perf.date := c(perf.date)] %>%
    .[, test.type := "falsification"]
  

  DT.est.int.rate.algo.placebo <- feols(
    int.rate.algo.mod ~ NOARMLOOKBKDAYS + FICOSCOREORIGINATIONCALC +
      ##ORIGLTVRATIOCALC +
      COMBINEDLIENLTVCALC + 
      MARGINCALC + ORIGINALBALCALC + INITIALINTRTCALC + libor.diff | first.pay.adj.month.char,
    DT, cluster = ~ zip3
  ) %>%
    broom::tidy(.) %>%
    setDT(.) %>%
    .[grepl("libor.diff", term)] %>%
    .[, mod.type := "int.rate.algo"] %>% 
    .[, perf.date := c(perf.date)] %>%
    .[, test.type := "placebo"]

  ## DT.est.bbx.mod.placebo <- feols(
  ##   mod.ind ~ NOARMLOOKBKDAYS + FICOSCOREORIGINATIONCALC + ORIGLTVRATIOCALC +
  ##     MARGINCALC + ORIGINALBALCALC + INITIALINTRTCALC + libor.diff | first.pay.adj.month.char,
  ##   DT, cluster = ~ zip3
  ## ) %>%
  ##   broom::tidy(.) %>%
  ##   setDT(.) %>%
  ##   .[grepl("libor.diff", term)] %>%
  ##   .[, mod.type := "bbx.mod"] %>% 
  ##   .[, perf.date := c(perf.date)] %>%
  ##   .[, test.type := "placebo"]

  DT.out <- rbind(DT.est.int.rate.algo.placebo, DT.est.int.rate.algo.falsification)

  return(DT.out)

}

perf.dates <- seq.Date(from = as.Date("2007-02-01"), to = as.Date("2009-05-01"),
                       by = "1 month")
DT.est.all <- lapply(perf.dates, f_get_libor_diff_mod_prob_est) %>%
  rbindlist

DT.est.all <- DT.est.all %>%
  setcolorder(c("mod.type", "perf.date")) %>%
  .[order(mod.type, perf.date)]

mkdir_p(here::here("work/"))
write_fst(
  DT.est.all,
  here::here("work/115-DT_est_libor_diff_mod_placebo_test.fst"),
  compress = 100
)
