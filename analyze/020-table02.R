## analyze/020-table02.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({
  library(CLmisc); library(readxl); library(stargazer); library(starpolishr);
})

DT.zip <- read_fst(
  here::here("data/DT_zip.fst"),
  as.data.table = TRUE
)

DT <- read_fst(
  here::here("data/DT_for_2sls_libor_diff.fst"),
  as.data.table = TRUE
) %>%
  .[FICOSCOREORIGINATIONCALC <= 660] %>%
  merge(DT.zip, by.x = c("PROPERTYZIPCDCALC", "zip3"), by.y = c("zip", "zip3"),
        all.x = TRUE)

DT.lkp <- read_xlsx("./020-balance_table_lkp.xlsx") %>% setDT

f_standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}


##Standardize LHS vars
DT <- DT[, c(DT.lkp$lhs.var) := lapply(.SD, f_standardize), .SDcols = DT.lkp$lhs.var]

##DT.out <- DT.lkp[, .(lhs.var)]

## felm(NOARMLOOKBKDAYS ~ LIBOR_FIRST_MEAS_PAY_ADJ_DIFF | first.payment.adj.month.char | 0 | zip3,
##      DT) %>% summary

f_round <- function(x) round(x, 3)
f_est <- function(lhs.var.temp) {

  DT.lkp.temp <- DT.lkp[lhs.var == c(lhs.var.temp)]
  f <- sprintf("%s ~ LIBOR_FIRST_MEAS_PAY_ADJ_DIFF +
                     NOARMLOOKBKDAYS.demeaned | first.payment.adj.month.char | 0 | zip3",
               lhs.var.temp) %>% as.formula
  DT.out <- felm(f, DT) %>%
    felm_broom_tidy %>%
    .[term == "LIBOR_FIRST_MEAS_PAY_ADJ_DIFF"] %>%
    .[, term := NULL] %>%
    .[, `LHS Var` := DT.lkp.temp$lhs.label] %>%
    setcolorder(c("LHS Var")) %>%
    mutate_by_ref(cols = is.numeric, f = f_round) %>%
    ## No asterisks in the table 
    ## .[as.numeric(p.value) < 0.01, estimate := paste0(estimate, "*")] %>%
    setnames(c("estimate", "std.error", "statistic", "p.value"),
             c("Estimate", "Std. Error", "t-statistic", "p-value"))
}



DT.balance.table <- lapply(DT.lkp$lhs.var, f_est) %>%
  rbindlist(use.names = TRUE)

lhs.names.est <- names(DT.balance.table) %>% .[. != "LHS Var"]
star.balance.tests <- stargazer(as.matrix(DT.balance.table),
                                title = "\\textbf{Coefficient Estimates from Regressions of Pre-Treatment Borrower Outcomes on the Change in 6m LIBOR between First Measurement and First Payment Adjustment}", label = "tab:balance_tests") %>%
  star_add_column_numbers(insert.after = 10, skip.col.1 = TRUE, multicol = "c") %>%
  star_asterisks %>%
  star_lhs_names(pattern = lhs.names.est, line1 = lhs.names.est,
                 multicol = "c") %>%
  ##sub("ccccc", "lcccc", x = .) %>%
  sub("t-statistic", "$t$-statistic", x = .) %>%
  star_notes_tex(note.type = "caption", note = "Left-hand-side variables are standardized to have zero mean and unit variance. The reported coefficient estimates correspond to a regression of the left-hand-side variable on the change in 6-month LIBOR between first measurement and first payment adjustment, where each regression is estimated separately by row. Controls include the number of days between first measurement and first payment adjustment as well as first payment adjustment month fixed effects. Regressions are based on 350,946 loan-level observations. Robust standard errors are clustered at the three-digit zip code level."
                 )

## siunitx columns
star.ncols <- star_ncol(star.balance.tests)
star.balance.tests <- star.balance.tests %>%
  gsub("ccccc", paste0("l",star_si_col(table.format=1.3,rep.times=star.ncols)),
       x = .)


star.balance.tests <- star.balance.tests %>%
  star_sidewaystable

mkdir_p(here::here("output-tex"))
star_write_and_compile_pdf(
  star.balance.tests,
  file = here::here("output-tex/020-table02.tex")
)


