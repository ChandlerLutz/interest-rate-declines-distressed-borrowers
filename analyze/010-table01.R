## analyze/010-table1.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({
  library(CLmisc); library(stargazer); library(starpolishr); library(stringr);
})

DT <- read_fst(
  here::here("data/table01.fst"),
  as.data.table = TRUE)

DT <- copy(DT) %>%
  ##prime versus subprime
  .[!is.na(FICOSCOREORIGINATIONCALC)] %>%
  .[, subprime := as.integer(FICOSCOREORIGINATIONCALC <= 660)] %>%
  ##LIBOR
  .[, index := INDEXNAMESUMMARY] %>%
  .[!is.na(LIBOR_TYPE) | str_trim(INDEXNAMESUMMARY) == "LIBOR" | INDEXCDSUMMARY == "09", index := "LIBOR"] %>%
  .[LIBOR_TYPE == "NULL", LIBOR_TYPE := NA] %>%
  .[, LIBOR_TYPE := gsub("month", "", x = LIBOR_TYPE)] %>%
  .[, LIBOR_TYPE := as.numeric(LIBOR_TYPE)] %>%
  .[, LIBOR_TYPE := as.character(LIBOR_TYPE)] %>%
  .[!is.na(LIBOR_TYPE), LIBOR_TYPE := paste0("LIBOR ", LIBOR_TYPE, "-month")] %>%
  .[is.na(LIBOR_TYPE) & index == "LIBOR", LIBOR_TYPE := "LIBOR Unknown"] %>%
  .[index == "LIBOR", index.type := LIBOR_TYPE] %>%
  ##Treasury
  .[grepl("Treasury", x = INDEXNAMESUMMARY), index := "Treasury"] %>%
  .[index == "Treasury", index.type := stringr::str_trim(INDEXNAMESUMMARY)] %>%
  .[, index.type := gsub("yr", "year", x = index.type)] %>%
  ##Unkown
  .[is.na(INDEXNAMESUMMARY) | grepl("Unknown", x = INDEXNAMESUMMARY), index := "Unknown"] %>%
  .[index == "Unknown", index.type := "Unknown"] %>%
  ##Other
  .[!(index %chin% c("LIBOR", "Treasury", "Unknown")), index := "Other"] %>%
  .[index == "Other", index.type := "Other"] %>%
  ##get the time-frame
  .[, pre.conventional.easing := as.integer(as.Date(FIRSTPAYMTADJDTCALC) <= as.Date("2007-09-18"))] %>%
  .[, conventional.easing := as.integer(as.Date(FIRSTPAYMTADJDTCALC) > as.Date("2007-09-18") &
                                          as.Date(FIRSTPAYMTADJDTCALC) < as.Date("2008-11-19"))] %>%
  .[, qe1 := as.integer(as.Date(FIRSTPAYMTADJDTCALC) >= as.Date("2008-11-19") &
                          as.Date(FIRSTPAYMTADJDTCALC) < as.Date("2010-02-17"))] %>%
  .[, post.qe1 := as.integer(as.Date(FIRSTPAYMTADJDTCALC) >= as.Date("2010-02-17"))]


numeric.vars <- c(
  "pct", "FICO",
  "LTV", "pct.pre.conventional.easing", "pct.conventional.easing",
  "pct.qe1", "pct.post.qe1"
)

DT.subprime <- DT[subprime == 1] %>%
  .[, total.obs := .N] %>%
  .[, .(.N, pct = (.N / total.obs[1]) * 100,
        FICO = mean(FICOSCOREORIGINATIONCALC, na.rm = TRUE),
        LTV = mean(ORIGLTVRATIOCALC, na.rm = TRUE),
        pct.pre.conventional.easing = mean(pre.conventional.easing, na.rm = TRUE) * 100,
        pct.conventional.easing = mean(conventional.easing, na.rm = TRUE) * 100,
        pct.qe1 = mean(qe1, na.rm = TRUE) * 100,
        pct.post.qe1 = mean(post.qe1, na.rm = TRUE) * 100),
    by = .(index, index.type, INTRTADJFREQCALC)] %>%
  .[order(-N)] %>%
  head(5) %>%
  .[, c(numeric.vars) := lapply(.SD, function(x) sprintf("%.02f", x)), .SDcols = numeric.vars] %>%
  .[, N := scales::comma(N)]

##Prime Loans
DT.prime <- DT[subprime == 0] %>%
  .[, total.obs := .N] %>%
  .[, .(.N, pct = (.N / total.obs[1]) * 100,
        FICO = mean(FICOSCOREORIGINATIONCALC, na.rm = TRUE),
        LTV = mean(ORIGLTVRATIOCALC, na.rm = TRUE),
        pct.pre.conventional.easing = mean(pre.conventional.easing, na.rm = TRUE) * 100,
        pct.conventional.easing = mean(conventional.easing, na.rm = TRUE) * 100,
        pct.qe1 = mean(qe1, na.rm = TRUE) * 100,
        pct.post.qe1 = mean(post.qe1, na.rm = TRUE) * 100),
    by = .(index, index.type, INTRTADJFREQCALC)] %>%
  .[order(-N)] %>%
  head(5) %>%
  .[, c(numeric.vars) := lapply(.SD, function(x) sprintf("%.02f", x)), .SDcols = numeric.vars] %>%
  .[, N := scales::comma(N)]


##helper function to make the table
f_table <- function(DT.for.table) {

  ##Get the colum names
  column.names <- names(DT.for.table)

  ##The stargazer table
  star.out <- stargazer(
    as.matrix(DT.for.table),
    title = "\\textbf{Counts and Summary Statistics for Subprime and non-Subprime ARM Loans by Interest Rate Index}",
    label = "tab:arm_summ_stats",
    type = "latex") %>%
    star_add_column_numbers(insert.after = 10, skip.col.1 = FALSE,
                            add.space = TRUE, multicol = "c") %>%
    star_lhs_names(
      pattern = column.names,
      line1 = c("Interest", "Interest", "Payment", "Num.", "\\\\% of", "Mean", "Mean",
                "Pre-Fed", "Fed", "", ""),
      line2 = c("Rate", "Rate", "Adj. Freq.", "of", "Loans By ", "Orig", "Orig",
                "Funds", "Funds", "", "Post-"),
      line3 = c("Index",  "Type", "(Months)", "Loans", "Panel", "FICO", "LTV",
                "Easing", "Easing", "QE1", "QE1"),
      multicol = "c"
    ) %>%
    ##update the column pattern
    sub("ccccccccccc", "llccccccccc", x = .) %>%
    star_sidewaystable


  return(star.out)

}

##the individual panels
star.subprime <- f_table(DT.subprime)
star.prime <- f_table(DT.prime)


##summary statistics by panel
star.panel <- star_panel(
  star.subprime, star.prime,
  panel.names = c("Top 5 Subprime ARM Categories (FICO $\\leq$ 660)",
                  "Top 5 Non-Subprime ARM Categories (Fico $>$ 660)"),
  reg = FALSE,
  panel.label.fontface = "bold"
)

##Add in some more in information
star.string <- c(" & & & & & & & \\multicolumn{4}{c}{First Payment Adj. Date by Monetary} \\\\",
                 " & & & & & & & \\multicolumn{4}{c}{Policy Episode Within Each Row (\\%)} \\\\",
                 "\\\\[-2ex]",
                 " \\cline{8-11}",
                 "\\\\[-2ex]")
star.panel <- star_insert_row(star.panel, string = star.string, insert.after = 9)

##inser the notes
star.panel <- star_notes_tex(
  star.panel, note.type = "caption",
  note = "Adjustable Rate Mortgage (ARM) Counts and Summary Statistics by ARM Index Category by subprime (FICO $\\leq$ 660) and non-subprime (FICO $>$ 660) categories. Only loans outstanding in September 2009, at the start of the Fed's QE Easing Cycle, are included. Column (1) shows the broad interest rate index category, while column (2) prints a more specific interest rate type. Column (3) shows how often the interest rate and interest rate payments adjust for the ARM loans. Column (4) counts the number of loans by each row, while column (5) shows the percent of loans for a given ARM interest rate index type within each panel. Columns (6) and (7) tabulate mean origination FICO credits scores and loan-to-value (LTV) ratios. Finally, columns (8) - (11), within each row, show the percent of loans whose first payment adjustment date falls within a given monetary episode (Pre-Conventional Monetary Easing Episode (pre-2007M09; column 8), Conventional Monetary Easing Episode (2007M09-2008M11; column 9), QE1 (2008M11-2010M02; column 10), post-QE1 (post-2010M02; column 11).")


mkdir_p(here::here("output-tex/"))

star_write_and_compile_pdf(
  star.panel,
  file = here::here("output-tex/010-table01.tex")
)



