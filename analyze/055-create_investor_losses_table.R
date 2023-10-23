## analyze/055-create_investor_losses_table.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({
  library(CLmisc); library(stargazer); library(starpolishr)
})

star_get_lhs_names <- function(star) {
  ## Assume that they are right below the "Dependent variable" line
  dep.var.row <- which(grepl("Dependent variable", x = star))
  col.numbers.row <- star[c(dep.var.row + 3)]
  if (!grepl("\\(1\\)", col.numbers.row)) 
    stop("Error: `star_get_lhs_names` only works on a default stargazer regression latex table")
  
  lhs.vars <- star[c(dep.var.row + 2)] %>%
    gsub("\\\\\\\\", "", x = .) %>%
    sub("\\[-1.8ex\\]", "", x = .) %>%
    strsplit(x = ., "&") %>%
    unlist(.) %>%
    stringr::str_trim() %>%
    .[2:length(.)]

  if (length(lhs.vars) != (star_ncol(star) - 1))
    stop("Error: In `star_get_lhs_names`, the length of the LHS variables is not equal to `1 - star_ncol(star)`. Are you sure you are passing a default stargazer latex regression table to `star_get_lhs_names`")

  return(lhs.vars)

}

## -- 2SLS -- ##

cum.loss.2sls <- readRDS(
  here::here("work/050-cum_loss_2sls.rds")
)

stage1.iv.coeff <- sapply(cum.loss.2sls, function(m) {
  m$stage1$coefficients["LIBOR_FIRST_MEAS_PAY_ADJ_DIFF", 1] %>%
    sprintf("%.03f", .)
}) %>%
  gsub("-", "$-$", x = .) %>% 
  paste0(collapse = " & ") %>%
  paste0("First Stage Coef on \\\\ LIBOR Diff IV & ", ., " \\\\")

stage1.fstat <- sapply(cum.loss.2sls, function(m) {
  m$stage1$ctval[["LIBOR_FIRST_MEAS_PAY_ADJ_DIFF"]] ^ 2
}) %>%
  sapply(function(x) sprintf("%.02f", x)) %>%
  paste0(collapse = " & ") %>%
  paste0("First Stage F-stat & ", ., " \\\\")

star.2sls <- stargazer(
  cum.loss.2sls,
  keep = "rate.actual.predicted.mod.1.0",
  keep.stat = "N", 
  digits = 4,
  star.char = "", 
  title = "\\textbf{Investor Loss Regression Estimates}",
  label = "tab:inv_loss"
) %>%
  star_insert_row(
    string = c(stage1.iv.coeff, "\\\\[-2ex]", stage1.fstat),
    insert.after = 18
  ) %>%
  ##delete the "Observations" line as it will be
  ##in the OLS panel
  .[!grepl("^Observations", x = .)] %>% 
  star_rhs_names(pattern = "`rate.actual.predicted.mod.1.0\\(fit\\)`",
                 line1 = "Modification",
                 line2 = "Indicator")


star.lhs.vars <- star_get_lhs_names(star.2sls)
star.lhs.line1 <- gsub("cum.loss.over.", "\\\\\\\\$", star.lhs.vars) %>%
  sub("discounted.cum.loss.*", "Losses (\\\\\\\\$)", x = .)

star.2sls <- star.2sls %>%
  star_lhs_names(pattern = star.lhs.vars, 
                 line1 = star.lhs.line1)

## -- Reduced Form -- ##

cum.loss.reduced.form <- readRDS(
  here::here("work/050-cum_loss_reduced_form.rds")
)

star.reduced.form <- stargazer(
  cum.loss.reduced.form, 
  keep = "LIBOR_FIRST_MEAS_PAY_ADJ_DIFF",
  keep.stat = "N",
  digits = 4,
  star.char = ""
) %>%
  star_rhs_names("LIBOR\\\\_FIRST\\\\_MEAS\\\\_PAY\\\\_ADJ\\\\_DIFF",
                 line1 = "LIBOR Diff Between",
                 line2 = "First Meas \\& Adj.") 


## -- OLS -- ##

cum.loss.ols <- readRDS(
  here::here("work/050-cum_loss_ols.rds")
)

star.ols <- stargazer(
  cum.loss.ols,
  keep = "rate.actual.predicted.mod.1.0",
  keep.stat = "n", 
  digits = 4,
  star.char = ""
) %>%
  star_rhs_names(pattern = "rate.actual.predicted.mod.1.0",
                 line1 = "Modification",
                 line2 = "Indicator") 

## -- The Panel -- ##

star.panel <- star_panel(
  star.2sls, star.reduced.form, star.ols,
  panel.names = c("2SLS Estimates", "Reduced Form", "OLS Estimates"),
  same.summary.stats = FALSE, 
  panel.label.fontface = "bold"
)

dep.var.row.panel <- which(grepl("Dependent variable", x = star.panel))
star.panel <- star.panel[-c(dep.var.row.panel, dep.var.row.panel + 1)]

star.panel <- star.panel %>% 
  star_insert_row(
    string = c("& \\multicolumn{8}{c}{ } & \\multicolumn{1}{c}{Given} \\\\",
               "& \\multicolumn{1}{c}{ } & \\multicolumn{7}{c}{Probability that Investor Losses Are Greater Than} & \\multicolumn{1}{c}{Loss $>$ \\$0} \\\\",
               "\\cline{3-9} \\cline{10-10}"
               ),
    insert.after = 9
  )

## Keep only the first two decimal digits for numbers in the hundreds
## or thousands
star.panel <- star.panel %>% 
  ## For the cumulative loss lhs var, keep only 2 decimal digits
  gsub("([0-9]{0,2}[,]{0-1}[0-9]{3}\\.[0-9]{2})[0-9]{2}", "\\1", x = .)

##delete the first observations line
first.obs.line <- which(grepl("Observations", x = star.panel))[1]
star.panel <- star.panel[-c(first.obs.line - 1, first.obs.line)]

##make some other cosmetic adjustments and add in the "Controls" line
last.obs.line <- grepl("Observations", x = star.panel) %>% which %>%
  max

##change the midrule above the observations line to an hline
last.cline <-  last.obs.line - 1
star.panel[last.cline] <- "\\hline \\\\[-2.0ex]"

##Add in the controls line
controls.line <- paste0(
  "Controls & ",
  paste0(rep(" \\checkmark ", star_ncol(star.panel) - 1), collapse = " & "),
  " \\\\"
)

star.panel <- star_insert_row(
  star.panel,
  string = controls.line,
  insert.after = last.obs.line
)

star.panel <- star_sidewaystable(star.panel)



star.panel <- star_notes_tex(
  star.panel,
  note.type = "caption",
  note = "Controls are listed in footnote \\ref{fn:controls}."
)

mkdir_p(here::here("output-tex/"))
star_write_and_compile_pdf(
  star.panel,
  file = here::here("output-tex/055-table5_investor_losses_2sls.tex")
)
