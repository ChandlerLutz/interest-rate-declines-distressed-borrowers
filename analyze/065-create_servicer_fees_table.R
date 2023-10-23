## analyze/065-create_servicer_fees_table.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({library(CLmisc); library(stargazer); library(starpolishr)})

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



mod.stage2.list <- readRDS(
  here::here("work/060-stage2_servicer_fee_felm_regs.rds")
)

stage1.iv.coeff <- sapply(mod.stage2.list, function(m) {
  m$stage1$coefficients["LIBOR_FIRST_MEAS_PAY_ADJ_DIFF", 1] %>%
    sprintf("%.03f", .)
}) %>%
  gsub("-", "$-$", x = .) %>% 
  paste0(collapse = " & ") %>%
  paste0("First Stage Coef on \\\\ LIBOR Diff IV & ", ., " \\\\")

stage1.fstat <- sapply(mod.stage2.list, function(m) {
  m$stage1$ctval[["LIBOR_FIRST_MEAS_PAY_ADJ_DIFF"]] ^ 2
}) %>%
  sapply(function(x) sprintf("%.02f", x)) %>%
  paste0(collapse = " & ") %>%
  paste0("First Stage F-stat & ", ., " \\\\")

star.2sls <- stargazer(
  mod.stage2.list,
  keep = "rate.actual.predicted.mod.1.0",
  keep.stat = "N", 
  digits = 4,
  star.char = "", 
  title = "\\textbf{Post-First Adjustment Discounted Servicer Fee Regression Estimates}",
  label = "tab:servicer_fees"
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
star.lhs.line1 <- gsub("servicer.fees.over.", "$>$\\\\\\\\$", star.lhs.vars) %>%
  sub("servicer.fees", "Servicer Fees (\\\\\\\\$)", x = .)

star.2sls <- star.2sls %>%
  star_lhs_names(pattern = star.lhs.vars, 
                 line1 = star.lhs.line1)

## Reduced form

mod.reduced.form.list <- readRDS(
  here::here("work/060-reduced_form_servicer_fee_felm_regs.rds")
)

star.reduced.form <- stargazer(
  mod.reduced.form.list,
  keep = "LIBOR_FIRST_MEAS_PAY_ADJ_DIFF",
  keep.stat = "N",
  digits = 4,
  star.char = ""
) %>%
  star_rhs_names("LIBOR\\\\_FIRST\\\\_MEAS\\\\_PAY\\\\_ADJ\\\\_DIFF",
                 line1 = "LIBOR Diff Between",
                 line2 = "First Meas \\& Adj.")


## OLS

mod.ols.list <- readRDS(
  here::here("work/060-ols_servicer_fee_felm_regs.rds")
)

star.ols <- stargazer(
  mod.ols.list,
  keep = "rate.actual.predicted.mod.1.0",
  keep.stat = "n", 
  digits = 4,
  star.char = ""
) %>%
  star_rhs_names(pattern = "rate.actual.predicted.mod.1.0",
                 line1 = "Modification",
                 line2 = "Indicator")


star.panel <- star_panel(
  star.2sls, star.reduced.form, star.ols,
  panel.names = c("2SLS Estimates", "Reduced Form", "OLS Estimates"),
  same.summary.stats = FALSE, 
  panel.label.fontface = "bold"
)

dep.var.row.panel <- which(grepl("Dependent variable", x = star.panel))
star.panel <- star.panel[-c(dep.var.row.panel, dep.var.row.panel + 1)]

star.panel <- star.panel %>% 
  ## ## For the cumulative loss lhs var, remove the decimal 
  ## sub("([0-9]{1,2},[0-9]{3})\\.[0-9]{1,4}", "\\1", x = .) %>%
  star_insert_row(
    string = c(
      ##"& \\multicolumn{7}{c}{ } & \\multicolumn{1}{c}{Given} \\\\",
      "& \\multicolumn{2}{c}{Prob that Servicer Fees} & \\multicolumn{1}{c}{ } \\\\",
      "\\cline{2-3} "
    ),
    insert.after = 9
  )


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

star.panel <- star_notes_tex(
  star.panel,
  note.type = "caption",
  note = "Controls are listed in footnote \\ref{fn:controls}."
)


mkdir_p(here::here("output-tex"))
star_write_and_compile_pdf(
  star.panel,
  file = here::here("output-tex/065-servicer_fees.tex")
)


