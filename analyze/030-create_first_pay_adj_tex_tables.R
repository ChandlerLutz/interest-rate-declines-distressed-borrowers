## analyze/030-create_first_pay_adj_tex_tables.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

suppressPackageStartupMessages({
  library(CLmisc); library(stargazer); library(starpolishr); library(readxl);
})

DT.table.params.lkp <- read_xlsx("./025-run_regs_for_tex_tables_lkp.xlsx",
                       sheet = "tables") %>%
  setDT

DT.lhs.vars.lkp <- read_xlsx("./025-run_regs_for_tex_tables_lkp.xlsx",
                                 sheet = "lhs.vars.first.adj") %>%
  setDT %>%
  .[include == 1] %>%
  .[, include := NULL]

f_create_latex_table <- function(table.id) {

  endog.var <- DT.table.params.lkp$endog.var[table.id]
  endog.var.latex <- gsub("_", "\\\\\\\\_", endog.var)
  iv.var <- DT.table.params.lkp$iv[table.id]
  iv.var.latex <- gsub("_", "\\\\\\\\_", iv.var)
  
  
  
  felm.files <- list.files(here::here("work"),
                           full.names = TRUE) %>%
    .[grepl("025-", x = .)] %>% 
    ##just for this table id
    .[grepl(paste0("id_", sprintf("%02.f", table.id)), x = .)]

  f_get_models <- function(type) {

    if (!(type %chin% c("2sls", "reduced_form", "ols")))
      stop("type must be one of '2sls', 'reduced_form', 'ols'")

    lhs.lkp <- gsub("\\\\", "", x = DT.lhs.vars.lkp$lhs)

    DT.models <- felm.files %>%
      ##get only the files associated with this type
      .[grepl(sprintf(".*_%s.rds", type), x = .)] %>%
      lapply(readRDS) %>%
      data.table(model = .) %>%
      ##get the lhs vars using `all.vars`
      .[, lhs := sapply(model, function(m) all.vars(m$formula)[1])] %>%
      .[lhs %chin% lhs.lkp] %>%
      ##order https://stackoverflow.com/a/34684973
      .[order(match(lhs, lhs.lkp))]

    return(DT.models)
  }

  ## -- 2SLS -- ##

  DT.models.2sls <- f_get_models(type = "2sls")

  stage1.iv.coeff <- sapply(DT.models.2sls$model, function(m) {
    m$stage1$coefficients[iv.var, 1] %>%
      sprintf("%.03f", .)
  }) %>%
    gsub("-", "$-$", x = .) %>% 
    paste0(collapse = " & ") %>%
    paste0("First Stage Coef on \\\\ LIBOR Diff IV & ", ., " \\\\")

  stage1.fstat <- sapply(DT.models.2sls$model, function(m) {
    m$stage1$ctval[[iv.var]] ^ 2
  }) %>%
    sapply(function(x) sprintf("%.02f", x)) %>%
    paste0(collapse = " & ") %>%
    paste0("First Stage F-stat & ", ., " \\\\")

  star.2sls <- stargazer(
    DT.models.2sls$model,
    title = sprintf("\\textbf{%s}",
                    DT.table.params.lkp$table.title.first.adj[table.id]),
    label = DT.table.params.lkp$table.label.first.adj[table.id], 
    type = "latex",
    digits = 4, 
    keep = endog.var,
    keep.stat = "N"
  ) %>%
    star_lhs_names(
      pattern = DT.lhs.vars.lkp$lhs,
      line1 = DT.lhs.vars.lkp$lhs.line1,
      line2 = DT.lhs.vars.lkp$lhs.line2,
      line3 = DT.lhs.vars.lkp$lhs.line3,
      multicol = "c"
    ) %>%
    star_rhs_names(
      pattern = sprintf("`%s\\(fit\\)`", endog.var.latex), 
      line1 = "Modification",
      line2 = "Indicator"
    ) %>%
    star_insert_row(
      string = "\\\\[-1.8ex]  & \\multicolumn{4}{c}{At First Adj.} & \\multicolumn{2}{c}{6-months After First Adj.} \\\\ \\cline{2-5} \\cline{6-7}",
      insert.after = 11
    ) %>%
    star_insert_row(
      string = c(stage1.iv.coeff, "\\\\[-2ex]", stage1.fstat),
      insert.after = 21
    ) %>%
    ##delete the "Observations" line as it will be
    ##in the OLS panel
    .[!grepl("^Observations", x = .)]

  ## -- Reduced Form -- ##

  DT.models.reduced.form <- f_get_models(type = "reduced_form")

  star.reduced.form <- stargazer(
    DT.models.reduced.form$model,
    type = "latex",
    digits = 4, 
    keep = iv.var,
    keep.stat = "N"
  ) %>%
    star_rhs_names(
      pattern = iv.var.latex, 
      line1 = "LIBOR Diff Between",
      line2 = "First Meas \\& Adj."
    )


  ## -- OLS -- ##

  DT.models.ols <- f_get_models(type = "ols")

  star.ols <- stargazer(
    DT.models.ols$model,
    type = "latex",
    digits = 4, 
    keep = endog.var,
    keep.stat = "N"
  ) %>% star_rhs_names(
      pattern = endog.var.latex,
      line1 = "Modification",
      line2 = "Indicator"
  )

  ## -- The Panel -- ##

  star.panel <- star_panel(star.2sls, star.reduced.form, star.ols,
                           panel.names = c("2SLS Estimates", "Reduced Form",
                                           "OLS Estimates"),
                           same.summary.stats = FALSE,
                           panel.label.fontface = "bold")

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
    note = DT.table.params.lkp$table.notes.first.adj[table.id]
  )
  
  tex.dir <- "output-tex/"
  mkdir_p(here::here(tex.dir))

  save.file.location <- sprintf(
    "%s/030-star_panel_first_adj_%s_%s.tex",
    tex.dir, sprintf("%02.f", table.id),
    DT.table.params.lkp$first.adj.tbl.name[table.id]
  )

  star.panel <- star_sidewaystable(star.panel)

  ##For the final table, delete the asterisks that are used
  ##to represent significance 
  star.panel <- gsub("\\{\\*+\\}", "{}", star.panel)

  star_write_and_compile_pdf(star.panel,
                             file = here::here(save.file.location)
                             )
    
  return(invisible())

}

## f_create_latex_table(1)
lapply(DT.table.params.lkp$id, f_create_latex_table)




