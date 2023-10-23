## analyze/170-diff_mod_takeup/120-plot_char_vars_est.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/170-diff_mod_takeup/"))

suppressPackageStartupMessages({
  library(CLmisc); library(readxl); library(patchwork);
  library(Cairo); 
})

char.vars <- c("DOCTYPESUMMARY", "OCCTYPE", "MBADELINQUENCYSTATUS_PRE4_FIRST")

f_read_lkp <- function(var) {
  DT.lkp <- read_excel("./030-diff_mod_takeup_char_vars_lkp.xlsx",
                       sheet = var) %>%
    as.data.table(.) %>%
    .[, sep.var := c(var)] %>%
    .[, var.level := 1L:.N] %>%
    .[, plot.label := clean_readxl_backslashes(plot.label)] %>%
    .[, plot.label := factor(plot.label, levels = unique(plot.label))]

  return(DT.lkp)
  
}

DT.lkp <- lapply(char.vars, f_read_lkp) %>%
  rbindlist(.) %>%
  .[, panel.var := fcase(
    sep.var == "OCCTYPE", "A: Owner-Occupancy",
    sep.var == "DOCTYPESUMMARY", "B: Documentation Type",
    sep.var == "MBADELINQUENCYSTATUS_PRE4_FIRST", "C: Delin Status 4 Mths Before First Adj.")]

DT.lkp.level1 <- DT.lkp %>%
  .[var.level == 1] %>%
  select_by_ref(c("sep.var", "value")) %>%
  setnames("value", "level1.value") 


DT.est <- read_fst(
  here::here("work/diff_mod_takeup/030-DT_est_char_vars.fst"),
  as.data.table = TRUE
) %>%
  .[stage %chin% c("reduced.form","stage2"), `:=`(estimate = estimate / 1000, std.error = std.error / 1000)] %>%
  .[controls == "full"] %>%
  .[, controls := NULL] %>%
  .[, fact.value := gsub("LIBOR_FIRST_MEAS_PAY_ADJ_DIFF:sep.var.fact", "", term)] %>%
  .[, fact.value := gsub("fit_rate.actual.predicted.mod.1.0:sep.var.fact", "", fact.value)] %>%
  .[, fact.value := gsub("fit_mod.ind_", "", fact.value)] %>%
  .[, fact.value := gsub("LIBOR_FIRST_MEAS_PAY_ADJ_DIFF|fit_rate.actual.predicted.mod.1.0", "", fact.value)]

DT.npv <- read_fst(
  here::here("work/diff_mod_takeup/045-DT_npv_of_mods_char_vars.fst"),
  as.data.table = TRUE
) %>%
  .[, estimate := estimate / 1e03] %>%
  .[, std.error := std.error / 1e03] %>%
  .[, stage := "npv"] %>%
  setnames("sep.var.value", "fact.value")
 



DT.est.all <- DT.est %>%
  rbind(DT.npv, use.names = TRUE, fill = TRUE) %>% 
  merge(DT.lkp.level1, by = "sep.var") %>%
  .[fact.value == "", fact.value := level1.value] %>%
  .[, level1.value := NULL] %>%
  merge(DT.lkp[, .(sep.var, value, plot.label, var.level, panel.var)],
        by.x = c("sep.var", "fact.value"),
        by.y = c("sep.var", "value")) %>%
  .[!(est.type == "rel" & var.level == 1)] %>%
  .[stage == "stage1", panel.var := paste0("1", panel.var)] %>%
  .[stage == "npv", panel.var := paste0("2", panel.var)] %>% 
  .[stage == "reduced.form", panel.var := paste0("4", panel.var)] %>%
  .[stage == "stage2", panel.var := paste0("5", panel.var)]

f_plot <- function(stage) {

  stage.tmp <- stage
  ggplot(DT.est.all[stage == c(stage.tmp)],
         aes(x = plot.label, color = est.type)) +
    geom_hline(yintercept = 0, color = "gray70", linewidth = 1.01) +
    geom_errorbar(aes(ymin = estimate - 2.5 * std.error,
                      ymax = estimate + 2.5 * std.error,
                      color = est.type),
                  width = 0.25,
                  position = position_dodge(width = 0.4)) +
    geom_point(aes(y = estimate, color = est.type),
               size = 3,
               position = position_dodge(width = 0.4)) +
    scale_color_manual(breaks = c("main", "rel"),
                       labels = c("Regression Estimation",
                                  "Regression Estimate Relative to First Category"),
                       values = c("darkblue", "darkred")) + 
    facet_wrap(facets = vars(panel.var), scales = "free_x") +
    theme_bw_cl() + 
    theme(axis.title.x = element_blank(),
          ## axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          legend.position = "bottom",
          legend.title = element_blank(), 
          ##brilliant answer to remove space left by deleted legend title
          ##https://github.com/tidyverse/ggplot2/issues/3587#issuecomment-546700387
          legend.margin = margin(1, 2, 2, 5),
          legend.spacing.y = unit(0, "pt")
          )

}

p.stage1 <- f_plot("stage1") +
  labs(title = "1: First Stage \u2013 LIBOR Change-Induced Interest Rate Modifications",
       y = "Modification Probability",
       subtitle = "LHS Var: Interest Rate Modification Indicator\nKey RHS Var: LIBOR Change Between First Measurement & First Adj.")

p.npv <- f_plot("npv") +
  labs(title = "2: NPV of Interest Rate Modifications from First Adjustment for Modified Loans", 
       subtitle = "LHS Var: Ex post Net Present Value (NPV) of Modification for Modified Loans\nRHS Var: Category",
       y = "Mod NPV ($ 000s)")

p.reduced.form <- f_plot("reduced.form") + 
  labs(title = "4: Reduced Form \u2013 Loan-level LIBOR Changes and Investor Losses",
       y = "Investor Losses ($ 000s)",
       subtitle = "LHS Var: Cumulative Investor Losses For Each Loan Discounted Back To First Adj.\nKey RHS Var: LIBOR Change Between First Measurement & First Adj.")


p.stage2 <- f_plot("stage2") + 
  labs(title = "Second Stage \u2013 LIBOR Change Induced Interest Rate Modifications and Investor Losses",
       y = "Investor Losses ($ 000s)",
       subtitle = "LHS Var: Cumulative Investor Losses For Each Loan Discounted Back To First Adj.\nEndogenous Var: Interest Rate Modification Indicator\nInstrument: LIBOR Change Between First Measurement & First Adj.")


## The NPV times the stage1 probability

DT.npv.stage1 <- DT.npv[est.type == "main", .(sep.var, fact.value, est.npv = estimate)] %>%
  merge(DT.est[stage == "stage1" & est.type == "main",
               .(sep.var, fact.value, est.stage1 = -1 * estimate)],
        by = c("sep.var", "fact.value")) %>%
  .[, npv.stage1 := est.npv * est.stage1] %>%
  merge(DT.lkp,
        by.x = c("sep.var", "fact.value"),
        by.y = c("sep.var", "value")
        ) %>%
  .[, panel.var := paste0("3", panel.var)]

p.npv.stage1 <- ggplot(DT.npv.stage1, aes(x = plot.label, y = npv.stage1)) +
  geom_point(size = 3) +
  facet_wrap(facets = vars(panel.var), scales = "free_x") +
  theme_bw_cl() + 
  theme(axis.title.x = element_blank()) +
  labs(title = "3: Benefit of LIBOR Change-Induced Mods For All Loans \u2013 Mod Prob (Row 1) \u00d7 Mod NPV (Row 2)",
       y = "Mean Benefit ($ 000s)"
       )


p.all <- p.stage1 / p.npv / p.npv.stage1 / p.reduced.form +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


mkdir_p(here::here("output-plots/diff_mod_takeup"))
ggsave(
  here::here("output-plots/diff_mod_takeup/120-fig09_char_vars.pdf"),
  plot = p.all, width = 8.7, height = 10.5, dpi = 600,
  device = cairo_pdf
)

