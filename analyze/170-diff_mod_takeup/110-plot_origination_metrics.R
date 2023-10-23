## analyze/170-diff_mod_takeup/110-plot_origination_metrics.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

suppressPackageStartupMessages({
  library(CLmisc); library(readxl); library(patchwork); library(Cairo); 
})

##Set wd using the here package
setwd(here::here("analyze/170-diff_mod_takeup/"))

DT.lkp <- read_excel("./010-diff_mod_takeup_numeric_vars_lkp.xlsx") %>%
  setDT %>%
  .[for.appendix == 0] %>%
  .[, plot.label := paste(LETTERS[1:.N], plot.label, sep = ": ")] %>%
  select_by_ref(c("sep.var", "plot.label"))

DT.stage1 <- read_fst(
  here::here("work/diff_mod_takeup/010-DT_est_numeric_vars_stage1.fst"),
  as.data.table = TRUE
) %>%
  .[, ntile := substr(ntile, 2, 2)] %>%
  merge(DT.lkp, by = "sep.var") %>%
  .[, plot.label := paste0("1", plot.label)]

DT.npv <- read_fst(
  here::here("work/diff_mod_takeup/040-DT_npv_of_mods_numeric_vars.fst"),
  as.data.table = TRUE
) %>%
  .[, estimate := estimate / 1e03] %>%
  .[, std.error := std.error / 1e03] %>%
  merge(DT.lkp, by = "sep.var") %>%
  .[, plot.label := paste0("2", plot.label)]


DT.rf.stage2 <- read_fst(
  here::here("work/diff_mod_takeup/020-DT_est_numeric_vars_reduced_form_stage2.fst"),
  as.data.table = TRUE
) %>% 
  merge(DT.lkp, by = "sep.var") %>%
  .[, estimate := estimate / 1e03] %>%
  .[, std.error := std.error / 1e03]

DT.reduced.form <- DT.rf.stage2[stage == "reduced.form"] %>%
  .[, stage := NULL] %>%
  .[, plot.label := paste0("4", plot.label)]

DT.stage2 <- DT.rf.stage2[stage == "stage2"] %>%
  .[, stage := NULL] %>%
  .[, plot.label := paste0("4", plot.label)]


f_plot <- function(DT) {
  
  DT <- DT %>%
    .[!(est.type == "rel.to.ntile01" & ntile == "1")]

  if ("controls" %chin% names(DT)) 
    DT <- DT[controls == "full"]
  
  ggplot(DT, aes(x = ntile)) +
    geom_hline(yintercept = 0, color = "gray70", size = 1.01) +
    geom_errorbar(aes(ymin = estimate - 2.5 * std.error,
                      ymax = estimate + 2.5 * std.error,
                      color = est.type),
                  width = 0.25,
                  position = position_dodge(width = 0.4)) +
    geom_point(aes(y = estimate, color = est.type),
               size = 3,
               position = position_dodge(width = 0.4)) +
    scale_color_manual(breaks = c("main", "rel.to.ntile01"),
                       labels = c("Regression Estimation",
                                  "Regression Estimate Relative to Quintile 1"),
                       values = c("darkblue", "darkred")) + 
    facet_wrap(facets = vars(plot.label)) +
    theme_bw_cl() +
    theme(axis.title.x = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(), 
          ##brilliant answer to remove space left by deleted legend title
          ##https://github.com/tidyverse/ggplot2/issues/3587#issuecomment-546700387
          legend.margin = margin(1, 2, 2, 5),
          legend.spacing.y = unit(0, "pt")
          )

}

p.stage1 <- f_plot(DT.stage1) + 
  labs(title = "1: First Stage \u2013 LIBOR Change-Induced Interest Rate Modifications",
       y = "Modification Probability",
       subtitle = "LHS Var: Interest Rate Modification Indicator\nKey RHS Var: LIBOR Change Between First Measurement & First Adj.")

p.npv <- f_plot(DT.npv) + 
  labs(
    title = "2: NPV of Interest Rate Modifications from First Adjustment for Modified Loans", 
    subtitle = "LHS Var: Ex post Net Present Value (NPV) of Modification for Modified Loans\nRHS Var: Risk Proxy Quintile",
    y = "Mod NPV ($ 000s)"
  )

p.reduced.form <- f_plot(DT.reduced.form) + 
  labs(title = "4: Reduced Form \u2013 Loan-level LIBOR Changes and Investor Losses",
       y = "Investor Losses ($ 000s)",
       subtitle = "LHS Var: Cumulative Investor Losses For Each Loan Discounted Back To First Adj.\nKey RHS Var: LIBOR Change Between First Measurement & First Adj.")

p.stage2 <- f_plot(DT.stage2) + 
  labs(title = "Second Stage \u2013 LIBOR Change Induced Interest Rate Modifications and Investor Losses",
       y = "Investor Losses ($ 000s)",
       subtitle = "LHS Var: Cumulative Investor Losses For Each Loan Discounted Back To First Adj.\nEndogenous Var: Interest Rate Modification Indicator\nInstrument: LIBOR Change Between First Measurement & First Adj.")


## The NPV times the stage1 probability

DT.npv.stage1 <- DT.npv[est.type == "main", .(sep.var, ntile, est.npv = estimate)] %>%
  merge(DT.stage1[est.type == "main" & controls == "full",
                  .(sep.var, ntile, est.stage1 = -1 * estimate)],
        by = c("sep.var", "ntile")) %>%
  .[, npv.stage1 := est.npv * est.stage1] %>%
  .[, ntile := as.numeric(ntile)] %>%
  merge(DT.lkp, by = "sep.var") %>%
  .[, plot.label := paste0("3", plot.label)]

p.npv.stage1 <- ggplot(DT.npv.stage1, aes(x = ntile, y = npv.stage1)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray60") +
  geom_point(size = 3) +
  facet_wrap(facets = vars(plot.label)) +
  theme_bw_cl() +
  theme(axis.title.x = element_blank()) +
  labs(
    title = "3: Benefit of LIBOR Change-Induced Mods For All Loans \u2013 Mod Prob (Row 1) \u00d7 Mod NPV (Row 2)",
    y = "Mean Benefit ($ 000s)"
  )



p.all <- p.stage1 / p.npv / p.npv.stage1 / p.reduced.form +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

mkdir_p(here::here("output-plots/diff_mod_takeup"))
ggsave(
  here::here("output-plots/diff_mod_takeup/110-fig08_origination_metrics.pdf"),
  plot = p.all, width = 8.7, height = 10, dpi = 600,
  device = cairo_pdf
  
)
