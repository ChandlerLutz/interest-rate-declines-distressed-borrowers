## analyze/170-diff_mod_takeup/130-plot_stage1_appendix.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/170-diff_mod_takeup/"))

suppressPackageStartupMessages({
  library(CLmisc); library(patchwork); library(readxl)
})

DT.lkp <- read_excel("./010-diff_mod_takeup_numeric_vars_lkp.xlsx") %>%
  setDT %>%
  .[, id := 1:.N] %>%
  setcolorder("id") %>%
  select_by_ref(c("id", "sep.var", "plot.label", "for.appendix")) %>%
  .[, plot.label := factor(plot.label, levels = plot.label)]

DT.lkp.appendix <- DT.lkp %>%
  .[for.appendix == 1] %>%
  .[, plot.label := paste0(LETTERS[1:.N], ": ", plot.label)]


DT.est <- read_fst(
  here::here("work/diff_mod_takeup/010-DT_est_numeric_vars_stage1.fst"),
  as.data.table = TRUE
) %>%
  .[est.type == "rel.to.ntile01"] %>% 
  .[, ntile1 := fifelse(ntile == "01",
                        "First Quintile Estimate",
                        "Estimate Relative to Quintile 1"
                        )] %>%
  .[, ntile1 := as.factor(ntile1)] %>%
  .[, ntile1 := relevel(ntile1, "First Quintile Estimate")] %>% 
  .[, ntile := substr(ntile, 2, 2)] %>%
  .[, controls := fcase(controls == "base", "Base Controls",
                        controls == "full", "All Controls"
                        )] %>%
  .[, controls := as.factor(controls)] %>%
  .[, controls := relevel(controls, "Base Controls")]

DT.est.appendix <- DT.est %>%
  merge(DT.lkp.appendix, by = "sep.var")

p.ntile.numeric.vars.appendix <-
  ggplot(DT.est.appendix, aes(x = ntile)) +
  geom_hline(yintercept = 0, color = "gray70", size = 1.01) + 
  geom_errorbar(aes(ymin = estimate - 2.5 * std.error,
                    ymax = estimate + 2.5 * std.error,
                    color = controls),
                width = 0.25, 
                position = position_dodge(width = 0.3)) +
    geom_point(aes(y = estimate, color = controls, shape = ntile1),
               size = 2.5, 
               position = position_dodge(width = 0.3)) +
  scale_color_manual(values = c("darkblue", "darkred")) + 
  scale_linetype_manual(values = c("22", "solid")) + 
  facet_wrap(facets = vars(plot.label)) +
  theme_bw_cl() +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.justification = "center",
        ##brilliant answer to remove space left by deleted legend title
        ##https://github.com/tidyverse/ggplot2/issues/3587#issuecomment-546700387
        legend.margin = margin(1, 2, 2, 5),
        legend.spacing.y = unit(0, "pt")
        ) +
    labs(x = "Quintile", 
         ## title = "LIBOR Change Induced Interest Rate Modification Probabilities by Risk Proxy Quintiles; \u00b1 2.5 Standard Error Bands",
         subtitle = "LHS Var: Interest Rate Modification Indicator; Key RHS Var: LIBOR Change Between First Measurement & First Adj\nRegressions Estimated Separately by Panel; Controls Vary With Each Quintile\nConfidence Bands Correspond to \u00b1 2.5 Robust Standard Error Clustered at the Three\u00adDigit Zip Code Level")

mkdir_p(here::here("output-plots/diff_mod_takeup"))
ggsave(here::here("output-plots/diff_mod_takeup/130-figG1_est_numeric_vars_stage1_appendix.pdf"),
       plot = p.ntile.numeric.vars.appendix,
       width = 10.3, height = 7, dpi = 600)

