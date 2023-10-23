## analyze/235-fig14_est_loan_perf_by_bartik_quintile.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({library(CLmisc); library(patchwork); })

DT <- read_fst(
  here::here(    "work/230-DT_est_reo_loss_delin_after_48m_by_bartik_ntile.fst"
  ),
  as.data.table = TRUE
) %>%
  .[id == 2] %>%
  .[, ntile := substr(ntile, 2, 2)] %>%
  .[est.type == "main" | ntile > 1]


f_plot <- function(lhs.var) {

  lhs.tmp <- lhs.var

  DT.plot <- DT[lhs.var == c(lhs.tmp)]

  ggplot(DT.plot, aes(x = ntile)) +
    geom_hline(yintercept = 0, color = "gray70", linewidth = 1.01) +
    geom_errorbar(aes(ymin = estimate - 2.5 * std.error,
                      ymax = estimate + 2.5 * std.error,
                      color = est.type,
                      ),
                  width = 0.25, 
                  position = position_dodge(width = 0.4)) +
    geom_point(aes(y = estimate, color = est.type),
               size = 3,
               position = position_dodge(width = 0.4)) +
    scale_color_manual(breaks = c("main", "rel"),
                       labels = c("Regression Estimate",
                                  "Regression Estimate Relative to Quintile 1"),
                       values = c("darkblue", "darkred")) + 
    theme_cowplot_cl() +
    theme(axis.title.y = element_blank(),
          legend.position = "bottom",
          legend.title = element_blank(), 
          ##brilliant answer to remove space left by deleted legend title
          ##https://github.com/tidyverse/ggplot2/issues/3587#issuecomment-546700387
          legend.margin = margin(1, 2, 2, 5),
          legend.spacing.y = unit(0, "pt")) +
    labs(x = "2006-2010 Bartik Quintile")

}

overall.title <- "2SLS Estimates -- LIBOR Change-Induced Modifications by Bartik Quintile"

p.2sls.ever.reo.delin90.plus <- f_plot(lhs.var = "ever.reo.or.is.delin90.plus") +
  labs(
    title = "A: LHS Var \u2013 Ever REO, Liquidated with a Loss, or 90+ Days Delinquent",
    subtitle = "Sample: FICO \u2264 660; Loan performance measured 48 months after first adjustment;\nEndogenous Var: Interest Rate Mod Between First Meas and First Adj (Indicator)\nInstrument: LIBOR Difference between First Payment Adjustment and First Measurement"
  )

p.2sls.ever.reo.delin180.plus <- f_plot(lhs.var = "ever.reo.or.is.delin180.plus") +
  labs(
    title = "B: LHS Var \u2013 Ever REO, Liquidated with a Loss, or 180+ Days Delinquent",
    subtitle = "Sample: FICO \u2264 660; Loan performance measured 48 months after first adjustment;\nEndogenous Var: Interest Rate Mod Between First Meas and First Adj (Indicator)\nInstrument: LIBOR Difference between First Payment Adjustment and First Measurement"
  )


p.all <- p.2sls.ever.reo.delin90.plus / p.2sls.ever.reo.delin180.plus +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
p.all

mkdir_p(here::here("output-plots"))
ggsave(
  here::here(
    "output-plots/235-fig14_ever_reo_loss_delin_after_48m.pdf"
  ),
  plot = p.all, width = 6.5, height = 7.75, dpi = 600, device = cairo_pdf)




