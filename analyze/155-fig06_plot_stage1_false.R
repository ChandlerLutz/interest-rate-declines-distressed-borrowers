## analyze/155-fig06_plot_stage1_false.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({
  library(CLmisc); library(cowplot); library(patchwork); library(Cairo)
})

p.falsification <- readRDS(
  here::here("work/145_p_stage1_mtg_credit_falsificaiton.rds")
)

## -- Bottom Panel -- ##

p.falsification.credit.vars <- p.falsification$p.falsification.credit.vars +
  theme(legend.position = c(0.75, 0.2)) +
  labs(title = "A: Credit Performance Falsificaiton Tests \u2013 Standard Error Bands",
       x = "First Payment Adj. Date")

p.falsification.mtg.vars <- p.falsification$p.falsification.mtg.vars +
  theme(legend.position = c(0.64, 0.2)) +
  labs(title = "B: Mortgage Performance Falsificaiton Tests \u2013 Standard Error Bands",
      x = "First Payment Adj. Date")

p.patch <- p.falsification.credit.vars / p.falsification.mtg.vars

mkdir_p(here::here("output-plots/"))
ggsave(
  here::here("output-plots/155-fig06_p_credit_perf_false.pdf"),
  plot = p.patch,
  device = cairo_pdf,
  width = 6, height = 7.2, dpi = 600
)
