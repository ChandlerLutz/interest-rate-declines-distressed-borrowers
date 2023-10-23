## analyze/150-fig04_plot_mod_stage1_stage2_all.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({
  library(CLmisc); library(cowplot); library(patchwork); library(Cairo); 
})

p.basic.reg <- readRDS(
  here::here("work/140-p_baseline_stage1_stage2.rds")
)

## -- Top Panel -- ##

p.libor.diff.range <- p.basic.reg$p.libor.diff.range +
  labs(title = "1A: LIBOR Change Between First Measurement & First Adj.")
p.mod.forc.for.libor.diff.top.quarter <- p.basic.reg$p.mod.forc.for.libor.diff.top.quarter +
  labs(title = "1B: Mod or REO Forc Rates for Baseline Subprime ARMs")

p.patch.top <- p.libor.diff.range + p.mod.forc.for.libor.diff.top.quarter

## -- Bottom Panel -- ##

p.mod.1.0 <- p.basic.reg$p.mod.1.0 +
  labs(title = "2A: First Stage \u2013 LIBOR Change-Induced Interest Rate Mods",
       subtitle = "LHS Var: Interest Rate Modification Indicator\nKey RHS Var: LIBOR Change Between First Measurement & First Adj."
       ) +
  theme(
    legend.position = c(0.2, 0.9),
    ##legend.key.size to increase space between legend items
    ##from https://stackoverflow.com/a/50593988
    legend.key.height = unit(1.6, 'lines')
  )


p.rate.payment.log.2sls <- p.basic.reg$p.rate.payment.log.2sls +
  theme(##axis.text.x = element_blank(),
    plot.margin = margin(1, 1, 0, 1)) +
  labs(title = "2B: 2SLS \u2013 LIBOR Change-Induced Mods and Rate Payments")

p.rate.payment.dollar.2sls.simple <- p.basic.reg$p.rate.payment.dollar.2sls.simple

p.patch.rate.payment <- p.rate.payment.log.2sls / p.rate.payment.dollar.2sls.simple +
  plot_layout(heights = c(3, 1.5))

p.patch.middle <- p.mod.1.0 + p.patch.rate.payment


p.all <- p.patch.top / p.patch.middle
  plot_layout(heights = c(1, 1.7))

mkdir_p(here::here("output-plots"))

ggsave(
  here::here("output-plots/150-fig04_p_all_mod_stage1_stage2.pdf"),
  plot = p.all,
  device = cairo_pdf,
  width = 11, height = 9, dpi = 600
)

