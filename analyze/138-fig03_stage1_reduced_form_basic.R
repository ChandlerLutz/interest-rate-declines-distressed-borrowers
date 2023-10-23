## analyze/138-fig03_stage1_reduced_form_basic.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({
  library(CLmisc); library(patchwork); library(zoo); library(Cairo); 
})

p.temporal <- readRDS(
  here::here("work/135-p_temporal_libor_change_stage1_reduced_form.rds")
)

p.bs <- readRDS(
  here::here("work/137-fig03_stage1_reduced_form_binscatters.rds")
)

p.intro <- p.temporal | p.bs

mkdir_p(here::here("output-plots"))
ggsave(
  here::here("output-plots/138-fig03_libor_change_stage1_reduced_form.pdf"),
  plot = p.intro, width = 10.9, height = 7.2, dpi = 450,
  device = cairo_pdf
  )


