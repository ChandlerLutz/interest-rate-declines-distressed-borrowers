## analyze/130-plot_binscatters.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({
  library(CLmisc); library(readxl); library(patchwork); library(Cairo)
})

DT.lhs.lkp <- read_excel("./125-binscatter_lhs_vars_lkp.xlsx") %>%
  as.data.table %>%
  .[, lhs.var.label := clean_readxl_backslashes(lhs.var.label)] %>%
  .[, lhs.var.label := gsub("\\u0394", "\u0394", x = lhs.var.label, fixed = TRUE)] %>% 
  .[to.plot == 1] %>%
  .[, id := 1:.N] 

DT.bin.est <- readRDS(
  here::here("work/125-binscatter_estimates.rds")
) %>%
  merge(DT.lhs.lkp, by = "lhs.var") %>%
  .[order(id)]

DT.bin.est.first.adj.labels <- DT.bin.est %>%
  .[period == "first.adj", .(lhs.var, lhs.var.label)] %>%
  .[, lhs.var.label := paste0(LETTERS[1:.N], ": ", lhs.var.label)]

DT.bin.est.48m.post.first.adj.labels <- DT.bin.est %>%
  .[period == "after.48.months", .(lhs.var, lhs.var.label)] %>%
  .[, id := 1:.N] %>%
  .[, panel.label := ceiling(id / 2)] %>%
  .[, panel.label := fifelse(id %% 2 == 1,
                             paste0(panel.label, "A: "),
                             paste0(panel.label, "B: "))] %>%
  .[, lhs.var.label := paste0(panel.label, lhs.var.label)] %>%
  .[, panel.label := NULL] %>%
  .[, id := NULL]


DT.lhs.labels.lkp <- rbind(
  DT.bin.est.first.adj.labels,
  DT.bin.est.48m.post.first.adj.labels
)

DT.bin.est <- DT.bin.est %>% 
  .[, lhs.var.label := NULL] %>%
  merge(DT.lhs.labels.lkp, by = "lhs.var")


f_plot <- function(lhs.var) {
  lhs.var.tmp <- lhs.var
  DT.bin.est.tmp <- DT.bin.est[lhs.var == c(lhs.var.tmp)]
  lhs.var.label <- DT.bin.est.tmp$lhs.var.label
  
  DT.all <- rbind(
    DT.bin.est.tmp$DT.bs.baseline[[1]],
    DT.bin.est.tmp$DT.bs.risk.cntrls[[1]]
  )

  ggplot(DT.all, aes(x = x, y = fit)) +
    geom_point(aes(shape = plot.label, color = plot.label), size = 7, fill = NA) +
    stat_smooth(aes(color = plot.label), method = lm, se = FALSE, show.legend = FALSE) +
    scale_x_continuous(limits = c(-2, 0.5), labels = \(x) paste0(x, "%")) + 
    scale_shape_manual(values = c(21, 22)) +
    scale_color_manual(values = c("#00BFC4", "#C77CFF")) + 
    theme_cowplot_cl() +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.spacing.y = unit(0, "pt"),
          legend.position = "bottom"
          ) +
    labs(title = lhs.var.label,
         x = "Loan-Level LIBOR Change: First Adj. \u2013 First Meas."
         )
    
    
}

DT.bin.est <- DT.bin.est[, plot := lapply(lhs.var, f_plot)]

p.first.adj <- DT.bin.est[period == "first.adj"] %>%
  .[order(lhs.var.label)] %>%
  .[, plot] %>% 
  wrap_plots(ncol = 1) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom"))
p.first.adj

p.48.mths.after.adj <- DT.bin.est[period == "after.48.months"] %>%
  .[order(lhs.var.label)] %>%
  .[, plot] %>% 
  wrap_plots(ncol = 2) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom"))
p.48.mths.after.adj


mkdir_p(here::here("output-plots/"))

ggsave(here::here("output-plots/130-fig05_binscatters_first_adj.pdf"),
       plot = p.first.adj,
       device = cairo_pdf,
       width = 6.5, height = 9, dpi = 600
       )

ggsave(here::here("output-plots/130-figH1_binscatters_48m_after_adj.pdf"),
       plot = p.48.mths.after.adj,
       device = cairo_pdf,
       width = 8.6, height = 7, dpi = 450,
       )





