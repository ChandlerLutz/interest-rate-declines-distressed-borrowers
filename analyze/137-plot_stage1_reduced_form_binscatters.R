## analyze/137-plot_stage1_reduced_form_binscatters.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

suppressPackageStartupMessages({
  library(CLmisc); library(readxl); library(patchwork)
})

DT.bs.lkp <- read_excel(
  "./137-binscatter_lhs_vars_lkp.xlsx"
) %>%
  setDT(.) %>%
  .[, lhs.var.label := clean_readxl_backslashes(lhs.var.label)] %>%
  .[, title := paste0(1:.N, "B: ", title)] %>%
  .[, id := 1:.N]


bs.est.file <- here::here(
  "work/125-binscatter_estimates.rds"
)

DT.bs <- readRDS(bs.est.file) %>%
  .[lhs.var %chin% c("rate.actual.predicted.mod.1.0", "ln.rate.payment.actual.predicted")] %>%
  merge(DT.bs.lkp, by = "lhs.var", all.x = TRUE) %>%
  .[order(id)]

f_plot <- function(lhs.var) {
  lhs.var.tmp <- lhs.var
  DT.bs.tmp <- DT.bs[lhs.var == c(lhs.var.tmp)]
  lhs.var.label <- DT.bs.tmp$lhs.var.label
  title <- DT.bs.tmp$title
  
  DT.all <- rbind(
    DT.bs.tmp$DT.bs.baseline[[1]],
    DT.bs.tmp$DT.bs.risk.cntrls[[1]]
  )

  p <- ggplot(DT.all, aes(x = x, y = fit)) +
    geom_point(aes(shape = plot.label, color = plot.label), size = 7, fill = NA) +
    stat_smooth(aes(color = plot.label), method = lm, se = FALSE, show.legend = FALSE) +
    scale_x_continuous(labels = \(x) paste0(x, "%")) + 
    scale_shape_manual(values = c(21, 22)) +
    scale_color_manual(values = c("#00BFC4", "#C77CFF")) + 
    theme_cowplot_cl() +
    theme(legend.title = element_blank(),
          legend.spacing.y = unit(0, "pt"),
          legend.position = "bottom"
          ) +
    labs(title = title,
         y = lhs.var.label)

  if (lhs.var == "ln.rate.payment.actual.predicted") {
    ## Add the x-axis title for the bottom plot
    p <- p + labs(x = "Loan\u00adLevel LIBOR Change: First Adj. \u2013 First Meas.")
  } else {
    ## Remove the x-axis title from the top plot
    p <- p + theme(axis.title.x = element_blank())
  }
    
  return(p)
}

plot.list <- lapply(DT.bs$lhs.var, f_plot)

p.all <- wrap_plots(plot.list, ncol = 1) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom"))
  

mkdir_p(here::here("work/"))
saveRDS(
  p.all,
  here::here("work/137-fig03_stage1_reduced_form_binscatters.rds")
)



