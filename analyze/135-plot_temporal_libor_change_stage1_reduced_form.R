## analyze/135-plot_temporal_libor_change_stage1_reduced_form.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({
  library(CLmisc); library(zoo); library(readxl); library(patchwork)
})

DT.lkp <- read_xlsx("./135-plot_temporal_libor_change_stage1_reduced_form_lkp.xlsx") %>% 
  setDT %>%
  .[, title := paste0(1:.N, "A: ", title)] %>%
  .[, title := clean_readxl_backslashes(title)] %>%
  .[, y.label := clean_readxl_backslashes(y.label)]

DT <- read_fst(
  here::here("data/DT_for_2sls_libor_diff.fst"),
  as.data.table = TRUE
) %>%
  .[FICOSCOREORIGINATIONCALC <= 660]

DT.first.pay.adj <- copy(DT) %>%
  .[, libor.diff.ntile := fntile(LIBOR_FIRST_MEAS_PAY_ADJ_DIFF, 4),
    by = first.payment.adj.quarter] %>%
  .[, .(rate.actual.predicted.mod.1.0.mean = mean(rate.actual.predicted.mod.1.0),
        ln.rate.payment.actual.predicted.mean = mean(ln.rate.payment.actual.predicted),
        rate.payment.actual.predicted.mean = mean(rate.payment.actual.predicted)
        ),
    by = .(first.payment.adj.quarter, libor.diff.ntile)] %>% 
  .[, libor.diff.ntile := as.character(libor.diff.ntile)] %>%
  .[libor.diff.ntile %in% c(1, 4)] %>%
  .[, plot.label := fcase(libor.diff.ntile == 1, "1st Quartile (Largest LIBOR Decline)",
                          ## libor.diff.ntile == 2, "2nd Quartile",
                          ## libor.diff.ntile == 3, "3rd Quartile",
                          libor.diff.ntile == 4, "4th Quartile (Smallest LIBOR Decline)")] %>%
  .[, index.yq := zoo::as.yearqtr(first.payment.adj.quarter)]

f_plot <- function(temp.rowid) {

  DT.lkp.row <- DT.lkp[rowid == c(temp.rowid)]
  y.var <- DT.lkp.row$y.var
  y.label <- DT.lkp.row$y.label
  title <- DT.lkp.row$title

  p <- ggplot(DT.first.pay.adj,
         aes_string(x = "index.yq", y = y.var)) +
    geom_line(aes(color = plot.label), size = 1.01) +
    scale_color_manual(values = c("#F8766D", "#7CAE00")) +
    scale_x_continuous(labels = function(x) format.yearqtr(x, "%Y\u00adQ%q")) +
    guides(color = guide_legend(title = "Loan\u00adLevel LIBOR Change:\nFirst Adj. \u2013 First Meas.",
                                byrow = TRUE, nrow = 2)) + 
    theme_cowplot_cl() +
    theme(legend.position = "bottom",
          legend.justification = "center",
          legend.title = element_text(margin = margin(r = 10), hjust = 0.5)) +
    labs(title = title,
         y = y.label)

  
  if (y.var == "ln.rate.payment.actual.predicted.mean") {
    ## Add the x-axis title for the bottom plot
    p <- p + labs(x = "First Payment Adjustment Year\u00adQuarter")
  } else {
    ## Remove the x-axis title from the top plot
    p <- p + theme(axis.title.x = element_blank())
  }

  return(p)

}

plot.list <- lapply(1:nrow(DT.lkp), f_plot) %>% setNames(DT.lkp$y.var)

plot.list$rate.actual.predicted.mod.1.0.mean <- plot.list$rate.actual.predicted.mod.1.0.mean +
  scale_y_continuous(breaks = c(0.05, 0.15, 0.25))

p.all <- wrap_plots(plot.list, ncol = 1) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "bottom"))


mkdir_p(here::here("work"))
saveRDS(
  p.all,
  here::here("work/135-p_temporal_libor_change_stage1_reduced_form.rds")
)

