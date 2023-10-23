## analyze/220-plot_fig10_11_dynamic_perf.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({
  library(CLmisc); library(readxl); library(patchwork); library(Cairo)
})

DT.lkp <- read_xlsx("220-reg_model_lkp.xlsx") %>%
  setDT %>%
  setnames("sample", "sample.lkp") %>%
  .[, panel.letter := LETTERS[1:.N], by = figure] %>%
  .[, is.last.in.panel := as.integer(1:.N == .N), by = figure] %>%
  .[, plot.id := paste(figure, panel.letter, lhs.var, sep = "_")] %>%
  .[name != "figNA"]

DT <- read_fst(
  here::here("work/210-DT_est_months_after_first_adj.fst"),
  as.data.table = TRUE
) %>%
  .[grepl("Intercept|fit|_DIFF",term)] %>%
  ##the geography
  .[, geo := fcase(grepl("^\\(PROPERTYSTATECALC", filter.string), "sand",
                   grepl("^!\\(PROPERTYSTATECALC", filter.string), "non.sand",
                   default = "full")] %>%
  ##do not show the intercept for the full model as it relies on a
  ##multitude of fixed effects
  .[controls == "base" | (controls == "full" & term != "(Intercept)")] %>%
  .[, controls := fcase(controls == "base", "Base Model",
                        controls == "full", "Full Model\nWith All Controls")] %>%
  merge(DT.lkp, by = "lhs.var", all.x = TRUE) %>%
  .[!is.na(lhs.label)] %>%
  .[, term := fcase(term == "(Intercept)", "Intercept",
                    term == "`rate.actual.predicted.mod.1.0(fit)`",
                    "2SLS Regression Estimates"
                    )] %>%
  .[, term := factor(term, levels = c("Intercept",
                                      "2SLS Regression Estimates"))] %>%
  .[order(figure, panel.letter, months.after.first.adj)]

f_plot_geo_sample <- function(geo.temp) {

  DT <- DT[geo == c(geo.temp)]

  f_plot_intercept_est <- function(plot.id.temp) {

    DT <- DT[plot.id == c(plot.id.temp)]
    lhs.label <- DT[["lhs.label"]] %>% unique
    panel.letter <-  DT[["panel.letter"]] %>% unique
    sample <- DT[["sample.lkp"]] %>% unique
    is.last.in.panel <- DT[["is.last.in.panel"]] %>% unique

    p <- ggplot(DT, aes(x = months.after.first.adj, y = estimate)) +
      geom_hline(yintercept = 0, linetype = "dashed", size = 1.01, alpha = 0.75) +
      geom_ribbon(aes(ymin = estimate - 2.5 * std.error,
                      ymax = estimate + 2.5 * std.error, fill = controls),
                  alpha = 0.25) +
      geom_line(aes(color = controls)) +
      facet_wrap(facets = vars(term), ncol = 2, scales = "free_y") +
      theme_bw() +
      labs(title = paste0(panel.letter, ": LHS Var \u2013 ", lhs.label),
           subtitle = paste0(
             ## "Sample: ", sample, "; ", 
             "Endogenous Var: Interest Rate Mod Between First Meas and First Adj (Indicator)",
             "\nInstrument: LIBOR Difference between First Payment Adjustment and First Measurement"
           ),
           x = "") +
      theme_bw_cl() +
      theme(axis.title.y = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.justification = "center", 
            )

    ##for the last in the figure, print x label
    if (is.last.in.panel == 1) {
      p <- p + xlab("Months After First Adjustment")
    } else {
      p <- p + theme(axis.title.x = element_blank())
    }




    return(p)
  }

  lhs.vars <- DT[["lhs.var"]] %>% unique
  plot.ids <- DT[["plot.id"]] %>% unique

  p.all <- lapply(plot.ids, f_plot_intercept_est) %>%
    setNames(plot.ids)

  for (fig.number in unique(DT.lkp$figure)) {
    plot.ids <- DT.lkp[figure == c(fig.number), plot.id]
    fig.name <- DT.lkp[figure == c(fig.number), unique(name)]

    p.grid <- wrap_plots(p.all[c(plot.ids)], ncol = 1) +
      plot_layout(guides = "collect")  &
      theme(legend.position = "bottom")

    mkdir_p("output_plots")
    save.file.location <- sprintf(
      here::here("output-plots/220-%s.pdf"),
      fig.name
    )

    ggsave(filename = save.file.location,
           plot = p.grid, width = 7.5, height = (length(plot.ids) * 2.8 + 1),
           dpi = 600, device = cairo_pdf)
  }

  return(invisible())

}

geos <- DT[, unique(geo)]

lapply(geos, f_plot_geo_sample)

