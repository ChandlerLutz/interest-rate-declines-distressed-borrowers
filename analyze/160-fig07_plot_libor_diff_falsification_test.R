## analyze/160-fig07_plot_libor_diff_falsification_test.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({library(CLmisc); library(Cairo)})

DT.libor.diff.mod.falsification.est <- read_fst(
  here::here("work/115-DT_est_libor_diff_mod_placebo_test.fst"),
  as.data.table = TRUE
) %>%
  ## .[test.type == "falsification" & perf.date <= "2008-09-01"]
  ## .[test.type == "falsification" & perf.date <= "2009-04-01"]
  .[test.type == "falsification" & perf.date <= "2008-12-01"]
  ## .[test.type == "falsification"]

p.libor.diff.mod.falsification.est <- ggplot(
  DT.libor.diff.mod.falsification.est[mod.type == "int.rate.algo"],
  aes(x = perf.date)
) +
  geom_errorbar(aes(ymin = estimate - 2.5 * std.error,
                    ymax = estimate + 2.5 * std.error),
                width = 15
                ) +
  scale_x_date(expand = c(0.05, 10)) + 
  theme_cowplot_cl() +
  labs(subtitle = "Confidence Bands Correspond to \u00b12.5 Robust Standard Errors",
       x = "Pre-Treatment Year-Month")
p.libor.diff.mod.falsification.est


mkdir_p(here::here("output-plots"))
ggsave(here::here("output-plots/160-fig07_libor_diff_mod_falsification.pdf"),
       plot = p.libor.diff.mod.falsification.est,
       width = 5.5, height = 3.75, dpi = 450,
       device = cairo_pdf
       )


