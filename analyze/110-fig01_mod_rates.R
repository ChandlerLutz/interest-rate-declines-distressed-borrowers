## analyze/110-fig01_mod_rates.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({library(CLmisc); library(patchwork); library(Cairo); })

## -- PLS vs libor6m mods -- ##

DT.libor6m <- read_fst(
  here::here("data/DT_panel_servicer_ever_any_mod_rate_libor6m.fst"),
  as.data.table = TRUE
) %>%
  .[, N_LIBOR6M_LOANS := as.numeric(N_LIBOR6M_LOANS)] %>%
  .[, .(loan.type = "6-month LIBOR ARMs",
        mod.rate = weighted.mean(LIBOR6M_EVER_MOD_RATE, N_LIBOR6M_LOANS)),
    by = .(index = ACTIVITYDATE)]


DT.arms <- read_fst(
  here::here("data/DT_panel_servicer_ever_any_mod_rate_all_arms.fst"),
  as.data.table = TRUE
) %>%
  .[, N_ARM_LOANS := as.numeric(N_ARM_LOANS)] %>%
  .[, .(loan.type = "All PLS ARMs", mod.rate = weighted.mean(ARM_EVER_MOD_RATE, N_ARM_LOANS)),
    by = .(index = ACTIVITYDATE)]



DT.all <- read_fst(
  here::here("data/DT_panel_servicer_ever_any_mod_rate_all_loans.fst"),
  as.data.table = TRUE
) %>%
  .[, N_ALL_LOANS := as.numeric(N_ALL_LOANS)] %>%
  .[, .(loan.type = "All PLS Loans", mod.rate = weighted.mean(ALL_LOANS_EVER_MOD_RATE, N_ALL_LOANS)),
    by = .(index = ACTIVITYDATE)]

DT <- rbind(DT.libor6m, DT.all)



p.mod.rates <- ggplot(DT[index <= "2010-12-31"], aes(x = index, y = mod.rate, color = loan.type)) +
  geom_line(size = 1.01) +
  scale_x_date(limits = as.Date(c("2007-01-01", "2010-12-31"))) +
  scale_color_manual(values = c("darkred", "darkblue")) +
  theme_cowplot_cl() +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.spacing.y = unit(0, "pt"),
        legend.position = c(0.2, 0.8)
        ) +
  labs(title = "A: Cumulative (Ever) Modification Rates by Loan Type",
       subtitle = "Moody's Blackbox PLS Loans Originated from 2002 to 2006; Any Modificaton Type;\nModifications Identified by Moody's Blackbox", 
       y = "Modification Probability")
p.mod.rates


## -- Percent of Subprime ARM mods by First Adj -- ##

DT.first.adj.date.cumsum.mods <- read_fst(
  here::here("data/DT_total_mods_cumsum_by_first_adj.fst"),
  as.data.table = TRUE
) %>%
  .[, has.been.modified := has.been.modified.pct / 100]

p.first.adj.date.cumsum.mods <- ggplot(DT.first.adj.date.cumsum.mods,
                        aes(x = index, y = has.been.modified)) +
  geom_line(aes(color = first.pay.adj.qtr.label), size = 1.01) +
    scale_color_viridis_d(option = "inferno", end = 0.85) +
    guides(color = guide_legend(title = "First Adj.\nYr-Qtr",
                                nrow = 2, byrow = TRUE)) +
    theme_cowplot_cl() +
    theme(axis.title.x = element_blank(),
          legend.position = "bottom",
          legend.justification = "center"
          ) +
  labs(title = "B: Subprime 6-month LIBOR Cumulative (Ever) Modification\nRates by First Payment Adjustment Year-Quarter",
       subtitle = "Moody's Blackbox 6-month LIBOR ARMs Originated from 2002 to 2006; Any Modificaton\nType; Modifications Identified by Moody's Blackbox",
       y = "Modification Probability")
p.first.adj.date.cumsum.mods


p.all <- p.mod.rates / p.first.adj.date.cumsum.mods

mkdir_p(here::here("output-plots/"))
ggsave(
  here::here("output-plots/110-fig01_mod_rates.pdf"),
  plot = p.all, width = 6, height = 7, device = cairo_pdf, 
  dpi = 600
)
