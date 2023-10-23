## analyze/120_fig02_libor_arm_rates_mod.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({
  library(CLmisc); library(cowplot); library(patchwork); library(Cairo); 
})

## -- LIBOR6m and Monetary Policy Shocks -- ##

DT.ff6.libor <-
  read_fst(here::here("data/DT_libor_ff6.fst"),
           as.data.table = TRUE) %>%
  .[index >= "2007-01-01" & index <= "2012-01-01"] %>%
  melt(id.vars = "index", variable.factor = FALSE) %>%
  .[, variable := ifelse(variable == "ff6.rate", "Expected Fed Funds Rate in 6-months", "6-month LIBOR")]

p.ff6.libor <- ggplot(DT.ff6.libor, aes(x = index, y = value)) +
  geom_line(aes(color = variable), size = 1.01) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, suffix = "%")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("darkred", "darkblue")) +
  labs(title = "1A: 6-month LIBOR versus the Expected Fed Funds\nRate in 6 months",
       subtitle = "Expected Fed Funds Rate From Fed Funds Futures") +
  theme_cowplot_cl() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.65, 0.9),
        ##brilliant answer to remove space left by deleted legend title
        ##https://github.com/tidyverse/ggplot2/issues/3587#issuecomment-546700387
        legend.spacing.y = unit(0, "pt"))
p.ff6.libor

DT.libor6m.shocks <- read_fst(
here::here("data/DT_conventional_qe_libor_shocks.fst"), as.data.table = TRUE)

p.libor6m.shocks <- ggplot(DT.libor6m.shocks, aes(x = index, y = libor.shock.cumsum)) +
  geom_line(aes(color = mp.regime), size = 1.01) +
  guides(color = guide_legend(title = "Monetary Policy Regime")) +
  scale_y_continuous(breaks = seq(from = -1.0, to = 0, by = 0.2),
                     labels = scales::number_format(accuracy = 0.1, suffix = "%")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("darkred", "darkblue")) +
  labs(title = "1B: Cumulative Impact of Conventional and\nQE Shocks on 6-month LIBOR",
       subtitle = "Conventional Dates from FOMC Meetings; QE Dates from GHHW") +
  theme_cowplot_cl() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.75, 0.8)
        )
p.libor6m.shocks

## -- Predicted vs Actual Interest Rates and Payments -- ##

DT.200901 <- readRDS(
  here::here("data/list_int_rate_vs_predicted/2009-01-01.rds")
)

f_finish_rate_payment_plot <- function(p) {
  p +
    geom_step(aes(linetype = variable, color = variable), size = 1.01) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    scale_color_manual(values = c("darkred", "darkblue")) +
    theme_cowplot_cl() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = c(0.3, 0.15),
          ##brilliant answer to remove space left by deleted legend title
          ##https://github.com/tidyverse/ggplot2/issues/3587#issuecomment-546700387
          legend.spacing.y = unit(0, "pt")) +
        guides(guide_legend(byrow = TRUE))

}

DT.200901.mean.adj.rate <- DT.200901$DT.mean.adj.rate.by.activitydt %>%
  .[index >= "2007-06-01"] %>%
  .[variable == "CURRENTINTRTCALC", variable := "Actual"] %>%
  .[variable == "predicted.rate.at.measurement", variable := "Predicted Based on Loan Terms"]

##for a borrower with a 600 credit score, an LTV of 82, a margin of 6, and the mean initial interest rate
p.200901.rates <- ggplot(DT.200901.mean.adj.rate,
                         aes(x = index, y = mean.estimate)) +
  labs(title = "2A:  Actual vs. Predicted Subprime ARM Interest\nRates",
       subtitle = "First Payment Adj. Date in Jan 2009; Credit Risk Adjusted Mean")
p.200901.rates <- f_finish_rate_payment_plot(p.200901.rates) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1, suffix = "%"))
p.200901.rates

DT.200901.mean.adj.rate.payments <- DT.200901$DT.mean.adj.rate.payment.by.activitydt %>%
  .[index >= "2007-06-01"] %>%
  .[variable == "SCHEDINTAMTCALC", variable := "Actual"] %>%
  .[variable == "predicted.rate.payment.at.measurement", variable := "Predicted Based on Loan Terms"]

p.200901.rate.payments <- ggplot(DT.200901.mean.adj.rate.payments,
                                 aes(x = index, y = mean.estimate)) +
  labs(title = "2B: Actual vs. Predicted Subprime ARM Interest\nRate Payments",
       subtitle = "First Payment Adj. Date in Jan 2009; Credit Risk Adjusted Mean")
p.200901.rate.payments <- f_finish_rate_payment_plot(p.200901.rate.payments) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1, prefix = "$", big.mark = ","))
p.200901.rate.payments

## p.rates.and.payments <- p.200901.rates + p.200901.rate.payments
p.rates.and.payments <- plot_grid(p.200901.rates, p.200901.rate.payments,
                                  nrow = 1, align = "hv")


## -- Mods -- All First payment adjustment dates -- ##

DT.int.rates.all.first.pay.adj.qtr <- read_fst(
  here::here("data/DT_int_rates_actual_predicted_by_first_adj.fst"),
  as.data.table = TRUE
)

p.actual.predicted.rate.diff <- ggplot(DT.int.rates.all.first.pay.adj.qtr,
                                       aes(x = index, y = actual.predicted.rate.diff)) +
  labs(title = "3A: Actual minus Predicted Subprime ARM\nInterest Rates",
       subtitle = "Mean Differences by First Payment Adj. Qtr; Credit Risk Adjusted") +
  geom_line(aes(color = first.pay.adj.qtr.label), size = 1.0) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1, suffix = "%")) +
    scale_color_viridis_d(option = "inferno", end = 0.85) +
    guides(color = guide_legend(title = "First Adj.\nYr-Qtr",
                                nrow = 2, byrow = TRUE)) +
    theme_cowplot_cl() +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = "bottom",
          legend.justification = "center"
          )
p.actual.predicted.rate.diff


## -- LIBOR Diff modification Placebo Test -- ##

DT.libor.diff.mod.placebo.est <- read_fst(
  here::here("work/115-DT_est_libor_diff_mod_placebo_test.fst"),
  as.data.table = TRUE
)


p.libor.diff.mod.placebo.est <- ggplot(
  DT.libor.diff.mod.placebo.est[
    mod.type == "int.rate.algo" & perf.date <= "2009-06-01" & test.type == "placebo"
  ],
  aes(x = perf.date)
) +
  geom_errorbar(aes(ymin = estimate - 2.5 * std.error,
                    ymax = estimate + 2.5 * std.error),
                width = 15,
                size = 0.6
                ) +
  theme_cowplot_cl() +
  labs(title = "3B: Loan-Level LIBOR Change Placebo\nRegression Estimates",
       subtitle = "Confidence Bands Correspond to \u00b12.5 Robust Standard Errors",
       x = "Placebo First Payment Adjustment Year-Month")
p.libor.diff.mod.placebo.est

p.all <- (p.ff6.libor + p.libor6m.shocks) / (p.200901.rates + p.200901.rate.payments) / (p.actual.predicted.rate.diff + p.libor.diff.mod.placebo.est)


mkdir_p(here::here("output-plots/"))
ggsave(
  here::here("output-plots/120-fig02_libor_shocks_subprime_rates_and_payments_mods.pdf"),
  plot = p.all, width = 9, height = 10, dpi = 600,
  device = cairo_pdf
)



