## analyze/140-libor_mod_stage1_stage2.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("analyze/"))

suppressPackageStartupMessages({library(CLmisc); library(cowplot); })

##For data a cleaning script for the origination data
source(here::here("core/f_clean_bbx_orig_data.R"))
##For the regression strings common throughout the project.
source(here::here("core/rhs_strings.R"))

f_quarter_label <- function(x) paste0(year(x), "Q", quarter(x))

DT <- read_fst(
  here::here("data/DT_for_2sls_libor_diff.fst"),
  as.data.table = TRUE
) %>%
  .[FICOSCOREORIGINATIONCALC <= 660]

first.payment.adj.quarters <- unique(DT$first.payment.adj.quarter)

DT <- lapply(first.payment.adj.quarters, function(qtr) {
  DT[first.payment.adj.quarter == c(qtr)] %>% f_clean_bbx_orig_data
}) %>% rbindlist
  

## -- Performance Data  -- ##

DT.perf <- read_fst(
  here::here("data/DT_perf_post_cumsum.fst"),
  as.data.table = TRUE
) %>%
  .[, ACTIVITYDT := NULL] %>%
  .[, MONTHS_AFTER_FIRST_PAY_ADJ_DATE := as.character(MONTHS_AFTER_FIRST_PAY_ADJ_DATE)] %>%
  .[IS_LAST_LOAN_OBS == 1, MONTHS_AFTER_FIRST_PAY_ADJ_DATE := "last"] %>%
  .[, IS_LAST_LOAN_OBS := NULL] %>%
  ## See https://www.urban.org/urban-wire/understand-mortgage-default-rates-ask-these-three-questions
  .[, EVER_DEFAULT := as.integer(EVER_DAYS_DELIN180 + EVER_REO_FORC + EVER_LIQUIDATED_WITH_LOSS >= 1)] %>%
  .[, lingering.in.delin180 := as.integer(EVER_DAYS_DELIN180 & !EVER_REO_FORC & !EVER_LIQUIDATED_WITH_LOSS &
                                            !EVER_PAID_OFF)] %>%
  .[, lingering.in.delin150 := as.integer(EVER_DAYS_DELIN150 & !EVER_REO_FORC & !EVER_LIQUIDATED_WITH_LOSS &
                                            !EVER_PAID_OFF)]

vars.to.cast <- names(DT.perf) %>% .[grepl("^EVER|MBADELINQUENCYSTATUS|lingering.in.delin", x = .)]
DT.perf <- dcast(DT.perf, LOANID ~ MONTHS_AFTER_FIRST_PAY_ADJ_DATE, value.var = c(vars.to.cast))


##update so when there is a missing variable, it is replaced by the last observation
for (var in vars.to.cast) {
  var.name.last <- paste0(var, "_last")
  var.names <- names(DT.perf) %>% .[grepl(paste0("^", var), x = .)] %>%
    .[!grepl("_last$", x = .)]

  for (temp.var in var.names) {
    DT.perf[is.na(get(temp.var)), c(temp.var) := get(var.name.last)]
  }

}

##Make sure all of the required variables are binary
vars.to.binary <- names(DT.perf) %>% .[grepl("^EVER", x = .)]
DT.perf <- DT.perf[, c(vars.to.binary) := lapply(.SD, function(x) fifelse(x >= 1, 1, 0)),
              .SDcols = vars.to.binary]

DT <- merge(DT, DT.perf, by = "LOANID")

DT <- DT %>%
  .[,current.or.paid.off_12 := as.integer(EVER_PAID_OFF_12 >= 1 | MBADELINQUENCYSTATUS_12 %chin% c("C"))]%>%
  .[,current.or.paid.off_24 := as.integer(EVER_PAID_OFF_12 >= 1 | MBADELINQUENCYSTATUS_24 %chin% c("C"))]%>%
  .[,current.or.paid.off_36 := as.integer(EVER_PAID_OFF_12 >= 1 | MBADELINQUENCYSTATUS_36 %chin% c("C"))]%>%
  .[,current.or.paid.off_last := as.integer(EVER_PAID_OFF_12 >= 1 | MBADELINQUENCYSTATUS_last %chin% c("C"))]


#######################################################################################
## -- LIBOR first adjustment diff Interquartile range by first adjustment quarter -- ##
#######################################################################################

DT.subprime.libor.range <- DT %>%
  .[FICOSCOREORIGINATIONCALC <= 660,
    .(num.subprime.loans = .N,
      LIBOR_FIRST_MEAS_PAY_ADJ_DIFF.50 = median(LIBOR_FIRST_MEAS_PAY_ADJ_DIFF),
      LIBOR_FIRST_MEAS_PAY_ADJ_DIFF.25 = quantile(LIBOR_FIRST_MEAS_PAY_ADJ_DIFF, 0.25),
      LIBOR_FIRST_MEAS_PAY_ADJ_DIFF.75 = quantile(LIBOR_FIRST_MEAS_PAY_ADJ_DIFF, 0.75)),
    keyby = .(year.quarter = first.payment.adj.quarter)] %>%
  .[, LIBOR_FIRST_MEAS_PAY_ADJ_DIFF.75.25 := LIBOR_FIRST_MEAS_PAY_ADJ_DIFF.75 -
        LIBOR_FIRST_MEAS_PAY_ADJ_DIFF.25] %>%
  .[, num.subprime.loans.thousands := num.subprime.loans / 1000] %>%
  .[, year.quarter := paste0(year(year.quarter), "Q", quarter(year.quarter))]

num.subprime.loans.median <- DT.subprime.libor.range$num.subprime.loans %>%
  median
num.subprime.loans.mean <- DT.subprime.libor.range$num.subprime.loans %>%
  mean

p.libor.diff.range <- ggplot(DT.subprime.libor.range, aes(x = year.quarter)) +
  geom_errorbar(aes(ymin = LIBOR_FIRST_MEAS_PAY_ADJ_DIFF.25,
                    ymax = LIBOR_FIRST_MEAS_PAY_ADJ_DIFF.75,
                    color = num.subprime.loans),
                width = 0.3, size = 1.2) +
  ##geom_point(aes(y = LIBOR_FIRST_MEAS_PAY_ADJ_DIFF.50, size = num.subprime.loans.thousands)) +
  scale_y_continuous(breaks = seq(-10, 10, by = 0.5),
                     labels = scales::number_format(accuracy = 0.1, suffix = "%")) +
  labs(title = "Difference In 6-month LIBOR Between ARM First Measurement & Adjustment",
       subtitle = "Interquartile Range for the Change in 6m LIBOR between First Measurement &\nFirst Payment Adj. by First Payment Adj. Year-Quarter") +
  ## guides(size = guide_legend(title = "Num. of Subprime\nARM Loans",
  ##                            nrow = 2, byrow = TRUE)) +
  scale_color_gradient2(low = "red", mid = "grey50", high = "blue",
                        midpoint = num.subprime.loans.mean,
                        ##breaks = seq(10000, 65000, by = 15000),
                        labels = scales::number_format(big.mark = ","),
                        name = "Number of\nSubprime\nARM Loans",
                        ) +
  theme_cowplot_cl() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.80, 0.25),
        legend.key.height = unit(0.55, 'lines'),
        )

#########################################################################################
## -- Prob of Mod for Baseline Subprime ARM loans in the top quartile of libor.diff -- ##
#########################################################################################

DT.mod.for.libor.diff.top.quarter <- DT %>%
  .[FICOSCOREORIGINATIONCALC <= 660] %>%
  .[, LIBOR_FIRST_MEAS_PAY_ADJ_DIFF.75th := quantile(LIBOR_FIRST_MEAS_PAY_ADJ_DIFF, 0.75),
    by = first.payment.adj.quarter.char] %>%
  ##just the top quartile for subprime arm loans
  .[LIBOR_FIRST_MEAS_PAY_ADJ_DIFF >= LIBOR_FIRST_MEAS_PAY_ADJ_DIFF.75th] %>%
  felm(rate.actual.predicted.mod.5 ~ 0 + first.payment.adj.quarter.char | 0 | 0 | zip3, data = .) %>%
  felm_broom_tidy(.) %>%
  ##multiple by 100 for percent
  .[, `:=`(estimate = estimate * 100, std.error = std.error * 100)] %>%
  .[, first.payment.adj.quarter.char := gsub(".*([0-9]{4}-[0-9]{2}-01)$", "\\1", x = term)] %>%
  .[, year.quarter := f_quarter_label(as.Date(first.payment.adj.quarter.char))]

DT.reo.forc.for.libor.diff.top.quarter <- DT %>%
  .[FICOSCOREORIGINATIONCALC <= 660] %>%
  .[, LIBOR_FIRST_MEAS_PAY_ADJ_DIFF.75th := quantile(LIBOR_FIRST_MEAS_PAY_ADJ_DIFF, 0.75),
    by = first.payment.adj.quarter.char] %>%
  ##just the top quartile for subprime arm loans
  .[LIBOR_FIRST_MEAS_PAY_ADJ_DIFF >= LIBOR_FIRST_MEAS_PAY_ADJ_DIFF.75th] %>%
  felm(EVER_REO_FORC_36 ~ 0 + first.payment.adj.quarter.char | 0 | 0 | zip3, data = .) %>%
  felm_broom_tidy(.) %>%
  ##multiple by 100 for percent
  .[, `:=`(estimate = estimate * 100, std.error = std.error * 100)] %>%
  .[, first.payment.adj.quarter.char := gsub(".*([0-9]{4}-[0-9]{2}-01)$", "\\1", x = term)] %>%
  .[, year.quarter := f_quarter_label(as.Date(first.payment.adj.quarter.char))]

DT.libor.diff.top.quarter <- rbind(
  DT.mod.for.libor.diff.top.quarter[, Variable := "Modification Between\nFirst Measurement and\nFirst Payment Adjustment"],
  DT.reo.forc.for.libor.diff.top.quarter[, Variable := "REO Foreclosure 36\nMonths After First\nPayment Adjustment"]
)


p.mod.forc.for.libor.diff.top.quarter <- ggplot(
  DT.libor.diff.top.quarter,
  aes(x = year.quarter)) +
  geom_errorbar(aes(ymin = estimate - 2.5 * std.error,
                    ymax = estimate + 2.5 * std.error,
                    color = Variable),
                width = 0.3, size = 1.1, alpha = 0.6) +
  geom_point(aes(y = estimate, color = Variable), size = 2.5, alpha = 0.7) +
  scale_color_manual(values = c("darkblue", "darkred")) +
  scale_y_continuous(labels = scales::number_format(suffix = "%")) +
  labs(title = "Prob. of Modifcation or REO Forc for Baseline Subprime ARM Loans",
       subtitle = "Loans in the top Quartile of the LIBOR Change Between First Measurement &\nFirst Payment Adj. Within Each First Payment Adj. Year-Quarter") +
  theme_cowplot_cl() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.2, 0.5),
        legend.key.height = unit(2.55, 'lines'),
        legend.title = element_blank(),
        ##brilliant answer to remove space left by deleted legend title
        ##https://github.com/tidyverse/ggplot2/issues/3587#issuecomment-546700387
        legend.margin = margin(0, 2, 1, 0),
        legend.spacing.y = unit(0, "pt"),
        )

## plot everything
p.libor.range.prob.mod.grid <- plot_grid(
  p.libor.diff.range, p.mod.forc.for.libor.diff.top.quarter,
  align = "hv", nrow = 2)

##Regression to run for payment adjustment quarter
f_reg_pay_adj_quarter <- function(temp.first.pay.adj.qrtr) {

  DT.temp <- DT[first.payment.adj.quarter.char == c(temp.first.pay.adj.qrtr) & FICOSCOREORIGINATIONCALC <= 660]

  ##helper function
  f_reg <- function(lhs.var) {
    
    ## First stage and reduced form --- "0" is for no instruments 
    reg.formula <- sprintf(rhs.first.pay.adj, "0") %>%
      paste0(lhs.var, " ~ LIBOR_FIRST_MEAS_PAY_ADJ_DIFF + ", .) %>%
      as.formula

    DT.reg.output <- eval(bquote(felm(.(reg.formula), data = DT.temp))) %>%
      felm_broom_tidy %>%
      .[grepl("LIBOR_FIRST_MEAS_PAY_ADJ_DIFF", term)] %>%
      .[, lhs.var := c(lhs.var)] %>%
      .[, first.payment.adj.quarter := as.Date(temp.first.pay.adj.qrtr)] %>%
      setcolorder(c("lhs.var", "first.payment.adj.quarter", "term"))

    return(DT.reg.output)

  }

  ##For reduced form regressions and first stage
  lhs.vars <- c(##"rate.actual.predicted",
                "rate.actual.predicted.mod.1.0",
                ##"rate.actual.predicted.mod.5", "rate.actual.predicted.mod.25",
                ##"MOD_LENDER", "MOD_ANY",
                "ln.rate.payment.actual.predicted")

  DT.reg.first.pay.adj.quarter <- lapply(lhs.vars, f_reg) %>%
    rbindlist

  return(DT.reg.first.pay.adj.quarter)

}


DT.reg.stage1.reduced.output <- lapply(unique(DT$first.payment.adj.quarter.char),
                        f_reg_pay_adj_quarter) %>%
  rbindlist(use.names = TRUE) %>%
  setkey(lhs.var, first.payment.adj.quarter, term) %>%
  .[, year.quarter := paste0(year(first.payment.adj.quarter), "Q", quarter(first.payment.adj.quarter))]

f_plot_stage1 <- function(lhs.var.temp) {

  ##Get the regression data that we need and combine to estiamte the magnitude of the
  ##effects
  DT.reg.mod <- DT.reg.stage1.reduced.output[lhs.var == lhs.var.temp] %>%
    .[, `:=`(estimate = estimate * 100, std.error = std.error * 100)] %>%
    ##the scaled estimate going from the 25th to the 75 percentile
    merge(DT.subprime.libor.range[, .(year.quarter, LIBOR_FIRST_MEAS_PAY_ADJ_DIFF.75.25)],
          by = "year.quarter") %>%
    .[, `:=`(estimate.libor.diff.75.25 = estimate * LIBOR_FIRST_MEAS_PAY_ADJ_DIFF.75.25,
             std.error.libor.diff.75.25 = std.error * abs(LIBOR_FIRST_MEAS_PAY_ADJ_DIFF.75.25))] %>%
    ##the increase in the modification rate going from the 75th to 25th percentile
    merge(DT.mod.for.libor.diff.top.quarter[, .(year.quarter, mean.mod.for.top.libor.diff.quartile = estimate)],
          by = "year.quarter") %>%
    .[, estimate.libor.diff.75.25.mod.top.quartile :=
          -1 * estimate.libor.diff.75.25 / mean.mod.for.top.libor.diff.quartile * 100] %>%
    ## .[statistic ^ 2 < 6.25, estimate.libor.diff.75.25.mod.top.quartile := NA]
    .[abs(statistic) <= 2.5, estimate.libor.diff.75.25.mod.top.quartile := NA]
    

  ##Melted reg output data
  DT.reg.mod.melted <- DT.reg.mod %>%
    .[, .(year.quarter, estimate, estimate.libor.diff.75.25.mod.top.quartile)] %>%
    melt(id = "year.quarter", variable.name = "estimate.type", variable.factor = FALSE) %>%
    .[, estimate.type := fifelse(
      estimate.type == "estimate",
      "Regression Estimate",
      "Estimated Interquartile\nChange in Modification\nRate Due to LIBOR Change")] %>%
    .[, estimate.type := factor(
      estimate.type,
      levels = c("Regression Estimate",
                 "Estimated Interquartile\nChange in Modification\nRate Due to LIBOR Change"))]

  ##plot reg
  p.mod <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dotted", size = 1.1) +
    geom_errorbar(data = DT.reg.mod,
                  mapping = aes(x = year.quarter,
                                ymin = estimate - 2.5 * std.error,
                                ymax = estimate + 2.5 * std.error),
                  width = 0.15, size = 1.2, alpha = 0.6, color = "darkblue") +
    geom_point(data = DT.reg.mod.melted,
               mapping = aes(x = year.quarter, y = value, color = estimate.type),
               size = 4, alpha = 0.7) +
    ##for the lollipop
    geom_segment(data = DT.reg.mod,
                 mapping = aes(x = year.quarter, xend = year.quarter,
                               y = 0,
                               yend = estimate.libor.diff.75.25.mod.top.quartile),
                 color = "darkred", alpha = 0.6, size = 1.05) +
    scale_color_manual(values = c("darkblue", "darkred")) +
    scale_y_continuous(breaks = seq(-50, 100, by = 10),
                       labels = scales::number_format(suffix = "%")) +
    theme_cowplot_cl() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          ##legend.key.size to increase space between legend items
          ##from https://stackoverflow.com/a/50593988
          legend.key.height = unit(1.7, 'lines'),
          legend.margin = margin(0, 2, 2, 2),
          ##brilliant answer to remove space left by deleted legend title
          ##https://github.com/tidyverse/ggplot2/issues/3587#issuecomment-546700387
          legend.spacing.y = unit(0, "pt")
          )

}

## p.mod.0.5 <- f_plot_stage1(lhs.var.temp = "rate.actual.predicted.mod.5") +
##   theme(legend.position = c(0.1, 0.1))
p.mod.1.0 <- f_plot_stage1(lhs.var.temp = "rate.actual.predicted.mod.1.0") +
  theme(legend.position = c(0.01, 0.1))



##########################
## -- 2SLS Estimates -- ##
##########################

f_2sls <- function(temp.lhs.var, temp.endog.var) {

  ##helper function
  f_reg <- function(temp.first.pay.adj.qrtr) {

    ##make sure f.stat is bigger than 10 or return an empty data.table
    f.stat <- DT.reg.stage1.reduced.output %>%
      .[lhs.var == c(temp.endog.var) & first.payment.adj.quarter == c(temp.first.pay.adj.qrtr),
        statistic ^ 2]
    if (f.stat < 6.25) return(data.table(first.payment.adj.quarter = as.Date(temp.first.pay.adj.qrtr)))


    DT.temp <- DT %>%
      .[first.payment.adj.quarter.char == c(temp.first.pay.adj.qrtr) & FICOSCOREORIGINATIONCALC <= 660]

    stage1.formula <- sprintf("(%s ~ LIBOR_FIRST_MEAS_PAY_ADJ_DIFF)", temp.endog.var)
    
    ## First stage and reduced form --- "0" is for no instruments
    reg.formula <- sprintf(rhs.first.pay.adj, stage1.formula) %>%
      paste0(temp.lhs.var, " ~ ", .) %>%
      as.formula

    DT.reg.output <- eval(bquote(felm(.(reg.formula), data = DT.temp, exactDOF = FALSE))) %>%
      felm_broom_tidy %>%
      .[grepl(temp.endog.var, term)] %>%
      .[, lhs.var := c(temp.lhs.var)] %>%
      .[, endog.var := c(temp.endog.var)] %>%
      .[, first.payment.adj.quarter := as.Date(temp.first.pay.adj.qrtr)] %>%
      .[, stage1.fstat := c(f.stat)]

    return(DT.reg.output)

  }

  DT.reg.output.all <- lapply(unique(DT$first.payment.adj.quarter.char),
                              f_reg) %>%
    rbindlist(use.names = TRUE, fill = TRUE) %>%
    setcolorder(c("lhs.var", "endog.var", "first.payment.adj.quarter", "term")) %>%
    setkey(lhs.var, endog.var, first.payment.adj.quarter) %>%
    .[, year.quarter := paste0(year(first.payment.adj.quarter), "Q", quarter(first.payment.adj.quarter))] %>%
    .[order(first.payment.adj.quarter)]

}

f_plot_2sls <- function(DT.reg.2sls) {
  p.out <- ggplot(DT.reg.2sls, aes(x = year.quarter)) +
    geom_hline(yintercept = 0, linetype = "dotted", size = 1.1) +
    geom_errorbar(aes(ymin = estimate - 2.5 * std.error,
                      ymax = estimate + 2.5 * std.error),
                  width = 0.15, size = 1.2, alpha = 0.6, color = "darkblue") +
    geom_point(aes(y = estimate), color = "darkblue", alpha = 0.7, size = 4) +
    theme_cowplot_cl() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()
          )
}



##Rate payments
DT.rate.payment.log.2sls <- f_2sls(temp.lhs.var = "ln.rate.payment.actual.predicted",
                                   temp.endog.var = "rate.actual.predicted.mod.1.0")

p.rate.payment.log.2sls <- f_plot_2sls(DT.rate.payment.log.2sls) +
  labs(title = "2SLS Estiamtes - The Impact of LIBOR Change-Induced Modifications\non Interest Rate Payments",
       subtitle = "LHS Var: Log(Interest Rate Payment), Actual \u2013 Expected\nEndogenous Var: Interest Rate Modification Indicator\nInstrument: LIBOR Change Between First Measurement & First Adj.")

DT.rate.payment.dollar.2sls <- f_2sls(temp.lhs.var = "rate.payment.actual.predicted",
                                      temp.endog.var = "rate.actual.predicted.mod.1.0")

p.rate.payment.dollar.2sls.simple <- ggplot(
  DT.rate.payment.dollar.2sls, aes(x = year.quarter, y = estimate)) +
  geom_point(color = "darkblue", alpha = 0.7, size = 3) +
  scale_y_continuous(breaks = c(-350, -550, -750, -950, -1150), labels = function(x) paste0("-$", -1 * x)) +
  theme_cowplot_cl() +
  theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()
          )

p.rate.payment.dollar.2sls <- f_plot_2sls(DT.rate.payment.dollar.2sls) +
  labs(title = "2SLS Estiamtes - The Impact of LIBOR Change-Induced Modifications\non Interest Rate Payments",
       subtitle = "LHS Var: Rate Payment ($), Actual - Predicted; Endogenous Var: Interest Rate Mod\nInstrument: LIBOR Change Between First Measurement & First Adj.") +
  scale_y_continuous(labels = function(x) paste0("$", x))

p.all <- list(
  p.libor.diff.range = p.libor.diff.range,
  p.mod.forc.for.libor.diff.top.quarter = p.mod.forc.for.libor.diff.top.quarter,
  p.mod.1.0 = p.mod.1.0,
  p.rate.payment.log.2sls = p.rate.payment.log.2sls,
  p.rate.payment.dollar.2sls = p.rate.payment.dollar.2sls,
  p.rate.payment.dollar.2sls.simple = p.rate.payment.dollar.2sls.simple
)


mkdir_p(here::here("work"))
saveRDS(p.all, here::here("work/140-p_baseline_stage1_stage2.rds"))
