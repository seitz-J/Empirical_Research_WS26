# ------------------------------------------------------------------------------
# Tutorial 4 · Simulated panel inspired by Chen, Hung and Wang (2018, JAE):
# China's 2008 mandate to publish CSR reports and firm profitability (ROA)
#
# Please do NOT read this file before Part 5 of the tutorial.
# Here we play "nature": we decide the true causal effect, then we check
# which research design finds it. The numbers are simulated, not the paper's.
# ------------------------------------------------------------------------------

set.seed(2008)

n_firms <- 600
years   <- 2004:2011

firms <- tibble::tibble(
  firm_id   = sprintf("CN%04d", 1:n_firms),
  industry  = sample(c("Manufacturing", "Utilities", "Materials", "Consumer", "Technology"),
                     n_firms, replace = TRUE),
  size_base = rnorm(n_firms, 8, 1.3),        # log total assets
  quality   = rnorm(n_firms, 0, 1)           # UNOBSERVED: governance and management quality
) |>
  dplyr::mutate(
    log_sales = 0.95 * size_base - 0.2 + rnorm(n_firms, 0, 0.3),
    # THE MANDATE: firms in the governance index and the large-firm index must
    # publish a CSR report from fiscal year 2008 on. Inclusion favours large
    # firms with good governance.
    index_score = 0.8 * (size_base - 8) + 0.8 * quality + rnorm(n_firms, 0, 0.6),
    treated     = as.integer(index_score > stats::quantile(index_score, 0.75))
  )

# common shocks: the global financial crisis hits all firms in 2008-2010
year_effect <- tibble::tibble(year = years,
                              shock = c(0, 0.2, 0.3, 0.4, -0.6, -1.4, -0.4, 0.1))

df <- tidyr::expand_grid(firm_id = firms$firm_id, year = years) |>
  dplyr::left_join(firms, by = "firm_id") |>
  dplyr::left_join(year_effect, by = "year") |>
  dplyr::mutate(
    post       = as.integer(year >= 2008),
    treat_post = treated * post,
    size       = size_base + 0.05 * (year - 2004) + rnorm(dplyr::n(), 0, 0.1)
  )

# idiosyncratic noise: heavy tails, larger for small firms, persistent within a firm
df <- df |>
  dplyr::group_by(firm_id) |>
  dplyr::mutate(u = 1.6 * exp(-0.35 * (size_base - 8)) * rt(dplyr::n(), df = 4),
                e = as.numeric(stats::filter(u, 0.5, method = "recursive"))) |>
  dplyr::ungroup()

# CSR spending (in % of assets) rises because of the mandate -- the MECHANISM (a mediator)
csr_fun <- function(d, tp) pmax(0, 0.4 + 0.10 * (d$size - 8) + 0.20 * d$quality + 1.0 * tp + d$c_noise)
# ROA in %: hump-shaped in size, higher for well-governed firms
roa_fun <- function(d, tp, csr) {
  4 + 0.6 * (d$size - 8) - 0.35 * (d$size - 8)^2 + 1.2 * d$quality + d$shock +
    -0.3 * tp -                      # direct effect of the mandate
    0.6 * csr +                      # CSR spending reduces profits
    d$e
}

df$c_noise   <- rnorm(nrow(df), 0, 0.25)
df$csr_spend <- csr_fun(df, df$treat_post)
df$roa       <- roa_fun(df, df$treat_post, df$csr_spend)

# TRUE EFFECT (ATT): each mandated firm-year compared with its own
# counterfactual without the mandate (Y1 - Y0)
d1       <- df[df$treat_post == 1, ]
true_att <- mean(roa_fun(d1, 1, csr_fun(d1, 1)) - roa_fun(d1, 0, csr_fun(d1, 0)))

# what the researcher observes
csr_panel <- df |>
  dplyr::transmute(firm_id, industry, year, treated, post, treat_post,
                   size      = round(size, 3),
                   log_sales = round(log_sales, 3),
                   csr_spend = round(csr_spend, 3),
                   roa       = round(roa, 3))

rm(firms, year_effect, df, d1, csr_fun, roa_fun)
