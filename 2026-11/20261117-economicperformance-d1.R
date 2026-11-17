# Real GDP, real investment, real wages, wage share and wage inequality
# France, Germany, Greece, Portugal, Spain, Italy
#
# Data (all Eurostat, via the public API, no key needed):
#   namq_10_gdp      quarterly national accounts: GDP, GFCF, compensation
#                    of employees
#   namq_10_a10_e    quarterly national accounts employment: employees,
#                    total employment and hours worked
#   prc_hicp_minr    HICP, all items, monthly (ECOICOP ver. 2, 2025=100)
#   earn_ses_hourly  Structure of Earnings Survey (4-yearly: 2014, 2018,
#                    2022): first and ninth decile of gross hourly earnings

# install.packages(c("eurostat", "dplyr", "tidyr", "ggplot2", "lubridate",
#                    "remotes"))
# remotes::install_github("jimjam-slam/ggflags")   # circular flags
library(eurostat)
library(ggflags)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

countries <- c(FR = "France", DE = "Germany", EL = "Greece",
               PT = "Portugal", ES = "Spain", IT = "Italy")

panels <- c(
  gdp        = "Real GDP\n(index: 2018=100)",
  gfcf       = "Real gross fixed capital formation\n(index: 2018=100)",
  prod_hour  = "Labour productivity per hour worked\n(index: 2018=100)",
  real_wage  = "Real compensation per employee\n(index: 2018=100)",
  wage_share = "Adjusted wage share\n(% of GDP)",
  d9d1       = "Earnings inequality\n(D9/D1 ratio, gross hourly earnings)"
)

# Newer {eurostat} versions name the time column TIME_PERIOD
fix_time <- function(d) rename(d, any_of(c(time = "TIME_PERIOD")))

# Not every country publishes every series with the same seasonal
# adjustment. Per series, keep the best one available:
#   SCA (seasonally + calendar adjusted) > SA (seasonally adjusted) > NSA.
# Unadjusted series are turned into a 4-quarter moving average so that
# bonus months (e.g. 13th/14th salaries in Q2/Q4) do not create a sawtooth.
pick_adjustment <- function(d, ...) {
  d |>
    filter(!is.na(values)) |>
    mutate(adj_rank = match(s_adj, c("SCA", "SA", "NSA"))) |>
    group_by(geo, ...) |>
    filter(adj_rank == min(adj_rank)) |>
    arrange(time, .by_group = TRUE) |>
    mutate(values = if (first(s_adj) == "NSA")
      as.numeric(stats::filter(values, rep(1 / 4, 4), sides = 1))
      else values) |>
    ungroup()
}

# ---- 1. National accounts (Eurostat, quarterly) ----------------------------
# B1GQ = GDP, P51G = gross fixed capital formation, D1 = compensation of
# employees.
# Current prices (CP_MEUR) and chain-linked volumes (reference year 2020,
# or 2010 where Eurostat has not yet switched; the newer one is kept).
na_raw <- suppressWarnings(get_eurostat(
  "namq_10_gdp",
  filters = list(
    geo     = names(countries),
    na_item = c("B1GQ", "P51G", "D1"),
    unit    = c("CP_MEUR", "CLV20_MEUR", "CLV10_MEUR"),
    s_adj   = c("SCA", "SA", "NSA")
  ),
  time_format = "date", cache = FALSE
)) |> fix_time()

na_sel <- na_raw |>
  mutate(price = if_else(unit == "CP_MEUR", "CP", "CLV")) |>
  group_by(geo, na_item, price) |>
  filter(unit == max(unit)) |>              # "CLV20_MEUR" > "CLV10_MEUR"
  ungroup() |>
  pick_adjustment(na_item, price)

# ---- 2. Employment (Eurostat, quarterly) -----------------------------------
# SAL_DC = employees, EMP_DC = total employment (domestic concept)
# THS_PER = thousand persons, THS_HW = thousand hours worked
emp_raw <- suppressWarnings(get_eurostat(
  "namq_10_a10_e",
  filters = list(
    geo     = names(countries),
    na_item = c("SAL_DC", "EMP_DC"),
    unit    = c("THS_PER", "THS_HW"),
    nace_r2 = "TOTAL",
    s_adj   = c("SCA", "SA", "NSA")
  ),
  time_format = "date", cache = FALSE
)) |> fix_time()

emp_sel <- pick_adjustment(emp_raw, na_item, unit)

# Diagnostic: which adjustment was used for each country and series?
bind_rows(
  distinct(na_sel,  geo, item = paste(na_item, price), s_adj),
  distinct(emp_sel, geo, item = paste(na_item, unit), s_adj)
) |>
  pivot_wider(names_from = item, values_from = s_adj) |>
  print()

# ---- 2b. Consumer prices (Eurostat HICP, monthly -> quarterly) -------------
# Since January 2026 the HICP is published under ECOICOP ver. 2 in
# prc_hicp_minr (all items = "TOTAL", index 2025 = 100); the older
# prc_hicp_midx stops in December 2025.
hicp_raw <- suppressWarnings(get_eurostat(
  "prc_hicp_minr",
  filters = list(geo = names(countries), coicop18 = "TOTAL", unit = "I25"),
  time_format = "date", cache = FALSE
)) |> fix_time()

hicp_q <- hicp_raw |>
  filter(!is.na(values)) |>
  mutate(time = floor_date(time, "quarter")) |>
  group_by(geo, time) |>
  filter(n() == 3) |>                        # complete quarters only
  summarise(hicp = mean(values), .groups = "drop")

# The HICP is not seasonally adjusted, while compensation is. Remove the
# (small) seasonal pattern with a ratio-to-moving-average: quarterly factors
# are the average deviation of log HICP from its centred 4-quarter moving
# average since 2010, normalised to sum to zero.
seasonal <- hicp_q |>
  group_by(geo) |>
  arrange(time, .by_group = TRUE) |>
  mutate(
    trend = as.numeric(stats::filter(log(hicp), c(1, 2, 2, 2, 1) / 8, sides = 2)),
    dev   = log(hicp) - trend,
    q     = quarter(time)
  ) |>
  filter(year(time) >= 2010) |>
  group_by(geo, q) |>
  summarise(sf = mean(dev, na.rm = TRUE), .groups = "drop_last") |>
  mutate(sf = sf - mean(sf)) |>
  ungroup()

hicp <- hicp_q |>
  mutate(q = quarter(time)) |>
  left_join(seasonal, by = c("geo", "q")) |>
  transmute(geo, time, HICP = hicp * exp(-sf))

# ---- 3. Derived quarterly indicators ---------------------------------------
na <- na_sel |>
  transmute(geo, time, key = paste(na_item, price, sep = "_"), values) |>
  pivot_wider(names_from = key, values_from = values)

emp <- emp_sel |>
  transmute(geo, time, key = paste(na_item, unit, sep = "_"), values) |>
  pivot_wider(names_from = key, values_from = values)

index_2018 <- function(x, t) 100 * x / mean(x[year(t) == 2018], na.rm = TRUE)

quarterly <- na |>
  left_join(emp, by = c("geo", "time")) |>
  left_join(hicp, by = c("geo", "time")) |>
  group_by(geo) |>
  arrange(time, .by_group = TRUE) |>
  mutate(
    gdp  = index_2018(B1GQ_CLV, time),
    gfcf = index_2018(P51G_CLV, time),
    # real GDP per hour worked by all persons employed
    prod_hour = index_2018(B1GQ_CLV / EMP_DC_THS_HW, time),
    # compensation per employee deflated by the (seasonally adjusted) HICP
    real_wage = index_2018((D1_CP / SAL_DC_THS_PER) / HICP, time),
    # adjusted wage share: imputes the average employee wage to the
    # self-employed (AMECO / European Commission definition)
    wage_share = 100 * (D1_CP / SAL_DC_THS_PER) * EMP_DC_THS_PER / B1GQ_CP
  ) |>
  ungroup() |>
  select(geo, time, gdp, gfcf, prod_hour, real_wage, wage_share) |>
  pivot_longer(-c(geo, time), names_to = "series", values_to = "value") |>
  filter(time >= as.Date("2015-01-01"))

# ---- 4. Earnings inequality (Eurostat SES, 4-yearly) -----------------------
# Harmonised across countries: enterprises with 10+ employees, NACE B-S
# excluding public administration (O), all employees, all occupations.
ses_raw <- suppressWarnings(get_eurostat(
  "earn_ses_hourly",
  filters = list(
    geo      = names(countries),
    nace_r2  = "B-S_X_O",
    isco08   = "TOTAL",
    worktime = "TOTAL",
    age      = "TOTAL",
    sex      = "T",
    indic_se = c("D1_E_EUR", "D9_E_EUR")
  ),
  time_format = "date", cache = FALSE
)) |> fix_time()

inequality <- ses_raw |>
  filter(!is.na(values), year(time) >= 2014) |>
  select(geo, time, indic_se, values) |>
  pivot_wider(names_from = indic_se, values_from = values) |>
  transmute(
    geo,
    time   = as.Date(paste0(year(time), "-10-01")),  # SES reference: October
    series = "d9d1",
    value  = D9_E_EUR / D1_E_EUR
  )

# ---- 5. Combine ------------------------------------------------------------
df <- bind_rows(quarterly, inequality) |>
  filter(!is.na(value)) |>
  # common window within each indicator: start at the latest first
  # observation and end at the earliest last observation across countries
  group_by(series, geo) |>
  mutate(first_obs = min(time), last_obs = max(time)) |>
  group_by(series) |>
  filter(time >= max(first_obs), time <= min(last_obs)) |>
  ungroup() |>
  select(-first_obs, -last_obs) |>
  mutate(
    country = factor(countries[geo], levels = countries),
    panel   = factor(panels[series], levels = panels)
  )

# endpoint of each series, for the flags.
# {ggflags} uses lower-case ISO 2 codes: Greece is "gr" (Eurostat uses "EL")
flag_codes <- c(FR = "fr", DE = "de", EL = "gr", PT = "pt", ES = "es", IT = "it")

ends <- df |>
  group_by(series, country) |>
  slice_max(time, n = 1) |>
  ungroup() |>
  mutate(flag = flag_codes[geo])

# Repel the flags: {ggrepel} only moves text/labels, not images, so the
# flags are spread vertically here and joined to the line ends by a thin
# leader line. Gap and horizontal offset are shares of each panel's range.
flag_gap    <- 0.09   # minimum vertical distance between flags
flag_offset <- 0.06   # horizontal distance of the flags from the line ends

repel_1d <- function(y, gap) {
  o  <- order(y)
  ys <- y[o]
  for (i in seq_along(ys)[-1]) ys[i] <- max(ys[i], ys[i - 1] + gap)
  ys <- ys - (mean(ys) - mean(y[o]))   # re-centre on the original values
  out <- y
  out[o] <- ys
  out
}

panel_span <- df |>
  group_by(series) |>
  summarise(y_span = diff(range(value)),
            x_span = as.numeric(diff(range(time))))

ends <- ends |>
  left_join(panel_span, by = "series") |>
  group_by(series) |>
  mutate(
    flag_y = repel_1d(value, flag_gap * first(y_span)),
    flag_x = max(time) + flag_offset * first(x_span)
  ) |>
  ungroup()

latest  <- max(df$time[df$series != "d9d1"])
caption <- paste0(
  "Sources: Eurostat (quarterly national accounts (namq_10_gdp, namq_10_a10_e); ",
  "HICP (prc_hicp_minr); ",
  "Structure of Earnings Survey 2014, 2018, 2022 (earn_ses_hourly))\n",
  "Real wages include social contributions. ",
  "Latest observation: Q", quarter(latest), " ", year(latest), "."
)

# Align the 100 line across the index panels (2018 = 100): give every
# index panel a y range in which 100 sits at the same relative height.
# Each range is the smallest one that still holds that panel's data and
# flags; the common height is the average of the panels' own positions.
index_series <- c("gdp", "gfcf", "prod_hour", "real_wage")

span_100 <- bind_rows(
  select(df, series, value),
  transmute(ends, series, value = flag_y)
) |>
  filter(series %in% index_series) |>
  group_by(series) |>
  summarise(below = 100 - min(value), above = max(value) - 100)

pos_100 <- with(span_100, mean(below / (below + above)))

align_100 <- span_100 |>
  mutate(
    range = pmax(below / pos_100, above / (1 - pos_100)),
    lo    = 100 - pos_100 * range,
    hi    = 100 + (1 - pos_100) * range
  ) |>
  select(series, lo, hi) |>
  pivot_longer(c(lo, hi), values_to = "value") |>
  left_join(summarise(group_by(df, series), time = max(time)), by = "series") |>
  mutate(panel = factor(panels[series], levels = panels))

ref_100 <- tibble(panel = factor(panels[index_series], levels = panels))

country_cols <- c(
  France   = "#1f3a93",
  Germany  = "#b2182b",
  Greece   = "#f03b20",
  Portugal = "#29a8e0",
  Spain    = "#fdb827",
  Italy    = "#4daf4a"
)

# ---- 6. Plot ---------------------------------------------------------------
# The flags are detailed vector drawings (36 of them) and are slow to draw.
# RStudio's plot pane can hit its time limit and show a broken image, so
# with show_flags = TRUE the plot is saved to PNG and shown in RStudio's
# Viewer tab; set it to FALSE for quick drafts in the Plots pane.
show_flags <- TRUE

flag_layers <- if (show_flags) list(
  # leader line from the last observation to the (repelled) flag
  geom_segment(data = ends,
               aes(x = time, y = value, xend = flag_x, yend = flag_y),
               colour = "grey60", linewidth = 0.2, inherit.aes = FALSE),
  # circular flag
  geom_flag(data = ends, aes(flag_x, flag_y, country = flag),
            size = 4, inherit.aes = FALSE),
  scale_country(guide = "none")
)

p <- ggplot(df, aes(time, value, colour = country)) +
  geom_line(linewidth = 0.9) +
  # the survey is 4-yearly, so mark the actual observations
  geom_point(data = filter(df, series == "d9d1"), size = 2) +
  # 100 reference line and invisible range points that align it
  geom_hline(data = ref_100, aes(yintercept = 100),
             colour = "grey55", linewidth = 0.3) +
  geom_blank(data = align_100, aes(time, value), inherit.aes = FALSE) +
  flag_layers +
  facet_wrap(~ panel, scales = "free", ncol = 3) +
  scale_x_date(
    breaks = seq(as.Date("2015-01-01"), as.Date("2025-01-01"), by = "2 years"),
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.03))   # room for the flags
  ) +
  coord_cartesian(clip = "off") +
  scale_colour_manual(values = country_cols) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(x = NULL, y = NULL, colour = NULL, caption = caption) +
  theme_void() +
  theme(
    axis.text        = element_text(size = 10, margin = margin(3, 3, 3, 3)),
    panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.3),
    strip.text       = element_text(size = 12, face = "bold",
                                    margin = margin(b = 8)),
    legend.position  = "top",
    legend.text      = element_text(size = 10),
    plot.caption     = element_text(hjust = 0, size = 8,
                                    margin = margin(t = 10)),
    plot.caption.position = "plot",
    panel.spacing    = unit(2, "lines"),
    plot.margin      = margin(10, 10, 10, 10)
  )

if (show_flags) {
  # draw to a file (no time limit), then display the PNG in the Viewer tab
  out_png <- "crisis_economies_wages.png"
  ggsave(out_png, p, width = 15, height = 9, dpi = 300, bg = "white")
  
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    # the Viewer only shows HTML files in the session temp folder,
    # so wrap a copy of the PNG in a minimal HTML page there
    view_dir <- tempfile("plotview")
    dir.create(view_dir)
    file.copy(out_png, file.path(view_dir, "plot.png"))
    writeLines('<img src="plot.png" style="width:100%">',
               file.path(view_dir, "index.html"))
    rstudioapi::viewer(file.path(view_dir, "index.html"))
  } else {
    message("Plot saved to ", normalizePath(out_png))
  }
} else {
  print(p)
}