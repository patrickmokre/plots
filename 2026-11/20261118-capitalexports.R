# Saving, net non-financial investment by sector, and net lending to RoW
# Euro area (Eurostat) vs United States (Fed Z.1 Integrated Macroeconomic Accounts)
# 4-quarter rolling sums, national currency bn, Q4 2002 onwards

library(eurostat)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(ggplot2)

start_date <- as.Date("2002-10-01")
levs <- c("Net lending\nto rest of world", "Household", "Corporate", "Government")

roll4 <- function(x) as.numeric(stats::filter(x, rep(1, 4), sides = 1))

# =============================================================================
# 1. Euro area: Eurostat nasq_10_nf_tr
# =============================================================================
raw_ea <- get_eurostat(
  "nasq_10_nf_tr",
  filters = list(
    unit    = "CP_MEUR",
    s_adj   = "NSA",
    sector  = c("S1", "S11", "S12", "S13", "S14_S15"),
    na_item = c("P5G", "NP", "P51C", "B9")
  ),
  time_format = "date",
  cache = FALSE
)
if ("TIME_PERIOD" %in% names(raw_ea)) raw_ea <- rename(raw_ea, time = TIME_PERIOD)

geo_pick <- intersect(c("EA21", "EA20", "EA19", "EA"), unique(raw_ea$geo))[1]

wide_ea <- raw_ea |>
  filter(geo == geo_pick) |>
  group_by(sector, na_item, time) |>
  arrange(desc(direct == "PAID"), .by_group = TRUE) |>   # prefer uses side
  slice(1) |>
  ungroup() |>
  select(sector, na_item, time, values) |>
  pivot_wider(names_from = na_item, values_from = values)

ea <- bind_rows(
  wide_ea |>
    filter(sector != "S1") |>
    mutate(
      value = P5G + coalesce(NP, 0) - P51C,              # net non-fin. investment
      component = case_when(
        sector == "S13"             ~ "Government",
        sector %in% c("S11", "S12") ~ "Corporate",
        TRUE                        ~ "Household"
      )
    ) |>
    group_by(component, time) |>
    summarise(value = sum(value), .groups = "drop"),
  wide_ea |>
    filter(sector == "S1") |>
    transmute(component = levs[1], time, value = B9)
) |>
  mutate(region = "Euro area, EUR bn")

# =============================================================================
# 2. United States: Fed Z.1 IMA tables via FRED (NSA quarterly flows, USD mn)
#    "Capital formation, net" = GFCF + inventories + non-produced assets - CFC
# =============================================================================
us_series <- tribble(
  ~id,                    ~component,  ~sign, # Z.1 code / sector
  "BOGZ1FU155050905Q",    "Household",   1,   # FA155050905 households & NPISH
  "BOGZ1FU115050985Q",    "Household",   1,   # FA115050985 nonfin. noncorporate business
  "BOGZ1FU105050985Q",    "Corporate",   1,   # FA105050985 nonfin. corporate business
  "BOGZ1FU795015085Q",    "Corporate",   1,   # FA795015085 domestic financial sectors
  "BOGZ1FU315050905Q",    "Government",  1,   # FA315050905 federal government
  "BOGZ1FU205050905Q",    "Government",  1,   # FA205050905 state & local governments
  "BOGZ1FU265000905Q",    levs[1],      -1    # FA265000905 RoW net lending (sign flipped)
)

read_fred <- function(id) {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", id)
  read_csv(url, col_types = cols(.default = "c"), na = c("", ".")) |>
    setNames(c("time", "value")) |>
    mutate(id = id, time = as.Date(time), value = as.numeric(value))
}

raw_us <- map(us_series$id, \(id) tryCatch(read_fred(id), error = \(e) {
  warning("FRED download failed for ", id, ": ", conditionMessage(e)); NULL
})) |> list_rbind()

us <- raw_us |>
  inner_join(us_series, by = "id") |>
  group_by(component, time) |>
  summarise(value = sum(sign * value), n = n(), .groups = "drop") |>
  # keep only quarters where every constituent series is present
  inner_join(count(us_series, component, name = "n_needed"), by = "component") |>
  filter(n == n_needed) |>
  select(component, time, value) |>
  mutate(region = "United States, USD bn")

# =============================================================================
# 3. Combine, 4-quarter rolling sums, bn
# =============================================================================
plot_df <- bind_rows(ea, us) |>
  arrange(region, component, time) |>
  group_by(region, component) |>
  mutate(value = roll4(value) / 1000) |>
  ungroup() |>
  filter(time >= start_date, !is.na(value)) |>
  mutate(component = factor(component, levels = levs),
         region    = factor(region, levels = c("Euro area, EUR bn",
                                               "United States, USD bn")))

# direct labels next to each facet's last bar (positive segments only)
labs_df <- plot_df |>
  group_by(region) |>
  filter(time == max(time)) |>
  arrange(region, desc(component)) |>                    # bottom to top
  mutate(y = cumsum(pmax(value, 0)) - pmax(value, 0) / 2,
         # negative net lending: place label in the middle of the negative bar
         y = if_else(value < 0, value / 2, y)) |>
  ungroup()

crisis <- tibble(region = factor("Euro area, EUR bn", levels = levels(plot_df$region)),
                 xmin = as.Date("2010-01-01"), xmax = as.Date("2010-10-01"))

# =============================================================================
# 4. Plot
# =============================================================================
cols <- c(
  "Net lending\nto rest of world" = "#3049FF",
  "Household"  = "#A6A6A6",
  "Corporate"  = "#5E5E5E",
  "Government" = "#222222"
)

ggplot(plot_df, aes(time, value, fill = component)) +
  geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.3) +
  geom_col(width = 75, position = "stack") +
  geom_text(data = labs_df,
            aes(x = time + 90, y = y, label = component),
            inherit.aes = FALSE, hjust = 0, lineheight = 0.9, size = 3.2) +
  facet_wrap(~ region, ncol = 1, scales="fixed") +
  scale_fill_manual(values = cols) +
  scale_x_date(breaks = as.Date(paste0(seq(2005, 2025, 5), "-01-01")),
               date_labels = "%Y", expand = expansion(mult = c(0.01, 0))) +
  scale_y_continuous(breaks = scales::breaks_extended(n = 6)) +
  coord_cartesian(clip = "off") +
  labs(caption = paste0(
    "Sources: Euro area (", geo_pick, ") - Eurostat nasq_10_nf_tr, CP_MEUR, NSA; ",
    "P5G + NP - P51C for S11+S12, S13, S14_S15; B9 for S1.\n",
    "United States - Federal Reserve Z.1 Integrated Macroeconomic Accounts via FRED, NSA: ",
    "capital formation, net (FA155050905, FA115050985, FA105050985, FA795015085,\n",
    "FA315050905, FA205050905); net lending = -FA265000905 (rest of world). ",
    "Net non-financial investment by sector. 4-quarter rolling sums, national currency bn."
  )) +
  theme_void(base_size = 12) +
  theme(
    legend.position     = "none",
    strip.text          = element_text(hjust = 0, face = "bold",
                                       margin = margin(b = 6, t = 6)),
    panel.spacing.y     = unit(18, "pt"),
    panel.grid.major.y  = element_line(colour = "grey92", linewidth = 0.3),
    axis.text.x         = element_text(margin = margin(t = 4)),
    axis.text.y         = element_text(hjust = 1, margin = margin(r = 6)),
    axis.ticks.x        = element_line(colour = "grey60"),
    axis.ticks.length.x = unit(4, "pt"),
    plot.caption        = element_text(hjust = 0, size = 7.5, colour = "grey40",
                                       lineheight = 1.1, margin = margin(t = 12)),
    plot.caption.position = "plot",
    plot.margin         = margin(10, 120, 10, 10)
  )
