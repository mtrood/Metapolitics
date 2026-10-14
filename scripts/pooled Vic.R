#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Meta-politics: Pooled Primary votes and 2PP - Victoria

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# I) Settings ------------------------------------------------------------------
window_weeks   <- 8     # rolling window length in weeks (8 needed to capture sufficient polls)
min_polls      <- 2     # minimum polls in a window to fit a model
pv_parties     <- c("ALP", "ONP", "LNP", "GRN", "OTH")

# II) Packages -----------------------------------------------------------------
library(tidyverse)
library(metafor)
library(scales)

# 1) Data ----------------------------------------------------------------------
df_raw<-read.csv('data/raw/data_Vic.csv')
names(df_raw) <- sub('^X', '', names(df_raw))

month_abb <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")

df_clean <- df_raw %>%
  mutate(
    election = str_detect(Polling.firm, regex("election", ignore_case = TRUE))
  ) %>%
  # Drop electorate-level polls (no sample size) - keep election results
  filter(election | !(is.na(Sample.size) | Sample.size == "")) %>%
  mutate(
    # Code the dates
    Date_lb = dmy(Date_lb, quiet = TRUE),
    # Month-range dates (e.g. 'May - Jun', 'Dec - Jan'): use 1st of first month
    m1 = match(str_to_lower(str_extract(Date, "^[A-Za-z]{3}")), month_abb),
    m2 = match(str_to_lower(str_extract(Date, "[A-Za-z]{3}(?=[a-z]*\\s*$)")), month_abb),
    range_year = if_else(!is.na(m2) & m1 > m2, Year - 1L, Year),
    # Extract components for fuzzy dates
    period = str_extract(Date, "Early|Mid|Late"),
    month  = str_extract(Date, "January|February|March|April|May|June|July|August|September|October|November|December"),
    month_num = match(month, month.name),
    day_est = case_when(
      period == "Early" ~ 1,
      period == "Mid"   ~ 15,
      period == "Late"  ~ 28,
      TRUE ~ NA_real_
    ),
    # Build final lowerbound date
    Date_lb = case_when(
      !is.na(Date_lb) ~ Date_lb,
      !is.na(month_num) & !is.na(day_est) ~ make_date(Year, month_num, day_est),
      !is.na(m1) ~ make_date(range_year, m1, 1)
    ),
    date_floor = floor_date(Date_lb, unit = "months"),
    week_floor = floor_date(Date_lb, unit = "weeks"),

    # Code other variables
    Sample.size = str_remove_all(Sample.size, "[,~]"),
    sample = suppressWarnings(as.numeric(if_else(election, NA_character_, Sample.size))),

    across(c('pv_party', '2pp_party_1', '2pp_party_2'),
           ~ if_else(.x == 'L/NP', 'LNP', .x)),
    across(c('Polling.firm', 'Client', 'Interview.mode'),
           ~ as.factor(.x)),
    pv_n  = round(pv_prop / 100 * sample),
    row_id = 1:nrow(.)
  ) %>%
  select(-m1, -m2, -range_year) %>%

  # Create poll ID, denote whether poll included LIB/NAT primary votes
  group_by(Year, Date, Polling.firm) %>%
  mutate(
    poll_id = cur_group_id(),
    pv_collapse = any(pv_party %in% c("LIB", "NAT"))
  ) %>%
  ungroup()

# Create LNP combined primary vote where LIB and NAT are reported separately
LNP_target_rows <- df_clean %>%
  filter(pv_collapse == TRUE &
           pv_party %in% c('LNP', 'NAT', 'LIB') &
           is.na(`2pp_party_1`))

df_clean_pv_coll <- LNP_target_rows %>%
  # If a merged LIB/NAT/LNP value already exists, use that, otherwise combine LIB and NAT PVs
  group_by(Year, Date, Date_lb, Polling.firm, Client, Interview.mode, Sample.size, poll_id, date_floor, week_floor) %>%
  mutate(
    n = n(),
    pv_prop = case_when(
      n > 1 & all(pv_prop == max(pv_prop)) ~ pv_prop,
      n > 1 ~ sum(pv_prop, na.rm = T),
      T ~ pv_prop
    ),
    pv_n = case_when(
      n > 1 & all(pv_n == max(pv_n)) ~ pv_n,
      n > 1 ~ sum(pv_n, na.rm = T),
      T ~ pv_n
    ),
    pv_party = 'LNP'
  ) %>%
  slice(1) %>%
  ungroup() %>%
  select(-n)

df_bound <- df_clean %>%
  filter(!row_id %in% LNP_target_rows$row_id) %>%
  bind_rows(df_clean_pv_coll) %>%
  mutate(
    # 2PP: ALP share v LNP
    tpp_alp_lnp = `2pp_party_1` %in% 'ALP' & `2pp_party_2` %in% 'LNP',
    tpp_n = round(`2pp_party_1_prop` / 100 * sample),
    # One series column for primary votes and the 2PP
    series = case_when(
      tpp_alp_lnp ~ "2PP",
      is.na(`2pp_party_1`) ~ pv_party
    ),
    xi = if_else(tpp_alp_lnp, tpp_n, pv_n)
  ) %>%
  arrange(desc(date_floor), Polling.firm) %>%
  mutate(effect_id = 1:nrow(.))

# Add effect size (logit proportion) and variance
df_bound <- escalc(measure = "PLO", xi = xi, ni = sample, data = df_bound)
names(df_bound) <- sub('^X', '', names(df_bound))

# Rows used for modelling: polls only (no elections, no MRP), known date and n
model_df <- df_bound %>%
  filter(
    series %in% c(pv_parties, "2PP"),
    election == FALSE,
    !str_detect(Polling.firm, "MRP"),
    !is.na(week_floor),
    !is.na(yi)
  )

all_series <- c(pv_parties, "2PP")

# 2) Pooled effects: single-level rolling window -------------------------------
fit_window <- function(week, series_name) {
  analysis_df <- model_df %>%
    filter(series == series_name,
           week_floor <= week,
           week_floor > week - weeks(window_weeks))

  if (nrow(analysis_df) >= min_polls) {
    mod <- rma(yi, vi,
               test = "t",   # Knapp & Hartung (2003) adjustment
               data = analysis_df)
    pr <- predict(mod)
    tibble(
      series    = series_name,
      week      = week,
      week_min  = min(analysis_df$week_floor),
      week_max  = max(analysis_df$week_floor),
      n         = length(unique(analysis_df$Polling.firm)),
      k         = mod$k,
      pval      = round(mod$pval, 3),
      est       = round(transf.ilogit(pr$pred), 3),
      est_ci_lb = round(transf.ilogit(pr$ci.lb), 3),
      est_ci_ub = round(transf.ilogit(pr$ci.ub), 3)
    )
  } else {
    tibble(series = series_name, week = week, week_min = as.Date(NA),
           week_max = as.Date(NA), n = NA_integer_, k = nrow(analysis_df),
           pval = NA_real_, est = NA_real_, est_ci_lb = NA_real_, est_ci_ub = NA_real_)
  }
}

# Create week skelet
poll_weeks <- sort(unique(model_df$week_floor))

mod_df_all <- expand_grid(week = poll_weeks, series = all_series) %>%
  pmap_dfr(~ fit_window(..1, ..2))

# Raw poll points for plotting
analy_df_all <- model_df %>%
  as_tibble() %>%
  transmute(
    series,
    week_floor = as.Date(week_floor),
    prop = if_else(series == "2PP", `2pp_party_1_prop`, pv_prop) * .01
  )

# Election results for plotting
election_df <- df_bound %>%
  as_tibble() %>%
  filter(election == TRUE, series %in% c(pv_parties, "2PP")) %>%
  transmute(series, Date_lb = as.Date(Date_lb),
            prop = if_else(series == "2PP", `2pp_party_1_prop`, pv_prop) * .01) %>%
  group_by(series) %>%
  slice_min(Date_lb, n = 1, with_ties = FALSE) %>%
  ungroup()

# 3) Interpolate across gaps so line and ribbon are continuous ------------------
interpolate_weekly <- function(df, group_col) {
  df <- df %>% filter(!is.na(est)) %>% mutate(week = as.Date(week))
  spine <- seq.Date(min(df$week), max(df$week), by = "week")
  df %>%
    group_by(.data[[group_col]]) %>%
    complete(week = spine) %>%
    mutate(
      week = as.Date(week),
      across(c(est, est_ci_lb, est_ci_ub), ~ {
        obs <- !is.na(.)
        if (sum(obs) < 2) return(.)
        approx(as.numeric(week[obs]), .[obs], xout = as.numeric(week), rule = 1)$y
      })
    ) %>%
    ungroup()
}

party_colours <- c(
  ALP = "#E41A1C",   # red
  LNP = "#377EB8",   # blue
  ONP = "#FF7F00",   # orange
  GRN = "#4DAF4A",   # green
  OTH = "#888888"    # grey
)

plot_theme <- theme_minimal(base_size = 12) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
    axis.title.y     = element_text(margin = margin(r = 8)),
    legend.position  = "bottom",
    legend.key.width = unit(1.5, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey92"),
    plot.caption     = element_text(colour = "grey55", size = 8,
                                    hjust = 0, margin = margin(t = 6))
  )

watermark <- annotate("text", x = as.Date(Inf), y = Inf, label = "Metapolitics",
                      hjust = 1.1, vjust = 1.5, size = 4,
                      colour = "grey80", fontface = "bold")

caption_text <- paste0(
  "Lines show pooled ", window_weeks, "-week rolling estimate with 95% CI shading.\n",
  "Small dots show individual poll results. Large dots show the 2022 election result."
)

out_date <- max(df_clean$Date_lb, na.rm = TRUE)
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# 4) Primary vote plot -----------------------------------------------------------
scale_roof <- .50

pv_plot_df <- mod_df_all %>%
  filter(series %in% pv_parties) %>%
  mutate(est_ci_ub = pmin(est_ci_ub, scale_roof)) %>%
  interpolate_weekly("series")

p_pv <- ggplot() +
  geom_ribbon(data = pv_plot_df,
              aes(x = week, ymin = est_ci_lb, ymax = est_ci_ub, fill = series, group = series),
              alpha = 0.15, na.rm = TRUE) +
  geom_point(data = filter(analy_df_all, series %in% pv_parties),
             aes(x = week_floor, y = prop, colour = series),
             size = 1.8, alpha = 0.35, shape = 16) +
  geom_point(data = filter(election_df, series %in% pv_parties),
             aes(x = Date_lb, y = prop, colour = series),
             size = 3, alpha = 0.85, shape = 16) +
  geom_line(data = pv_plot_df, aes(x = week, y = est, colour = series),
            linewidth = 0.9, na.rm = TRUE) +
  scale_colour_manual(values = party_colours, name = NULL) +
  scale_fill_manual(values = party_colours, name = NULL, guide = "none") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(NA, scale_roof),
                     expand = expansion(mult = c(0.02, 0))) +
  labs(x = NULL, y = "Primary vote",
       title = "Victorian state voting intention",
       caption = caption_text) +
  plot_theme + watermark

ggsave(paste0("output/figures/poll_plot_Vic ", out_date, ".png"),
       plot = p_pv, width = 10, height = 6, dpi = 150)

# 6) 2PP plot (ALP v LNP) ----------------------------------------------------------
# ALP share is modelled; LNP is its complement (1 - ALP, CI mirrored)
tpp_alp <- mod_df_all %>%
  filter(series == "2PP") %>%
  mutate(party = "ALP") %>%
  interpolate_weekly("party")

tpp_plot_df <- bind_rows(
  tpp_alp,
  tpp_alp %>% mutate(party = "LNP", est = 1 - est,
                     lb = 1 - est_ci_ub, ub = 1 - est_ci_lb) %>%
    mutate(est_ci_lb = lb, est_ci_ub = ub) %>% select(-lb, -ub)
)

tpp_points <- bind_rows(
  filter(analy_df_all, series == "2PP") %>% mutate(party = "ALP"),
  filter(analy_df_all, series == "2PP") %>% mutate(party = "LNP", prop = 1 - prop)
)

tpp_election <- bind_rows(
  filter(election_df, series == "2PP") %>% mutate(party = "ALP"),
  filter(election_df, series == "2PP") %>% mutate(party = "LNP", prop = 1 - prop)
)

p_tpp <- ggplot() +
  geom_hline(yintercept = .5, colour = "grey60", linetype = "dashed") +
  geom_ribbon(data = tpp_plot_df,
              aes(x = week, ymin = est_ci_lb, ymax = est_ci_ub, fill = party, group = party),
              alpha = 0.15, na.rm = TRUE) +
  geom_point(data = tpp_points, aes(x = week_floor, y = prop, colour = party),
             size = 1.8, alpha = 0.35, shape = 16) +
  geom_point(data = tpp_election, aes(x = Date_lb, y = prop, colour = party),
             size = 3, alpha = 0.85, shape = 16) +
  geom_line(data = tpp_plot_df, aes(x = week, y = est, colour = party),
            linewidth = 0.9, na.rm = TRUE) +
  scale_colour_manual(values = party_colours[c("ALP", "LNP")], name = NULL) +
  scale_fill_manual(values = party_colours[c("ALP", "LNP")], name = NULL, guide = "none") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = seq(.40, .70, .05)) +
  coord_cartesian(ylim = c(.40, .65)) +   # zoom without dropping CI/poll rows
  labs(x = NULL, y = "Two-party preferred vote",
       title = "Victorian two-party preferred: ALP v LNP",
       caption = caption_text) +
  plot_theme + watermark

ggsave(paste0("output/figures/tpp_plot_Vic ", out_date, ".png"),
       plot = p_tpp, width = 10, height = 6, dpi = 150)

# 7) Save model estimates --------------------------------------------------------
# write.csv(mod_df_all, "output/tables/pooled_estimates_Vic.csv", row.names = FALSE)
