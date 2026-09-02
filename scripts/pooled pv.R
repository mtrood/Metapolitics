#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Meta-politics: Pooled Primary votes

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# III) Running notes -----------------------------------------------------------

# II) FUnctions ----------------------------------------------------------------

# II) Packages -----------------------------------------------------------------
library(tidyverse)
library(metafor)
library(DescTools)
library(scales)

# 1) Data ----------------------------------------------------------------------
df_raw<-read.csv('data/raw/data.csv')
names(df_raw) <- sub('^X', '', names(df_raw))

df_clean <- df_raw %>%
  mutate(
    # Code the dates
    Date_lb = dmy(Date_lb),
    # Extract components for fuzzy dates
    period = str_extract(Date, "Early|Mid|Late"),
    month  = str_extract(Date, "January|February|March|April|May|June|July|August|September|October|November|December"),
    # Convert month name to number
    month_num = match(month, month.name),
    # Assign day based on period
    day_est = case_when(
      period == "Early" ~ 1,
      period == "Mid"   ~ 15,
      period == "Late"  ~ 28,
      TRUE ~ NA_real_
    ),
    # Build final lowerbound date
    Date_lb = case_when(
      !is.na(Date_lb) ~ Date_lb,
      !is.na(month_num) ~ make_date(Year, month_num, day_est)
    ),
    date_floor = floor_date(Date_lb,unit = "months",),
    week_floor = floor_date(Date_lb,unit = "weeks",),
    
    # Code other variables
    election = if_else(Sample.size == 'Election', T,F),
    Sample.size = str_remove_all(Sample.size, ','),
    sample = as.numeric(if_else(Sample.size == 'Election', NA, Sample.size)),
    
    across(c('pv_party','2pp_party_2'),
           ~ if_else(.x == 'L/NP', 'LNP', .x)), 
    across(c('Polling.firm','Client','Interview.mode','2pp_party_1'),
           ~as.factor(.x)
    ),
    pv_n = round(pv_prop / 100 * sample),
    row_id = 1:nrow(.)
  ) %>%
  
  # Create poll ID, denote whether poll included LNP primary vote
  group_by(Year, Date, Polling.firm) %>%
  mutate(
    poll_id = cur_group_id(),
    pv_collapse =  case_when(
      any(pv_party == "NAT") |  any(pv_party == 'LIB') ~ TRUE,
    ) 
  )%>%
  ungroup()

# Create LNP combined primary vote where LIB and NAT are reported separately
LNP_target_rows <- df_clean %>%
  
  # Select LIB/NAT rows from polls without a an LNP pv, exlude MRP polls.
  filter(pv_collapse == TRUE & 
           (pv_party == 'LNP' | 
            pv_party == 'NAT' |  
            pv_party == 'LIB') &
           is.na(`2pp_party_1`))

df_clean_pv_coll <- LNP_target_rows %>%
  # Combine LIB and NAT PVs
  group_by(Year, Date, Date_lb, Polling.firm, Client, Interview.mode, Sample.size, poll_id,date_floor, week_floor) %>%
  summarise(
    pv_prop_recode = sum(pv_prop),
    pv_n = sum(pv_n)
  ) %>%
  ungroup()%>%
  # Add pv_party variable
  mutate(
    pv_party = 'LNP'
  )

df_bound <- df_clean %>%
  filter(!row_id %in% LNP_target_rows$row_id) %>%
  mutate(
    pv_prop_recode = pv_prop,
  )%>%
  bind_rows(., df_clean_pv_coll) %>%
  arrange(desc(date_floor), Polling.firm) %>%
  # Add effect id
  mutate(
    effect_id = 1:nrow(.)
  )

# Add month ID
month_id <- df_clean %>%
  group_by(date_floor) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(date_floor))%>%
  mutate( 
    month_id = length(unique(date_floor)): 1,
  ) %>%
  select(date_floor, month_id) 

# Add week ID
week_id <- df_clean %>%
  group_by(week_floor) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(week_floor))%>%
  mutate( 
    week_id = length(unique(week_floor)): 1,
  ) %>%
  select(week_floor, week_id) 


df_bound <- left_join(df_bound, month_id) %>%
  left_join(., week_id)

# Add effect size and variance
df_bound <- escalc(measure = "PLO", xi = pv_n, ni = sample, data = df_bound)
names(df_bound) <- sub('^X', '', names(df_bound))

# 2) Pooled effects: Single-level two week  combined ---------------------------

# Pre-compute the set of week_ids that actually have polls (after filters)
# This is used inside the lapply to find the nearest prior week with data.
weeks_with_polls <- df_bound %>%
  filter(
    is.na(`2pp_party_1`),
    pv_party != 'LIB',
    pv_party != 'NAT',
    election == FALSE,
    !str_detect(Polling.firm, "MRP"),
    !is.na(week_id)
  ) %>%
  pull(week_id) %>%
  unique() %>%
  sort()

uni_model_list <- lapply(
  # Lapply on all weeks except the first
  weeks_with_polls[weeks_with_polls != min(weeks_with_polls)], function(week){
    
    # Select week and week prior
    master_df = df_bound %>%
      
      # Filter out: 2pp, and LIB/NAT pv polls in favor of LNP 
      filter(is.na(`2pp_party_1`) &                 # two party preferred rows
               pv_party != 'LIB' &                  # LIB and NAT rows now that we have LNP
               pv_party != 'NAT' &                  
               election == FALSE &                  # Election rows
               !str_detect(Polling.firm, "MRP"))    # MRP rows
    
    # Dynamically find the most recent prior week that actually has polls,
    # however far back that is — handles gaps of any length.
    prior_weeks_available <- weeks_with_polls[weeks_with_polls < week]
    week_prior <- if (length(prior_weeks_available) > 0) max(prior_weeks_available) else NA
    
    rolling_df = master_df[master_df$week_id %in% week | master_df$week_id %in% week_prior,]
    
    # Lapply a meta-analysis to each primary vote in each party
    month_model_list <- lapply(
      c("ALP","ONP","LNP", "GRN", "IND"), function(party){
        # Filter part rows 
        analysis_df = rolling_df[rolling_df$pv_party %in% party,]
        
        if (nrow(analysis_df) > 1){
          # Single level pooled effect: two or more effects
          mod_overall <- rma(yi ,vi,
                             test="t", # Note: We have used the Knapp & Hartung (2003) adjustment (i.e., used t rather than Z)
                             data = analysis_df)
          mod_df <- as.data.frame(predict(mod_overall)) %>%
            mutate(
              pv_party = party,
              week_id = week,
              week_min = min(analysis_df$week_floor),
              week_max = max(analysis_df$week_floor),
              pval = round(mod_overall$pval,3),
              est = round(transf.ilogit(pred),2),
              est_ci_lb = round(transf.ilogit(ci.lb),2),
              est_ci_ub = round(transf.ilogit(ci.ub),2),
              n = length(unique(analysis_df$Polling.firm)),
              k = mod_overall$k,
            ) %>%
            select(pv_party, n, k, week_min, week_max,everything(),
                   -pred, -se,-ci.lb,-ci.ub,-pi.lb,-pi.ub)
        }
        
        # If insufficient effects
        if (nrow(analysis_df) < 2){
          mod_df <- as.data.frame(
            list(
              pv_party = party,
              week_id = week,
              week_min = NA,
              week_max = NA,
              pval = NA,
              est = NA,
              est_ci_lb = NA,
              est_ci_ub = NA,
              n = NA,
              k = NA
            )
          )%>%
            select(pv_party, n, k, week_min, week_max,everything())
        }
        return(list(analysis_df,mod_df))
      })
  }
)

# Extract and bind DFs
uni_flat <- do.call(c, uni_model_list)

analy_df_all <- do.call(rbind, lapply(uni_flat, `[[`, 1)) %>%
  
  # select relevant variables
  select(pv_party,pv_prop,Date_lb,week_floor) %>%
  # Make pv_prop a decimal number
  mutate(
    pv_prop = pv_prop *.01
  )

# Unlist model data
mod_df_all <- do.call(rbind, lapply(uni_flat, `[[`, 2))

# Extract election result rows for plotting (excluded from model above)
election_df <- df_clean %>%
  filter(
    election == TRUE,
    pv_party %in% c("ALP", "LNP", "GRN", "ONP", "IND")
  ) %>%
  select(pv_party, pv_prop, Date_lb) %>%
  mutate(pv_prop = pv_prop * .01) %>%
  group_by(pv_party) %>%
  slice_min(Date_lb) %>%
  ungroup

# 3) Create Geom line/ point plot ----------------------------------------------

# Create Colour palette
party_colours <- c(
  ALP = "#E41A1C",   # red
  LNP = "#377EB8",   # blue
  ONP = "#FF7F00",   # orange
  GRN = "#4DAF4A",   # green
  IND = "#888888"    # grey
)

# Faded versions for the raw-poll dots (alpha handled in geom, but we keep
# the same hue so the mapping is identical)
party_colours_faded <- scales::alpha(party_colours, 0.35)
names(party_colours_faded) <- names(party_colours)


# 4) Amend data for plot -------------------------------------------------------
# Create upper scale limit
scale_roof <- .50

mod_plot_df <- mod_df_all %>%
  mutate(
    week_max  = as.Date(week_max),
    week_min  = as.Date(week_min),
    est_ci_ub = ifelse(est_ci_ub > scale_roof, scale_roof, est_ci_ub)
  )

# Ensure Date columns are Date class in all plot dataframes
analy_df_all <- analy_df_all %>%
  mutate(
    Date_lb    = as.Date(Date_lb),
    week_floor = as.Date(week_floor)
  )

election_df <- election_df %>%
  mutate(Date_lb = as.Date(Date_lb))

# 5) Interpolate across gaps so line and ribbon are continuous ------------

# mod_plot_df week_max values may be irregular (polls don't fall on exact
# weekly boundaries). We build a single regular weekly spine across the full
# date range, snap each observed estimate to its nearest spine date, then
# interpolate linearly across any remaining gaps.

mod_plot_df <- mod_plot_df %>%
  mutate(week_max = as.Date(week_max)) %>%
  filter(!is.na(week_max)) %>%
  # Where multiple estimates share a week_max keep the most recent window
  group_by(pv_party, week_max) %>%
  slice_max(week_min, n = 1, with_ties = FALSE) %>%
  ungroup()

# Build a single regular weekly spine from the global min to max date
global_min <- min(mod_plot_df$week_max, na.rm = TRUE)
global_max <- max(mod_plot_df$week_max, na.rm = TRUE)
weekly_spine <- seq.Date(global_min, global_max, by = "week")

# Snap each observed week_max to the nearest spine date so complete() works
# on a consistent grid across all parties
mod_plot_df <- mod_plot_df %>%
  mutate(
    week_max = weekly_spine[
      findInterval(week_max, weekly_spine - 3L, rightmost.closed = TRUE)
    ]
  ) %>%
  # Re-deduplicate after snapping (two obs may snap to same spine date)
  group_by(pv_party, week_max) %>%
  slice_max(week_min, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  # Expand each party to the full spine and interpolate
  group_by(pv_party) %>%
  complete(week_max = weekly_spine) %>%
  mutate(
    # complete() can silently coerce Date to numeric — re-assert Date class
    week_max = as.Date(week_max),
    across(c(est, est_ci_lb, est_ci_ub), ~ {
      obs <- !is.na(.)
      if (sum(obs) < 2) return(.)
      approx(
        x    = as.numeric(week_max[obs]),
        y    = .[obs],
        xout = as.numeric(week_max),
        rule = 1    # no extrapolation beyond first/last observed point
      )$y
    })
  ) %>%
  ungroup() %>%
  # Final safety check — ensure week_max is Date throughout
  mutate(week_max = as.Date(week_max))

# 6) Build the plot ------------------------------------------------------------
p <- ggplot() +
  
  # Shaded CI ribbon (one per party)
  geom_ribbon(
    data    = mod_plot_df,
    mapping = aes(x = week_max, ymin = est_ci_lb, ymax = est_ci_ub,
                  fill = pv_party, group = pv_party),
    alpha   = 0.15,
    na.rm   = TRUE
  ) +
  
  # Raw poll observations – faded dots
  geom_point(
    data    = analy_df_all,
    mapping = aes(x = week_floor, y = pv_prop, colour = pv_party),
    size    = 1.8,
    alpha   = 0.35,
    shape   = 16
  ) +
  
  # Election result – enlarged dot per party
  geom_point(
    data    = election_df,
    mapping = aes(x = Date_lb, y = pv_prop, colour = pv_party),
    size    = 3,
    alpha   = 0.85,
    shape   = 16
  ) +
  
  # Pooled rolling estimate line (drawn on top)
  geom_line(
    data      = mod_plot_df,
    mapping   = aes(x = week_max, y = est, colour = pv_party),
    linewidth = 0.9
  ) +
  
  # ── Scales ----------------------------------------------------------------
scale_colour_manual(
  values = party_colours,
  name   = NULL
) +
  scale_fill_manual(
    values = party_colours,
    name   = NULL,
    guide  = "none"    # suppress separate fill legend; colour legend suffices
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    expand      = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(NA, scale_roof),
    expand = expansion(mult = c(0.02, 0))
  ) +
  
  # ── Labels & theme ---------------------------------------------------------
labs(
  x     = NULL,
  y     = "Primary vote",
  title = "Australian federal voting intention",
  caption = "Lines show 2-week weighted average with 95% CI shading.\nSmall dots show individual poll results. Large dots show the 2025 election result."
) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
    axis.title.y     = element_text(margin = margin(r = 8)),
    legend.position  = "bottom",
    legend.key.width = unit(1.5, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey92"),
    plot.caption     = element_text(colour = "grey55", size = 8,
                                    hjust = 0, margin = margin(t = 6))
  ) +
  annotate(
    "text",
    x     = Inf, y = Inf,
    label = "Metapolitics",
    hjust = 1.1, vjust = 1.5,
    size  = 4,
    colour = "grey80",
    fontface = "bold"
  )
p
# ── 5. Save  -----------------------------------------------------------------
out_address <- paste("output/figures/poll_plot ", max(df_clean$Date_lb), ".png", sep = '')
ggsave(out_address, plot = p, width = 10, height = 6, dpi = 150)
