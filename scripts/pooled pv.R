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
    pv_n = round(pv_prop / 100 * sample)
  ) %>%

  # Create poll ID, denote whether poll included LNP primary vote
  group_by(Year, Date, Polling.firm) %>%
  mutate(
    poll_id = cur_group_id(),
    pv_prop_recode =  case_when(
        pv_party != "NAT" &  pv_party != 'LIB' ~ pv_prop,
        any(pv_party == 'LNP') ~ NA,
        T ~ pv_prop
    )
  ) %>%
  ungroup()

# Create LNP combined primary vote where LIB and NAT are reported separately
df_clean_pv_coll <- df_clean %>%
  
  # Select LIB/NAT rows from polls without a an LNV pv, exlude MRP polls.
  filter(is.na(pv_prop_recode) & is.na(`2pp_party_1`)) %>%
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

df_bound <- bind_rows(df_clean, df_clean_pv_coll) %>%
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

uni_model_list <- lapply(
  # Lapply on all months except the first
  unique(na.omit(df_bound$week_id))[!unique(na.omit(df_bound$week_id)) %in% min(na.omit(df_bound$week_id))], function(week){
    # Create time band parameters
    week_prior = week-1
    two_weeks_prior = week-2
    # Select week and week prior
    master_df = df_bound %>%
      
      # Filter out: 2pp, and LIB/NAT pv polls in favor of LNP 
      filter(is.na(`2pp_party_1`) &                 # two party preferred rows
               pv_party != 'LIB' &                  # LIB and NAT rows now that we have LNP
               pv_party != 'NAT' &                  
               election == FALSE &                  # Election rows
               !str_detect(Polling.firm, "MRP"))    # MRP rows
    
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


# ── 4.Amend data for plot -----------------------------------------------------
# Create upper scale limit
scale_roof <- .50

mod_plot_df <- mod_df_all %>%
  mutate(
    est_ci_ub = ifelse(est_ci_ub > scale_roof, scale_roof, est_ci_ub)
  )

# ── 5. Build the plot --------------------------------------------------------
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
  caption = "Lines show 2-week rolling pooled estimates with 95% CI shading.\nDots show individual poll results."
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
  )
p
# ── 5. Save  -----------------------------------------------------------------
out_address <- paste("output/figures/poll_plot ", max(df_clean$Date), ".png", sep = '')
ggsave(out_address, plot = p, width = 10, height = 6, dpi = 150)
