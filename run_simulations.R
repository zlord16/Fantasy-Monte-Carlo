library(dplyr)
library(purrr)
library(readr)
library(nflreadr)

# ==========================================
# 1. LOAD AND AGGREGATE LIVE PLAYER STATS
# ==========================================
# Fetch current season stats
stats <- load_player_stats(seasons = most_recent_season())

# Aggregate stats to create per-game averages for WRs, TEs, RBs, and QBs
player_pool <- stats %>%
  filter(position %in% c("WR", "TE", "RB", "QB")) %>% 
  group_by(player = player_display_name, position) %>%
  summarize(
    games = n(),
    
    # Receiving Stats
    expected_targets = sum(targets, na.rm = TRUE) / games,
    catch_rate = sum(receptions, na.rm = TRUE) / sum(targets, na.rm = TRUE),
    avg_yards_per_catch = sum(receiving_yards, na.rm = TRUE) / sum(receptions, na.rm = TRUE),
    rec_td_rate = sum(receiving_tds, na.rm = TRUE) / sum(targets, na.rm = TRUE),
    
    # Rushing Stats (For RBs, QBs, and WR sweeps)
    expected_carries = sum(carries, na.rm = TRUE) / games,
    avg_yards_per_carry = sum(rushing_yards, na.rm = TRUE) / sum(carries, na.rm = TRUE),
    rush_td_rate = sum(rushing_tds, na.rm = TRUE) / sum(carries, na.rm = TRUE),
    
    # Passing Stats (For QBs)
    expected_pass_attempts = sum(attempts, na.rm = TRUE) / games,
    completion_rate = sum(completions, na.rm = TRUE) / sum(attempts, na.rm = TRUE),
    avg_yards_per_completion = sum(passing_yards, na.rm = TRUE) / sum(completions, na.rm = TRUE),
    pass_td_rate = sum(passing_tds, na.rm = TRUE) / sum(attempts, na.rm = TRUE),
    int_rate = sum(passing_interceptions, na.rm = TRUE) / sum(attempts, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  # Filter out players who don't touch the ball enough to be fantasy relevant
  filter(games >= 3, (expected_targets >= 4.5 | expected_carries >= 8 | expected_pass_attempts >= 20)) %>% 
  
  # 2. Clean up NaN values
  mutate(across(where(is.numeric), ~coalesce(., 0))) %>% 
  
  # 3. Cap unsustainable outlier efficiency metrics to realistic NFL maximums
  mutate(
    rec_td_rate = pmin(rec_td_rate, 0.12),   # Max 12% receiving TD rate per target
    rush_td_rate = pmin(rush_td_rate, 0.08), # Max 8% rushing TD rate per carry
    pass_td_rate = pmin(pass_td_rate, 0.08), # Max 8% passing TD rate per attempt
    catch_rate = pmin(catch_rate, 0.85),     # Max 85% catch rate
    avg_yards_per_catch = pmin(avg_yards_per_catch, 16.0) # Cap extreme yards-per-catch
  ) %>% 

# ==========================================
# 2. THE MULTI-POSITION SIMULATION ENGINE
# ==========================================
run_player_sim <- function(row) {
  num_simulations <- 10000
  
  # Initialize point arrays
  points <- numeric(num_simulations)
  
  # --- A. SIMULATE RECEIVING (WR/TE/RB) ---
  if (row$expected_targets > 0) {
    targets <- rnbinom(num_simulations, size = 3, mu = row$expected_targets)
    receptions <- rbinom(num_simulations, size = targets, prob = row$catch_rate)
    
    rec_yards <- vapply(receptions, function(rec) {
      if (rec > 0) max(0, sum(rnorm(rec, mean = row$avg_yards_per_catch, sd = row$std_dev_rec_yards))) else 0
    }, numeric(1))
    
    rec_tds <- rbinom(num_simulations, size = targets, prob = row$rec_td_rate)
    points <- points + (receptions * 1.0) + (rec_yards * 0.1) + (rec_tds * 6.0)
  }
  
  # --- B. SIMULATE RUSHING (RB/QB) ---
  if (row$expected_carries > 0) {
    carries <- rnbinom(num_simulations, size = 5, mu = row$expected_carries)
    
    rush_yards <- vapply(carries, function(rush) {
      if (rush > 0) max(0, sum(rnorm(rush, mean = row$avg_yards_per_carry, sd = row$std_dev_rush_yards))) else 0
    }, numeric(1))
    
    rush_tds <- rbinom(num_simulations, size = carries, prob = row$rush_td_rate)
    points <- points + (rush_yards * 0.1) + (rush_tds * 6.0)
  }
  
  # --- C. SIMULATE PASSING (QB) ---
  if (row$expected_pass_attempts > 0) {
    pass_attempts <- rpois(num_simulations, lambda = row$expected_pass_attempts)
    completions <- rbinom(num_simulations, size = pass_attempts, prob = row$completion_rate)
    
    pass_yards <- vapply(completions, function(comp) {
      if (comp > 0) max(0, sum(rnorm(comp, mean = row$avg_yards_per_completion, sd = row$std_dev_pass_yards))) else 0
    }, numeric(1))
    
    pass_tds <- rbinom(num_simulations, size = pass_attempts, prob = row$pass_td_rate)
    interceptions <- rbinom(num_simulations, size = pass_attempts, prob = row$int_rate)
    
    # Passing touchdowns are typically worth 4 points, interceptions are -2 points
    points <- points + (pass_yards * 0.04) + (pass_tds * 6.0) - (interceptions * 2.0)
  }
  
  # --- D. RETURN RESULTS ---
  tibble(
    Player = row$player,
    Position = row$position,
    Floor_5th = round(quantile(points, 0.05), 1),
    Median_50th = round(quantile(points, 0.50), 1),
    Ceiling_95th = round(quantile(points, 0.95), 1)
  )
}

# ==========================================
# 3. RUN SIMULATIONS & SAVE RESULTS
# ==========================================
set.seed(42)

# Use map_dfr to iterate over each row of our live player pool
results <- player_pool %>%
  split(1:nrow(.)) %>%
  map_dfr(run_player_sim) %>% 
  arrange(desc(Median_50th)) # Sort by highest ceiling upside

# Create the output directory and save
dir.create("results", showWarnings = FALSE)
write_csv(results, "results/projections.csv")

cat("Simulations complete! Output saved to results/projections.csv\n")
