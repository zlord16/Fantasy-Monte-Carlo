library(dplyr)
library(purrr)
library(readr)
library(nflreadr)

# 1. Load stats (Removed the early filter so partial injury games don't skew the math)
stats <- load_player_stats(seasons = most_recent_season())

# 2. Live rosters and injury report
active_rosters <- load_rosters(seasons = most_recent_season()) %>%
  filter(status == "ACT") %>%
  select(player_id = gsis_id)

weekly_injuries <- load_injuries(seasons = most_recent_season()) %>%
  filter(week == max(week), report_status %in% c("Out", "Doubtful")) %>%
  select(player_id = gsis_id)

# 3. Pull live Vegas lines for the upcoming week
schedules <- load_schedules(seasons = most_recent_season()) %>%
  filter(is.na(result), !is.na(spread_line), !is.na(total_line)) %>%
  filter(week == min(week)) %>%
  mutate(
    # FIXED: Subtracted the spread for the home team so favorites actually get the higher total
    home_implied = (total_line - spread_line) / 2,
    away_implied = (total_line + spread_line) / 2
  )

vegas_totals <- bind_rows(
  schedules %>% select(team = home_team, implied_pts = home_implied),
  schedules %>% select(team = away_team, implied_pts = away_implied)
) %>%
  # Use square root to safely scale volume without going to extremes
  mutate(matchup_multiplier = sqrt(implied_pts / 21.0)) 

# 4. Build the clean player pool
player_pool <- stats %>%
  inner_join(active_rosters, by = "player_id") %>%
  anti_join(weekly_injuries, by = "player_id") %>%
  filter(position %in% c("WR", "TE", "RB", "QB")) %>% 
  group_by(player = player_display_name, position, team) %>%
  summarize(
    games = n(),
    expected_targets = sum(targets, na.rm = TRUE) / games,
    catch_rate = sum(receptions, na.rm = TRUE) / sum(targets, na.rm = TRUE),
    avg_yards_per_catch = sum(receiving_yards, na.rm = TRUE) / sum(receptions, na.rm = TRUE),
    rec_td_rate = sum(receiving_tds, na.rm = TRUE) / sum(targets, na.rm = TRUE),
    expected_carries = sum(carries, na.rm = TRUE) / games,
    avg_yards_per_carry = sum(rushing_yards, na.rm = TRUE) / sum(carries, na.rm = TRUE),
    rush_td_rate = sum(rushing_tds, na.rm = TRUE) / sum(carries, na.rm = TRUE),
    expected_pass_attempts = sum(attempts, na.rm = TRUE) / games,
    completion_rate = sum(completions, na.rm = TRUE) / sum(attempts, na.rm = TRUE),
    avg_yards_per_completion = sum(passing_yards, na.rm = TRUE) / sum(completions, na.rm = TRUE),
    pass_td_rate = sum(passing_tds, na.rm = TRUE) / sum(attempts, na.rm = TRUE),
    int_rate = sum(passing_interceptions, na.rm = TRUE) / sum(attempts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  inner_join(vegas_totals, by = "team") %>%
  mutate(
    # FIXED: Apply multiplier ONLY to volume. Removing it from rates stops the TD compounding error.
    expected_targets = expected_targets * matchup_multiplier,
    expected_carries = expected_carries * matchup_multiplier,
    expected_pass_attempts = expected_pass_attempts * matchup_multiplier
  ) %>%
  # FIXED: Restored games >= 3 to instantly drop the 1-game backups
  filter(games >= 3, (expected_targets >= 4.0 | expected_carries >= 6 | expected_pass_attempts >= 20)) %>% 
  mutate(across(where(is.numeric), ~coalesce(., 0))) %>% 
  mutate(
    rec_td_rate = pmin(rec_td_rate, 0.12),
    rush_td_rate = pmin(rush_td_rate, 0.08),
    pass_td_rate = pmin(pass_td_rate, 0.08),
    catch_rate = pmin(catch_rate, 0.85),
    avg_yards_per_catch = pmin(avg_yards_per_catch, 16.0),
    std_dev_rec_yards = 5.0,
    std_dev_rush_yards = 3.5,
    std_dev_pass_yards = 6.0
  )

# 5. Multi-Position Simulation Engine
run_player_sim<- function(row) {
  num_simulations <- 10000
  points <- numeric(num_simulations)
  
  # RECEIVING
  if (row$expected_targets > 0) {
    # rpois prevents absurd 12-target rolls for backs who average 4
    targets <- rpois(num_simulations, lambda = row$expected_targets)
    receptions <- rbinom(num_simulations, size = targets, prob = row$catch_rate)
    rec_yards <- vapply(receptions, function(rec) {
      if (rec > 0) max(0, sum(rnorm(rec, mean = row$avg_yards_per_catch, sd = row$std_dev_rec_yards))) else 0
    }, numeric(1))
    rec_tds <- rbinom(num_simulations, size = targets, prob = row$rec_td_rate)
    points <- points + (receptions * 1.0) + (rec_yards * 0.1) + (rec_tds * 6.0)
  }
  
  # RUSHING
  if (row$expected_carries > 0) {
    # rpois keeps carries bounded to realistic NFL ranges
    carries <- rpois(num_simulations, lambda = row$expected_carries)
    rush_yards <- vapply(carries, function(rush) {
      if (rush > 0) max(0, sum(rnorm(rush, mean = row$avg_yards_per_carry, sd = row$std_dev_rush_yards))) else 0
    }, numeric(1))
    rush_tds <- rbinom(num_simulations, size = carries, prob = row$rush_td_rate)
    points <- points + (rush_yards * 0.1) + (rush_tds * 6.0)
  }
  
  # PASSING
  if (row$expected_pass_attempts > 0) {
    pass_attempts <- rpois(num_simulations, lambda = row$expected_pass_attempts)
    completions <- rbinom(num_simulations, size = pass_attempts, prob = row$completion_rate)
    pass_yards <- vapply(completions, function(comp) {
      if (comp > 0) max(0, sum(rnorm(comp, mean = row$avg_yards_per_completion, sd = row$std_dev_pass_yards))) else 0
    }, numeric(1))
    pass_tds <- rbinom(num_simulations, size = pass_attempts, prob = row$pass_td_rate)
    interceptions <- rbinom(num_simulations, size = pass_attempts, prob = row$int_rate)
    points <- points + (pass_yards * 0.04) + (pass_tds * 4.0) - (interceptions * 2.0)
  }
  
  tibble(
    Player = row$player,
    Team = row$team,
    Position = row$position,
    Floor_5th = round(quantile(points, 0.05), 1),
    Median_50th = round(quantile(points, 0.50), 1),
    Ceiling_95th = round(quantile(points, 0.95), 1)
  )
}

# 6. Run Simulations & Save Results
set.seed(42)
results <- player_pool %>%
  split(1:nrow(.)) %>%
  map_dfr(run_player_sim) %>% 
  arrange(Position, desc(Median_50th))

dir.create("results", showWarnings = FALSE)
write_csv(results, "results/projections.csv")

cat("Simulations complete! Output saved.\n")
