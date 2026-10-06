library(dplyr)
library(purrr)
library(readr)
library(nflreadr)

# 1. Load live player stats
stats <- load_player_stats(seasons = most_recent_season())

# 2. Identify players who are on IR or ruled Out this week
active_rosters <- load_rosters(seasons = most_recent_season()) %>%
  filter(status == "ACT") %>%
  select(player_id = gsis_id)

weekly_injuries <- load_injuries(seasons = most_recent_season()) %>%
  filter(week == max(week), report_status %in% c("Out", "Doubtful")) %>%
  select(player_id = gsis_id)

# 3. Build the clean, healthy player pool
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
  filter(games >= 3, (expected_targets >= 4.5 | expected_carries >= 8 | expected_pass_attempts >= 20)) %>% 
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

# 4. Multi-Position Simulation Engine
run_player_sim <- function(row) {
  num_simulations <- 10000
  points <- numeric(num_simulations)
  
  if (row$expected_targets > 0) {
    targets <- rnbinom(num_simulations, size = 3, mu = row$expected_targets)
    receptions <- rbinom(num_simulations, size = targets, prob = row$catch_rate)
    rec_yards <- vapply(receptions, function(rec) if (rec > 0) max(0, sum(rnorm(rec, mean = row$avg_yards_per_catch, sd = row$std_dev_rec_yards))) else 0, numeric(1))
    rec_tds <- rbinom(num_simulations, size = targets, prob = row$rec_td_rate)
    points <- points + (receptions * 1.0) + (rec_yards * 0.1) + (rec_tds * 6.0)
  }
  
  if (row$expected_carries > 0) {
    carries <- rnbinom(num_simulations, size = 5, mu = row$expected_carries)
    rush_yards <- vapply(carries, function(rush) if (rush > 0) max(0, sum(rnorm(rush, mean = row$avg_yards_per_carry, sd = row$std_dev_rush_yards))) else 0, numeric(1))
    rush_tds <- rbinom(num_simulations, size = carries, prob = row$rush_td_rate)
    points <- points + (rush_yards * 0.1) + (rush_tds * 6.0)
  }
  
  if (row$expected_pass_attempts > 0) {
    pass_attempts <- rpois(num_simulations, lambda = row$expected_pass_attempts)
    completions <- rbinom(num_simulations, size = pass_attempts, prob = row$completion_rate)
    pass_yards <- vapply(completions, function(comp) if (comp > 0) max(0, sum(rnorm(comp, mean = row$avg_yards_per_completion, sd = row$std_dev_pass_yards))) else 0, numeric(1))
    pass_tds <- rbinom(num_simulations, size = pass_attempts, prob = row$pass_td_rate)
    interceptions <- rbinom(num_simulations, size = pass_attempts, prob = row$int_rate)
    points <- points + (pass_yards * 0.04) + (pass_tds * 6.0) - (interceptions * 2.0)
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

# 5. Run Simulations & Save Results
set.seed(42)
results <- player_pool %>%
  split(1:nrow(.)) %>%
  map_dfr(run_player_sim) %>% 
  arrange(Position, desc(Median_50th))

dir.create("results", showWarnings = FALSE)
write_csv(results, "results/projections.csv")

cat("Simulations complete! Output saved.\n")
