library(dplyr)
library(purrr)
library(readr)
library(nflreadr)

# 1. Load and aggregate live player stats
stats <- load_player_stats(seasons = 2024)

# Convert the raw weekly box scores into our required baseline averages
player_pool <- stats %>%
  filter(position %in% c("WR", "TE")) %>% 
  group_by(player = player_display_name, position) %>%
  summarize(
    games = n(),
    expected_targets = sum(targets, na.rm = TRUE) / games,
    catch_rate = sum(receptions, na.rm = TRUE) / sum(targets, na.rm = TRUE),
    avg_yards_per_catch = sum(receiving_yards, na.rm = TRUE) / sum(receptions, na.rm = TRUE),
    td_rate = sum(receiving_tds, na.rm = TRUE) / sum(targets, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Filter for relevant players (averaging at least 4 targets a game)
  filter(games >= 2, expected_targets >= 4) %>% 
  mutate(
    # Clean up any NaN values from dividing by zero
    catch_rate = coalesce(catch_rate, 0),
    avg_yards_per_catch = coalesce(avg_yards_per_catch, 0),
    td_rate = coalesce(td_rate, 0),
    std_dev_yards = 5.0 # We will keep variance static for now
  )
# 2. Simulation engine function
run_player_sim <- function(player, position, expected_targets, catch_rate, avg_yards_per_catch, std_dev_yards, td_rate) {
  num_simulations <- 10000
  
  # Binomial target dispersion allows for wider real-world variance
  targets <- rnbinom(num_simulations, size = 3, mu = expected_targets)
  receptions <- rbinom(num_simulations, size = targets, prob = catch_rate)
  
  yards <- vapply(receptions, function(rec) {
    if (rec > 0) {
      max(0, sum(rnorm(rec, mean = avg_yards_per_catch, sd = std_dev_yards)))
    } else {
      0
    }
  }, numeric(1))
  
  touchdowns <- rbinom(num_simulations, size = targets, prob = td_rate)
  
  points <- (receptions * 1.0) + (yards * 0.1) + (touchdowns * 6.0)
  
  tibble(
    Player = player,
    Position = position,
    Floor_5th = round(quantile(points, 0.05), 1),
    Median_50th = round(quantile(points, 0.50), 1),
    Ceiling_95th = round(quantile(points, 0.95), 1)
  )
}

# 3. Run simulations across all players
set.seed(42)
results <- pmap_dfr(player_pool, run_player_sim)

# 4. Save results to a CSV file
dir.create("results", showWarnings = FALSE)
write_csv(results, "results/projections.csv")

cat("Simulations completed successfully. Output written to results/projections.csv\n")
