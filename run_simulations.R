library(dplyr)
library(purrr)
library(readr)

# 1. Define sample player baseline inputs
player_pool <- tibble(
  player = c("Jaylen Waddle", "Tyreek Hill", "CeeDee Lamb", "Justin Jefferson", "Amon-Ra St. Brown"),
  position = "WR",
  expected_targets = c(6.5, 9.5, 10.0, 9.0, 9.2),
  catch_rate = c(0.65, 0.68, 0.70, 0.67, 0.74),
  avg_yards_per_catch = c(11.6, 14.2, 12.8, 14.5, 11.2),
  std_dev_yards = c(5.0, 6.5, 5.5, 6.0, 4.5),
  td_rate = c(0.04, 0.07, 0.06, 0.06, 0.05)
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
