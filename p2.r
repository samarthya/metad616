# Crawford Development Risk Analysis - STREAMLINED VERSION
# Enterprise Risk Management - Essential Analysis Only

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(gridExtra)
})

set.seed(42)

if (!dir.exists("graphs")) dir.create("graphs")

# ==================== CORE CONSTANTS ====================

ECONOMIC_OUTLOOK_LEVELS <- c("Pessimistic", "Neutral", "Optimistic")
LOAN_AMOUNT <- 38375
INTEREST_RATE_BASE <- 0.07
LOAN_TERM <- 3
OFFICE_EXPECTED_SALES <- 65153.54
DEFAULT_SIMULATIONS <- 10000

# ==================== DATA SETUP ====================

convert_outlook <- function(numeric_outlook) {
  case_when(
    numeric_outlook == -1 ~ ECONOMIC_OUTLOOK_LEVELS[1],
    numeric_outlook == 0 ~ ECONOMIC_OUTLOOK_LEVELS[2],
    numeric_outlook == 1 ~ ECONOMIC_OUTLOOK_LEVELS[3]
  )
}

historical_data <- read.csv("crawford/historical_data.csv")

# Calculate probabilities from data
outlook_prob_summary <- historical_data %>%
  count(economic_outlook) %>%
  mutate(probability = n / sum(n))

# Create a named vector for probabilities for easier access
outlook_probabilities <- setNames(
  outlook_prob_summary$probability,
  outlook_prob_summary$economic_outlook
)

# View(outlook_probabilities)

# ==================== CORE FUNCTIONS ====================

calculate_sales_performance <- function(data) {
  data %>%
    group_by(.data$economic_outlook) %>%
    summarise(
      count = n(),
      mean_sales_ratio = mean(.data$sales_ratio),
      sd_sales_ratio = sd(.data$sales_ratio),
      .groups = "drop"
    ) %>%
    arrange(factor(
      .data$economic_outlook,
      levels = ECONOMIC_OUTLOOK_LEVELS
    ))
}

calculate_loan_outcomes <- function(
  realized_sales,
  loan_amount,
  interest_rate,
  loan_term
) {
  total_owed <- loan_amount * (1 + interest_rate)^loan_term

  if (realized_sales >= total_owed) {
    bank_return <- total_owed - loan_amount
    crawford_return <- realized_sales - total_owed
    loan_default <- FALSE
  } else {
    bank_return <- realized_sales - loan_amount
    crawford_return <- 0
    loan_default <- TRUE
  }

  return(list(
    bank_return = bank_return,
    crawford_return = crawford_return,
    loan_default = loan_default
  ))
}

simulate_office_project <- function(
  n_simulations = DEFAULT_SIMULATIONS,
  interest_rate = INTEREST_RATE_BASE,
  perf_stats = performance_stats # Pass pre-calculated stats
) {
  # Ensure outlook_probabilities are in the correct order for sampling
  current_outlook_probs <- c(
    outlook_probabilities["Pessimistic"],
    outlook_probabilities["Neutral"],
    outlook_probabilities["Optimistic"]
  )

  results <- data.frame(
    economic_outlook = sample(
      ECONOMIC_OUTLOOK_LEVELS, n_simulations,
      replace = TRUE,
      prob = current_outlook_probs
    ),
    bank_return = numeric(n_simulations),
    crawford_return = numeric(n_simulations),
    loan_default = logical(n_simulations)
  )

  for (i in seq_along(results$economic_outlook)) {
    outlook <- results$economic_outlook[i]
    stats_row <- perf_stats[perf_stats$economic_outlook == outlook, ]
    sales_ratio <- rnorm(
      1,
      stats_row$mean_sales_ratio,
      stats_row$sd_sales_ratio
    )
    sales_ratio <- pmax(0.3, pmin(1.5, sales_ratio))
    realized_sales <- OFFICE_EXPECTED_SALES * sales_ratio

    loan_outcomes <- calculate_loan_outcomes(
      realized_sales,
      LOAN_AMOUNT,
      interest_rate,
      LOAN_TERM
    )
    results$bank_return[i] <- loan_outcomes$bank_return
    results$crawford_return[i] <- loan_outcomes$crawford_return
    results$loan_default[i] <- loan_outcomes$loan_default
  }

  return(results)
}

simulate_residential_project <- function(
  n_simulations = DEFAULT_SIMULATIONS,
  interest_rate = INTEREST_RATE_BASE
) {
  results <- data.frame(
    bank_return = numeric(n_simulations),
    crawford_return = numeric(n_simulations),
    loan_default = logical(n_simulations)
  )

  for (i in seq_along(results$bank_return)) {
    scenario_rand <- runif(1)

    if (scenario_rand < 0.25) {
      realized_sales <- runif(1, 18000, 22000) # Crisis
    } else if (scenario_rand < 0.65) {
      realized_sales <- rnorm(1, 42300, 5000) # Base
      realized_sales <- pmax(35000, pmin(50000, realized_sales))
    } else {
      realized_sales <- runif(1, 120000, 140000) # Boom
    }

    loan_outcomes <- calculate_loan_outcomes(
      realized_sales,
      LOAN_AMOUNT,
      interest_rate,
      LOAN_TERM
    )
    results$bank_return[i] <- loan_outcomes$bank_return
    results$crawford_return[i] <- loan_outcomes$crawford_return
    results$loan_default[i] <- loan_outcomes$loan_default
  }

  return(results)
}

# ==================== MAIN ANALYSIS ====================

# Pre-calculate performance_stats once
performance_stats <- calculate_sales_performance(historical_data)

cat("=== HISTORICAL PERFORMANCE ===\n")
performance_summary <- calculate_sales_performance(historical_data)
print(performance_summary)

cat("\n=== PROJECT SIMULATIONS ===\n")
office_sim <- simulate_office_project(perf_stats = performance_stats)

# Residential doesn't use historical performance stats in the same way
residential_sim <- simulate_residential_project()

office_metrics <- list(
  expected_return = mean(office_sim$bank_return),
  default_prob = mean(office_sim$loan_default),
  crawford_return = mean(office_sim$crawford_return)
)

residential_metrics <- list(
  expected_return = mean(residential_sim$bank_return),
  default_prob = mean(residential_sim$loan_default),
  crawford_return = mean(residential_sim$crawford_return)
)

cat("Office Project:\n")
cat("  Expected Return: $", round(office_metrics$expected_return, 0), "K\n")
cat(
  "  Default Probability:",
  round(office_metrics$default_prob * 100, 1), "%\n"
)

cat("\nResidential Project:\n")
cat(
  "  Expected Return: $",
  round(residential_metrics$expected_return, 0),
  "K\n"
)

cat(
  "  Default Probability:",
  round(residential_metrics$default_prob * 100, 1),
  "%\n"
)

cat("\nCrawford's Choice:",
    ifelse(residential_metrics$crawford_return > office_metrics$crawford_return,
           "Residential",
           "Office"),
    "\n")

# ==================== INTEREST RATE SENSITIVITY ====================

print("\n=== INTEREST RATE OPTIMIZATION ===")
interest_rates <- seq(0.07, 0.11, 0.01)
sensitivity_results <- data.frame(
  rate = interest_rates,
  bank_return = numeric(length(interest_rates)),
  acceptance_prob = numeric(length(interest_rates)),
  expected_profit = numeric(length(interest_rates))
)

for (i in seq_along(interest_rates)) {
  rate <- interest_rates[i]
  office_temp <- simulate_office_project(n_simulations = 5000,
                                         interest_rate = rate,
                                         perf_stats = performance_stats)

  sensitivity_results$bank_return[i] <- mean(office_temp$bank_return)

  rate_premium <- (rate - INTEREST_RATE_BASE) * 100
  sensitivity_results$acceptance_prob[i] <- max(0, 1 - 0.25 * rate_premium)

  sensitivity_results$expected_profit[i] <-
    sensitivity_results$bank_return[i] * sensitivity_results$acceptance_prob[i]
}

optimal_rate_index <- which.max(sensitivity_results$expected_profit)
optimal_rate <- sensitivity_results$rate[optimal_rate_index]

cat("Optimal Interest Rate:", round(optimal_rate * 100, 1), "%\n")
cat("  Expected Bank Return: $",
    round(sensitivity_results$bank_return[optimal_rate_index], 0),
    "K\n")
cat("  Acceptance Probability:",
    round(sensitivity_results$acceptance_prob[optimal_rate_index] * 100, 0),
    "%\n")

###################################################################
#  KEY VISUALIZATIONS
###################################################################

# 1. Historical Performance
p1 <- ggplot(historical_data, aes(x = sales_ratio)) +
  geom_histogram(bins = 12, alpha = 0.7, fill = "#45B7D1") +
  geom_vline(
    xintercept = 1.0,
    linetype = "dashed",
    color = "red",
    linewidth = 1
  ) +
  labs(
    title = "Crawford's Historical Performance",
    x = "Sales Ratio (Realized/Stated)",
    y = "Number of Loans"
  ) +
  theme_minimal()

# 2. Risk Comparison
risk_data <- data.frame(
  project = c("Office", "Residential"),
  expected_return = c(
    office_metrics$expected_return,
    residential_metrics$expected_return
  ),
  default_prob = c(
    office_metrics$default_prob,
    residential_metrics$default_prob
  )
)

p2 <- ggplot(
  risk_data,
  aes(x = default_prob * 100, y = expected_return, color = project)
) + geom_point(size = 6) +
  geom_text(aes(label = project), vjust = -1, size = 4) +
  scale_color_manual(
    values = c(
      "Office" = "#45B7D1",
      "Residential" = "#FF6B6B"
    )
  ) +
  labs(
    title = "Risk-Return Comparison",
    x = "Default Probability (%)",
    y = "Expected Return ($000s)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 3. Interest Rate Sensitivity
p3 <- ggplot(sensitivity_results, aes(x = rate * 100)) +
  geom_line(
    aes(y = expected_profit, color = "Expected Profit"), linewidth = 1.5
  ) +
  geom_vline(
    xintercept = optimal_rate * 100, linetype = "dashed", linewidth = 1
  ) +
  scale_color_manual(values = c("Expected Profit" = "#2ECC71")) +
  labs(
    title = paste("Optimal Rate:", round(optimal_rate * 100, 1), "%"),
    x = "Interest Rate (%)", y = "Expected Profit ($000s)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Save plots
ggsave("graphs/01_historical_performance.png", p1, width = 8, height = 5)
ggsave("graphs/02_risk_comparison.png", p2, width = 8, height = 5)
ggsave("graphs/03_rate_sensitivity.png", p3, width = 8, height = 5)

grid.arrange(p1, p2, p3, ncol = 2)

# ==================== FINAL RECOMMENDATION ====================

cat("\n=== RECOMMENDATION ===\n")
cat("APPROVE loan with following terms:\n")
cat("  - Interest Rate:", round(optimal_rate * 100, 1), "%\n")
cat("  - Project Restriction: Office development only\n")
cat("  - Expected Return: $",
    round(sensitivity_results$bank_return[optimal_rate_index], 0), "K\n")
cat("  - Default Risk:", round(office_metrics$default_prob * 100, 1), "%\n")
