suppressPackageStartupMessages(
  suppressWarnings({
    library(dplyr)
    library(tidyverse)
  })
)

convert_outlook <- function(numeric_outlook) {
  case_when(
    numeric_outlook == -1 ~ "Pessimistic",
    numeric_outlook == 0 ~ "Neutral",
    numeric_outlook == 1 ~ "Optimistic"
  )
}

historical_data <- data.frame(
  loan_id = 1:32,
  stated_sales = c(
    66382.35, 60852.39, 65872.30,
    64541.97, 73871.30, 52689.86,
    63470.57, 55828.80,
    63972.39, 61999.35, 78151.43,
    75928.84, 73195.79, 70009.95,
    58471.00, 66242.29,
    79315.70, 60000.95, 60839.91,
    65844.29, 70511.40, 49527.53,
    67793.12, 72333.48,
    55992.65, 75956.10, 61969.04,
    55875.70, 74283.09,
    85382.88, 69717.37, 56725.72
  ),
  economic_outlook_numeric = c(
    -1, 0, 0, 1, 1, -1,
    -1, 1, 0, 1, 1, 1,
    -1, -1, 0, 1, 0,
    -1, 1, -1, -1, -1,
    0, -1, 1, -1,
    0, 0, 1,
    0, 1, -1
  ),
  realized_sales = c(
    51302.37, 63452.49, 67932.41,
    79689.09, 70408.31, 46006.15,
    61575.80, 65413.68,
    57756.02, 72319.87, 84962.93,
    82105.66, 64361.14, 64554.01,
    56729.86, 76268.45,
    84896.93, 43384.19, 68533.78,
    51601.49, 62379.33, 36775.17,
    70128.14, 61806.21,
    68218.22, 63401.88, 60720.96,
    65842.55, 78509.94, 83909.33,
    87853.35, 38732.77
  )
) %>%
  mutate(
    # Transform to Pesimistic, Optimistic and Neutral
    economic_outlook = convert_outlook(economic_outlook_numeric),
    # Key performance metric (realized ÷ stated sales)
    sales_ratio = realized_sales / stated_sales,
    year = 2007 - (32 - loan_id)  # Assuming loans span from 1975 to 2007
  )

write.csv(historical_data, "./historical_data.csv", row.names = FALSE)
