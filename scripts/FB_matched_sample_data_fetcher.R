# ============================
# 1. Load Necessary Libraries
# ============================
library(tidyverse)
library(tidyquant)
library(here)
library(glue)        # For enhanced messaging
library(progress)    # For progress bars

# ============================
# 2. Define Constants and Helper Functions
# ============================


# Constants
START_DATE <- as.Date("2000-01-01")
END_DATE <- Sys.Date()
DATA_DIR <- here("data")

# Function to fetch and process stock data for a list of tickers
fetch_stock_data <- function(tickers, start_date, end_date) {
  # Initialize progress bar
  pb <- progress_bar$new(
    format = "  Fetching [:bar] :current/:total (:percent) in :elapsed",
    total = length(tickers), clear = FALSE, width = 60
  )
  
  # Fetch data with progress updates
  tickers %>%
    purrr::map_dfr(
      ~ {
        pb$tick()
        tryCatch(
          {
            tq_get(.x,
                   from = start_date,
                   to = end_date,
                   periodicity = "monthly") %>%
              select(symbol, date, adjusted)
          },
          error = function(e) {
            message(glue("❌ Failed to fetch data for ticker: {.x}"))
            NULL
          }
        )
      }
    )
}

# ============================
# 3. Import and Prepare Data
# ============================

pairs <- read_csv2(here('data', 'imperfect_sample.csv'))
tickers_needed <- tibble(ticker = c(pairs$f_ticker, pairs$nf_ticker))

# ============================
# 4. Fetch Stock Data
# ============================

# Fetch data for all tickers in tickers_needed
stock_data <- fetch_stock_data(tickers_needed$ticker, START_DATE, END_DATE)


# ============================
# 5. Save Processed Data
# ============================

# Save fetched stock data as RDS for efficient storage and retrieval
saveRDS(stock_data, here(DATA_DIR, "stock_data.rds"))

