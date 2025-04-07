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
    total = length(tickers), clear = FALSE, width=60
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

# Import largest companies data
largest_companies <- read_csv(here(DATA_DIR, "largest_companies_world.csv")) %>%
  rename(Ticker = Symbol)

# Import public family companies' ticker data
family_tickers <- read_csv2(here(DATA_DIR, "public_family.csv")) %>%
  rename(Ticker = Symbol) %>%  # Adjust 'Symbol' if the column name differs
  mutate(Family = TRUE)

# Identify non-family tickers
non_family_tickers <- largest_companies %>%
  filter(!Ticker %in% family_tickers$Ticker) %>%
  pull(Ticker) %>%
  unique()

# ============================
# 4. Fetch Stock Data
# ============================

# Fetch data for family tickers
family_stock_data <- fetch_stock_data(family_tickers$Ticker, START_DATE, END_DATE)

# Fetch data for non-family tickers
non_family_stock_data <- fetch_stock_data(non_family_tickers, START_DATE, END_DATE)

# ============================
# 5. Process and Merge Data
# ============================

# Process family stock data
family_stock_prices <- family_stock_data %>%
  left_join(family_tickers, by = c("symbol" = "Ticker")) %>%
  left_join(largest_companies, by = c("symbol" = "Ticker")) %>%  # To get Company and Country
  select(Company, Country, symbol, date, adjusted, Family) %>%
  arrange(symbol, date)

# Process non-family stock data
nonfamily_stock_prices <- non_family_stock_data %>%
  left_join(largest_companies, by = c("symbol" = "Ticker")) %>%
  select(Country, symbol, date, adjusted) %>%
  arrange(symbol, date) %>%
  mutate(Family = FALSE)

# Combine family and non-family stock data
all_stock_prices <- bind_rows(family_stock_prices, nonfamily_stock_prices)
all_stock_prices <- all_stock_prices %>%
  mutate(Country = coalesce(Country, country)) %>%  # Create/overwrite 'Country' with non-NA values
  select(date, symbol, Family, Country, adjusted)

# ============================
# 6. Save Processed Data
# ============================

# Save family and non-family stock data separately (optional)
# write_csv(family_stock_prices, here(DATA_DIR, "family_stock_prices.csv"))
# write_csv(nonfamily_stock_prices, here(DATA_DIR, "nonfamily_stock_prices.csv"))

# Save combined stock data as RDS for efficient storage and retrieval
saveRDS(all_stock_prices, here(DATA_DIR, "all_stock_prices.rds"))

# ============================
# 7. Optional: Explore or Output Results
# ============================

# View distinct family companies
family_companies <- family_stock_prices %>%
  distinct(Company) %>%
  arrange(Company)

message("Family Companies:")
print(family_companies)

# View distinct non-family companies
non_family_companies <- nonfamily_stock_prices %>%
  distinct(Company) %>%
  arrange(Company)

message("Non-Family Companies:")
print(non_family_companies)
