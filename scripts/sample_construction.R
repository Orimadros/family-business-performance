# Load necessary libraries
library(tidyverse)
library(here)
library(readxl)

# Importing the stock price data
data <- readRDS(here('data', 'stock_prices.rds'))

# Function to standardize company names
standardize_company <- function(name) {
  name %>%
    str_to_lower() %>%                     # Convert to lowercase
    str_remove_all("[^a-z]")               # Remove non-letter characters
}

# Importing and wrangling top 414 public FAMILY companies by revenue (FamCap)
topFB_revenue <- read_excel(here('data', 'world_750_FB.xlsx'))
topFB_revenue <- topFB_revenue %>%
  mutate(
    family = factor("Family", levels = c("Family", "Non-Family")),
  )

# Importing and wrangling top 3980 public companies by revenue
top3980_revenue <- read_excel(here('data', 'top3980_revenue_world_2022.xlsx'))
top3980_revenue <- top3980_revenue %>%
  filter(company != 'CARNIVAL CORP')

# Checking which FBs are in the stock data

# Extract 'symbol' as a vector
companies <- data %>% distinct(symbol) %>% pull(symbol)

# Now, filter using the vector
FBs_in_data <- topFB_revenue %>%
  filter(ticker %in% companies) %>%
  select(ticker)

# Mapping bloomberg tickers to yahoo tickers

# Define the exchange mapping as a named vector with quoted keys
exchange_mapping <- c(
  "GR" = "DE",
  "AB" = "SR",
  "AR" = "BA",
  "AU" = "AX",
  "AV" = "VI",
  "BB" = "BR",
  "BO" = "BO",
  "BZ" = "SA",
  "CB" = "CL",
  "CH" = "SS",
  "CI" = "SN",
  "CN" = "TO",
  "DC" = "CO",
  "EY" = "CA",
  "FH" = "HE",
  "FP" = "PA",
  "GA" = "AT",
  "HK" = "HK",
  "HM" = "HM",
  "ID" = "IR",
  "IJ" = "JK",
  "IM" = "MI",
  "IN" = "NS",
  "IT" = "TA",
  "JP" = "T",
  "KF" = "KS",
  "KS" = "KS",
  "LN" = "L",
  "MK" = "KL",
  "MM" = "MX",
  "NA" = "AS",
  "NL" = "JO",
  "NO" = "OL",
  "NZ" = "NZ",
  "PL" = "LS",
  "PW" = "WA",
  "RM" = "ME",
  "RU" = "ME",
  "SM" = "MC",
  "SP" = "SI",
  "SS" = "ST",
  "SZ" = "SZ",
  "SW" = "SW",
  "TB" = "BK",
  "TI" = "IS",
  "TT" = "TW",
  "VN" = "VN",
  "US" = NA  # Added 'US' to handle exchanges from the US
)

# Function to map exchange codes
map_exchange <- function(codes) {
  # Coerce the input to character
  codes_char <- as.character(codes)
  
  # Convert inputs to uppercase to ensure case-insensitive matching
  codes_upper <- toupper(codes_char)
  
  # Retrieve mapped values; unmapped codes will result in NA
  mapped_values <- exchange_mapping[codes_upper]
  
  # Return as character vector
  return(as.character(mapped_values))
}

# Apply mapping and create 'yahoo_ticker'
top3980_revenue <- top3980_revenue %>%
  mutate(
    mapped_exchange = map_exchange(exchange),
    ticker = if_else(
      is.na(mapped_exchange),
      ticker,  # If mapping is NA, set yahoo_ticker as ticker
      paste0(ticker, ".", mapped_exchange)
    )
  )

# Flagging FBs in top3980

# Primary left_join by symbol_key
top3980_flagged <- top3980_revenue %>%
  left_join(
    topFB_revenue %>% select(ticker, family),
    by = "ticker"
  ) %>%
  mutate(family = coalesce(family, 'Non-Family'))


# saving flagged data to construct matched samples in python
write_csv(top3980_flagged, here('data', 'top3980_flagged.csv'))
