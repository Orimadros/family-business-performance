# Load necessary libraries
library(tidyverse)
library(lubridate)
library(here)
library(glue)
library(ggplot2)
library(scales)
library(grid)
library(readxl)
library(viridis)
library(waterfalls)

# -------------------------------
# 1. Load the Data
# -------------------------------

# Load stock data
stock_data <- readRDS(here('data', 'stock_data.rds'))

# Load pairs data
pairs <- read_csv2(here('data', 'imperfect_sample.csv'), show_col_types = FALSE)

# -------------------------------
# 2. Prepare and Clean the Data
# -------------------------------

# 2.1 Prepare the Pairs Data
pairs_long <- pairs %>%
  mutate(pair_id = row_number()) %>%  # Create a unique pair_id
  pivot_longer(
    cols = c(f_ticker, nf_ticker),
    names_to = "family",
    values_to = "ticker"
  ) %>%
  mutate(family = ifelse(family == 'f_ticker', 'Family', 'Non-Family'))

# 2.2 Identify Pair IDs Containing 'SHP', 'GME', and '253'
pair_ids_SHP <- pairs_long %>%
  filter(ticker == "SHP") %>%
  pull(pair_id)

pair_ids_GME <- pairs_long %>%
  filter(ticker == "GME") %>%
  pull(pair_id)

# Combine all pair_ids to exclude (SHP, GME, and 253)
pair_ids_exclude <- unique(c(pair_ids_SHP, pair_ids_GME, 253))

# **Adjust dates: if date is the first day of the month, change it to the last day of the previous month**
stock_data <- stock_data %>%
  mutate(date = if_else(day(date) == 1, date - days(1), date))

# 2.3 Prepare the Stock Data with Additional Filters
stock_data <- stock_data %>%
  rename(ticker = symbol) %>%  # Rename for consistency
  inner_join(pairs_long, by = 'ticker') %>%  # Merge with pairs data to get pair_id and family status
  
  # **Filter out specific tickers before their designated dates and exclude pairs containing 'SHP', 'GME', or 253**
  filter(
    !(ticker == "FRAS.L" & date < as.Date("2007-03-01")),
    !(ticker == "RAJESHEXPO.NS" & date < as.Date("2005-09-01")),
    !(ticker == "017940.KS" & date < as.Date("2005-09-01")),
    !(ticker == "LIVEPOL1.MX" & date < as.Date("2008-01-01")),
    !(ticker == "1913.HK" & date < as.Date("2011-06-01")),
    !(ticker == "ENGI3.SA" & date < as.Date("2007-05-01")),
    !(ticker == "BZU.MI" & date < as.Date("2000-08-31")),
    !(ticker == "FRAGUAB.MX" & date < as.Date("2006-07-31")),
    !(pair_id %in% pair_ids_exclude)  # Exclude all data for pairs containing 'SHP', 'GME', or pair_id 253
  )

# -------------------------------
# 3. Handle Negative Adjusted Close Prices
# -------------------------------

# 3.1 Identify Latest Negative Price Date per Ticker
latest_negative_dates <- stock_data %>%
  filter(adjusted < 0) %>%                    # Filter negative adjusted close prices
  group_by(ticker) %>%                        # Group by ticker
  summarize(latest_neg_date = max(date), .groups = 'drop')  # Find latest negative date per ticker

# 3.2 Remove All Data from Latest Negative Date Backwards for Each Ticker
# This includes the latest negative date and all dates before it

# Join the latest_negative_dates with the stock_data
stock_data_cleaned <- stock_data %>%
  left_join(latest_negative_dates, by = "ticker") %>%
  # If latest_neg_date is not NA, filter out records on or before that date
  filter(is.na(latest_neg_date) | date > latest_neg_date) %>%
  select(-latest_neg_date)  # Remove the helper column

# Inform about the data removal
removed_records <- stock_data %>%
  left_join(latest_negative_dates, by = "ticker") %>%
  filter(!is.na(latest_neg_date) & date <= latest_neg_date)

cat("Removed", nrow(removed_records), "records due to negative adjusted close prices.\n")

# Update stock_data to the cleaned version
stock_data <- stock_data_cleaned

# -------------------------------
# 4. Identify Earliest Available Data for Each Company and Pair
# -------------------------------

# 4.1 Find the Earliest Date for Each Company
company_earliest_dates <- stock_data %>%
  group_by(pair_id, family) %>%
  summarize(first_date = min(date), .groups = 'drop')

# 4.2 Find the Earliest Date for Each Pair
pair_earliest_dates <- company_earliest_dates %>%
  pivot_wider(names_from = family, values_from = first_date) %>%
  mutate(both_earliest_date = pmax(Family, `Non-Family`, na.rm = TRUE))

# -------------------------------
# 5. Define the Time Periods
# -------------------------------

periods <- list(
  "2000-2023" = c(as.Date("2000-02-01"), as.Date("2023-12-31"))
)

# -------------------------------
# 6. Calculate and Plot Average Total Returns for Each Period
# -------------------------------

# Initialize a list to store average returns for each period
average_returns <- list()

# Initialize a dataframe to store G_t for each stock and date
stock_returns <- list()

# Initialize a list to store annualized TSR for each period and family
annualized_TSR_list <- list()

# Loop over each period
for (period_name in names(periods)) {
  
  period_dates <- periods[[period_name]]
  start_date <- period_dates[1]
  end_date <- period_dates[2]
  
  # Identify pairs with data from the start of the period
  eligible_pairs <- pair_earliest_dates %>%
    filter(both_earliest_date <= start_date) %>%
    pull(pair_id)
  
  num_pairs <- length(eligible_pairs)
  
  cat("Period:", period_name, "- Eligible Pairs:", num_pairs, "\n")
  
  # Filter stock data for eligible pairs and dates within the period
  period_stock_data <- stock_data %>%
    filter(pair_id %in% eligible_pairs, date >= start_date, date <= end_date)
  
  # Check if there is sufficient data for the period
  if (nrow(period_stock_data) == 0) {
    warning(paste("No data available for period:", period_name))
    next
  }
  
  # Step 1: Calculate p0 for Each Stock
  stock_p0 <- period_stock_data %>%
    arrange(date) %>%
    group_by(ticker, pair_id, family) %>%
    summarize(p0 = first(adjusted), .groups = 'drop')
  
  # Step 2: Merge p0 with Period Stock Data
  period_stock_with_p0 <- period_stock_data %>%
    left_join(stock_p0, by = c("ticker", "pair_id", "family")) 
  
  # Step 3: Calculate G_t for Each Stock and Date
  period_stock_with_Gt <- period_stock_with_p0 %>%
    mutate(G_t = adjusted / p0 - 1)
  
  # Store G_t in stock_returns list
  stock_returns[[period_name]] <- period_stock_with_Gt
  
  # Step 4: Calculate Average G_t per Family and Date
  portfolio_average_returns <- period_stock_with_Gt %>%
    group_by(family, date) %>%
    summarize(
      average_G_t = mean(G_t, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Store the average returns in the list
  average_returns[[period_name]] <- portfolio_average_returns
  
  # -------------------------------
  # 7. Calculate Annualized TSR for Each Family
  # -------------------------------
  
  # Step 5: Extract Cumulative TSR from the Last Date for Each Family
  cumulative_TSR <- portfolio_average_returns %>%
    group_by(family) %>%
    summarize(
      cumulative_TSR = last(average_G_t),
      first_date = first(date),
      last_date = last(date),
      .groups = 'drop'
    )
  
  # Step 6: Calculate the Number of Years in the Period
  cumulative_TSR <- cumulative_TSR %>%
    mutate(
      num_days = as.numeric(difftime(last_date, first_date, units = "days")),
      num_years = num_days / 365.25  # Accounting for leap years
    )
  
  # Step 7: Annualize the TSR
  cumulative_TSR <- cumulative_TSR %>%
    mutate(
      annualized_TSR = (1 + cumulative_TSR)^(1 / num_years) - 1
    )
  
  # Store the annualized TSR
  annualized_TSR_list[[period_name]] <- cumulative_TSR
}

# Combine all average returns into a single dataframe for plotting
average_returns_df <- bind_rows(average_returns, .id = "Period")

# Combine annualized TSR into a single dataframe
annualized_TSR_df <- bind_rows(annualized_TSR_list, .id = "Period")

# -------------------------------
# 8. Print Annualized TSR to R Console
# -------------------------------

# Format the TSR as percentages with one decimal place
annualized_TSR_df <- annualized_TSR_df %>%
  mutate(
    annualized_TSR_percent = scales::percent(annualized_TSR, accuracy = 0.1)
  )

# -------------------------------
# 9. Plot the Average Total Returns
# -------------------------------

# Plot the average total returns for each portfolio over time
ggplot(average_returns_df, aes(x = date, y = average_G_t, color = family)) +
  geom_line(size = 1) +
  # Add labels at the end of each line
  geom_text(
    data = average_returns_df %>% 
      group_by(family) %>% 
      slice_tail(n = 1) %>%
      ungroup(),
    aes(label = family),
    hjust = -0.1,          # Slightly to the right of the last point
    fontface = "bold",
    show.legend = FALSE
  ) +
  labs(
    title = "Average Total Returns by Portfolio (2000-2023)",
    subtitle = paste("Number of Pairs Included:", num_pairs),
    x = "Date",
    y = "Total Return",
    color = "Portfolio"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("Family" = "darkgreen", "Non-Family" = "darkgrey")) +
  theme(
    legend.position = "none",               # Remove the legend
    plot.margin = margin(5.5, 80, 5.5, 5.5) # Increase right margin to accommodate labels
  ) +
  # Extend the x-axis to make space for labels
  coord_cartesian(clip = "off")  # Allow drawing outside plot area

# Print the Annualized TSR for each portfolio
cat("\nAnnualized Total Shareholder Return (TSR) for Each Portfolio (2000-2023):\n")
annualized_TSR_df %>%
  select(family, annualized_TSR_percent) %>%
  print(n = Inf)

# -------------------------------
# 10. Compute Real Value Growth Over a Generation (30 years)
# -------------------------------

# Assume a constant annual inflation rate (e.g., 2% per year as a proxy for CPI)
inflation_rate <- 0.0255

# Extract the annualized TSR for Family and Non-Family from the computed data (period "2000-2023")
tsr_family <- annualized_TSR_df %>% filter(family == "Family") %>% pull(annualized_TSR)
tsr_non_family <- annualized_TSR_df %>% filter(family == "Non-Family") %>% pull(annualized_TSR)

# In case there are multiple rows, take the first value for each
if(length(tsr_family) > 1) { tsr_family <- tsr_family[1] }
if(length(tsr_non_family) > 1) { tsr_non_family <- tsr_non_family[1] }

# Define the generation period (30 years) and initial investment ($100 million, in millions)
generation_years <- 30
initial_investment <- 100

# Create a data frame simulating the portfolio growth year by year
years <- seq(0, generation_years, by = 1)
growth_df <- tibble(
  year = years,
  # Nominal portfolio values using compound growth with the annualized TSR
  nominal_family = initial_investment * (1 + tsr_family)^year,
  nominal_non_family = initial_investment * (1 + tsr_non_family)^year,
  # Deflate the nominal values by the cumulative inflation factor to get real values
  real_family = nominal_family / ((1 + inflation_rate)^year),
  real_non_family = nominal_non_family / ((1 + inflation_rate)^year),
  # Calculate the extra real value created by the family company
  extra_value = real_family - real_non_family
)

# -------------------------------
# 11. Plot Evolution of the Two Portfolios with Extra Value Highlighted using theme_bw()
# -------------------------------

# Pivot the data to long format
growth_df_long <- growth_df %>%
  select(year, real_family, real_non_family) %>%
  pivot_longer(
    cols = c(real_family, real_non_family),
    names_to = "Portfolio",
    values_to = "Value"
  ) %>%
  mutate(Portfolio = if_else(Portfolio == "real_family", 
                             "Family Portfolio", 
                             "Non-Family Portfolio"))

# Calculate final endpoints for the generation (year = generation_years)
final_data <- growth_df %>% filter(year == generation_years)
final_family <- final_data$real_family
final_non_family <- final_data$real_non_family
final_extra_value <- final_data$extra_value
mid_y <- (final_family + final_non_family) / 2

ggplot() +
  # Shade the area between the two portfolios
  geom_ribbon(
    data = growth_df,
    aes(x = year, ymin = real_non_family, ymax = real_family),
    fill = "lightblue", alpha = 0.5
  ) +
  # Plot both lines in long format
  geom_line(
    data = growth_df_long,
    aes(x = year, y = Value, color = Portfolio),
    size = 1.2
  ) +
  # Add text labels at the end of each line
  geom_text(
    data = growth_df_long %>%
      group_by(Portfolio) %>%
      slice_tail(n = 1) %>%
      ungroup(),
    aes(x = year, y = Value, label = Portfolio, color = Portfolio),
    hjust = -0.1,    # Positions text slightly to the right of the last point
    fontface = "bold",
    show.legend = FALSE
  ) +
  # Annotate with the extra value text
  annotate("text", 
           x = generation_years + 1, 
           y = mid_y, 
           label = paste("+ $", round(final_extra_value, 1), "m", sep = ""), 
           color = "blue", 
           fontface = "bold", 
           hjust = 0) +
  labs(
    title = "Evolution of Portfolio Values Over a Generation",
    subtitle = "Real value of a US$100 million portfolio over 30 years",
    x = "Years",
    y = "Real Value (US$ millions)"
  ) +
  coord_cartesian(clip = "off") +
  theme_classic() +
  theme(
    legend.position = "none",
    plot.margin = margin(5.5, 120, 5.5, 5.5)  # Increase right margin for labels
  ) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "m", scale = 1)) +
  scale_x_continuous(
    breaks = seq(0, generation_years, 5),
    limits = c(0, generation_years + 5)  # Extend x-axis to ensure labels are fully visible
  ) +
  scale_color_manual(values = c("Family Portfolio" = "darkgreen",
                                "Non-Family Portfolio" = "darkred"))


# Extract and display the eligible pairs in wide format (Family and Non-Family tickers)
eligible_pairs_df <- pairs_long %>%
  filter(pair_id %in% eligible_pairs) %>%
  pivot_wider(names_from = family, values_from = ticker) %>%
  arrange(pair_id)

eligible_pairs_df %>% write_csv('eligible_pairs_tickers.csv')


#############

# ROB TSR ANALYSIS

# Calculate TSR for each company
company_TSR <- period_stock_data %>%
  group_by(ticker, pair_id, family) %>%
  arrange(date) %>%
  summarize(
    start_date = first(date),
    end_date   = last(date),
    p0         = first(adjusted),
    pT         = last(adjusted),
    cumulative_TSR = pT / p0 - 1,  # Total return over the period
    num_days   = as.numeric(difftime(end_date, start_date, units = "days")),
    num_years  = num_days / 365.25, # Accounting for leap years
    annualized_TSR = (pT / p0)^(1 / num_years) - 1,  # Annualized return
    .groups    = 'drop'
  )


correct_ticker <- function(ticker) {
  # Ensure the ticker is a character string
  if (!is.character(ticker)) {
    ticker <- as.character(ticker)
  }
  
  # If the ticker contains a period, split and take the second element
  if (str_detect(ticker, "\\.")) {
    parts <- str_split(ticker, "\\.", simplify = TRUE)
    # If there is a second part, replace ticker with it; otherwise, keep as is
    if (ncol(parts) >= 2 && parts[1, 2] != "") {
      ticker <- parts[1, 1]
    }
  }
  
  # For South Korea tickers: if ticker is exactly six digits, prefix with "A"
  if (str_detect(ticker, "^\\d{6}$")) {
    ticker <- paste0("A", ticker)
  }
  
  # For tickers exactly four digits, remove leading zeros by converting to an integer
  if (str_detect(ticker, "^\\d{4}$")) {
    ticker <- as.character(as.integer(ticker))
  }
  
  return(ticker)
}

# Apply the function to the 'ticker' column in company_TSR
company_TSR <- company_TSR %>%
  mutate(ticker = sapply(ticker, correct_ticker))


# Load the sample_with_ciq_data.xlsx file
ciq_sample <- read_excel(here("Data", "sample_with_ciq_data.xlsx"))

# To avoid many-to-many issues, collapse ciq_sample so each ticker appears only once.
# Also, remove any columns that overlap with company_TSR (except "ticker") to avoid suffixes.
common_cols <- intersect(names(ciq_sample), names(company_TSR))
common_cols <- setdiff(common_cols, "ticker")  # allow ticker to remain


# Subset all rows with duplicate tickers
ciq_sample_duplicates <- ciq_sample %>%
  group_by(ticker) %>%
  filter(n() > 1) %>%
  ungroup()

# View the duplicate rows
print(ciq_sample_duplicates)

ciq_sample_unique <- ciq_sample %>%
  select(-all_of(common_cols)) %>%
  group_by(ticker) %>%
  sample_n(1) %>%    # randomly choose one row per ticker
  ungroup()

# Perform the left join on "ticker"
company_TSR_merged <- company_TSR %>%
  inner_join(ciq_sample_unique, by = "ticker", relationship = "many-to-many")

# 1) Filter data for valid rows
df_valid <- company_TSR_merged %>%
  filter(
    !is.na(iq_total_rev_2023),
    !is.na(iq_total_rev_2000),
    !is.na(iq_ebitda_margin_2023),
    !is.na(iq_ebitda_margin_2000),
    !is.na(num_years),
    !is.na(annualized_TSR),
    !is.na(price_growth_factor)
  )

df_valid <- df_valid %>%
  mutate(
    # Convert growth rates into multiplicative factors:
    rev_factor         = 1 + rev_growth,
    net_margin_factor  = 1 + net_margin_growth,
    shares_factor      = 1 + shares_out_growth,  # note: if shares increase, factor > 1
    pe_factor          = 1 + pe_growth,
    tsr_factor         = 1 + cumulative_TSR,
    
    # Fundamental (operational) price change factor from underlying business:
    fundamental_factor = (rev_factor * net_margin_factor * pe_factor) / shares_factor,
    
    # Dividend factor as the residual needed to reach the observed TSR:
    dividend_factor    = tsr_factor / fundamental_factor,
    
    # Calculate the logarithms:
    log_tsr      = log(tsr_factor),
    log_rev      = log(rev_factor),
    log_margin   = log(net_margin_factor),
    log_shares   = -log(shares_factor),  # negative because an increase in shares dilutes EPS
    log_pe       = log(pe_factor),

    # Calculate contribution shares as fractions of total log return:
    rev_contribution_share      = log_rev / log_tsr,
    margin_contribution_share   = log_margin / log_tsr,
    shares_contribution_share   = log_shares / log_tsr,
    pe_contribution_share       = log_pe / log_tsr,
  ) %>%
  select(-start_date, -end_date, -p0, -pT, -annualized_TSR, -num_days, num_years,
         -'...1', -sp_entity_id)

# Filter to include only companies with positive TSR (tsr_factor > 1, i.e. cumulative_TSR > 0)
df_valid_pos <- df_valid %>% 
  filter(tsr_factor > 1)

# Aggregate the average contribution shares across the positive TSR sample:
df_summary_pos <- df_valid_pos %>%
  summarise(
    rev = mean(rev_contribution_share, na.rm = TRUE),
    margin = mean(margin_contribution_share, na.rm = TRUE),
    shares = mean(shares_contribution_share, na.rm = TRUE),
    pe = mean(pe_contribution_share, na.rm = TRUE),
    dividend = 1 - rev - margin - shares - pe
  ) %>%
  pivot_longer(cols = everything(), names_to = "Component", values_to = "Share") %>%
  mutate(SharePct = Share * 100)

# Extract the numeric contributions and corresponding component labels
values <- df_summary_pos$SharePct
labels <- df_summary_pos$Component

# Create the waterfall chart using the numeric vector for positive TSR companies:
waterfall(
  values = values,
  labels = labels,
  calc_total = TRUE,
  total_axis_text = "Total TSR",
  rect_text_labels = paste0(round(values, 1), "%"),
  rect_text_size = 4,
  fill_colours = c("#2E86AB", "#F6C85F", "#6B8E23", "#D7263D", "#8E44AD")
)
