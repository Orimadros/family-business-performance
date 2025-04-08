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
stock_data_cleaned <- stock_data %>%
  left_join(latest_negative_dates, by = "ticker") %>%
  filter(is.na(latest_neg_date) | date > latest_neg_date) %>%
  select(-latest_neg_date)

# Update stock_data to the cleaned version
stock_data <- stock_data_cleaned

# -------------------------------
# 4. Calculate TSR and Growth Factors
# -------------------------------

# Calculate TSR for each company
company_TSR <- stock_data %>%
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
    price_growth_factor = pT / p0,  # Price growth factor
    .groups    = 'drop'
  )

# Correct ticker function
correct_ticker <- function(ticker) {
  if (!is.character(ticker)) {
    ticker <- as.character(ticker)
  }
  
  if (str_detect(ticker, "\\.")) {
    parts <- str_split(ticker, "\\.", simplify = TRUE)
    if (ncol(parts) >= 2 && parts[1, 2] != "") {
      ticker <- parts[1, 1]
    }
  }
  
  if (str_detect(ticker, "^\\d{6}$")) {
    ticker <- paste0("A", ticker)
  }
  
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

# Print column names
cat("\nColumns in ciq_sample:\n")
print(names(ciq_sample))

# Remove any columns that overlap with company_TSR (except "ticker")
common_cols <- intersect(names(ciq_sample), names(company_TSR))
common_cols <- setdiff(common_cols, "ticker")

# Handle duplicate tickers by randomly selecting one row per ticker
ciq_sample_unique <- ciq_sample %>%
  select(-all_of(common_cols)) %>%
  group_by(ticker) %>%
  sample_n(1) %>%
  ungroup()

# Merge company_TSR with ciq_sample_unique
company_TSR_merged <- company_TSR %>%
  inner_join(ciq_sample_unique, by = "ticker", relationship = "many-to-many")

# -------------------------------
# 5. Generate TSR vs Growth Scatter Plot (Mimicking Image)
# -------------------------------

# Calculate annualized revenue growth
plot_data <- company_TSR_merged %>%
  mutate(
    # Ensure rev_growth and num_years are numeric and handle potential issues
    rev_growth = as.numeric(rev_growth),
    num_years = as.numeric(num_years),
    # Calculate annualized revenue growth, handle cases where num_years is zero or NA
    annualized_rev_growth = if_else(
      num_years > 0 & !is.na(rev_growth),
      (1 + rev_growth)^(1/num_years) - 1,
      NA_real_
    )
  ) %>%
  # Filter out rows with NA values needed for the plot
  filter(!is.na(annualized_TSR) & !is.na(annualized_rev_growth))

# Determine plot limits, adding some padding
x_min <- min(plot_data$annualized_rev_growth, na.rm = TRUE) - 0.05
x_max <- max(plot_data$annualized_rev_growth, na.rm = TRUE) + 0.05
y_min <- min(plot_data$annualized_TSR, na.rm = TRUE) - 0.05
y_max <- max(plot_data$annualized_TSR, na.rm = TRUE) + 0.05

# Define quadrant boundaries based on image (TSR=10%, Growth=0%)
tsr_threshold <- 0.10
growth_threshold <- 0

# Calculate average ROA for relevant quadrants
avg_roa_value_creating <- plot_data %>%
  filter(annualized_rev_growth > growth_threshold & annualized_TSR > tsr_threshold) %>%
  summarise(avg_roa = mean(as.numeric(roa_2023), na.rm = TRUE)) %>%
  pull(avg_roa)

avg_roa_value_destroying <- plot_data %>%
  filter(annualized_rev_growth > growth_threshold & annualized_TSR < tsr_threshold) %>%
  summarise(avg_roa = mean(as.numeric(roa_2023), na.rm = TRUE)) %>%
  pull(avg_roa)

# Format the ROA strings
roa_text_value_creating <- sprintf("AVG ROA (2023) = %.1f%%", avg_roa_value_creating)
roa_text_value_destroying <- sprintf("AVG ROA (2023) = %.1f%%", avg_roa_value_destroying)

# Create the scatter plot
growth_tsr_scatter <- ggplot(plot_data, aes(x = annualized_rev_growth, y = annualized_TSR)) +
  geom_point(aes(color = family), alpha = 0.7, size = 2) + # Color points by family status

  # Add quadrant lines (subtler than rectangles)
  geom_hline(yintercept = tsr_threshold, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = growth_threshold, linetype = "dashed", color = "grey40") +

  # Add quadrant defining rectangles (using plot limits)
  # Value-creating growth (Top-Right)
  annotate("rect", xmin = growth_threshold, xmax = x_max, ymin = tsr_threshold, ymax = y_max,
           alpha = 0, color = "darkgreen", linetype = "dashed", size = 0.8) +
  # No growth (Top-Left)
  annotate("rect", xmin = x_min, xmax = growth_threshold, ymin = tsr_threshold, ymax = y_max,
           alpha = 0, color = "#E69F00", linetype = "dashed", size = 0.8) + # Approx Yellow/Orange
  # Value-destroying growth (Bottom-Right)
  annotate("rect", xmin = growth_threshold, xmax = x_max, ymin = y_min, ymax = tsr_threshold,
           alpha = 0, color = "firebrick", linetype = "dashed", size = 0.8) +

  # Add quadrant labels
  annotate("text", x = growth_threshold + 0.01, y = y_max - 0.01, # Top-left of box + offset
           label = paste("Value-creating growth", roa_text_value_creating, sep = "\n"), # Add ROA
           hjust = 0, vjust = 1, color = "darkgreen", size = 4, fontface = "bold", lineheight = 0.9) +
  annotate("text", x = x_min + 0.01, y = y_max - 0.01, # Top-left of box + offset
           label = "No growth",
           hjust = 0, vjust = 1, color = "#E69F00", size = 4, fontface = "bold") +
  annotate("text", x = growth_threshold + 0.01, y = y_min + 0.01, # Bottom-left of box + offset
           label = paste("Value-destroying growth", roa_text_value_destroying, sep = "\n"), # Add ROA
           hjust = 0, vjust = 0, color = "firebrick", size = 4, fontface = "bold", lineheight = 0.9) +

  # Formatting
  scale_x_continuous(labels = percent_format(), limits = c(x_min, x_max)) + # Reset x limit
  scale_y_continuous(labels = percent_format(), limits = c(y_min, y_max)) +
  scale_color_manual(values = c("Family" = "#021882", "Non-Family" = "grey60")) + # Set custom colors
  labs(
    title = "Correlation between Annualized TSR and Annualized Revenue Growth",
    subtitle = paste("Based on available data (", nrow(plot_data), " companies)", sep=""),
    x = "Average Annual Turnover Growth (%)",
    y = "Annual TSR (%)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey30"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), # Reduce margin
    legend.title = element_blank(), # Make legend title bold
    legend.position = "bottom" # Move legend to bottom
   )

# Save the plot
ggsave(here("plots", "growth_vs_tsr_scatter.png"), growth_tsr_scatter, width = 11, height = 7, dpi = 300)

cat("
Scatter plot 'growth_vs_tsr_scatter.png' saved to plots directory.
")

# -------------------------------
# 6. TSR Decomposition for Positive TSR Companies
# -------------------------------

# Filter and calculate growth factors
df_valid <- company_TSR_merged %>%
  filter(
    !is.na(iq_total_rev_2023) & iq_total_rev_2000 != 0,
    !is.na(iq_total_rev_2000),
    !is.na(iq_ebitda_margin_2023) & iq_ebitda_margin_2000 != 0,
    !is.na(iq_ebitda_margin_2000),
    !is.na(pe_ratio_2023) & pe_ratio_2000 != "0",
    !is.na(pe_ratio_2000),
    !is.na(common_shares_out_2023) & common_shares_out_2000 != 0,
    !is.na(common_shares_out_2000),
    !is.na(num_years),
    !is.na(annualized_TSR),
    !is.na(price_growth_factor),
    (1 + cumulative_TSR) > 0  # Keep only positive TSR companies
  ) %>%
  mutate(
    # Use pre-calculated growth rates
    rev_growth = rev_growth,
    net_margin_growth = net_margin_growth,
    shares_out_growth = shares_out_growth,
    pe_growth = pe_growth,
    
    # Calculate EPS growth components
    net_income_growth = rev_growth * (1 + net_margin_growth) - 1,  # Combined effect of revenue and margin
    eps_growth = (1 + net_income_growth) / (1 + shares_out_growth) - 1,  # EPS growth accounting for dilution
    
    # Price appreciation components
    price_appreciation = (1 + eps_growth) * (1 + pe_growth) - 1,  # Price change from EPS and PE changes
    
    # Calculate dividend contribution (as residual)
    dividend_contribution = cumulative_TSR - price_appreciation  # This includes dividend reinvestment effect
  )

# Filter for positive TSR companies
df_positive_TSR <- df_valid %>%
  filter(cumulative_TSR > 0)

# Print summary statistics
cat("\nTotal valid companies:", nrow(df_valid))
cat("\nCompanies with positive TSR:", nrow(df_positive_TSR))

# Calculate and print average TSR and revenue growth by family status
avg_stats <- df_positive_TSR %>%
  group_by(family) %>%
  summarise(
    avg_TSR = mean(cumulative_TSR, na.rm = TRUE) * 100,
    avg_revenue_growth = mean(rev_growth, na.rm = TRUE) * 100,
    avg_annualized_TSR = mean(annualized_TSR, na.rm = TRUE) * 100,
    avg_annualized_revenue_growth = mean((1 + rev_growth)^(1/num_years) - 1, na.rm = TRUE) * 100,
    n_companies = n()
  )

cat("\n\nAverage Statistics by Company Type:")
print(avg_stats)

# Calculate average contributions for the positive TSR group
avg_contributions_positive <- df_positive_TSR %>%
  group_by(family) %>%
  summarise(
    avg_TSR = mean(cumulative_TSR, na.rm = TRUE),
    n_positive = n(),
    
    # Average contribution of each component
    revenue_contribution = mean(rev_growth, na.rm = TRUE),
    margin_contribution = mean(net_margin_growth, na.rm = TRUE),
    shares_contribution = mean(-shares_out_growth, na.rm = TRUE),  # Simplified share count contribution
    pe_contribution = mean(eps_growth * pe_growth + pe_growth, na.rm = TRUE),
    dividend_contribution = mean(dividend_contribution, na.rm = TRUE),
    
    # Verify total
    total_contribution = revenue_contribution + margin_contribution + shares_contribution + pe_contribution + dividend_contribution,
    
    .groups = 'drop'
  )

# Prepare data for waterfall plots
waterfall_data <- avg_contributions_positive %>%
  mutate(
    # First calculate absolute contributions
    `Revenue Growth` = revenue_contribution * 100,
    `Margin Growth` = margin_contribution * 100,
    `Share Count Change` = shares_contribution * 100,
    `PE Multiple Change` = pe_contribution * 100,
    `Dividend Effect` = dividend_contribution * 100,
    avg_TSR_positive = avg_TSR * 100,
    
    # Then convert to percentage of total TSR and round to 1 decimal
    `Revenue Growth %` = round(`Revenue Growth` / avg_TSR_positive * 100, 1),
    `Margin Growth %` = round(`Margin Growth` / avg_TSR_positive * 100, 1),
    `Share Count Change %` = round(`Share Count Change` / avg_TSR_positive * 100, 1),
    `PE Multiple Change %` = round(`PE Multiple Change` / avg_TSR_positive * 100, 1),
    `Other Effects %` = round(100 - `Revenue Growth %` - `Margin Growth %` - `Share Count Change %` - `PE Multiple Change %`, 1)
  ) %>%
  select(family, 
         `Revenue Growth`, `Margin Growth`, `Share Count Change`, `PE Multiple Change`, `Dividend Effect`,  # Absolute values
         `Revenue Growth %`, `Margin Growth %`, `Share Count Change %`, `PE Multiple Change %`, `Other Effects %`,  # Relative values
         avg_TSR_positive, n_positive)

# Print summary of both absolute and relative contributions
print("\nAverage TSR Decomposition for Positive TSR Companies:")
print("Absolute Contributions (%):")
print(waterfall_data %>% 
      select(family, `Revenue Growth`, `Margin Growth`, `Share Count Change`, 
             `PE Multiple Change`, `Dividend Effect`, avg_TSR_positive))

print("\nRelative Contributions (% of Total TSR):")
print(waterfall_data %>% 
      select(family, `Revenue Growth %`, `Margin Growth %`, `Share Count Change %`, 
             `PE Multiple Change %`, `Other Effects %`))

# Prepare data for waterfall plots using relative contributions
waterfall_plot_data_family <- waterfall_data %>%
  filter(family == "Family") %>%
  select(ends_with("%")) %>%
  pivot_longer(cols = everything(), 
               names_to = "component", 
               values_to = "value") %>%
  mutate(component = str_remove(component, " %$"))

waterfall_plot_data_nonfamily <- waterfall_data %>%
  filter(family == "Non-Family") %>%
  select(ends_with("%")) %>%
  pivot_longer(cols = everything(), 
               names_to = "component", 
               values_to = "value") %>%
  mutate(component = str_remove(component, " %$"))

# Define component order
component_order <- c("Revenue Growth", "Margin Growth", "Share Count Change", "PE Multiple Change", "Other Effects")

# Order components
waterfall_plot_data_family <- waterfall_plot_data_family %>%
  mutate(component = factor(component, levels = component_order)) %>%
  arrange(component)

waterfall_plot_data_nonfamily <- waterfall_plot_data_nonfamily %>%
  mutate(component = factor(component, levels = component_order)) %>%
  arrange(component)

# Function to create custom labels with % sign
create_labels <- function(values) {
  ifelse(values >= 0,
         paste0("+", format(round(values, 1), nsmall = 1), "%"),
         paste0(format(round(values, 1), nsmall = 1), "%"))
}

# Create waterfall plot for family companies
waterfall_plot_family <- waterfall(waterfall_plot_data_family, 
                                 calc_total = TRUE,
                                 rect_text_labels = create_labels(waterfall_plot_data_family$value)) +
  theme_classic(base_size = 12) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),  # Remove x-axis label
        axis.text.x = element_text(face = "bold")) +  # Make x-axis labels bold
  labs(title = "TSR Decomposition - Family Companies",
       subtitle = "Contribution to Total TSR (Total = 100%)",
       y = "% of Total TSR") +
  scale_y_continuous(labels = scales::number_format(suffix = "%"))

# Create waterfall plot for non-family companies
waterfall_plot_nonfamily <- waterfall(waterfall_plot_data_nonfamily,
                                    calc_total = TRUE,
                                    rect_text_labels = create_labels(waterfall_plot_data_nonfamily$value)) +
  theme_classic(base_size = 12) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(),  # Remove x-axis label
        axis.text.x = element_text(face = "bold")) +  # Make x-axis labels bold
  labs(title = "TSR Decomposition - Non-Family Companies",
       subtitle = "Contribution to Total TSR (Total = 100%)",
       y = "% of Total TSR") +
  scale_y_continuous(labels = scales::number_format(suffix = "%"))

# Save the plots
ggsave("plots/waterfall_family_relative.png", waterfall_plot_family, width = 10, height = 6)
ggsave("plots/waterfall_nonfamily_relative.png", waterfall_plot_nonfamily, width = 10, height = 6)
