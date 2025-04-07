# Load Necessary Libraries
library(ggplot2)
library(dplyr)
library(here)
library(plotly)

# 1. Importing Prices Dataset
prices <- readRDS(here('data', 'stock_prices.rds'))

# 2. Define Date Range for Subsetting
min_date <- as.Date("2000-01-31")
max_date <- as.Date("2024-12-31")

# 3. Identify Companies First Listed on or Before min_date
old_companies_list <- prices %>%
  group_by(symbol) %>%
  summarize(
    first_date = min(date, na.rm = TRUE)
  ) %>%
  filter(first_date <= min_date)

# 4. Filter Prices for These Companies Within the Date Range
old_prices <- prices %>%
  filter(symbol %in% old_companies_list$symbol,
         date >= min_date,
         date <= max_date)

# 5. Identifying Symbols with Negative Adjusted Prices
symbols_with_negative_prices <- old_prices %>%
  filter(adjusted < 0) %>%
  distinct(symbol)

# 6. Option 1: Exclude Symbols with Negative Prices
clean_old_prices <- old_prices %>%
  filter(!(symbol %in% symbols_with_negative_prices$symbol))

# 7. Option 2: Correct Negative Prices by Taking Absolute Value (If Appropriate)
# Uncomment the following lines if you choose to correct instead of exclude
# corrected_old_prices <- old_prices %>%
#   mutate(adjusted = if_else(adjusted < 0, abs(adjusted), adjusted))

# 8. Proceed with Option 1 (Excluding) to Create TSR_aux
# Ensure that 'Family' and 'Country' columns exist. If not, adjust accordingly.
TSR_aux <- clean_old_prices %>%
  group_by(symbol) %>%
  arrange(date) %>%
  summarize(
    first_date = first(date),
    first_price = first(adjusted),
    last_date = last(date),
    last_price = last(adjusted),
    Family = first(Family),     # Include Family indicator
    Country = first(Country),   # Include Country indicator
    .groups = 'drop'
  ) %>%
  mutate(
    years = as.numeric(difftime(last_date, first_date, units = 'days')) / 365.25,
    TSR = ((last_price / first_price)^(1 / years) - 1) * 100,  # CAGR Formula
    TSR = round(TSR, 1)
  ) %>%
  # Exclude rows where years <= 0 to prevent undefined calculations
  filter(years > 0)

# 9. Calculate Mean TSR for Each Family Category
mean_TSR_df <- TSR_aux %>%
  group_by(Family) %>%
  summarize(mean_TSR = mean(TSR, na.rm = TRUE))

# 10. Create the Basic KDE Plot with Mean Lines
kde_plot <- ggplot(TSR_aux, aes(x = TSR, fill = Family, color = Family)) +
  geom_density(alpha = 0.5) +  # KDE curves with 50% transparency
  geom_vline(data = mean_TSR_df, 
             aes(xintercept = mean_TSR, color = Family), 
             linetype = "dashed", 
             size = 1) +  # Dashed lines for means
  labs(
    title = "Distribution of Annualized TSR by Family Category",
    x = "Annualized TSR (%)",
    y = "Density",
    fill = "Family Category",
    color = "Family Category"
  ) +
  coord_cartesian(xlim = c(-50, 55)) +  # Set x-axis limits from -50 to 55
  theme_minimal()

# 11. Extract Density Data to Determine Y-Positions for Labels
density_data <- ggplot_build(kde_plot)$data[[1]]

# 12. Map 'group' to 'Family' in density_data
# Assuming 'Family' is a factor and the levels correspond to 'group' numbers
family_levels <- levels(TSR_aux$Family)

# Check if the number of unique groups matches the number of Family levels
unique_groups <- unique(density_data$group)
if(length(unique_groups) != length(family_levels)) {
  stop("Number of groups does not match number of Family levels.")
}

# Assign Family categories based on group numbers
density_data$Family <- family_levels[density_data$group]

# 13. Merge Mean TSR Data with Density Data to Get Y-Positions
mean_TSR_df <- mean_TSR_df %>%
  left_join(density_data, by = "Family") %>%  # Join by 'Family'
  group_by(Family) %>%
  # For each Family, find the density point closest to mean_TSR
  mutate(diff = abs(x - mean_TSR)) %>%
  slice_min(order_by = diff, n = 1, with_ties = FALSE) %>%  # Find closest x
  summarize(y = y, mean_TSR = mean_TSR)

# 14. Add Text Labels for the Mean Lines
kde_plot <- kde_plot +
  geom_text(data = mean_TSR_df, 
            aes(x = mean_TSR, y = y, 
                label = paste0("Mean: ", round(mean_TSR, 1), "%"), 
                color = Family),
            vjust = -0.5,  # Position text slightly above the line
            hjust = 0.5,   # Center the text horizontally
            size = 3.5)     # Adjust text size as needed

# 15. Display the Enhanced Plot
print(kde_plot)
