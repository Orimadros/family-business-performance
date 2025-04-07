# Family Business Performance

## Description
This project analyzes Total Shareholder Return (TSR) for family and non-family companies using R. The analysis includes data fetching, cleaning, calculation of TSR, and visualization of results.

## Data
- `data/stock_data.rds`: Stock data for analysis.
- `data/imperfect_sample.csv`: Sample data for company pairs.
- `data/sample_with_ciq_data.xlsx`: Additional data for analysis.
- `data/CIQ_screening_raw_prices.xlsx`: Raw price data for screening.
- `data/CIQ_screening_raw.xlsx`: Raw data for screening.
- `data/CIQ_screening_raw_McKinsey.xlsx`: Another version of raw screening data.
- `data/pairings_data_fmt.xlsx`: Formatted pairings data.
- `data/pairings_data.xlsx`: Pairings data.
- `data/pairings_data.csv`: CSV version of pairings data.
- `data/family_tickers.csv`: List of family company tickers.
- `data/largest_companies_world.csv`: Data for the largest companies.
- `data/operadoras_ativas.csv`: Active operators data.
- `data/top3980_flagged.csv`: Flagged data for top 3980 companies.
- `data/top3980_revenue_world_2022.xlsx`: Revenue data for top 3980 companies.

## Scripts

### `scripts/TSR_decomposition.R`
This script performs the following tasks:
1. **Load Necessary Libraries**: Loads required libraries for data manipulation, visualization, and reading files.
2. **Load Data**: Reads stock data and pairs data from CSV files.
3. **Prepare and Clean Data**: 
   - Reshapes the pairs data to long format and creates unique identifiers.
   - Filters out specific tickers and handles negative adjusted close prices.
4. **Calculate TSR and Growth Factors**: 
   - Computes cumulative and annualized TSR for each company based on stock price data.
   - Calculates growth factors for revenue, margins, shares, and price-to-earnings ratios.
5. **TSR Decomposition**: 
   - Filters for companies with positive TSR and calculates contributions from revenue growth, margin growth, share count changes, and price-to-earnings changes.
6. **Visualization**: 
   - Generates waterfall plots to visualize the contributions to TSR for family and non-family companies.
   - Saves the plots and summary statistics to the results directory.

### `scripts/FB_analysis.R`
This script focuses on analyzing family business data:
1. **Load Necessary Libraries**: Similar to the previous script, it loads required libraries.
2. **Load Data**: Reads stock data and pairs data.
3. **Prepare Data**: Cleans and reshapes the data, including filtering out specific tickers and handling negative prices.
4. **Calculate Average Total Returns**: Computes average total returns for each family and non-family company over specified periods.
5. **Visualization**: Generates plots to visualize average total returns and the evolution of portfolio values over time.

### `scripts/FB_performance_plots.R`
This script creates performance plots based on annualized TSR:
1. **Load Necessary Libraries**: Loads libraries for plotting and data manipulation.
2. **Import Prices Dataset**: Reads stock prices data.
3. **Filter Data**: Identifies companies first listed on or before a specified date and filters out those with negative adjusted prices.
4. **Calculate Mean TSR**: Computes mean TSR for family categories and creates density plots to visualize the distribution of annualized TSR.

### `scripts/FB_stock_data_fetcher.R`
This script fetches stock data for family and non-family companies:
1. **Load Necessary Libraries**: Loads libraries for data fetching and manipulation.
2. **Define Constants and Helper Functions**: Sets constants for date ranges and defines a function to fetch stock data.
3. **Import Data**: Loads data for the largest companies and public family companies.
4. **Fetch Stock Data**: Uses the helper function to fetch stock data for the specified tickers.
5. **Process and Merge Data**: Merges family and non-family stock data and saves it as an RDS file.

### `scripts/FB_matched_sample_data_fetcher.R`
This script fetches stock data specifically for the tickers identified in the `imperfect_sample.csv` file:
1. **Load Necessary Libraries**: Similar to the previous script.
2. **Import Data**: Loads pairs data from the CSV file.
3. **Fetch Stock Data**: Uses the helper function to fetch stock data for the tickers in the pairs.
4. **Save Processed Data**: Saves the fetched stock data as an RDS file for later use.

### `scripts/sample_construction.R`
This script constructs the sample used in the analysis:
1. **Load Necessary Libraries**: Loads libraries for data manipulation.
2. **Import Data**: Loads necessary datasets for constructing the sample.
3. **Data Processing**: Cleans and prepares the data for analysis, ensuring consistency and removing duplicates.

## Results
- `results/TSR_decomposition_summary.csv`: Summary of the TSR decomposition analysis.
- `plots/waterfall_family_relative.png`: Waterfall plot for family companies.
- `plots/waterfall_nonfamily_relative.png`: Waterfall plot for non-family companies.
- `plots/imperfect_match_boxplots.png`: Plot related to imperfect matches.
- `plots/revenue_marketcap_comparison.png`: Plot comparing revenue and market cap.

## How to Run
1. Install required R packages.
2. Run the relevant scripts in R or RStudio.