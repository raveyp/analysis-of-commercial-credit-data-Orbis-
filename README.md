# Appendix-A-transform-commercial-credit-data-Orbis-
Formatting of balance sheet data from Orbis of accounts payable and receivable (log) and visualisations of quartile distributions and time series
# Load required libraries
library(tidyverse)
library(readxl)
library(writexl)

# Read the Excel file
orbis_data <- read_excel("Desktop/PhD/Chapters/C&C Article/Second Revision/WTO:BIS Data/Orbis All East Asia Account Rec Data.xlsx")

# Get column names
col_names <- colnames(orbis_data)

# Show column structure to understand the patterns
print("First 20 column names:")
print(col_names[1:min(20, length(col_names))])

# Identify static columns (first 3 columns)
static_cols <- c(
  "Company name Latin alphabet",
  "Country",
  "Primary code in national industry classification - description"
)

# Filter to only include static columns that actually exist
static_cols <- static_cols[static_cols %in% col_names]

print(paste("Found", length(static_cols), "static columns"))

# Identify financial metric columns - using the actual column names
# Accounts receivable columns
receivable_cols <- col_names[grep("Accounts receivable", col_names)]

# Operating revenue columns  
revenue_cols <- col_names[grep("Operating revenue \\(Turnover\\)", col_names)]

# Trade creditors columns
creditors_cols <- col_names[grep("Trade creditors", col_names)]

print(paste("Found", length(receivable_cols), "accounts receivable columns"))
print(paste("Found", length(revenue_cols), "operating revenue columns"))
print(paste("Found", length(creditors_cols), "trade creditors columns"))

# Create a function to pivot each metric type
pivot_metric <- function(data, metric_cols, metric_name, static_cols) {
  if (length(metric_cols) == 0) return(NULL)
  
  result <- data %>%
    select(all_of(c(static_cols, metric_cols))) %>%
    # Convert all metric columns to character first
    mutate(across(all_of(metric_cols), as.character)) %>%
    pivot_longer(
      cols = all_of(metric_cols),
      names_to = "year_metric",
      values_to = metric_name
    ) %>%
    mutate(
      year = str_extract(year_metric, "20[0-9]{2}|19[0-9]{2}"),  # Extract year
      year = as.numeric(year),
      !!metric_name := as.numeric(.data[[metric_name]])
    ) %>%
    select(-year_metric)
  
  return(result)
}

# Pivot each metric type
receivable_long <- pivot_metric(orbis_data, receivable_cols, "Accounts_receivable_USD", static_cols)
revenue_long <- pivot_metric(orbis_data, revenue_cols, "Operating_revenue_USD", static_cols)
creditors_long <- pivot_metric(orbis_data, creditors_cols, "Trade_creditors_USD", static_cols)

# Create a list of all pivoted datasets (remove NULLs)
pivoted_list <- list(
  receivable_long, revenue_long, creditors_long
)
pivoted_list <- pivoted_list[!sapply(pivoted_list, is.null)]

print(paste("Number of pivoted datasets:", length(pivoted_list)))

# If we have at least one pivoted dataset, proceed with joining
if(length(pivoted_list) > 0) {
  # Join all datasets by static columns + year
  final_data <- pivoted_list[[1]]
  
  for (i in 2:length(pivoted_list)) {
    final_data <- final_data %>%
      full_join(
        pivoted_list[[i]],
        by = c(static_cols, "year")
      )
  }
  
  # Rename columns for clarity
  final_data <- final_data %>%
    rename(
      Company = `Company name Latin alphabet`,
      Country = Country,
      Industry_Description = `Primary code in national industry classification - description`
    ) %>%
    arrange(Company, year)
  
  # Identify which metric columns actually exist in final_data
  metric_columns <- c()
  if("Accounts_receivable_USD" %in% names(final_data)) metric_columns <- c(metric_columns, "Accounts_receivable_USD")
  if("Operating_revenue_USD" %in% names(final_data)) metric_columns <- c(metric_columns, "Operating_revenue_USD")
  if("Trade_creditors_USD" %in% names(final_data)) metric_columns <- c(metric_columns, "Trade_creditors_USD")
  
  print(paste("Found", length(metric_columns), "metric columns in final_data:"))
  print(metric_columns)
  
  # Create a long format version if metric columns exist
  if(length(metric_columns) > 0) {
    long_format_data <- final_data %>%
      pivot_longer(
        cols = all_of(metric_columns),
        names_to = "Metric",
        values_to = "Value"
      ) %>%
      filter(!is.na(Value))  # Remove NA values
  } else {
    long_format_data <- data.frame()
    print("WARNING: No metric columns found in final_data")
  }
  
  # View the structure
  print("=== FINAL DATA STRUCTURE (WIDE FORMAT) ===")
  str(final_data)
  print(head(final_data, 10))
  
  if(nrow(long_format_data) > 0) {
    print("=== FINAL DATA STRUCTURE (LONG FORMAT) ===")
    str(long_format_data)
    print(head(long_format_data, 10))
  }
  
  # Summary statistics
  cat("\n=== DATA SUMMARY ===\n")
  cat("Number of companies:", n_distinct(final_data$Company), "\n")
  cat("Number of countries:", n_distinct(final_data$Country), "\n")
  cat("Number of years:", n_distinct(final_data$year), "\n")
  cat("Years available:", paste(sort(unique(final_data$year[!is.na(final_data$year)])), collapse = ", "), "\n")
  cat("Total observations (wide format):", nrow(final_data), "\n")
  if(nrow(long_format_data) > 0) {
    cat("Total observations (long format):", nrow(long_format_data), "\n\n")
  }
  
  # Count number of rows per company
  company_years <- final_data %>%
    group_by(Company, Country) %>%
    summarise(
      n_years = n_distinct(year, na.rm = TRUE),
      has_receivables = if("Accounts_receivable_USD" %in% names(final_data)) sum(!is.na(Accounts_receivable_USD)) else 0,
      has_revenue = if("Operating_revenue_USD" %in% names(final_data)) sum(!is.na(Operating_revenue_USD)) else 0,
      has_creditors = if("Trade_creditors_USD" %in% names(final_data)) sum(!is.na(Trade_creditors_USD)) else 0,
      .groups = "drop"
    ) %>%
    arrange(desc(n_years))
  
  print("Top 20 companies by years of data:")
  print(head(company_years, 20))
  
  # Check for missing values
  missing_summary <- final_data %>%
    summarise(
      total_rows = n(),
      receivables_na = if("Accounts_receivable_USD" %in% names(final_data)) sum(is.na(Accounts_receivable_USD)) else 0,
      revenue_na = if("Operating_revenue_USD" %in% names(final_data)) sum(is.na(Operating_revenue_USD)) else 0,
      creditors_na = if("Trade_creditors_USD" %in% names(final_data)) sum(is.na(Trade_creditors_USD)) else 0,
      receivables_pct = if("Accounts_receivable_USD" %in% names(final_data)) round(100 * receivables_na / n(), 1) else 0,
      revenue_pct = if("Operating_revenue_USD" %in% names(final_data)) round(100 * revenue_na / n(), 1) else 0,
      creditors_pct = if("Trade_creditors_USD" %in% names(final_data)) round(100 * creditors_na / n(), 1) else 0
    )
  
  print("\n=== MISSING VALUES SUMMARY ===")
  print(missing_summary)
  
  # Summary by year
  year_summary <- final_data %>%
    group_by(year) %>%
    summarise(
      companies = n_distinct(Company),
      receivables_non_na = if("Accounts_receivable_USD" %in% names(final_data)) sum(!is.na(Accounts_receivable_USD)) else 0,
      revenue_non_na = if("Operating_revenue_USD" %in% names(final_data)) sum(!is.na(Operating_revenue_USD)) else 0,
      creditors_non_na = if("Trade_creditors_USD" %in% names(final_data)) sum(!is.na(Trade_creditors_USD)) else 0,
      .groups = "drop"
    )
  
  print("\n=== DATA AVAILABILITY BY YEAR ===")
  print(year_summary)
  
  # Save to Excel on desktop
  desktop_path <- file.path("~", "Desktop", "Orbis_transposed.xlsx")
  
  # Create a list of sheets to export
  excel_sheets <- list(
    "Wide_Format" = final_data
  )
  
  if(nrow(long_format_data) > 0) {
    excel_sheets[["Long_Format"]] <- long_format_data
  }
  excel_sheets[["Company_Summary"]] <- company_years
  excel_sheets[["Year_Summary"]] <- year_summary
  excel_sheets[["Missing_Summary"]] <- missing_summary
  
  write_xlsx(excel_sheets, desktop_path)
  
  # Print file locations
  cat("\n=== FILES CREATED ===\n")
  cat("Excel file on desktop:", desktop_path, "\n")
  
  # Display first few rows
  cat("\n=== FIRST 10 ROWS OF TRANSPOSED DATA (WIDE FORMAT) ===\n")
  print(head(final_data, 10))
  
  if(nrow(long_format_data) > 0) {
    cat("\n=== FIRST 10 ROWS OF TRANSPOSED DATA (LONG FORMAT) ===\n")
    print(head(long_format_data, 10))
  }
  
} else {
  print("ERROR: No metric columns were found. Please check the column name patterns.")
  print("Here are the actual column names containing 'Accounts receivable':")
  print(col_names[grep("Accounts receivable", col_names)])
  print("Here are the actual column names containing 'Operating revenue':")
  print(col_names[grep("Operating revenue", col_names)])
  print("Here are the actual column names containing 'Trade creditors':")
  print(col_names[grep("Trade creditors", col_names)])
}

## read_excel("Desktop/PhD/Chapters/C&C Article/Second Revision/WTO:BIS Data/G7_Orbis_Transposed_with_Ratio.xlsx") ###
### read_excel("Desktop/PhD/Chapters/C&C Article/Second Revision/WTO:BIS Data/East_Asia_Orbis_transposed_with_quartiles.xlsx") ###

final_data <- read_excel("Desktop/PhD/Chapters/C&C Article/Second Revision/WTO:BIS Data/East_Asia_Orbis_transposed_with_quartiles.xlsx")


final_data_with_quartiles <- final_data %>%
  group_by(year) %>%
  mutate(
    quartile = ntile(Operating_revenue_USD, 4) 
  ) %>%
  ungroup()

# View the result - you'll see the new quartile column
final_data_with_quartiles %>%
  select(Company, year, Operating_revenue_USD, quartile) %>%
  head(20)

# Save back to Excel
write_xlsx(final_data_with_quartiles, "~/Desktop/Orbis_transposed_with_quartiles.xlsx")

# Add the ratio and log-transformed ratio to your data
final_data <- final_data %>%
  mutate(
    # Calculate the ratio (handle division by zero)
    credit_ratio = ifelse(
      Accounts_receivable_USD > 0, 
      Trade_creditors_USD / Accounts_receivable_USD, 
      NA
    ),
    # Log transformation (natural log)
    log_credit_ratio = log(credit_ratio),
    # Alternative: log base 10 (more intuitive for some audiences)
    log10_credit_ratio = log10(credit_ratio)
  )

# Check the distribution
summary(final_data$credit_ratio)
summary(final_data$log_credit_ratio)

# Visualize
ggplot(final_data %>% filter(!is.na(log_credit_ratio)), 
       aes(x = log_credit_ratio)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Distribution of Log-Transformed Credit Ratio",
    x = "log(Trade Creditors / Accounts Receivable)",
    y = "Count",
    caption = "Values < 0 indicate net credit seller; Values > 0 indicate net credit buyer"
  ) +
  theme_minimal()

library(tidyverse)
library(ggplot2)

# Load required libraries
library(tidyverse)
library(ggplot2)

# Method 1: Faceted box plots (all countries in one plot) - RECOMMENDED
faceted_plot <- ggplot(final_data_with_quartiles %>% 
                         filter(!is.na(quartile), !is.na(log_credit_ratio), !is.na(Country)),
                       aes(x = factor(quartile), y = log_credit_ratio, fill = factor(quartile))) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  facet_wrap(~ Country, scales = "free_y", ncol = 4) +
  labs(
    title = "Credit Ratio by Firm Size Quartile Across Countries",
    subtitle = "log(Trade Creditors / Accounts Receivable) - Values > 0 indicate net credit buyers",
    x = "Firm Size Quartile (1 = smallest, 4 = largest)",
    y = "log(Trade Creditors / Accounts Receivable)",
    fill = "Quartile"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9)
  ) +
  scale_fill_brewer(palette = "Blues")

# Display the plot
print(faceted_plot)

# Save the faceted plot
ggsave("~/Desktop/Credit_Ratio_All_Countries_Faceted.png", 
       plot = faceted_plot, width = 14, height = 10, dpi = 300)

# Method 2: Separate box plots for each country (saved as individual files)
# Create a list of countries
countries <- unique(final_data_with_quartiles$Country)
countries <- countries[!is.na(countries)]

# Create and save individual plots for each country
for(country in countries) {
  
  # Filter data for this country
  country_data <- final_data_with_quartiles %>%
    filter(Country == country, !is.na(quartile), !is.na(log_credit_ratio))
  
  # Only create plot if there's data
  if(nrow(country_data) > 0) {
    p <- ggplot(country_data, aes(x = factor(quartile), y = log_credit_ratio, fill = factor(quartile))) +
      geom_boxplot(alpha = 0.7, outlier.size = 1) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
      labs(
        title = paste("Credit Ratio by Firm Size -", country),
        subtitle = "log(Trade Creditors / Accounts Receivable) - Values > 0 indicate net credit buyers",
        x = "Firm Size Quartile (1 = smallest, 4 = largest)",
        y = "log(Trade Creditors / Accounts Receivable)",
        fill = "Quartile",
        caption = paste("Based on", nrow(country_data), "observations")
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_fill_brewer(palette = "Blues")
    
    # Display the plot
    print(p)
    
    # Save each plot
    ggsave(paste0("~/Desktop/Credit_Ratio_", gsub(" ", "_", country), ".png"), 
           plot = p, width = 8, height = 6, dpi = 300)
    
    cat("Saved plot for", country, "\n")
  }
}

# Method 3: Summary statistics table for each country and quartile
summary_table <- final_data_with_quartiles %>%
  filter(!is.na(quartile), !is.na(log_credit_ratio), !is.na(Country)) %>%
  group_by(Country, quartile) %>%
  summarise(
    n = n(),
    mean_log = mean(log_credit_ratio, na.rm = TRUE),
    median_log = median(log_credit_ratio, na.rm = TRUE),
    sd_log = sd(log_credit_ratio, na.rm = TRUE),
    q25 = quantile(log_credit_ratio, 0.25, na.rm = TRUE),
    q75 = quantile(log_credit_ratio, 0.75, na.rm = TRUE),
    pct_net_buyer = mean(exp(log_credit_ratio) > 1, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(Country, quartile)

# Print the summary table
print(summary_table)

# Save summary table to Excel
library(writexl)
write_xlsx(summary_table, "~/Desktop/Credit_Ratio_Summary_by_Country.xlsx")

# Method 4: Box plot with jitter points (shows individual observations)
jitter_plot <- ggplot(final_data_with_quartiles %>% 
                        filter(!is.na(quartile), !is.na(log_credit_ratio), !is.na(Country)),
                      aes(x = factor(quartile), y = log_credit_ratio, fill = factor(quartile))) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(alpha = 0.3, size = 0.5, width = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  facet_wrap(~ Country, scales = "free_y", ncol = 4) +
  labs(
    title = "Credit Ratio by Firm Size Quartile Across Countries (with individual points)",
    subtitle = "log(Trade Creditors / Accounts Receivable) - Values > 0 indicate net credit buyers",
    x = "Firm Size Quartile (1 = smallest, 4 = largest)",
    y = "log(Trade Creditors / Accounts Receivable)",
    fill = "Quartile"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_fill_brewer(palette = "Blues")

# Display and save the jitter plot
print(jitter_plot)
ggsave("~/Desktop/Credit_Ratio_All_Countries_Jitter.png", 
       plot = jitter_plot, width = 14, height = 10, dpi = 300)

# Method 5: Violin plots (alternative to box plots for showing distribution)
violin_plot <- ggplot(final_data_with_quartiles %>% 
                        filter(!is.na(quartile), !is.na(log_credit_ratio), !is.na(Country)),
                      aes(x = factor(quartile), y = log_credit_ratio, fill = factor(quartile))) +
  geom_violin(alpha = 0.7, trim = FALSE) +
  geom_boxplot(width = 0.1, alpha = 0.5, outlier.shape = NA) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  facet_wrap(~ Country, scales = "free_y", ncol = 4) +
  labs(
    title = "Credit Ratio Distribution by Firm Size Quartile Across Countries",
    subtitle = "log(Trade Creditors / Accounts Receivable) - Values > 0 indicate net credit buyers",
    x = "Firm Size Quartile (1 = smallest, 4 = largest)",
    y = "log(Trade Creditors / Accounts Receivable)",
    fill = "Quartile"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_fill_brewer(palette = "Blues")

# Display and save the violin plot
print(violin_plot)
ggsave("~/Desktop/Credit_Ratio_All_Countries_Violin.png", 
       plot = violin_plot, width = 14, height = 10, dpi = 300)

# Method 6: Create a single combined plot showing only the most important countries
# (if you have many countries, this might be more readable)
top_countries <- final_data_with_quartiles %>%
  filter(!is.na(Country)) %>%
  group_by(Country) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(9) %>%  # Top 9 countries
  pull(Country)

combined_plot <- final_data_with_quartiles %>%
  filter(Country %in% top_countries, !is.na(quartile), !is.na(log_credit_ratio)) %>%
  ggplot(aes(x = factor(quartile), y = log_credit_ratio, fill = factor(quartile))) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  facet_wrap(~ Country, scales = "free_y", ncol = 3) +
  labs(
    title = "Credit Ratio by Firm Size Quartile and Country",
    subtitle = "log(Trade Creditors / Accounts Receivable)",
    x = "Firm Size Quartile (1 = smallest, 4 = largest)",
    y = "log(Trade Creditors / Accounts Receivable)",
    fill = "Quartile"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_fill_brewer(palette = "Blues")

# Display and save the top countries plot
print(combined_plot)
ggsave("~/Desktop/Credit_Ratio_Top_9_Countries.png", 
       plot = combined_plot, width = 12, height = 10, dpi = 300)

cat("\n=== SUMMARY ===\n")
cat("Files saved to desktop:\n")
cat("1. Credit_Ratio_All_Countries_Faceted.png - All countries in one plot\n")
cat("2. Credit_Ratio_All_Countries_Jitter.png - With individual points\n")
cat("3. Credit_Ratio_All_Countries_Violin.png - Violin plots showing distribution\n")
cat("4. Credit_Ratio_Top_9_Countries.png - Only top 9 countries\n")
cat("5. Credit_Ratio_Summary_by_Country.xlsx - Summary statistics table\n")
cat("6. Individual country plots: Credit_Ratio_[CountryName].png\n")
<img width="468" height="635" alt="image" src="https://github.com/user-attachments/assets/bb0c308e-7b5a-4f3a-bdc3-ca88bc5e2387" />
