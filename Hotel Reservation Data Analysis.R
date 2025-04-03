

library(tidyverse)
library(ggplot2)
library(dplyr)
#install.packages("readr")
library(readr)
library(tidyr)
#install.packages("ggcorrplot")
library(ggcorrplot)


# on my school computer
hotel_data <- read_csv("//bengal2/dfs/home/getawm/Downloads/hotel_booking.csv")

# on my laptop
hotel_data <- read_csv("C://Users/dante/Documents/Data229_Local_Work/hotel_booking.csv")


# Exploratory Data Analysis
str(hotel_data) # Show each column's data type

# Categorical Variable EDA

# Correct code to summarize each categorical variable
hotel_data$is_canceled <- as.factor(hotel_data$is_canceled) #is_cancelled was not being accounted for
#as a categorical. This line makes sure it is
categorical_vars <- hotel_data[, sapply(hotel_data, function(x) is.factor(x) || is.character(x))]

# Apply summary to each column individually
cat_var_summaries <- lapply(categorical_vars, function(var) summary(as.factor(var)))
cat_var_summaries

#Put the categorical Variable data in a table
cat_var_summaries_table <- lapply(categorical_vars, table)
cat_var_summaries_table

# Put the categorical variable summary table into a more readable data
# frame to read for certain variable summaries
cat_var_summary_df <- do.call(
  rbind,
  lapply(names(cat_var_summaries), function(var) {
    # Convert to a data frame
    data.frame(
      Variable = var,
      Level = names(cat_var_summaries[[var]]),
      Freq = as.numeric(cat_var_summaries[[var]]),
      stringsAsFactors = FALSE
    )
  })
)

# Put variables results in data frames to easily see 
# their values. This will be used alongside the plots in
# the report. I didn't do hotel because it was the first one
# in the whole categorical variable frame and i just screen snipped

# Organized tables for necessary variables
# is_cancelled
is_canceled_table <- as.data.frame(table(hotel_data$is_canceled))
colnames(is_canceled_table) <- c("Status", "Count") 

# arrival_date_month
arrival_date_month_table <- as.data.frame(table(hotel_data$arrival_date_month))
colnames(arrival_date_month_table) <- c("Month", "Count")

# Here below I Group the categorical variables to lighten the load on R
# (My application crashed everytime I tried to loop through
# all the categoricals at once)

# Each loop saves each plot to the current working directory with ggsave()

# Group 1: Hotel and Booking Details
group_1 <- c("hotel", "is_canceled", "reservation_status", "meal", "reservation_status")
# I'm considering the variable "arrival_date_month" as a group 1 variable, but
# for plotting purposes it will be separate from the group 1
# for loop to avoid issues with the colors

for (var in group_1) {
  p <- ggplot(hotel_data, aes_string(x = paste0("factor(", var, ")"), fill = paste0("factor(", var, ")"))) +  
    geom_bar() +
    labs(
      title = paste("Distribution of", var), 
      x = var, 
      y = "Count"
    ) +
    theme_light() +  # Use a light theme for better visibility
    theme(
      axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  # Larger, rotated x-axis labels
      axis.text.y = element_text(size = 14),  # Larger y-axis labels
      axis.title = element_text(size = 16),  # Larger axis titles
      plot.title = element_text(size = 18, face = "bold")  # Larger, bold plot title
    ) +
    scale_fill_brewer(palette = "Set2")  # Use a color palette for the bars
  
  # Save the plot to the current working directory
  ggsave(filename = paste0("plot_", var, ".png"), plot = p, width = 8, height = 6)
}

# Define a custom palette with named colors for the months
month_colors <- c(
  "January" = "red", 
  "February" = "blue", 
  "March" = "green",
  "April" = "orange", 
  "May" = "purple", 
  "June" = "pink",
  "July" = "cyan", 
  "August" = "yellow", 
  "September" = "brown",
  "October" = "magenta", 
  "November" = "gray", 
  "December" = "limegreen"
)

# Plot `arrival_date_month`
p <- ggplot(hotel_data, aes(x = factor(arrival_date_month), fill = factor(arrival_date_month))) +
  geom_bar() +
  labs(
    title = "Distribution of Arrival Months",
    x = "Month",
    y = "Count"
  ) +
  theme_light() +  # Use a light theme for better visibility
  theme(
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  # Larger, rotated x-axis labels
    axis.text.y = element_text(size = 14),  # Larger y-axis labels
    axis.title = element_text(size = 16),  # Larger axis titles
    plot.title = element_text(size = 18, face = "bold")  # Larger, bold plot title
  ) +
  scale_fill_manual(values = month_colors)  # Use custom colors

# Save 
ggsave(filename = "plot_arrival_date_month.png", plot = p, width = 8, height = 6)

# The country variable has too many options to graph or put in
# a table, so I will grab the most frequent countries and plot

# Count and sort countries by frequency
top_countries <- hotel_data %>%
  count(country, sort = TRUE) %>%
  top_n(10, n)  # Select top 10 countries

# Define a custom color palette with 10 distinct colors
custom_colors <- c(
  "red", "blue", "green", "orange", "purple", 
  "cyan", "magenta", "yellow", "brown", "pink"
)

# Create the plot for the top countries
p <- ggplot(top_countries, aes(x = reorder(country, n), y = n, fill = country)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 10 Countries by Frequency",
    x = "Country",
    y = "Count"
  ) +
  theme_light() +  
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Rotate x-axis labels
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  scale_fill_manual(values = custom_colors)  # Use custom colors

# Save the plot to a file
ggsave(filename = "top_countries_plot.png", plot = p, width = 8, height = 6)



# Group 2: Customer Demographics
group_2 <- c("market_segment", "distribution_channel", "is_repeated_guest")

# Define custom colors (can be reused across all variables)
custom_colors <- c(
  "red", "blue", "green", "orange", "purple",
  "cyan", "magenta", "yellow", "brown", "pink"
)

for (var in group_2) {
  p <- ggplot(hotel_data, aes_string(x = paste0("factor(", var, ")"), fill = paste0("factor(", var, ")"))) +
    geom_bar() +
    labs(
      title = paste("Distribution of", var), 
      x = var, 
      y = "Count"
    ) +
    theme_light() +  
    theme(
      axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  # Rotate x-axis labels
      axis.text.y = element_text(size = 12),  # Larger y-axis labels
      axis.title = element_text(size = 14),  # Larger axis titles
      plot.title = element_text(size = 16, face = "bold")  # Larger, bold plot title
    ) +
    scale_fill_manual(values = custom_colors)  # Custom colors
  
  # Save the plot to the current working directory
  ggsave(filename = paste0("plot_", var, ".png"), plot = p, width = 8, height = 6)
}


# Group 3: Booking Specifics
group_3 <- c("reserved_room_type", "assigned_room_type", "deposit_type", "customer_type")

# Custom Colors
custom_colors <- c(
  "red", "blue", "green", "orange", "purple",
  "cyan", "magenta", "yellow", "brown", "pink", "gray",
  "gold"
)

for (var in group_3) {
  p <- ggplot(hotel_data, aes_string(x = paste0("factor(", var, ")"), fill = paste0("factor(", var, ")"))) +
    geom_bar() +
    labs(
      title = paste("Distribution of", var), 
      x = var, 
      y = "Count"
    ) +
    theme_light() +  
    theme(
      axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  # Rotate x-axis labels
      axis.text.y = element_text(size = 12),  
      axis.title = element_text(size = 14),  
      plot.title = element_text(size = 16, face = "bold")
    ) +
    scale_fill_manual(values = custom_colors)  
  
  # Save the plot to the current working directory
  ggsave(filename = paste0("plot_", var, ".png"), plot = p, width = 8, height = 6)
}





# Quantitative Variables 
quantitative_vars <- hotel_data[, sapply(hotel_data, is.numeric)]
summary(quantitative_vars)

# Count frequencies of specific values for a quantitative variable
table(hotel_data$arrival_date_year)

# Group quantitative variables to mitigate crashes in R
group_1 <- c("arrival_date_year", "arrival_date_week_number", "arrival_date_day_of_month")
group_2 <- c("stays_in_weekend_nights", "stays_in_week_nights", "adults", "children")
group_3 <- c("babies", "previous_cancellations", "previous_bookings_not_canceled", "booking_changes")
group_4 <- c("days_in_waiting_list", "adr", "required_car_parking_spaces", "total_of_special_requests")

# Use a function this time instead of 3 separate for loops for less code
create_boxplots <- function(var_group, data) {
  for (var in var_group) {
    p <- ggplot(data, aes_string(y = var)) +
      geom_boxplot(fill = "blue", color = "black") +
      labs(
        title = paste("Boxplot of", var),
        y = var
      ) +
      theme_light() +
      theme(
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold")
      )
    
    # Save each plot to a file
    ggsave(filename = paste0("boxplot_", var, ".png"), plot = p, width = 8, height = 6)
  }
}

# Apply the function to each group of quantitative variables
create_boxplots(group_1, hotel_data)
create_boxplots(group_2, hotel_data)
create_boxplots(group_3, hotel_data)
create_boxplots(group_4, hotel_data)

# Trying histograms 
# Define a function to create histograms for a group of variables
create_histograms <- function(var_group, data) {
  for (var in var_group) {
    # Use a sensible default binwidth; adjust as needed
    bin_width <- ifelse(var %in% c("arrival_date_week_number", "arrival_date_day_of_month"), 1, 5)
    
    p <- ggplot(data, aes_string(x = var)) +
      geom_histogram(binwidth = bin_width, fill = "blue", color = "black") +
      labs(
        title = paste("Histogram of", var),
        x = var,
        y = "Frequency"
      ) +
      theme_light() +
      theme(
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold")
      )
    
    # Save each plot to a file
    ggsave(filename = paste0("histogram_", var, ".png"), plot = p, width = 8, height = 6)
  }
}

# Apply the function to each group of quantitative variables
create_histograms(group_1, hotel_data)
create_histograms(group_2, hotel_data)
create_histograms(group_3, hotel_data)
create_histograms(group_4, hotel_data)

# Create a frequency table for stays_in_weekend_nights
freq_table_weekendnights <- as.data.frame(table(hotel_data$stays_in_weekend_nights))
# Rename the columns for clarity
colnames(freq_table_weekendnights) <- c("Weekend Nights", "Count")
print(freq_table_weekendnights)

# Frequency table for stays_in_week_nights
freq_table_weeknights <- as.data.frame(table(hotel_data$stays_in_week_nights))
# Rename the columns for clarity
colnames(freq_table_weeknights) <- c("Week Nights", "Count")
print(freq_table_weeknights)

# Frequency table for number of adults
freq_table_adults <- as.data.frame(table(hotel_data$adults))
colnames(freq_table_adults) <- c("Number of Adults", "Count")
print(freq_table_adults)

# Frequency table for children
freq_table_children <- as.data.frame(table(hotel_data$children))
colnames(freq_table_children) <- c("Number of Children", "Count")
print(freq_table_children)

# Frequency table for babies
freq_table_babies <- as.data.frame(table(hotel_data$babies))
colnames(freq_table_babies) <- c("Number of Babies", "Count")
print(freq_table_babies)

# Frequency table for previous_cancellations 
freq_table_previous_cancellations  <- as.data.frame(table(hotel_data$previous_cancellations ))
colnames(freq_table_previous_cancellations ) <- c("Cancellations", "Count")
print(freq_table_previous_cancellations)

# Frequency table for previous_bookings_not_canceled 
freq_table_previous_bookings_not_canceled  <- as.data.frame(table(hotel_data$previous_bookings_not_canceled))
colnames(freq_table_previous_bookings_not_canceled) <- c("Previous not canceled", "Count")
print(freq_table_previous_bookings_not_canceled)

# Frequency Table for booking changes
freq_table_booking_changes  <- as.data.frame(table(hotel_data$booking_changes))
colnames(freq_table_booking_changes) <- c("Booking Changes", "Count")
print(freq_table_booking_changes)

# Frequency Table for days in waiting list 
freq_table_days_in_waiting_list <- as.data.frame(table(hotel_data$days_in_waiting_list))
colnames(freq_table_days_in_waiting_list) <- c("Days in Waiting List", "Count")
print(freq_table_days_in_waiting_list)

# Frequency table for ADR
freq_table_adr <- as.data.frame(table(hotel_data$adr))
colnames(freq_table_adr) <- c("Average Rate", "Count")
print(freq_table_adr)

# Frequency table for required_car_parking_spaces
freq_table_required_car_parking_spaces <- as.data.frame(table(hotel_data$required_car_parking_spaces))
colnames(freq_table_required_car_parking_spaces) <- c("Required Parking Spaces", "Count")
print(freq_table_required_car_parking_spaces)

# frequency table for special requests
freq_table_total_of_special_requests <- as.data.frame(table(hotel_data$total_of_special_requests))
colnames(freq_table_total_of_special_requests) <- c("Number of Special Requests", "Count")
print(freq_table_total_of_special_requests)


# 1/2 - 	Previous_cancellations and previous_bookings_not_cancelled with the variable hotel 

# Summarize total counts by hotel
total_counts <- hotel_data %>%
  group_by(hotel) %>%
  summarise(
    Total_Previous_Cancellations = sum(previous_cancellations, na.rm = TRUE),
    Total_Previous_Bookings_Not_Canceled = sum(previous_bookings_not_canceled, na.rm = TRUE)
  )

# Convert to long format for visualization
long_counts <- total_counts %>%
  pivot_longer(cols = c("Total_Previous_Cancellations", "Total_Previous_Bookings_Not_Canceled"),
               names_to = "Metric", values_to = "Count")

# Bar plot
ggplot(long_counts, aes(x = hotel, y = Count, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Total Previous Cancellations and Bookings Not Canceled by Hotel Type",
    x = "Hotel Type",
    y = "Total Count",
    fill = "Metric"
  ) +
  theme_light() +
  scale_fill_manual(values = c("Total_Previous_Cancellations" = "red", "Total_Previous_Bookings_Not_Canceled" = "green"))

# 3 summarize the hotel variable based on year
hotel_by_year <- hotel_data %>%
  group_by(arrival_date_year, hotel) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Proportion = Count / sum(Count))

ggplot(hotel_by_year, aes(x = factor(arrival_date_year), y = Count, fill = hotel)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Hotel Bookings by Year",
    x = "Year",
    y = "Number of Bookings",
    fill = "Hotel Type"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  scale_fill_manual(values = c("City Hotel" = "blue", "Resort Hotel" = "orange"))

# 4 Bookings by month
bookings_by_month <- hotel_data %>%
  group_by(arrival_date_month) %>%
  summarise(Count = n(), .groups = "drop")

print(bookings_by_month)

# Reorder months in the correct order
bookings_by_month <- bookings_by_month %>%
  mutate(arrival_date_month = factor(
    arrival_date_month,
    levels = c("January", "February", "March", "April", "May", "June",
               "July", "August", "September", "October", "November", "December")
  ))


ggplot(bookings_by_month, aes(x = arrival_date_month, y = Count, fill = arrival_date_month)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of Bookings by Month",
    x = "Month",
    y = "Number of Bookings",
    fill = "Month"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set3")  # Use a colorful palette

# 5 find the unique countries
# Find unique countries
unique_countries <- unique(hotel_data$country)

# Print the unique countries
print(unique_countries)
# Count the total number of unique countries
num_unique_countries <- length(unique_countries)

# Print the total count
cat("Number of unique countries:", num_unique_countries, "\n")

# 6 Number of bookings based on countries

# Count bookings by country
bookings_by_country <- hotel_data %>%
  group_by(country) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))

# View the top 10 countries
head(bookings_by_country, 10)

# Count bookings by country and select the top 20
top_20_countries <- hotel_data %>%
  group_by(country) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count)) %>%
  top_n(20, Count)

# Define a custom color palette with 20 distinct colors and randomize the order
set.seed(123)  # Set seed for reproducibility
custom_colors <- sample(c(
  "red", "blue", "green", "orange", "purple", 
  "cyan", "magenta", "yellow", "brown", "pink", 
  "turquoise", "gold", "darkgreen", "darkblue", "darkred", 
  "lightblue", "lightgreen", "lavender", "gray", "black"
))

# Bar plot for top 20 countries
p <- ggplot(top_20_countries, aes(x = reorder(country, Count), y = Count, fill = country)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 20 Countries by Number of Bookings",
    x = "Country",
    y = "Number of Bookings",
    fill = "Country"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  scale_fill_manual(values = custom_colors)

# Save the plot to a file
ggsave("top_20_countries_bookings.png", plot = p, width = 10, height = 6)

# 7 check outliers for average daily rate (adr) based on hotel types
# Boxplot of ADR by hotel type
p <- ggplot(hotel_data, aes(x = hotel, y = adr, fill = hotel)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +  # Highlight outliers in red
  labs(
    title = "ADR Outliers by Hotel Type",
    x = "Hotel Type",
    y = "Average Daily Rate (ADR)"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  scale_fill_manual(values = c("City Hotel" = "blue", "Resort Hotel" = "orange"))

# Save the plot
ggsave("adr_outliers_by_hotel.png", plot = p, width = 10, height = 6)

library(dplyr)

# Summarize ADR statistics by hotel type
adr_summary <- hotel_data %>%
  group_by(hotel) %>%
  summarise(
    Min = min(adr, na.rm = TRUE),
    Q1 = quantile(adr, 0.25, na.rm = TRUE),
    Median = median(adr, na.rm = TRUE),
    Q3 = quantile(adr, 0.75, na.rm = TRUE),
    Max = max(adr, na.rm = TRUE),
    IQR = IQR(adr, na.rm = TRUE)
  )

# View summary statistics
print(adr_summary)

# Calculate IQR and boundaries for outliers by hotel type
outlier_data <- hotel_data %>%
  group_by(hotel) %>%
  summarise(
    Q1 = quantile(adr, 0.25, na.rm = TRUE),
    Q3 = quantile(adr, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    Lower_Bound = Q1 - 1.5 * IQR,
    Upper_Bound = Q3 + 1.5 * IQR
  ) %>%
  left_join(hotel_data, by = "hotel") %>%
  filter(adr < Lower_Bound | adr > Upper_Bound)

# View outliers
print(outlier_data)
# Summarize outliers by hotel type
outlier_summary <- outlier_data %>%
  group_by(hotel) %>%
  summarise(
    Total_Outliers = n(),
    Min_ADR = min(adr),
    Max_ADR = max(adr),
    Avg_ADR = mean(adr)
  )

# View summary
print(outlier_summary)
# Convert to a data frame
outlier_summary_df <- as.data.frame(outlier_summary)

# Print the data frame
print(outlier_summary_df)

# 8 Check the average daily rate (adr) vs hotel.
# Summarize ADR by hotel type
adr_vs_hotel_summary <- hotel_data %>%
  group_by(hotel) %>%
  summarise(
    Mean_ADR = mean(adr, na.rm = TRUE),
    Median_ADR = median(adr, na.rm = TRUE),
    Min_ADR = min(adr, na.rm = TRUE),
    Max_ADR = max(adr, na.rm = TRUE),
    Std_Dev_ADR = sd(adr, na.rm = TRUE)
  )

adr_vs_hotel_summary_df <- as.data.frame(adr_vs_hotel_summary)

# 9 Customer type vs Hotel Type
# Count customer types for each hotel 
customer_vs_hotel_summary <- hotel_data %>%
  group_by(hotel, customer_type) %>%
  summarise(Count = n(), .groups = "drop")

# View the summary table
customer_vs_hotel_summary_df <- as.data.frame(customer_vs_hotel_summary)
# Bar chart for customer type vs hotel
ggplot(customer_vs_hotel_summary, aes(x = hotel, y = Count, fill = customer_type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Customer Type Distribution by Hotel Type",
    x = "Hotel Type",
    y = "Number of Bookings",
    fill = "Customer Type"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set3")
ggsave("customer_type_vs_hotel_stacked.png", width = 10, height = 6)

# 10 customer type vs special request
requests_summary <- hotel_data %>%
  group_by(customer_type) %>%
  summarise(
    Mean_Requests = mean(total_of_special_requests, na.rm = TRUE),
    Median_Requests = median(total_of_special_requests, na.rm = TRUE),
    Max_Requests = max(total_of_special_requests, na.rm = TRUE),
    Total_Bookings = n()
  )
requests_summary_df <- as.data.frame(requests_summary)

# Bar plot of mean special requests by customer type
ggplot(requests_summary, aes(x = customer_type, y = Mean_Requests, fill = customer_type)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(
    title = "Average Special Requests by Customer Type",
    x = "Customer Type",
    y = "Average Number of Special Requests",
    fill = "Customer Type"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set2")
ggsave("average_special_requests_by_customer_type_barplot.png", width = 10, height = 6)

# 11. Hotel preference by customer type

hotel_preference_summary <- hotel_data %>%
  group_by(customer_type, hotel) %>%
  summarise(Count = n(), .groups = "drop") 

# View the summary
hotel_preference_summary_df <- as.data.frame(hotel_preference_summary)

ggplot(hotel_preference_summary, aes(x = customer_type, y = Count, fill = hotel)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Hotel Preference by Customer Type",
    x = "Customer Type",
    y = "Number of Bookings",
    fill = "Hotel Type"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  scale_fill_manual(values = c("City Hotel" = "blue", "Resort Hotel" = "orange"))
ggsave("hotel_preference_by_customer_type_stacked.png", width = 10, height = 6)

# 12 discuss the correlation of dataset
# only numericals are chosen for this

# Select only numerical variables
numerical_data <- hotel_data %>%
  select(arrival_date_year, arrival_date_week_number, arrival_date_day_of_month,
         stays_in_weekend_nights, stays_in_week_nights, adults, children, babies,
         previous_cancellations, previous_bookings_not_canceled, booking_changes,
         days_in_waiting_list, adr, required_car_parking_spaces, total_of_special_requests)

# Calculate the correlation matrix
correlation_matrix <- cor(numerical_data, use = "complete.obs")

# View the correlation matrix
print(correlation_matrix)


library(ggcorrplot)

# Plot the correlation matrix with enhanced readability
ggcorrplot(correlation_matrix,
           method = "circle",
           type = "lower",
           lab = TRUE,
           title = "Correlation Matrix of Numerical Variables",
           colors = c("red", "white", "blue"),
           lab_size = 4,              # Increase label size
           ggtheme = theme_minimal()) # Use a light, minimal theme


ggsave("correlation_matrix_heatmap_readable.png", width = 10, height = 8)


# 13. pick any two variables and fit a regression line (ADR and children)
# Fit the linear regression model
adr_children_model <- lm(adr ~ children, data = hotel_data)

# Summarize the model
summary(adr_children_model)


ggplot(hotel_data, aes(x = children, y = adr)) +
  geom_jitter(alpha = 0.5, color = "blue", width = 0.2, height = 10) +  # Add jitter
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regression line
  scale_y_continuous(limits = c(0, 500)) +  # Focus on ADR values between 0 and 500
  labs(
    title = "Regression Line: ADR vs Children (Zoomed In)",
    x = "Number of Children",
    y = "Average Daily Rate (ADR)"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  )



ggsave("adr_vs_children_zoomed.png", width = 10, height = 6)



# 14. Find the average daily rate trend over the three years 

# Summarize average ADR by year
adr_trend_df <- hotel_data %>%
  group_by(arrival_date_year) %>%
  summarise(Average_ADR = mean(adr, na.rm = TRUE))
  as.data.frame()
adr_trend_df$Average_ADR <- round(adr_trend_df$Average_ADR, 2)
  
  


# Code needed to complete the PowerPoint


# Compute the correlation between two variables
correlation_value <- cor(hotel_data$stays_in_weekend_nights, hotel_data$stays_in_week_nights, use = "complete.obs")

# Print the correlation value
print(correlation_value)


scatter_plot <- ggplot(hotel_data, aes(x = stays_in_weekend_nights, y = stays_in_week_nights)) +
  geom_point(alpha = 0.5, color = "blue") +  # Scatter points
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Correlation: Weekend Nights vs Week Nights",
    x = "Stays in Weekend Nights",
    y = "Stays in Week Nights"
  ) +
  theme_minimal()
ggsave("weekend_vs_week_nights_correlation.png", plot = scatter_plot, width = 8, height = 6)



###

adr_children_plot <- ggplot(hotel_data, aes(x = children, y = adr)) +
  geom_point(alpha = 0.5, color = "blue") +  # Scatter points
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Correlation: ADR vs Children",
    x = "Number of Children",
    y = "Average Daily Rate (ADR)"
  ) +
  theme_minimal()

ggsave("adr_vs_children_correlation.png", plot = adr_children_plot, width = 8, height = 6)


# Filter out the outlier where ADR is $5400
filtered_data <- hotel_data %>% filter(adr < 5400)


adr_children_plot_filtered <- ggplot(filtered_data, aes(x = children, y = adr)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Correlation: ADR vs Children (Outlier Removed)",
    x = "Number of Children",
    y = "Average Daily Rate (ADR)"
  ) +
  theme_minimal()


ggsave("adr_vs_children_correlation_filtered.png", plot = adr_children_plot_filtered, width = 8, height = 6)

# Find correlation value now without the outlier
correlation_value <- cor(filtered_data$adr, filtered_data$children, use = "complete.obs")
print(correlation_value)



###
adr_cancellations_plot <- ggplot(hotel_data, aes(x = previous_cancellations, y = adr)) +
  geom_point(alpha = 0.5, color = "blue") +  
  geom_smooth(method = "lm", color = "red", se = TRUE) +  
  labs(
    title = "Correlation: ADR vs Previous Cancellations",
    x = "Previous Cancellations",
    y = "Average Daily Rate (ADR)"
  ) +
  theme_minimal()

# Save the plot to a file
ggsave("adr_vs_previous_cancellations.png", plot = adr_cancellations_plot, width = 8, height = 6)


# Without the outlier
filtered_data <- hotel_data %>% filter(adr < 5400)


adr_cancellations_plot <- ggplot(filtered_data, aes(x = previous_cancellations, y = adr)) +
  geom_point(alpha = 0.5, color = "blue") +  # Scatter points
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regression line
  labs(
    title = "Correlation: ADR vs Previous Cancellations",
    x = "Previous Cancellations",
    y = "Average Daily Rate (ADR)"
  ) +
  theme_minimal()

# Save the plot to a file
ggsave("adr_vs_previous_cancellations_filtered.png", plot = adr_cancellations_plot, width = 8, height = 6)


### 

year_week_correlation_plot <- ggplot(hotel_data, aes(x = arrival_date_year, y = arrival_date_week_number)) +
  geom_point(alpha = 0.5, color = "blue") +  # Scatter points
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regression line
  labs(
    title = "Correlation: Arrival Year vs Week Number (𝑟 = −0.541)",
    x = "Arrival Year",
    y = "Arrival Week Number"
  ) +
  theme_minimal()

# Save the plot to a file
ggsave("arrival_year_vs_week_number_correlation.png", plot = year_week_correlation_plot, width = 8, height = 6)

###


waiting_special_requests_plot <- ggplot(hotel_data, aes(x = days_in_waiting_list, y = total_of_special_requests)) +
  geom_point(alpha = 0.5, color = "blue") +  # Scatter points
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Regression line
  labs(
    title = "Correlation: Days in Waiting List vs Total of Special Requests (𝑟 = −0.083)",
    x = "Days in Waiting List",
    y = "Total of Special Requests"
  ) +
  theme_minimal()

# Save the plot to a file
ggsave("waiting_list_vs_special_requests_correlation.png", plot = waiting_special_requests_plot, width = 8, height = 6)



